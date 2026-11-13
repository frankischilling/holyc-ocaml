open Yojson.Safe.Util

let require condition message = if not condition then failwith message

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let with_file suffix contents action =
  let path = Filename.temp_file "holyc native source " suffix in
  Fun.protect
    ~finally:(fun () -> if Sys.file_exists path then Sys.remove path)
    (fun () ->
      let channel = open_out_bin path in
      Fun.protect
        ~finally:(fun () -> close_out channel)
        (fun () -> output_string channel contents);
      action path)

let () =
  require
    (Array.length Sys.argv = 4)
    "usage: test_native_source_cli.exe <holyc.exe> \
     <native-source-initializers.hc> <native-source-arrays.hc>"

let compiler = Sys.argv.(1)
let fixture = Sys.argv.(2)
let array_fixture = Sys.argv.(3)

let invoke expected arguments =
  with_file ".out" "" (fun stdout ->
      with_file ".err" "" (fun stderr ->
          let out_fd =
            Unix.openfile stdout [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
          in
          let err_fd =
            Unix.openfile stderr [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
          in
          let pid =
            Fun.protect
              ~finally:(fun () ->
                Unix.close out_fd;
                Unix.close err_fd)
              (fun () ->
                Unix.create_process compiler
                  (Array.of_list (compiler :: arguments))
                  Unix.stdin out_fd err_fd)
          in
          let _, status = Unix.waitpid [] pid in
          let stdout = read stdout and stderr = read stderr in
          require
            (status = Unix.WEXITED expected)
            (Printf.sprintf "unexpected CLI exit for %s\n%s\n%s"
               (String.concat " " arguments)
               stdout stderr);
          (stdout, stderr)))

let json_path ?(status = 0) ?(target = "host-jit-task") ?(mode = "jit")
    ?(options = []) path =
  let stdout, stderr =
    invoke status
      ([ "run"; "--target=" ^ target; "--mode=" ^ mode; "--format=json" ]
      @ options @ [ path ])
  in
  require (stderr = "") ("JSON CLI wrote stderr: " ^ stderr);
  Yojson.Safe.from_string stdout

let final_bits json = json |> member "final_value" |> member "bits" |> to_string
let fragments json = json |> member "native" |> member "fragments" |> to_list

let () =
  let baseline = json_path fixture in
  require
    (baseline |> member "schema" |> to_string = "holyc-integer-program-v2")
    "report schema";
  require
    (baseline |> member "target" |> to_string = "host-jit-task")
    "explicit native task target";
  require
    (baseline |> member "arithmetic" |> to_string = "runtime-native")
    "native arithmetic evidence";
  require (final_bits baseline = "0x000000000000002a") "native source result 42";
  require
    (baseline |> member "native" |> member "image" = `Null)
    "task is not one isolated image";
  let images = fragments baseline in
  require
    (List.length images = 3)
    "two original live leaves and one original resumed expression";
  require
    (List.map (fun fragment -> fragment |> member "kind" |> to_string) images
    = [ "initializer"; "initializer"; "command" ])
    "source fragment order";
  require
    (List.map
       (fun fragment -> fragment |> member "global_bytes" |> to_int)
       images
    = [ 8; 16; 16 ])
    "original cumulative native global allocation";
  List.iter
    (fun fragment ->
      require
        (fragment |> member "outcome" |> to_string = "success")
        "actual native fragment completion")
    images;
  let steps = baseline |> member "executed_steps" |> to_int in
  require (steps > 1) "nonzero actual native work";
  require
    (List.hd (List.rev images) |> member "executed_steps" |> to_int = steps)
    "fragment and task counters agree";
  let exact =
    json_path ~options:[ "--step-limit=" ^ string_of_int steps ] fixture
  in
  require
    (final_bits exact = final_bits baseline)
    "exact CLI native step budget";
  let stopped =
    json_path ~status:1
      ~options:[ "--step-limit=" ^ string_of_int (steps - 1) ]
      fixture
  in
  require
    (stopped |> member "executed_steps" |> to_int = steps - 1)
    "one-below cumulative CLI steps";
  require
    (stopped |> member "final_value" = `Null)
    "fault does not invent a final value";
  require
    (List.hd (List.rev (fragments stopped))
    |> member "outcome" |> to_string = "fault")
    "CLI preserves real native fault";
  let ir = json_path ~target:"ir" fixture in
  require (final_bits ir = final_bits baseline) "independent IR value";
  let aot = json_path ~target:"host-jit" ~mode:"aot" fixture in
  require
    (final_bits aot = final_bits baseline)
    "existing isolated AOT native path";
  let isolated_jit = json_path ~status:1 ~target:"host-jit" fixture in
  require
    (isolated_jit |> member "outcome" |> to_string = "error")
    "isolated JIT contract remains explicit";
  let task_aot = json_path ~mode:"aot" fixture in
  require
    (final_bits task_aot = "0x000000000000002a")
    "AOT task target module result";
  require (List.length (fragments task_aot) = 1) "distinct AOT module image";
  let invalid = json_path ~status:1 ~options:[ "--step-limit=0" ] fixture in
  require
    (invalid |> member "command_error" |> member "code" |> to_string
   = "HCIRVM0001")
    "CLI invalid bound";
  with_file ".hc" "I64 A; I64 B=A+1; B;" (fun path ->
      let fault = json_path ~status:1 path in
      require
        (fault |> member "diagnostics" |> to_list
        |> List.exists (fun diagnostic ->
            diagnostic |> member "code" |> to_string = "HCIRVM0012"))
        "native uninitialized read reaches CLI");
  with_file ".hc" "I64 A=40; I64 B=++A; A+B-40;" (fun path ->
      require
        (final_bits (json_path path) = "0x000000000000002a")
        "original initializer side effect executes once");
  with_file ".hc" "U64 A=0x8000000000000000; U64 B=A+1; B;" (fun path ->
      let unsigned = json_path path in
      require
        (final_bits unsigned = "0x8000000000000001")
        "native full unsigned CLI bits";
      require
        (unsigned |> member "final_value" |> member "type" |> to_string = "u64")
        "native unsigned CLI result class");
  let stdout, _ =
    invoke 0 [ "run"; "--target=host-jit-task"; "--format=human"; fixture ]
  in
  require
    (String.split_on_char '\n' stdout
    |> List.exists (String.starts_with ~prefix:"native-fragments=3"))
    "human report identifies individual native fragments";
  let arrays = json_path array_fixture in
  require (final_bits arrays = "0x000000000000002a") "native array result 42";
  let array_fragments = fragments arrays in
  require (List.length array_fragments = 4) "three array leaves and one command";
  require
    (List.map
       (fun fragment -> fragment |> member "global_bytes" |> to_int)
       array_fragments
    = [ 16; 16; 24; 24 ])
    "array logical data grows only for new declarations";
  require
    (List.map
       (fun fragment -> fragment |> member "global_arena_bytes" |> to_int)
       array_fragments
    = [ 32; 32; 48; 48 ])
    "array byte flags retain their original offsets";
  require
    (final_bits (json_path ~target:"ir" array_fixture) = final_bits arrays)
    "independent interpreted array result";
  let array_steps = arrays |> member "executed_steps" |> to_int in
  require
    (final_bits
       (json_path
          ~options:[ "--step-limit=" ^ string_of_int array_steps ]
          array_fixture)
    = final_bits arrays)
    "exact native array step limit";
  let array_fault =
    json_path ~status:1
      ~options:[ "--step-limit=" ^ string_of_int (array_steps - 1) ]
      array_fixture
  in
  require
    (array_fault |> member "executed_steps" |> to_int = array_steps - 1)
    "array quota preserves reached work";
  with_file ".hc" "U8 A[2]={257,41}; A[0]+A[1];" (fun path ->
      let narrow = json_path ~options:[ "--global-byte-limit=2" ] path in
      require
        (final_bits narrow = "0x000000000000002a")
        "narrow array fits its declared-byte limit";
      require
        (List.hd (fragments narrow)
        |> member "global_arena_bytes"
        |> to_int = 18)
        "narrow array privately reserves all element flags");
  with_file ".hc" "I64 A[2]; A[0]=42; A[1];" (fun path ->
      let fault = json_path ~status:1 path in
      require
        (fault |> member "diagnostics" |> to_list
        |> List.exists (fun diagnostic ->
            diagnostic |> member "code" |> to_string = "HCIRVM0012"))
        "unwritten array element remains uninitialized");
  with_file ".hc" "I64 A[2]={41,1}; A[2];" (fun path ->
      let fault = json_path ~status:1 path in
      require
        (List.hd (List.rev (fragments fault))
        |> member "outcome" |> to_string = "fault")
        "array bounds fault reaches native CLI");
  let stdout, _ =
    invoke 0
      [ "run"; "--target=host-jit-task"; "--format=human"; array_fixture ]
  in
  require
    (String.split_on_char '\n' stdout
    |> List.exists (String.starts_with ~prefix:"native-fragments=4"))
    "human report preserves original array fragments";
  print_endline "Native source CLI: 20 executions passed."

(* Hosted controls: these do not add an original TempleOS capture. *)
let () =
  let cases =
    [
      ("1.5", [ 0; 0; 0; 0; 0; 0; 248; 63 ]);
      ("-0.0", [ 0; 0; 0; 0; 0; 0; 0; 128 ]);
      ("1.5+0.5", [ 0; 0; 0; 0; 0; 0; 0; 64 ]);
      ("2.5/2.0", [ 0; 0; 0; 0; 0; 0; 244; 63 ]);
    ]
  in
  List.iter
    (fun (expression, bytes) ->
      let checks =
        List.mapi
          (fun index byte -> Printf.sprintf "(&b)(U8*)[%d]==%d" index byte)
          bytes
        |> String.concat " && "
      in
      let source =
        Printf.sprintf "class Box{F64 x;I64 y;};Box b={%s,42};(%s) && b.y==42;"
          expression checks
      in
      with_file ".hc" source (fun path ->
          let report = json_path path in
          require
            (member "outcome" report = `String "success"
            && final_bits report = "0x0000000000000001"
            && member "diagnostics" report = `List [])
            ("exact F64 leaf bytes and neighbor: " ^ expression);
          List.iter
            (fun (target, mode) ->
              let result = json_path ~target ~mode ~status:1 path in
              require
                (member "outcome" result = `String "error")
                "unimplemented floating initializer target stays gated")
            [ ("ir", "jit"); ("ir", "aot"); ("host-jit-task", "aot") ]))
    cases;
  List.iter
    (fun source ->
      with_file ".hc" source (fun path ->
          let result = json_path ~status:1 path in
          require
            (member "outcome" result = `String "error")
            "cross-class initializer conversion remains gated"))
    [
      "class Box{F64 x;I64 y;};Box b={1,42};42;";
      "class Box{I64 x;I64 y;};Box b={1.5,42};42;";
    ]

let () =
  let source =
    "F64 Value(){return 1.5;}class Box{F64 x;I64 y;};Box \
     b={Value(),42};(&b)(U8*)[7]*256+(&b)(U8*)[6];"
  in
  with_file ".hc" source (fun path ->
      let baseline = json_path path in
      require
        (member "outcome" baseline = `String "success"
        && final_bits baseline = "0x0000000000003ff8")
        "same-class floating call result reaches its leaf";
      let steps = member "compiled_initializer_steps" baseline |> to_int in
      require (steps > 0) "floating initialization records real work";
      let exact =
        json_path
          ~options:[ "--initializer-step-limit=" ^ string_of_int steps ]
          path
      in
      require
        (member "outcome" exact = `String "success"
        && final_bits exact = final_bits baseline)
        "exact floating initializer budget";
      let limited =
        json_path ~status:1
          ~options:[ "--initializer-step-limit=" ^ string_of_int (steps - 1) ]
          path
      in
      require
        (member "outcome" limited = `String "error"
        && member "diagnostics" limited
           |> to_list
           |> List.exists (fun d -> member "code" d = `String "HCIRVM0007"))
        "one-below floating initializer budget faults")

let () =
  with_file ".hc"
    "extern U0 Print(U8 *fmt,...);F64 Value(){Print(\"A\");return 1.5;}class \
     Box{F64 x;I64 y;};Box b={Value(),42};Print(\"B\"); (&b)(U8*)[6];"
    (fun path ->
      let report = json_path ~options:[ "--code-byte-limit=524288" ] path in
      require
        (member "outcome" report = `String "success"
        && member "output_hex" report = `String "4142"
        && final_bits report = "0x00000000000000f8")
        "floating initializer effects run once before later commands")
