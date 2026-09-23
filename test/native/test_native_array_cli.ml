open Yojson.Safe.Util
module Runtime = Holyc_lib.Native_program_execution

let require condition message = if not condition then failwith message

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let with_file suffix contents action =
  let path = Filename.temp_file "holyc native array cli " suffix in
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
    (Array.length Sys.argv = 6)
    "usage: test_native_array_cli.exe <holyc.exe> <layout.hc> \
     <integer-arrays.hc> <aliases.hc> <persistent-arrays.hc>"

let compiler = Sys.argv.(1)
let layout_fixture = Sys.argv.(2)
let integer_array_fixture = Sys.argv.(3)
let alias_fixture = Sys.argv.(4)
let persistent_fixture = Sys.argv.(5)

let invoke arguments =
  with_file ".stdout" "" (fun stdout ->
      with_file ".stderr" "" (fun stderr ->
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
          (status, read stdout, read stderr)))

let status_name = function
  | Unix.WEXITED code -> Printf.sprintf "exit %d" code
  | Unix.WSIGNALED signal -> Printf.sprintf "signal %d" signal
  | Unix.WSTOPPED signal -> Printf.sprintf "stopped %d" signal

let checked_json expected arguments =
  let status, stdout, stderr = invoke arguments in
  require
    (status = Unix.WEXITED expected)
    (Printf.sprintf "%s: expected exit %d, got %s\nstdout: %s\nstderr: %s"
       (String.concat " " arguments)
       expected (status_name status) stdout stderr);
  require (stderr = "") ("JSON report escaped diagnostics to stderr: " ^ stderr);
  Yojson.Safe.from_string stdout

let host_json ?(status = 0) ?(options = []) ~mode source =
  let report =
    checked_json status
      ([ "run"; "--target=host-jit"; "--format=json"; "--mode=" ^ mode ]
      @ options @ [ source ])
  in
  require
    (report |> member "schema" |> to_string = "holyc-integer-program-v2"
    && report |> member "target" |> to_string = "host-jit"
    && report |> member "mode" |> to_string = mode
    && report |> member "arithmetic" |> to_string = "runtime-native")
    "host-jit array report identity";
  let expected_platform =
    match Runtime.platform () with
    | Runtime.Windows_x86_64 -> "windows-x86_64"
    | Runtime.Linux_x86_64 -> "linux-x86_64"
    | Runtime.Unsupported -> failwith "native array CLI tests require x86-64"
  in
  require
    (report |> member "native" |> member "platform" |> to_string
   = expected_platform)
    "host-jit array report platform";
  report

let ir_json ?(status = 0) ?(options = []) ~mode source =
  checked_json status
    ([
       "run";
       "--target=ir";
       "--report-version=2";
       "--format=json";
       "--mode=" ^ mode;
     ]
    @ options @ [ source ])

let diagnostics report = report |> member "diagnostics" |> to_list

let first_diagnostic report =
  match diagnostics report with
  | first :: _ -> first
  | [] -> failwith "expected an array diagnostic"

let first_code report = first_diagnostic report |> member "code" |> to_string

let check_success report =
  require
    (report |> member "outcome" |> to_string = "success"
    && report |> member "termination" |> to_string = "stream-end"
    && diagnostics report = []
    && member "command_error" report = `Null)
    "successful array report"

let check_word report type_ value bits =
  let word = member "final_value" report in
  require
    (word |> member "type" |> to_string = type_
    && word |> member "value" |> to_string = value
    && word |> member "bits" |> to_string = bits)
    ("unexpected array final word: " ^ Yojson.Safe.to_string word)

let check_native_ir_success ~mode ~label source =
  let native = host_json ~mode source in
  let interpreted = ir_json ~mode source in
  check_success native;
  check_success interpreted;
  check_word native "i64" "42" "0x000000000000002a";
  check_word interpreted "i64" "42" "0x000000000000002a";
  require
    (member "executed_steps" native = member "executed_steps" interpreted)
    (label ^ " native work differs from checked IR");
  require
    (member "dimension_preparation_work" native
    = member "dimension_preparation_work" interpreted)
    (label ^ " dimension work differs from checked IR");
  native

let width_source type_name =
  Printf.sprintf
    "I64 F(){%s a[3];a[0]=13;a[2]=29;a[1]=42;return a[0]+a[1]+a[2]-42;}F();"
    type_name

let () =
  List.iter
    (fun mode ->
      let report = host_json ~mode layout_fixture in
      check_success report;
      check_word report "i64" "42" "0x000000000000002a";
      check_word (ir_json ~mode layout_fixture) "i64" "42" "0x000000000000002a";
      let work = report |> member "dimension_preparation_work" |> to_int in
      require (work = 2) "two original dimension expressions";
      require
        (report |> member "compiled_initializer_steps" |> to_int = 0)
        "dimension work stays separate from initializers";
      check_success
        (host_json ~mode ~options:[ "--dimension-work-limit=2" ] layout_fixture);
      let failed =
        host_json ~status:1 ~mode
          ~options:[ "--dimension-work-limit=1" ]
          layout_fixture
      in
      require
        (failed |> member "dimension_preparation_work" |> to_int = 1)
        "failed dimension work is retained";
      require
        (member "executed_steps" failed = `Null)
        "dimension exhaustion precedes native entry";
      let status, stdout, stderr =
        invoke [ "run"; "--target=host-jit"; "--mode=" ^ mode; layout_fixture ]
      in
      require (status = Unix.WEXITED 0 && stderr = "") "human array report";
      require
        (String.split_on_char '\n' stdout
        |> List.exists (fun line ->
            String.trim line = "dimension-preparation-work=2"))
        "human dimension count";
      with_file ".hc" "I64 F(){I8 a[1+2][7;return 42;}F();" (fun path ->
          let failed = host_json ~status:1 ~mode path in
          require
            (failed |> member "dimension_preparation_work" |> to_int = 4)
            "closing bracket failure retains original preparation";
          require
            (member "executed_steps" failed = `Null)
            "parse failure has no entry");
      List.iter
        (fun (label, source) ->
          ignore (check_native_ir_success ~mode ~label source))
        [
          ("maintained integer arrays", integer_array_fixture);
          ("same-producer alias loop", alias_fixture);
        ];
      List.iter
        (fun type_name ->
          with_file ".hc" (width_source type_name) (fun path ->
              ignore
                (check_native_ir_success ~mode
                   ~label:(type_name ^ " automatic array")
                   path)))
        [ "I8"; "U8"; "I16"; "U16"; "I32"; "U32"; "I64"; "U64" ];
      let maintained =
        check_native_ir_success ~mode ~label:"array runtime quota"
          integer_array_fixture
      in
      let steps = maintained |> member "executed_steps" |> to_int in
      check_success
        (host_json ~mode
           ~options:[ "--step-limit=" ^ string_of_int steps ]
           integer_array_fixture);
      let one_below =
        host_json ~status:1 ~mode
          ~options:[ "--step-limit=" ^ string_of_int (steps - 1) ]
          integer_array_fixture
      in
      require
        (first_code one_below = "HCIRVM0007"
        && one_below |> member "executed_steps" |> to_int = steps - 1)
        "array step quota one below";
      with_file ".hc" "I64 F(){I64 a[2];a[1]=42;I64 *p=&a[2];return p[-1];}F();"
        (fun path ->
          ignore
            (check_native_ir_success ~mode ~label:"one-past materialization"
               path));
      List.iter
        (fun (label, expected, source) ->
          with_file ".hc" source (fun path ->
              let native = host_json ~status:1 ~mode path in
              let interpreted = ir_json ~status:1 ~mode path in
              require
                (first_code native = expected
                && first_code interpreted = expected)
                (label ^ " diagnostic code");
              require
                (member "executed_steps" native
                = member "executed_steps" interpreted)
                (label ^ " executed work")))
        [
          ( "per-element unknown state",
            "HCIRVM0012",
            "I64 F(){I64 a[2];a[0]=42;return a[1];}F();" );
          ( "unsigned index overflow",
            "HCIRVM0020",
            "I64 F(){I64 a[2];U64 i=-1;return a[i];}F();" );
          ( "past-one-past materialization",
            "HCIRVM0019",
            "I64 F(){I64 a[2];I64 *p=&a[3];return 42;}F();" );
          ( "RHS fault precedes final bounds",
            "HCIRVM0009",
            "I64 F(){I64 a[2];a[2]=1/0;return 42;}F();" );
        ];
      let persistent =
        check_native_ir_success ~mode ~label:"persistent array source gate"
          persistent_fixture
      in
      require
        (persistent |> member "executed_steps" |> to_int = 101
        && persistent |> member "compiled_initializer_steps" |> to_int = 21
        && persistent |> member "dimension_preparation_work" |> to_int = 4)
        "persistent native source work";
      require
        (ir_json ~mode persistent_fixture
        |> member "compiled_initializer_steps"
        |> to_int = 23)
        "ordinary static preparation retains its frame-exit work";
      let image = persistent |> member "native" |> member "image" in
      let globals = image |> member "global_bytes" |> to_int in
      let literals = image |> member "literal_bytes" |> to_int in
      let metadata = image |> member "arena_metadata_bytes" |> to_int in
      require
        (globals = 19 && literals = 3 && metadata > 0
        && image
           |> member "global_arena_bytes"
           |> to_int
           = globals + literals + metadata)
        "persistent data and private metadata accounting";
      check_success
        (host_json ~mode
           ~options:
             [
               "--initializer-step-limit=21";
               "--dimension-work-limit=4";
               "--global-byte-limit=19";
               "--literal-byte-limit=3";
             ]
           persistent_fixture);
      List.iter
        (fun option ->
          let failed =
            host_json ~status:1 ~mode ~options:[ option ] persistent_fixture
          in
          require
            (member "executed_steps" failed = `Null)
            (option ^ " must fail before native execution"))
        [
          "--initializer-step-limit=20";
          "--dimension-work-limit=3";
          "--global-byte-limit=18";
          "--literal-byte-limit=2";
        ];
      with_file ".hc"
        "I64 F(){U8 *s=\"41\";s[1]++;return (s[0]-48)*10+s[1]-48;}F();"
        (fun path ->
          ignore (check_native_ir_success ~mode ~label:"mutable literal" path);
          check_success
            (host_json ~mode ~options:[ "--literal-byte-limit=3" ] path);
          let failed =
            host_json ~status:1 ~mode ~options:[ "--literal-byte-limit=2" ] path
          in
          require
            (member "executed_steps" failed = `Null)
            "one-below literal bytes must reject before native entry");
      List.iter
        (fun (label, source) ->
          with_file ".hc" source (fun path ->
              ignore (check_native_ir_success ~mode ~label path)))
        [
          ("global initializer", "I16 a[2]={40,2};a[0]+a[1];");
          ( "static initializer",
            "I64 F(){static U8 a[2]={40,2};return a[0]+a[1];}F();" );
          ( "byte initializer terminator",
            "U8 a[3]=\"41\";a[1]++;(a[0]-48)*10+a[1]-48+a[2];" );
          ("flat persistent extent", "I16 a[2][3];a[0][3]=42;a[1][0];");
        ])
    [ "jit"; "aot" ]
