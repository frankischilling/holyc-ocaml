open Yojson.Safe.Util

let require condition message = if not condition then failwith message

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let with_file suffix contents action =
  let path = Filename.temp_file "holyc runtime layout " suffix in
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
    (Array.length Sys.argv = 3
    || (Array.length Sys.argv = 5 && Sys.argv.(3) = "--native"))
    "expected compiler and runtime-layout example, optionally --native \
     F64-example"

let compiler = Sys.argv.(1)
let example = Sys.argv.(2)
let native = Array.length Sys.argv = 5

let invoke ?(status = 0) ?(options = []) target mode path =
  let args =
    [
      "run";
      "--report-version=2";
      "--format=json";
      "--target=" ^ target;
      "--mode=" ^ mode;
    ]
    @ (if target = "host-jit-task" then [ "--code-byte-limit=1048576" ] else [])
    @ options @ [ path ]
  in
  with_file ".stdout" "" (fun stdout ->
      with_file ".stderr" "" (fun stderr ->
          let out_fd =
            Unix.openfile stdout [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
          and err_fd =
            Unix.openfile stderr [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
          in
          let pid =
            Fun.protect
              ~finally:(fun () ->
                Unix.close out_fd;
                Unix.close err_fd)
              (fun () ->
                Unix.create_process compiler
                  (Array.of_list (compiler :: args))
                  Unix.stdin out_fd err_fd)
          in
          let _, actual = Unix.waitpid [] pid in
          let output = read stdout in
          require
            (actual = Unix.WEXITED status)
            ("unexpected CLI exit: " ^ output ^ read stderr);
          require (read stderr = "") "JSON diagnostics escaped to stderr";
          Yojson.Safe.from_string output))

let word bits report =
  require
    (member "outcome" report = `String "success")
    "successful runtime-layout report";
  require
    (member "diagnostics" report = `List [])
    "successful runtime-layout diagnostics";
  require
    (member "command_error" report = `Null)
    "successful runtime-layout command";
  require
    (report |> member "final_value" |> member "value" = `String bits)
    ("unexpected final runtime-layout word: " ^ Yojson.Safe.to_string report)

let error code report =
  require
    (member "outcome" report = `String "error")
    "failed runtime-layout report";
  let first = report |> member "diagnostics" |> to_list |> List.hd in
  require
    (member "code" first = `String code)
    ("unexpected runtime-layout error: " ^ Yojson.Safe.to_string report)

let closed_aot_float () =
  with_file ".hc" "I64 A[1.25+2.5];sizeof(A);" (fun path ->
      let report =
        invoke ~options:[ "--dimension-work-limit=3" ] "ir" "aot" path
      in
      word "24" report;
      require
        (member "dimension_preparation_work" report = `Int 3)
        "closed AOT floating bound charges all three expression visits";
      let exhausted =
        invoke ~status:1 ~options:[ "--dimension-work-limit=2" ] "ir" "aot" path
      in
      error "HCIRVM0007" exhausted;
      require
        (member "dimension_preparation_work" exhausted = `Int 2)
        "closed AOT one-below bound retains reached work");
  List.iter
    (fun (source, work, count) ->
      with_file ".hc" source (fun path ->
          let report = invoke ~status:1 "ir" "aot" path in
          error "HCRUN0004" report;
          let diagnostic =
            report |> member "diagnostics" |> to_list |> List.hd
          in
          require
            (member "message" diagnostic
            = `String (Printf.sprintf "array extent %Ld is negative" count))
            "closed AOT F64 conversion precedes the signed extent check";
          require
            (member "dimension_preparation_work" report = `Int work)
            "failed closed AOT F64 bound retains original visit work"))
    [
      ("I64 A[-1.5];sizeof(A);", 2, -1L);
      ("I64 A[4294967296.0*2147483648.0];sizeof(A);", 3, Int64.min_int);
      ("I64 A[1.0/0.0];sizeof(A);", 3, Int64.min_int);
      ("I64 A[0.0/0.0];sizeof(A);", 3, Int64.min_int);
    ]

let () =
  List.iter
    (fun target ->
      let baseline = invoke target "jit" example in
      word "42" baseline;
      require
        (member "output_hex" baseline = `String "64696d6f6666")
        "once-only layout output";
      let steps = member "executed_steps" baseline |> to_int
      and prep = member "compiled_initializer_steps" baseline |> to_int in
      word "42"
        (invoke
           ~options:
             [
               Printf.sprintf "--step-limit=%d" steps;
               Printf.sprintf "--initializer-step-limit=%d" prep;
               "--output-byte-limit=6";
               "--global-byte-limit=16";
             ]
           target "jit" example);
      error "HCIRVM0007"
        (invoke ~status:1
           ~options:[ Printf.sprintf "--step-limit=%d" (steps - 1) ]
           target "jit" example);
      error "HCIRVM0007"
        (invoke ~status:1
           ~options:[ Printf.sprintf "--initializer-step-limit=%d" (prep - 1) ]
           target "jit" example);
      error "HCIRVM0022"
        (invoke ~status:1
           ~options:[ "--output-byte-limit=5" ]
           target "jit" example);
      error "HCIRVM0023"
        (invoke ~status:1
           ~options:
             [
               Printf.sprintf "--output-work-limit=%d"
                 ((member "output_work" baseline |> to_int) - 1);
             ]
           target "jit" example);
      error "HCIRVM0016"
        (invoke ~status:1
           ~options:[ "--global-byte-limit=15" ]
           target "jit" example);
      (if target = "host-jit-task" then
         let fragments =
           baseline |> member "native" |> member "fragments" |> to_list
         in
         List.iter
           (fun kind ->
             let reached =
               List.filter (fun f -> member "kind" f = `String kind) fragments
             in
             require (List.length reached = 1) ("one original native " ^ kind);
             require
               (member "outcome" (List.hd reached) = `String "success")
               (kind ^ " completes"))
           [ "dimension"; "offset" ]);
      with_file ".hc"
        "extern U0 Print(U8 *s,...);I64 Next(){Print(\"kept\");return -1;}I64 \
         A[Next()];" (fun path ->
          let failed = invoke ~status:1 target "jit" path in
          require
            (member "outcome" failed = `String "error")
            "negative bound fails";
          require
            (member "output_hex" failed = `String "6b657074")
            "output precedes extent rejection"))
    (if native then [ "ir"; "host-jit-task" ] else [ "ir" ]);
  if native then (
    let floating_example = Sys.argv.(4) in
    let baseline = invoke "host-jit-task" "jit" floating_example in
    word "42" baseline;
    require
      (member "output_hex" baseline = `String "")
      "F64 bounds must not emit ordinary output";
    let dimensions =
      baseline |> member "native" |> member "fragments" |> to_list
      |> List.filter (fun fragment ->
          member "kind" fragment = `String "dimension")
    in
    require
      (List.length dimensions = 4)
      "four source-owned native F64 dimension fragments";
    List.iter
      (fun fragment ->
        require
          (member "outcome" fragment = `String "success")
          "every native F64 bound completes")
      dimensions;
    let work = baseline |> member "compiled_initializer_steps" |> to_int in
    require (work > 0) "F64 bounds charge their actual execution";
    word "42"
      (invoke
         ~options:[ Printf.sprintf "--initializer-step-limit=%d" work ]
         "host-jit-task" "jit" floating_example);
    error "HCIRVM0007"
      (invoke ~status:1
         ~options:[ Printf.sprintf "--initializer-step-limit=%d" (work - 1) ]
         "host-jit-task" "jit" floating_example);
    with_file ".hc"
      "extern U0 Print(U8 *fmt,...);F64 Next(){Print(\"kept\");return \
       -1.5;}I64 A[Next()];" (fun path ->
        let report = invoke ~status:1 "host-jit-task" "jit" path in
        require
          (member "outcome" report = `String "error")
          "negative converted F64 extent rejects";
        require
          (member "output_hex" report = `String "6b657074")
          "native F64 failure preserves earlier output"));
  closed_aot_float ();
  Printf.printf "%d original runtime-layout CLI reports passed\n"
    (if native then 27 else 15)
