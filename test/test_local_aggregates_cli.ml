open Yojson.Safe.Util

let require condition message = if not condition then failwith message

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let with_file suffix contents action =
  let path = Filename.temp_file "holyc local aggregate " suffix in
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
    (Array.length Sys.argv = 4 || Array.length Sys.argv = 5)
    "expected compiler, declaration and object examples, with optional --native"

let compiler = Sys.argv.(1)
let example = Sys.argv.(2)
let object_example = Sys.argv.(3)
let native = Array.length Sys.argv = 5 && Sys.argv.(4) = "--native"

let invoke ?(status = 0) ?(options = []) target mode path =
  let args =
    [
      "run";
      "--report-version=2";
      "--format=json";
      "--target=" ^ target;
      "--mode=" ^ mode;
    ]
    @ (if target = "host-jit-task" then [ "--code-byte-limit=262144" ] else [])
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
    "successful local-aggregate report";
  require
    (member "diagnostics" report = `List [])
    "successful local-aggregate diagnostics";
  require
    (member "command_error" report = `Null)
    "successful local-aggregate command";
  require
    (report |> member "final_value" |> member "value" = `String bits)
    ("unexpected final local-aggregate word: " ^ Yojson.Safe.to_string report)

let error code report =
  require
    (member "outcome" report = `String "error")
    "failed local-aggregate report";
  let first = report |> member "diagnostics" |> to_list |> List.hd in
  require
    (member "code" first = `String code)
    ("unexpected local-aggregate error: " ^ Yojson.Safe.to_string report)

let () =
  List.iter
    (fun mode ->
      let report = invoke "ir" mode example in
      word "42" report;
      require (member "output_hex" report = `String "3432") "example output")
    [ "jit"; "aot" ];
  List.iter
    (fun target ->
      List.iter
        (fun mode ->
          let report = invoke target mode object_example in
          word "42" report;
          require
            (member "output_hex" report = `String "3432")
            "local class object example output")
        [ "jit"; "aot" ])
    (if native then [ "ir"; "host-jit" ] else [ "ir" ]);
  let targets = if native then [ "ir"; "host-jit-task" ] else [ "ir" ] in
  List.iter
    (fun target ->
      let objects = invoke target "jit" object_example in
      word "42" objects;
      require
        (member "output_hex" objects = `String "3432")
        "retained local class object output";
      let report = invoke target "jit" example in
      word "42" report;
      require
        (member "output_hex" report = `String "3432")
        "native example output";
      with_file ".hc"
        "extern U0 Print(U8 *fmt,...);I64 N=34;I64 \
         Count(){Print(\"dim\");return 8;}I64 Offset(){Print(\"off\");return \
         N;}U0 Make(){class C{U8 a;$$=Offset();U8 b[Count()];};}sizeof(C);"
        (fun path ->
          let baseline = invoke target "jit" path in
          word "42" baseline;
          require
            (member "output_hex" baseline = `String "6f666664696d")
            "original effects";
          let steps = member "executed_steps" baseline |> to_int
          and prep = member "compiled_initializer_steps" baseline |> to_int
          and work = member "output_work" baseline |> to_int in
          word "42"
            (invoke
               ~options:
                 [
                   Printf.sprintf "--step-limit=%d" steps;
                   Printf.sprintf "--initializer-step-limit=%d" prep;
                   "--output-byte-limit=6";
                   Printf.sprintf "--output-work-limit=%d" work;
                   "--global-byte-limit=8";
                 ]
               target "jit" path);
          List.iter
            (fun (code, option) ->
              error code
                (invoke ~status:1 ~options:[ option ] target "jit" path))
            [
              ("HCIRVM0007", Printf.sprintf "--step-limit=%d" (steps - 1));
              ( "HCIRVM0007",
                Printf.sprintf "--initializer-step-limit=%d" (prep - 1) );
              ("HCIRVM0022", "--output-byte-limit=5");
              ("HCIRVM0023", Printf.sprintf "--output-work-limit=%d" (work - 1));
              ("HCIRVM0016", "--global-byte-limit=7");
            ];
          if target = "host-jit-task" then
            let fragments =
              baseline |> member "native" |> member "fragments" |> to_list
            in
            List.iter
              (fun kind ->
                let reached =
                  List.filter
                    (fun f -> member "kind" f = `String kind)
                    fragments
                in
                require (List.length reached = 1) ("one original native " ^ kind);
                require
                  (member "outcome" (List.hd reached) = `String "success")
                  (kind ^ " completes"))
              [ "dimension"; "offset" ]))
    targets;
  Printf.printf
    "Local aggregate CLI reports passed in %d targets and both IR modes\n"
    (List.length targets)
