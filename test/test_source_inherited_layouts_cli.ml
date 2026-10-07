open Yojson.Safe.Util

let require condition message = if not condition then failwith message

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let with_file suffix contents action =
  let path = Filename.temp_file "holyc inherited layout " suffix in
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
    (Array.length Sys.argv = 3 || Array.length Sys.argv = 4)
    "expected compiler and inherited-layout example, with optional --native"

let compiler = Sys.argv.(1)
let example = Sys.argv.(2)
let native = Array.length Sys.argv = 4 && Sys.argv.(3) = "--native"

let invoke ?(status = 0) ?(options = []) target mode path =
  let args =
    [
      "run";
      "--report-version=2";
      "--format=json";
      "--target=" ^ target;
      "--mode=" ^ mode;
    ]
    @ (if
         target = "host-jit-task"
         && not
              (List.exists
                 (String.starts_with ~prefix:"--code-byte-limit=")
                 options)
       then [ "--code-byte-limit=262144" ]
       else [])
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
    "successful inherited-layout report";
  require
    (member "diagnostics" report = `List [])
    "successful inherited-layout diagnostics";
  require
    (member "command_error" report = `Null)
    "successful inherited-layout command";
  require
    (report |> member "final_value" |> member "value" = `String bits)
    ("unexpected final inherited-layout word: " ^ Yojson.Safe.to_string report)

let error code report =
  require
    (member "outcome" report = `String "error")
    "failed inherited-layout report";
  let first = report |> member "diagnostics" |> to_list |> List.hd in
  require
    (member "code" first = `String code)
    ("unexpected inherited-layout error: " ^ Yojson.Safe.to_string report)

let () =
  with_file ".hc" "class B{U8 a[34];};class C:B{I64 b;};sizeof(C);" (fun path ->
      List.iter (fun mode -> word "42" (invoke "ir" mode path)) [ "jit"; "aot" ]);
  List.iter
    (fun target ->
      let baseline = invoke target "jit" example in
      word "42" baseline;
      require
        (member "output_hex" baseline = `String "64696d6f66663432")
        "once-only inherited output";
      let steps = member "executed_steps" baseline |> to_int
      and prep = member "compiled_initializer_steps" baseline |> to_int
      and work = member "output_work" baseline |> to_int in
      let native_limits =
        if target = "host-jit-task" then (
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
                "original preparation completed")
            [ "dimension"; "offset" ];
          let sum field =
            List.fold_left
              (fun n f -> n + (member field f |> to_int))
              0 fragments
          in
          let ir = sum "ir_instructions" and bytes = sum "code_bytes" in
          [
            ( "HCBACK0001",
              Printf.sprintf "--ir-instruction-limit=%d" (ir - 1),
              Printf.sprintf "--ir-instruction-limit=%d" ir );
            ( "HCBACK0005",
              Printf.sprintf "--code-byte-limit=%d" (bytes - 1),
              Printf.sprintf "--code-byte-limit=%d" bytes );
          ])
        else []
      in
      word "42"
        (invoke
           ~options:
             ([
                Printf.sprintf "--step-limit=%d" steps;
                Printf.sprintf "--initializer-step-limit=%d" prep;
                "--output-byte-limit=8";
                Printf.sprintf "--output-work-limit=%d" work;
                "--global-byte-limit=8";
              ]
             @ List.map (fun (_, _, exact) -> exact) native_limits)
           target "jit" example);
      List.iter
        (fun (code, option) ->
          error code (invoke ~status:1 ~options:[ option ] target "jit" example))
        ([
           ("HCIRVM0007", Printf.sprintf "--step-limit=%d" (steps - 1));
           ( "HCIRVM0007",
             Printf.sprintf "--initializer-step-limit=%d" (prep - 1) );
           ("HCIRVM0022", "--output-byte-limit=7");
           ("HCIRVM0023", Printf.sprintf "--output-work-limit=%d" (work - 1));
           ("HCIRVM0016", "--global-byte-limit=7");
         ]
        @ List.map (fun (code, option, _) -> (code, option)) native_limits);
      with_file ".hc"
        "extern U0 Print(U8 *fmt,...);class B{U8 a[34];};class C:B \
         #exe{Print(\"%d\",sizeof(C));} Missing;" (fun path ->
          let failed = invoke ~status:1 target "jit" path in
          error "HCPARSE0110" failed;
          require
            (member "output_hex" failed = `String "30")
            "lookahead observes unattached child"))
    (if native then [ "ir"; "host-jit-task" ] else [ "ir" ]);
  error "HCRUN0006" (invoke ~status:1 "ir" "aot" example);
  Printf.printf "Original inherited-layout CLI reports passed\n"
