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
    (Array.length Sys.argv = 3 || Array.length Sys.argv = 4)
    "expected compiler and runtime-layout example, with optional --native"

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
  Printf.printf "%d original runtime-layout CLI reports passed\n"
    (if native then 16 else 8)
