open Yojson.Safe.Util

let require condition message = if not condition then failwith message

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let with_file suffix contents action =
  let path = Filename.temp_file "holyc internal bindings " suffix in
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
    "expected compiler and internal-binding example, with optional --native"

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
    "successful internal-binding report";
  require
    (member "diagnostics" report = `List [])
    "successful internal-binding diagnostics";
  require
    (member "command_error" report = `Null)
    "successful internal-binding command";
  require
    (report |> member "final_value" |> member "value" = `String bits)
    ("unexpected final internal-binding word: " ^ Yojson.Safe.to_string report)

let error code report =
  require
    (member "outcome" report = `String "error")
    "failed internal-binding report";
  let first = report |> member "diagnostics" |> to_list |> List.hd in
  require
    (member "code" first = `String code)
    ("unexpected internal-binding error: " ^ Yojson.Safe.to_string report)

let () =
  List.iter
    (fun target ->
      let baseline = invoke target "jit" example in
      word "42" baseline;
      require
        (member "output_hex" baseline = `String "62696e64")
        "once-only target output";
      let steps = member "executed_steps" baseline |> to_int
      and prep = member "compiled_initializer_steps" baseline |> to_int in
      word "42"
        (invoke
           ~options:
             [
               Printf.sprintf "--step-limit=%d" steps;
               Printf.sprintf "--initializer-step-limit=%d" prep;
               "--output-byte-limit=4";
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
           ~options:[ "--output-byte-limit=3" ]
           target "jit" example);
      let output_work = member "output_work" baseline |> to_int in
      error "HCIRVM0023"
        (invoke ~status:1
           ~options:
             [ Printf.sprintf "--output-work-limit=%d" (output_work - 1) ]
           target "jit" example);
      if target = "host-jit-task" then (
        let fragments =
          baseline |> member "native" |> member "fragments" |> to_list
        in
        let bindings =
          List.filter
            (fun f -> member "kind" f = `String "internal-binding")
            fragments
        in
        require (List.length bindings = 1) "one actual native binding entry";
        require
          (member "outcome" (List.hd bindings) = `String "success")
          "native binding completes");
      with_file ".hc"
        "extern U0 Print(U8 *s,...);I64 Target(){Print(\"kept\");return \
         0x1e;}_intern Target() Missing Convert(U8 c);" (fun path ->
          let failed = invoke ~status:1 target "jit" path in
          require
            (member "outcome" failed = `String "error")
            "later invalid type fails";
          require
            (member "output_hex" failed = `String "6b657074")
            "target precedes type validation"))
    (if native then [ "ir"; "host-jit-task" ] else [ "ir" ]);
  with_file ".hc" "_intern 1.0 I64 Convert(U8 c);" (fun path ->
      error "HCRUN0001" (invoke ~status:1 "ir" "jit" path));
  Printf.printf "%d original internal-binding CLI reports passed\n"
    (if native then 15 else 8)
