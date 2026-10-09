open Yojson.Safe.Util

let require condition message = if not condition then failwith message
let compiler = Sys.argv.(1)
let fixture = Sys.argv.(2)

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let invoke ?(command_error = false) expected options =
  let stdout = Filename.temp_file "holyc aot native " ".out" in
  let stderr = Filename.temp_file "holyc aot native " ".err" in
  Fun.protect
    ~finally:(fun () ->
      Sys.remove stdout;
      Sys.remove stderr)
    (fun () ->
      let output = Unix.openfile stdout [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600 in
      let errors = Unix.openfile stderr [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600 in
      let arguments =
        [ "run"; "--mode=aot"; "--target=host-jit-task"; "--format=json" ]
        @ options @ [ fixture ]
      in
      let pid =
        Unix.create_process compiler
          (Array.of_list (compiler :: arguments))
          Unix.stdin output errors
      in
      Unix.close output;
      Unix.close errors;
      let _, status = Unix.waitpid [] pid in
      let output = read stdout and errors = read stderr in
      require
        (status = Unix.WEXITED expected)
        ("unexpected CLI exit\n" ^ output ^ errors);
      if command_error then (
        require (output = "") "command error wrote stdout";
        Yojson.Safe.from_string errors)
      else (
        require (errors = "") ("JSON stderr: " ^ errors);
        Yojson.Safe.from_string output))

let failure code report =
  require
    (report |> member "outcome" |> to_string = "error")
    "expected CLI failure";
  require
    (report |> member "diagnostics" |> to_list
    |> List.exists (fun d -> d |> member "code" |> to_string = code))
    ("expected " ^ code)

let () =
  let baseline = invoke 0 [ "--code-byte-limit=1048576" ] in
  require (baseline |> member "mode" |> to_string = "aot") "AOT mode report";
  require
    (baseline |> member "arithmetic" |> to_string = "runtime-native")
    "native evidence";
  require
    (baseline |> member "output_hex" |> to_string
   = "706172736534333b6c6f6164313b")
    "source-derived output";
  require
    (baseline |> member "final_value" |> member "bits" |> to_string
   = "0x000000000000002a")
    "source-derived value";
  let fragments =
    baseline |> member "native" |> member "fragments" |> to_list
  in
  require
    (List.hd (List.rev fragments) |> member "kind" |> to_string = "aot-module")
    "distinct final module";
  List.iter
    (fun f ->
      require
        (f |> member "outcome" |> to_string = "success")
        "native completion")
    fragments;
  let code, ir =
    List.fold_left
      (fun (code, ir) f ->
        ( code + (f |> member "code_bytes" |> to_int),
          ir + (f |> member "ir_instructions" |> to_int) ))
      (0, 0) fragments
  in
  let steps = baseline |> member "executed_steps" |> to_int in
  let preparation = baseline |> member "compiled_initializer_steps" |> to_int in
  let exact =
    [
      "--code-byte-limit=" ^ string_of_int code;
      "--ir-instruction-limit=" ^ string_of_int ir;
      "--step-limit=" ^ string_of_int steps;
      "--initializer-step-limit=" ^ string_of_int preparation;
    ]
  in
  ignore (invoke 0 exact);
  failure "HCIRVM0007"
    (invoke 1
       [
         "--code-byte-limit=1048576"; "--step-limit=" ^ string_of_int (steps - 1);
       ]);
  failure "HCBACK0005"
    (invoke 1 [ "--code-byte-limit=" ^ string_of_int (code - 1) ]);
  failure "HCBACK0001"
    (invoke 1
       [
         "--code-byte-limit=1048576";
         "--ir-instruction-limit=" ^ string_of_int (ir - 1);
       ]);
  failure "HCIRVM0022"
    (invoke 1 [ "--code-byte-limit=1048576"; "--output-byte-limit=13" ]);
  let legacy =
    invoke ~command_error:true 1
      [ "--code-byte-limit=1048576"; "--report-version=1" ]
  in
  require
    (legacy |> member "code" |> to_string = "HCRUN0005")
    "native report version boundary";
  print_endline "Native AOT API and CLI resource reports passed."
