open Yojson.Safe.Util

let require condition message = if not condition then failwith message

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let with_file suffix contents action =
  let path = Filename.temp_file "holyc internal toupper cli " suffix in
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
    "expected compiler and two maintained examples"

let compiler = Sys.argv.(1)
let example = Sys.argv.(2)
let retained_example = Sys.argv.(3)
let native = Array.length Sys.argv = 5 && Sys.argv.(4) = "--native"

let invoke ~target ~mode ?(steps = 100_000) ?(status = 0) path =
  let arguments =
    [
      "run";
      "--report-version=2";
      "--format=json";
      "--target=" ^ target;
      "--mode=" ^ mode;
      "--step-limit=" ^ string_of_int steps;
      path;
    ]
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
                  (Array.of_list (compiler :: arguments))
                  Unix.stdin out_fd err_fd)
          in
          let _, actual = Unix.waitpid [] pid in
          let output = read stdout in
          require
            (actual = Unix.WEXITED status)
            ("unexpected exit: " ^ output ^ read stderr);
          require (read stderr = "") "JSON command wrote diagnostics to stderr";
          Yojson.Safe.from_string output))

let output report hex bytes work =
  require (member "output_hex" report = `String hex) "exact captured bytes";
  require (member "output_byte_length" report = `Int bytes) "captured length";
  require (member "output_work" report = `Int work) "format work"

let error report code =
  require (member "outcome" report = `String "error") "expected fault outcome";
  require (member "final_value" report = `Null) "fault retained a final value";
  match member "diagnostics" report |> to_list with
  | first :: _ ->
      require (member "code" first = `String code) "fault diagnostic"
  | [] -> failwith "fault has no diagnostic"

let validate report ~steps ~prep ~hex ~bytes ~work =
  require (member "outcome" report = `String "success") "success outcome";
  require (member "executed_steps" report = `Int steps) "runtime work";
  require
    (member "compiled_initializer_steps" report = `Int prep)
    "preparation work";
  require
    (member "final_value" report |> member "value" = `String "42")
    "actual source value";
  output report hex bytes work

let () =
  List.iter
    (fun target ->
      List.iter
        (fun mode ->
          validate
            (invoke ~target ~mode ~steps:81 example)
            ~steps:81 ~prep:0 ~hex:"34323a313b" ~bytes:5 ~work:12;
          let below = invoke ~target ~mode ~steps:80 ~status:1 example in
          error below "HCIRVM0007";
          output below "34323a313b" 5 12)
        [ "jit"; "aot" ])
    (if native then [ "ir"; "host-jit" ] else [ "ir" ]);
  List.iter
    (fun mode ->
      let report = invoke ~target:"ir" ~mode ~steps:113 retained_example in
      validate report ~steps:113 ~prep:18 ~hex:"" ~bytes:0 ~work:7;
      error
        (invoke ~target:"ir" ~mode ~steps:112 ~status:1 retained_example)
        "HCIRVM0007";
      if native then
        error
          (invoke ~target:"host-jit" ~mode ~status:1 retained_example)
          "HCPP0008")
    [ "jit"; "aot" ];
  print_endline "Unary integer internal CLI checks passed."
