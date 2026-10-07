open Yojson.Safe.Util

let require condition message = if not condition then failwith message

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let with_file suffix contents action =
  let path = Filename.temp_file "holyc swap cli " suffix in
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

let invoke ~target ~mode ?(steps = 100_000) ?(preparation = 100_000)
    ?(status = 0) path =
  let arguments =
    [
      "run";
      "--report-version=2";
      "--format=json";
      "--target=" ^ target;
      "--mode=" ^ mode;
      "--step-limit=" ^ string_of_int steps;
      "--initializer-step-limit=" ^ string_of_int preparation;
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
  require
    (member "final_value" report |> member "type" = `String "i64")
    "actual source result class";
  output report hex bytes work

let () =
  List.iter
    (fun target ->
      List.iter
        (fun mode ->
          let steps, prep =
            if target = "ir" && mode = "jit" then (274, 13) else (256, 0)
          in
          validate
            (invoke ~target ~mode ~steps example)
            ~steps ~prep ~hex:"34323a34323a34323a34323a3132383a42413b" ~bytes:19
            ~work:41;
          let below =
            invoke ~target ~mode ~steps:(steps - 1) ~status:1 example
          in
          error below "HCIRVM0007";
          output below "34323a34323a34323a34323a3132383a42413b" 19 41)
        [ "jit"; "aot" ])
    (if native then [ "ir"; "host-jit" ] else [ "ir" ]);
  List.iter
    (fun mode ->
      let report = invoke ~target:"ir" ~mode ~steps:118 retained_example in
      validate report ~steps:118 ~prep:12 ~hex:"" ~bytes:0 ~work:7;
      validate
        (invoke ~target:"ir" ~mode ~steps:118 ~preparation:12 retained_example)
        ~steps:118 ~prep:12 ~hex:"" ~bytes:0 ~work:7;
      error
        (invoke ~target:"ir" ~mode ~preparation:11 ~status:1 retained_example)
        "HCIRVM0007";
      error
        (invoke ~target:"ir" ~mode ~steps:117 ~status:1 retained_example)
        "HCIRVM0007";
      if native then
        error
          (invoke ~target:"host-jit" ~mode ~status:1 retained_example)
          "HCPP0008")
    [ "jit"; "aot" ];
  List.iter
    (fun target ->
      List.iter
        (fun mode ->
          with_file ".hc"
            "_intern 0xa8 U0 Exchange(I64 *a,I64 *b);I64 \
             a=8,b=42;42;Exchange(&a,&b);" (fun path ->
              let report = invoke ~target ~mode path in
              require
                (member "outcome" report = `String "success")
                "void success";
              require
                (member "final_value" report = `Null)
                "actual void clears the public final value";
              output report "" 0 0);
          with_file ".hc"
            "_intern 0xa8 U0 Exchange(I64 *a,I64 *b);extern U0 Print(U8 \
             *fmt,...);U0 F(){I64 \
             q[1];Print(\"kept\");Exchange(&q[0],&q[1]);}F();" (fun path ->
              let report = invoke ~target ~mode ~status:1 path in
              error report "HCIRVM0012";
              require
                (member "output_hex" report = `String "6b657074")
                "first original read faults before second bounds check"))
        [ "jit"; "aot" ])
    (if native then [ "ir"; "host-jit" ] else [ "ir" ]);
  print_endline "Owned swap CLI checks passed."
