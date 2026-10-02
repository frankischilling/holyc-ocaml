open Yojson.Safe.Util

let require condition message = if not condition then failwith message

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let with_file suffix contents action =
  let path = Filename.temp_file "holyc pointer subtraction cli " suffix in
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
          validate
            (invoke ~target ~mode ~steps:157 example)
            ~steps:157 ~prep:0 ~hex:"34323a34323a34323a41423b" ~bytes:12
            ~work:28;
          let below = invoke ~target ~mode ~steps:156 ~status:1 example in
          error below "HCIRVM0007";
          output below "34323a34323a34323a41423b" 12 28;
          with_file ".hc" "I64 q[2];42;q-(-2);" (fun path ->
              let report = invoke ~target ~mode path in
              require
                (member "outcome" report = `String "success")
                "one-past address completes";
              require
                (member "final_value" report = `Null)
                "owned reference clears the public final word";
              output report "" 0 0);
          with_file ".hc"
            "extern U0 Print(U8 *fmt,...);U0 F(){I64 \
             q[2];Print(\"kept\");*(q-(-2));}F();" (fun path ->
              let report = invoke ~target ~mode ~status:1 path in
              error report "HCIRVM0019";
              require
                (member "output_hex" report = `String "6b657074")
                "reached bytes survive the original bounds fault");
          with_file ".hc"
            "extern U0 Print(U8 *fmt,...);U8 Index(){return 257;}U0 F(){I64 \
             q[2];q[0]=42;Print(\"kept\");*(&q[1]-Index());}F();" (fun path ->
              let report = invoke ~target ~mode ~status:1 path in
              error report "HCIRVM0019";
              require
                (member "output_hex" report = `String "6b657074")
                "full computed offset faults instead of narrowing"))
        [ "jit"; "aot" ])
    (if native then [ "ir"; "host-jit" ] else [ "ir" ]);
  List.iter
    (fun target ->
      List.iter
        (fun mode ->
          with_file ".hc"
            "extern U0 Print(U8 *fmt,...);I64 Index(){return \
             -9223372036854775808;}U0 F(){U8 \
             q[2];Print(\"kept\");*(q-Index());}F();" (fun path ->
              let report = invoke ~target ~mode ~status:1 path in
              error report "HCIRVM0020";
              require
                (member "output_hex" report = `String "6b657074")
                "minimum signed subtraction faults before access"))
        [ "jit"; "aot" ])
    (if native then [ "ir"; "host-jit" ] else [ "ir" ]);
  List.iter
    (fun mode ->
      validate
        (invoke ~target:"ir" ~mode ~steps:138 ~preparation:4 retained_example)
        ~steps:138 ~prep:4 ~hex:"" ~bytes:0 ~work:7;
      error
        (invoke ~target:"ir" ~mode ~preparation:3 ~status:1 retained_example)
        "HCIRVM0007";
      error
        (invoke ~target:"ir" ~mode ~steps:137 ~status:1 retained_example)
        "HCIRVM0007";
      if native then
        error
          (invoke ~target:"host-jit" ~mode ~status:1 retained_example)
          "HCPP0008")
    [ "jit"; "aot" ];
  print_endline "Owned pointer subtraction CLI checks passed."
