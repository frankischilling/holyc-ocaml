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
    (Array.length Sys.argv = 5)
    "expected compiler and maintained example paths"

let compiler = Sys.argv.(1)
let example = Sys.argv.(2)
let retained_example = Sys.argv.(3)
let binding_example = Sys.argv.(4)

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

let () =
  List.iter
    (fun target ->
      List.iter
        (fun mode ->
          let steps = if target = "ir" && mode = "jit" then 202 else 194 in
          let report = invoke ~target ~mode ~steps example in
          require
            (member "outcome" report = `String "success")
            "example outcome";
          require
            (member "executed_steps" report = `Int steps)
            "example instruction work";
          require
            (report |> member "final_value" |> member "bits"
           = `String "0x000000000000002a")
            "example I64 42";
          require
            (member "conditional_recovery" report = `String "hosted-strict")
            "source recovery policy";
          require
            (member "arithmetic" report
            = `String (if target = "ir" then "runtime-ir" else "runtime-native")
            )
            "execution target";
          output report "415a213a33" 5 15;
          let below =
            invoke ~target ~mode ~steps:(steps - 1) ~status:1 example
          in
          error below "HCIRVM0007";
          require
            (member "executed_steps" below = `Int (steps - 1))
            "one-below reached work";
          output below "415a213a33" 5 15;
          with_file ".hc" "_intern 0x1e I64 Bad(U8 *ch);Bad(\"a\");"
            (fun path ->
              let rejected = invoke ~target ~mode ~status:1 path in
              require
                (member "outcome" rejected = `String "error")
                "wrong internal signature executed";
              output rejected "" 0 0))
        [ "jit"; "aot" ])
    [ "ir"; "host-jit" ]

let () =
  List.iter
    (fun mode ->
      let steps = if mode = "jit" then 58 else 57 in
      let report = invoke ~target:"ir" ~mode ~steps retained_example in
      require
        (member "outcome" report = `String "success")
        "retained example outcome";
      require
        (member "executed_steps" report = `Int steps)
        "retained cumulative work";
      require
        (report |> member "final_value" |> member "bits"
       = `String "0x000000000000002a")
        "retained generated I64 42";
      output report "413a33" 3 55;
      let below =
        invoke ~target:"ir" ~mode ~steps:(steps - 1) ~status:1 retained_example
      in
      error below "HCIRVM0007";
      output below "413a33" 3 55;
      let native = invoke ~target:"host-jit" ~mode ~status:1 retained_example in
      error native "HCPP0008";
      output native "" 0 0)
    [ "jit"; "aot" ]

let () =
  List.iter
    (fun mode ->
      let steps = if mode = "jit" then 60 else 59 in
      let report = invoke ~target:"ir" ~mode ~steps binding_example in
      require
        (member "outcome" report = `String "success")
        "effectful binding outcome";
      require
        (report |> member "final_value" |> member "bits"
       = `String "0x000000000000002a")
        "effectful binding I64 42";
      require
        (member "output_hex" report = `String "7461726765743a313b413a31")
        "binding target executes once before generated output";
      require
        (member "executed_steps" report = `Int steps)
        "binding example cumulative execution";
      require
        (member "compiled_initializer_steps" report = `Int 3)
        "binding example cumulative preparation";
      output report "7461726765743a313b413a31" 12 75;
      let below =
        invoke ~target:"ir" ~mode ~steps:(steps - 1) ~status:1 binding_example
      in
      error below "HCIRVM0007";
      output below "7461726765743a313b413a31" 12 75;
      let native = invoke ~target:"host-jit" ~mode ~status:1 binding_example in
      error native "HCPP0008";
      output native "" 0 0)
    [ "jit"; "aot" ]
