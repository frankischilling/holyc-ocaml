open Yojson.Safe.Util
module Runtime = Holyc_lib.Native_program_execution
module VM = Holyc_lib.Ir_integer_interpreter

let require condition message = if not condition then failwith message

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let with_file suffix contents action =
  let path = Filename.temp_file "holyc native switch cli " suffix in
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
    (Array.length Sys.argv = 3)
    "usage: test_native_switch_cli.exe <holyc.exe> <integer-switch.hc>"

let compiler = Sys.argv.(1)
let switch_fixture = Sys.argv.(2)

let invoke arguments =
  with_file ".stdout" "" (fun stdout ->
      with_file ".stderr" "" (fun stderr ->
          let out_fd =
            Unix.openfile stdout [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
          in
          let err_fd =
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
          let _, status = Unix.waitpid [] pid in
          (status, read stdout, read stderr)))

let status_name = function
  | Unix.WEXITED code -> Printf.sprintf "exit %d" code
  | Unix.WSIGNALED signal -> Printf.sprintf "signal %d" signal
  | Unix.WSTOPPED signal -> Printf.sprintf "stopped %d" signal

let checked_json expected arguments =
  let status, stdout, stderr = invoke arguments in
  require
    (status = Unix.WEXITED expected)
    (Printf.sprintf "%s: expected exit %d, got %s\nstdout: %s\nstderr: %s"
       (String.concat " " arguments)
       expected (status_name status) stdout stderr);
  require (stderr = "") ("JSON report escaped diagnostics to stderr: " ^ stderr);
  Yojson.Safe.from_string stdout

let host_json ?(status = 0) ?(options = []) ~mode source =
  let report =
    checked_json status
      ([ "run"; "--target=host-jit"; "--format=json"; "--mode=" ^ mode ]
      @ options @ [ source ])
  in
  require
    (report |> member "schema" |> to_string = "holyc-integer-program-v2"
    && report |> member "target" |> to_string = "host-jit"
    && report |> member "mode" |> to_string = mode
    && report |> member "arithmetic" |> to_string = "runtime-native")
    "host-jit switch report identity";
  let expected_platform =
    match Runtime.platform () with
    | Runtime.Windows_x86_64 -> "windows-x86_64"
    | Runtime.Linux_x86_64 -> "linux-x86_64"
    | Runtime.Unsupported -> failwith "native switch CLI tests require x86-64"
  in
  require
    (report |> member "native" |> member "platform" |> to_string
   = expected_platform)
    "host-jit switch platform";
  report

let ir_json ?(status = 0) ?(options = []) ~mode source =
  checked_json status
    ([
       "run";
       "--target=ir";
       "--report-version=2";
       "--format=json";
       "--mode=" ^ mode;
     ]
    @ options @ [ source ])

let diagnostics report = report |> member "diagnostics" |> to_list

let first_code report =
  match diagnostics report with
  | first :: _ -> first |> member "code" |> to_string
  | [] -> (
      match member "command_error" report with
      | `Assoc _ as error -> error |> member "code" |> to_string
      | _ -> failwith "expected switch diagnostic")

let check_success report =
  require
    (report |> member "outcome" |> to_string = "success"
    && report |> member "termination" |> to_string = "stream-end"
    && diagnostics report = []
    && member "command_error" report = `Null)
    "successful switch CLI report"

let check_word report =
  let word = member "final_value" report in
  require
    (word |> member "type" |> to_string = "i64"
    && word |> member "value" |> to_string = "42"
    && word |> member "bits" |> to_string = "0x000000000000002a")
    ("unexpected switch final value: " ^ Yojson.Safe.to_string word)

let switch_work report = report |> member "switch_preparation_work" |> to_int
let switch_limit report = report |> member "switch_work_limit" |> to_int
let executed_steps report = report |> member "executed_steps" |> to_int

let mode_value = function
  | "jit" -> Holyc_lib.Preprocessor.Jit
  | "aot" -> Holyc_lib.Preprocessor.Aot
  | value -> failwith ("unknown switch CLI mode: " ^ value)

let fixture_diagnostics diagnostics =
  diagnostics
  |> List.map (fun (error : Holyc_lib.Diagnostic.t) ->
      error.code ^ ": " ^ error.message)
  |> String.concat "; "

let batch_fixture ~mode source =
  match
    Native_scalar_fixture.compile ~mode:(mode_value mode) ~path:source
      ~contents:(read source) ()
  with
  | Ok fixture -> fixture
  | Error diagnostics -> failwith (fixture_diagnostics diagnostics)

let batch_execution fixture =
  match Native_scalar_fixture.execute ~max_steps:100_000 fixture with
  | Ok execution -> execution
  | Error errors ->
      failwith
        (errors
        |> List.map (fun (error : VM.error) ->
            error.code ^ ": " ^ error.message)
        |> String.concat "; ")

let maintained_fixture_contract () =
  List.iter
    (fun mode ->
      let interpreted = ir_json ~mode switch_fixture in
      check_success interpreted;
      check_word interpreted;
      require
        (switch_limit interpreted = 100_000 && switch_work interpreted = 8)
        "maintained switch IR fixture work contract";
      let fixture = batch_fixture ~mode switch_fixture in
      let checked = batch_execution fixture in
      (match VM.final_value checked with
      | Some word ->
          require
            (word.type_ = VM.I64 && word.bits = 42L)
            "checked switch fixture value"
      | None -> failwith "checked switch fixture has no word");
      let batch_steps = VM.executed_steps checked in
      let native = host_json ~mode switch_fixture in
      check_success native;
      check_word native;
      require
        (switch_limit native = 100_000 && switch_work native = 8)
        "maintained native switch work contract";
      require
        (executed_steps native = batch_steps)
        "native switch runtime meter differs from checked batch";
      let exact_work =
        host_json ~mode ~options:[ "--switch-work-limit=8" ] switch_fixture
      in
      check_success exact_work;
      check_word exact_work;
      require (switch_work exact_work = 8) "exact switch work allowance";
      let one_below_work =
        host_json ~status:1 ~mode
          ~options:[ "--switch-work-limit=7" ]
          switch_fixture
      in
      require
        (first_code one_below_work = "HCSW0003"
        && switch_work one_below_work = 7
        && member "executed_steps" one_below_work = `Null
        && one_below_work |> member "native" |> member "image" = `Null)
        "one-below switch preparation allowance";
      let exact_runtime =
        host_json ~mode
          ~options:[ "--step-limit=" ^ string_of_int batch_steps ]
          switch_fixture
      in
      check_success exact_runtime;
      check_word exact_runtime;
      require
        (executed_steps exact_runtime = batch_steps
        && switch_work exact_runtime = 8)
        "exact runtime budget keeps independent switch work";
      let one_below_runtime =
        host_json ~status:1 ~mode
          ~options:[ "--step-limit=" ^ string_of_int (batch_steps - 1) ]
          switch_fixture
      in
      require
        (first_code one_below_runtime = "HCIRVM0007"
        && executed_steps one_below_runtime = batch_steps - 1
        && switch_work one_below_runtime = 8)
        "runtime one-below keeps completed switch preparation")
    [ "jit"; "aot" ]

let invalid_limits_precede_parsing () =
  with_file ".hc" "@invalid" (fun source ->
      List.iter
        (fun mode ->
          List.iter
            (fun limit ->
              let option = "--switch-work-limit=" ^ string_of_int limit in
              List.iter
                (fun run ->
                  let report = run ~status:1 ~options:[ option ] ~mode source in
                  require
                    (first_code report = "HCIRVM0001" && switch_work report = 0)
                    "invalid switch-work limit must fail before parsing")
                [
                  (fun ~status ~options ~mode source ->
                    ir_json ~status ~options ~mode source);
                  (fun ~status ~options ~mode source ->
                    host_json ~status ~options ~mode source);
                ])
            [ 0; -1 ])
        [ "jit"; "aot" ])

let resource_and_domain_failures () =
  let cases =
    [
      ( "I64 Bad(I64 n){switch[n]{case 1:return 1;default:return 0;}return \
         -1;}42;",
        "HCRUN0001" );
      ( "I64 Bad(I64 n){switch(n){case n++:return 1;default:return 0;}return \
         -1;}42;",
        "HCRUN0001" );
      ( "I64 Bad(I64 n){switch(n){case 1:return 1;case 1:return \
         2;default:return 0;}return -1;}42;",
        "HCSW0002" );
      ( "I64 A(I64 n){switch(n){case 0...39999:return 1;default:return \
         0;}return -1;} I64 B(I64 n){switch(n){case 0...39999:return \
         2;default:return 0;}return -1;} 42;",
        "HCSW0004" );
    ]
  in
  List.iter
    (fun (contents, code) ->
      with_file ".hc" contents (fun source ->
          List.iter
            (fun mode ->
              let interpreted = ir_json ~status:1 ~mode source in
              let native = host_json ~status:1 ~mode source in
              require
                (first_code interpreted = code && first_code native = code)
                "switch CLI domain/resource diagnostic mismatch";
              require
                (member "executed_steps" native = `Null
                && native |> member "native" |> member "image" = `Null)
                "invalid switch source entered native execution")
            [ "jit"; "aot" ]))
    cases

let selected_runtime_fault () =
  with_file ".hc"
    "I64 Pick(I64 n){switch(n){case 1:return 84/0;default:return 0;}return \
     -1;}Pick(1);" (fun source ->
      List.iter
        (fun mode ->
          let interpreted = ir_json ~status:1 ~mode source in
          let native = host_json ~status:1 ~mode source in
          require
            (first_code interpreted = "HCIRVM0009"
            && first_code native = "HCIRVM0009")
            "selected switch runtime fault code";
          require
            (switch_work interpreted = 1 && switch_work native = 1)
            "selected switch runtime fault keeps preparation work";
          require
            (member "executed_steps" native <> `Null)
            "selected native switch fault has no runtime meter")
        [ "jit"; "aot" ])

let () =
  maintained_fixture_contract ();
  invalid_limits_precede_parsing ();
  resource_and_domain_failures ();
  selected_runtime_fault ()
