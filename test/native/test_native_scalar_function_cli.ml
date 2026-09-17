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
  let path = Filename.temp_file "holyc native scalar cli " suffix in
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
    (Array.length Sys.argv = 4)
    "usage: test_native_scalar_function_cli.exe <holyc.exe> \
     <native-scalar-functions.hc> <native-u0-functions.hc>"

let compiler = Sys.argv.(1)
let scalar_fixture = Sys.argv.(2)
let u0_fixture = Sys.argv.(3)

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
    "host-jit scalar report identity";
  let expected_platform =
    match Runtime.platform () with
    | Runtime.Windows_x86_64 -> "windows-x86_64"
    | Runtime.Linux_x86_64 -> "linux-x86_64"
    | Runtime.Unsupported -> failwith "native scalar CLI tests require x86-64"
  in
  require
    (report |> member "native" |> member "platform" |> to_string
   = expected_platform)
    "host-jit scalar report platform";
  report

let ir_json ?(status = 0) ?(options = []) ~mode source =
  let report =
    checked_json status
      ([
         "run";
         "--target=ir";
         "--report-version=2";
         "--format=json";
         "--mode=" ^ mode;
       ]
      @ options @ [ source ])
  in
  require
    (report |> member "schema" |> to_string = "holyc-integer-program-v2"
    && report |> member "target" |> to_string = "ir"
    && report |> member "mode" |> to_string = mode
    && report |> member "arithmetic" |> to_string = "runtime-ir"
    && member "native" report = `Null)
    "IR scalar report identity";
  report

let diagnostics report = report |> member "diagnostics" |> to_list

let first_code report =
  match diagnostics report with
  | first :: _ -> first |> member "code" |> to_string
  | [] -> (
      match member "command_error" report with
      | `Assoc _ as error -> error |> member "code" |> to_string
      | _ -> failwith "expected a diagnostic or command error")

let check_success report =
  require
    (report |> member "outcome" |> to_string = "success"
    && report |> member "termination" |> to_string = "stream-end"
    && diagnostics report = []
    && member "command_error" report = `Null)
    "successful scalar report"

let check_word report type_ value bits =
  let word = member "final_value" report in
  require
    (word |> member "type" |> to_string = type_
    && word |> member "value" |> to_string = value
    && word |> member "bits" |> to_string = bits)
    ("unexpected final scalar word: " ^ Yojson.Safe.to_string word)

let check_no_word report =
  require (member "final_value" report = `Null) "expected no final value"

let preparation report = report |> member "compiled_initializer_steps" |> to_int
let default_bytes report = report |> member "prepared_default_bytes" |> to_int
let executed_steps report = report |> member "executed_steps" |> to_int

let function_count report =
  report |> member "native" |> member "image" |> member "function_count"
  |> to_int

let mode_value = function
  | "jit" -> Holyc_lib.Preprocessor.Jit
  | "aot" -> Holyc_lib.Preprocessor.Aot
  | mode -> failwith ("unknown scalar CLI test mode: " ^ mode)

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

let batch_execution ~max_steps fixture =
  match Native_scalar_fixture.execute ~max_steps fixture with
  | Ok execution -> execution
  | Error errors ->
      failwith
        (errors
        |> List.map (fun (error : VM.error) ->
            error.code ^ ": " ^ error.message)
        |> String.concat "; ")

let check_native_meter_against_ir ~mode ~source ~expected_preparation
    ~expected_default_bytes ~check_final =
  (* This public IR run is a fresh source-stream semantic oracle. Expression
     defaults intentionally activate separate JIT task units, so its executed
     step count is not the native batch meter. *)
  let ir = ir_json ~mode source in
  check_success ir;
  check_final ir;
  require
    (preparation ir = expected_preparation)
    "IR preparation work changed from source-derived fixture contract";
  let source_stream_steps = executed_steps ir in
  require (source_stream_steps > 1)
    "source-stream fixture needs nontrivial runtime work";
  (* Rebuild exactly the isolated unit shape used for native source lowering and
     execute that checked IR directly. This meter, not a source-stream delta, is
     the independent native runtime oracle. *)
  let fixture = batch_fixture ~mode source in
  require
    (fixture.preparation_steps = expected_preparation
    && fixture.default_bytes = expected_default_bytes)
    "isolated native-batch preparation metadata differs from fixture contract";
  let batch = batch_execution ~max_steps:100_000 fixture in
  let batch_steps = VM.executed_steps batch in
  require (batch_steps > 1)
    "native-batch fixture needs a nontrivial one-below runtime boundary";
  let native = host_json ~mode source in
  check_success native;
  check_final native;
  require
    (executed_steps native = batch_steps)
    "host-jit runtime work differs from explicit execution of its checked \
     batch IR";
  require
    (preparation native = expected_preparation
    && default_bytes native = expected_default_bytes)
    "host-jit preparation/default meters differ from fixture contract";
  (match Native_scalar_fixture.execute ~max_steps:(batch_steps - 1) fixture with
  | Error (first :: _) ->
      require
        (first.code = "HCIRVM0007" && first.executed_steps = batch_steps - 1)
        "checked batch IR did not expose its exact one-below runtime boundary"
  | Error [] -> failwith "checked batch IR returned an empty one-below failure"
  | Ok _ -> failwith "checked batch IR accepted a one-below runtime allowance");
  let exact =
    host_json ~mode
      ~options:[ "--step-limit=" ^ string_of_int batch_steps ]
      source
  in
  check_success exact;
  check_final exact;
  require
    (executed_steps exact = batch_steps)
    "host-jit exact scalar runtime allowance was not fully charged";
  let one_below =
    host_json ~status:1 ~mode
      ~options:[ "--step-limit=" ^ string_of_int (batch_steps - 1) ]
      source
  in
  require
    (first_code one_below = "HCIRVM0007"
    && executed_steps one_below = batch_steps - 1
    && member "final_value" one_below = `Null)
    "host-jit scalar one-below runtime allowance did not stop at the exact \
     meter";
  (source_stream_steps, batch_steps)

let scalar_fixture_modes () =
  List.iter
    (fun mode ->
      let check_final report =
        check_word report "i64" "42" "0x000000000000002a"
      in
      ignore
        (check_native_meter_against_ir ~mode ~source:scalar_fixture
           ~expected_preparation:6 ~expected_default_bytes:16 ~check_final);
      let native = host_json ~mode scalar_fixture in
      require
        (function_count native = 13)
        "maintained scalar fixture must retain its thirteen named functions";
      let exact_preparation =
        host_json ~mode ~options:[ "--initializer-step-limit=6" ] scalar_fixture
      in
      check_success exact_preparation;
      check_final exact_preparation;
      require
        (preparation exact_preparation = 6
        && default_bytes exact_preparation = 16)
        "maintained scalar fixture exact preparation allowance";
      let one_below_preparation =
        host_json ~status:1 ~mode
          ~options:[ "--initializer-step-limit=5" ]
          scalar_fixture
      in
      require
        (first_code one_below_preparation = "HCIRVM0007"
        && preparation one_below_preparation = 5
        && default_bytes one_below_preparation = 8
        && member "executed_steps" one_below_preparation = `Null
        && one_below_preparation |> member "native" |> member "image" = `Null)
        "maintained scalar fixture one-below preparation retains first default";
      let exact_bytes =
        host_json ~mode ~options:[ "--default-byte-limit=16" ] scalar_fixture
      in
      check_success exact_bytes;
      check_final exact_bytes;
      require
        (preparation exact_bytes = 6 && default_bytes exact_bytes = 16)
        "maintained scalar fixture exact saved-default allowance";
      let one_below_bytes =
        host_json ~status:1 ~mode
          ~options:[ "--default-byte-limit=15" ]
          scalar_fixture
      in
      require
        (first_code one_below_bytes = "HCIRVM0011"
        && preparation one_below_bytes = 3
        && default_bytes one_below_bytes = 8
        && member "executed_steps" one_below_bytes = `Null
        && one_below_bytes |> member "native" |> member "image" = `Null)
        "maintained scalar fixture one-below bytes retain exactly one default")
    [ "jit"; "aot" ]

let u0_fixture_modes () =
  List.iter
    (fun mode ->
      let check_final = check_no_word in
      ignore
        (check_native_meter_against_ir ~mode ~source:u0_fixture
           ~expected_preparation:0 ~expected_default_bytes:0 ~check_final);
      let native = host_json ~mode u0_fixture in
      require
        (function_count native = 4)
        "maintained U0 fixture must retain its four named procedures")
    [ "jit"; "aot" ]

let narrow_default_cli_contract () =
  with_file ".hc" "U8 Value(U8 n=554){return n;}\nValue();" (fun source ->
      List.iter
        (fun mode ->
          let check_final report =
            check_word report "u64" "42" "0x000000000000002a"
          in
          ignore
            (check_native_meter_against_ir ~mode ~source ~expected_preparation:3
               ~expected_default_bytes:8 ~check_final);
          let exact =
            host_json ~mode
              ~options:
                [ "--initializer-step-limit=3"; "--default-byte-limit=8" ]
              source
          in
          check_success exact;
          check_final exact;
          require
            (preparation exact = 3 && default_bytes exact = 8)
            "U8 554 default exact preparation and payload limits";
          let prep_one_below =
            host_json ~status:1 ~mode
              ~options:
                [ "--initializer-step-limit=2"; "--default-byte-limit=8" ]
              source
          in
          require
            (first_code prep_one_below = "HCIRVM0007"
            && preparation prep_one_below = 2
            && default_bytes prep_one_below = 0
            && member "executed_steps" prep_one_below = `Null)
            "U8 554 default one-below preparation retains work without payload";
          let byte_one_below =
            host_json ~status:1 ~mode
              ~options:
                [ "--initializer-step-limit=3"; "--default-byte-limit=7" ]
              source
          in
          require
            (first_code byte_one_below = "HCIRVM0011"
            && preparation byte_one_below = 0
            && default_bytes byte_one_below = 0
            && member "executed_steps" byte_one_below = `Null)
            "U8 554 default byte quota rejects before its constant preparation")
        [ "jit"; "aot" ])

let u0_final_latch_cli_contract () =
  List.iter
    (fun (contents, has_word) ->
      with_file ".hc" contents (fun source ->
          List.iter
            (fun mode ->
              let ir = ir_json ~mode source in
              let fixture = batch_fixture ~mode source in
              let batch = batch_execution ~max_steps:100_000 fixture in
              let native = host_json ~mode source in
              check_success ir;
              check_success native;
              require
                (executed_steps native = VM.executed_steps batch)
                "U0 final-latch runtime work differs from checked batch IR";
              if has_word then (
                check_word ir "i64" "42" "0x000000000000002a";
                check_word native "i64" "42" "0x000000000000002a")
              else (
                check_no_word ir;
                check_no_word native))
            [ "jit"; "aot" ]))
    [ ("U0 V(){}\n42;V();", false); ("U0 V(){}\nV();42;", true) ]

let () =
  scalar_fixture_modes ();
  u0_fixture_modes ();
  narrow_default_cli_contract ();
  u0_final_latch_cli_contract ()
