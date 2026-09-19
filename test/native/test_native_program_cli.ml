open Yojson.Safe.Util
module Program = Holyc_lib.X86_64_program
module Runtime = Holyc_lib.Native_program_execution

let require condition message = if not condition then failwith message

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let with_file suffix contents action =
  let path = Filename.temp_file "holyc native program cli " suffix in
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
    (Array.length Sys.argv = 6)
    "usage: test_native_program_cli.exe <holyc.exe> <native-program.hc> \
     <integer-control-flow.hc> <native-functions.hc> <native-defaults.hc>"

let compiler = Sys.argv.(1)
let native_fixture = Sys.argv.(2)
let control_fixture = Sys.argv.(3)
let function_fixture = Sys.argv.(4)
let default_fixture = Sys.argv.(5)

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

let check_keys label expected json =
  let actual = json |> to_assoc |> List.map fst |> List.sort String.compare in
  require
    (actual = List.sort String.compare expected)
    (label ^ " fields: " ^ String.concat ", " actual)

let native_json ?(status = 0) ?(mode = "jit") ?(options = []) source =
  let report =
    checked_json status
      ([ "run"; "--target=host-jit"; "--format=json"; "--mode=" ^ mode ]
      @ options @ [ source ])
  in
  check_keys "host-jit report"
    [
      "schema";
      "implementation_commit";
      "reference_commit";
      "mode";
      "target";
      "arithmetic";
      "outcome";
      "step_limit";
      "executed_steps";
      "termination";
      "frame_byte_limit";
      "call_depth_limit";
      "global_byte_limit";
      "literal_byte_limit";
      "initializer_step_limit";
      "dimension_work_limit";
      "dimension_preparation_work";
      "switch_work_limit";
      "switch_preparation_work";
      "compiled_initializer_steps";
      "prepared_default_bytes";
      "output_byte_limit";
      "output_work_limit";
      "output_byte_length";
      "output_work";
      "output_hex";
      "final_value";
      "diagnostics";
      "command_error";
      "native";
    ]
    report;
  require
    (report |> member "schema" |> to_string = "holyc-integer-program-v2"
    && report
       |> member "implementation_commit"
       |> to_string = Holyc_lib.Version.implementation_commit
    && report |> member "reference_commit" |> to_string
       = Holyc_lib.Version.reference_commit)
    "host-jit preserves program v2 provenance";
  require
    (report |> member "mode" |> to_string = mode
    && report |> member "target" |> to_string = "host-jit"
    && report |> member "arithmetic" |> to_string = "runtime-native")
    "host-jit must identify actual native execution";
  let native = member "native" report in
  check_keys "native metadata" [ "platform"; "limits"; "image" ] native;
  let platform = native |> member "platform" |> to_string in
  let expected_platform =
    match Runtime.platform () with
    | Runtime.Windows_x86_64 -> "windows-x86_64"
    | Runtime.Linux_x86_64 -> "linux-x86_64"
    | Runtime.Unsupported -> failwith "native program CLI tests require x86-64"
  in
  require (platform = expected_platform) "native platform metadata";
  check_keys "native limits"
    [
      "ir_instructions";
      "code_bytes";
      "stack_bytes";
      "blocks";
      "active_stack_bytes";
      "default_bytes";
    ]
    (member "limits" native);
  report

let check_word report type_ value bits =
  let word = member "final_value" report in
  check_keys "final word" [ "type"; "value"; "bits" ] word;
  require
    (word |> member "type" |> to_string = type_
    && word |> member "value" |> to_string = value
    && word |> member "bits" |> to_string = bits)
    ("unexpected final word: " ^ Yojson.Safe.to_string word)

let diagnostics report = report |> member "diagnostics" |> to_list

let first_code report =
  match diagnostics report with
  | first :: _ -> first |> member "code" |> to_string
  | [] -> failwith "expected a diagnostic"

let report_error_code report =
  match diagnostics report with
  | first :: _ -> first |> member "code" |> to_string
  | [] -> (
      match member "command_error" report with
      | `Assoc _ as error -> error |> member "code" |> to_string
      | _ -> failwith "expected a diagnostic or command error")

let check_success ?(preparation = 0) ?(default_bytes = 0) report =
  require
    (report |> member "outcome" |> to_string = "success"
    && report |> member "termination" |> to_string = "stream-end"
    && diagnostics report = []
    && member "command_error" report = `Null)
    "successful host-jit outcome";
  require
    (report |> member "compiled_initializer_steps" |> to_int = preparation
    && report |> member "prepared_default_bytes" |> to_int = default_bytes
    && report |> member "dimension_preparation_work" |> to_int = 0
    && report |> member "output_byte_length" |> to_int = 0
    && report |> member "output_work" |> to_int = 0
    && report |> member "output_hex" |> to_string = "")
    "closed native gate must report bounded preparation and no output work";
  let image = report |> member "native" |> member "image" in
  check_keys "native image"
    [
      "ir_instructions";
      "machine_instructions";
      "register_peak";
      "frame_bytes";
      "block_count";
      "function_count";
      "entry_stack_bytes";
    ]
    image;
  require
    (image |> member "ir_instructions" |> to_int >= 1
    && image |> member "machine_instructions" |> to_int >= 1
    && image |> member "register_peak" |> to_int >= 2
    && image |> member "register_peak" |> to_int <= 7
    && image |> member "frame_bytes" |> to_int <= Program.hard_max_stack_bytes
    && image |> member "block_count" |> to_int >= 1
    && image |> member "function_count" |> to_int >= 0
    && image |> member "entry_stack_bytes" |> to_int > 0)
    "bounded native image metrics"

let checked_source_compile contents =
  let session = Holyc_lib.Session.create () in
  let source =
    Holyc_lib.Session.add_source session ~path:"native-program-cli-limit.hc"
      ~contents
  in
  let config =
    match Holyc_lib.Preprocessor.Config.create () with
    | Ok config -> config
    | Error message -> failwith message
  in
  match Holyc_lib.Native_program.compile session ~config ~source with
  | Ok checked -> checked.value
  | Error diagnostics ->
      failwith
        (diagnostics
        |> List.map (fun (error : Holyc_lib.Diagnostic.t) ->
            error.code ^ ": " ^ error.message)
        |> String.concat "; ")

let fixture_modes () =
  List.iter
    (fun mode ->
      let report = native_json ~mode native_fixture in
      check_success report;
      require
        (report |> member "compiled_initializer_steps" |> to_int = 0
        && report |> member "prepared_default_bytes" |> to_int = 0)
        "default-free native fixture performs no declaration preparation";
      check_word report "i64" "42" "0x000000000000002a";
      require
        (report |> member "executed_steps" |> to_int = 26)
        "maintained control fixture executes its exact 26 IR steps";
      let one_below =
        native_json ~status:1 ~mode ~options:[ "--step-limit=25" ]
          native_fixture
      in
      require
        (one_below |> member "executed_steps" |> to_int = 25
        && first_code one_below = "HCIRVM0007")
        "maintained control fixture one-below stops before its final END")
    [ "jit"; "aot" ]

let function_fixture_modes () =
  List.iter
    (fun mode ->
      let report = native_json ~mode function_fixture in
      check_success report;
      require
        (report |> member "compiled_initializer_steps" |> to_int = 0
        && report |> member "prepared_default_bytes" |> to_int = 0)
        "default-free function fixture performs no declaration preparation";
      check_word report "i64" "42" "0x000000000000002a";
      let image = report |> member "native" |> member "image" in
      require
        (image |> member "function_count" |> to_int = 2)
        "checked-in native function fixture must retain Add and Wrap";
      require
        (image |> member "entry_stack_bytes" |> to_int > 0)
        "callable fixture exposes its root physical stack charge")
    [ "jit"; "aot" ]

let default_fixture_modes () =
  List.iter
    (fun mode ->
      let report = native_json ~mode default_fixture in
      let preparation =
        report |> member "compiled_initializer_steps" |> to_int
      in
      check_success ~preparation ~default_bytes:32 report;
      check_word report "i64" "42" "0x000000000000002a";
      require (preparation > 0)
        "default fixture performs declaration preparation";
      require
        (report |> member "prepared_default_bytes" |> to_int = 32)
        "four fixture defaults occupy four saved words";
      require
        (report |> member "native" |> member "limits" |> member "default_bytes"
       |> to_int = 65_536)
        "native report exposes the configured default-byte limit";
      let exact_steps =
        native_json ~mode
          ~options:[ "--initializer-step-limit=" ^ string_of_int preparation ]
          default_fixture
      in
      check_success ~preparation ~default_bytes:32 exact_steps;
      require
        (exact_steps
        |> member "compiled_initializer_steps"
        |> to_int = preparation)
        "exact declaration preparation allowance succeeds";
      let one_below_steps =
        native_json ~status:1 ~mode
          ~options:
            [ "--initializer-step-limit=" ^ string_of_int (preparation - 1) ]
          default_fixture
      in
      require
        (first_code one_below_steps = "HCIRVM0007"
        && one_below_steps
           |> member "compiled_initializer_steps"
           |> to_int = preparation - 1
        && one_below_steps |> member "prepared_default_bytes" |> to_int = 24
        && member "executed_steps" one_below_steps = `Null
        && one_below_steps |> member "native" |> member "image" = `Null)
        "one-below declaration preparation fails before native entry";
      let exact_bytes =
        native_json ~mode ~options:[ "--default-byte-limit=32" ] default_fixture
      in
      check_success ~preparation ~default_bytes:32 exact_bytes;
      require
        (exact_bytes |> member "prepared_default_bytes" |> to_int = 32)
        "exact saved-default byte allowance succeeds";
      let one_below_bytes =
        native_json ~status:1 ~mode
          ~options:[ "--default-byte-limit=31" ]
          default_fixture
      in
      require
        (first_code one_below_bytes = "HCIRVM0011"
        && one_below_bytes |> member "prepared_default_bytes" |> to_int = 24
        && one_below_bytes
           |> member "compiled_initializer_steps"
           |> to_int = preparation - 3
        && member "executed_steps" one_below_bytes = `Null
        && one_below_bytes |> member "native" |> member "image" = `Null)
        "one-below saved-default bytes retain the first three values before \
         entry")
    [ "jit"; "aot" ];
  let status, stdout, stderr =
    invoke [ "run"; "--target=host-jit"; default_fixture ]
  in
  require
    (status = Unix.WEXITED 0 && stderr = "")
    "default fixture human report succeeds";
  require
    (stdout |> String.split_on_char '\n'
    |> List.exists (fun line -> String.trim line = "prepared-default-bytes=32")
    )
    "human report exposes saved-default payload bytes";
  List.iter
    (fun option ->
      let report = native_json ~status:1 ~options:[ option ] default_fixture in
      require
        (report_error_code report = "HCIRVM0001"
        && report |> member "compiled_initializer_steps" |> to_int = 0
        && report |> member "prepared_default_bytes" |> to_int = 0
        && member "executed_steps" report = `Null)
        (option ^ " rejects before source preparation"))
    [ "--initializer-step-limit=0"; "--default-byte-limit=0" ]

let default_failure_reporting () =
  List.iter
    (fun mode ->
      List.iter
        (fun (contents, code, did_prepare) ->
          with_file ".hc" contents (fun source ->
              let report = native_json ~status:1 ~mode source in
              require
                (first_code report = code
                && report
                   |> member "compiled_initializer_steps"
                   |> to_int > 0 = did_prepare
                && report |> member "prepared_default_bytes" |> to_int = 0
                && member "executed_steps" report = `Null
                && member "final_value" report = `Null)
                "declaration failure retains preparation separately from \
                 absent native execution";
              let primary = List.hd (diagnostics report) |> member "primary" in
              require
                (primary |> member "path" |> to_string = source)
                "default failure retains its original source path"))
        [
          ("I64 F(I64 n=1/0){return n;} F(42);", "HCIRVM0009", true);
          ("I64 F(I64 n=1/0){return n;} 42;", "HCIRVM0009", true);
          ("I64 F(I64 n=1<<3){return n;} F();", "HCRUN0006", false);
          ("1/0; I64 F(I64 n=42){return n;} F();", "HCRUN0006", false);
        ];
      with_file ".hc" "I64 F(I64 n=42){return n;} 1/0;" (fun source ->
          let report = native_json ~status:1 ~mode source in
          require
            (first_code report = "HCIRVM0009"
            && report |> member "compiled_initializer_steps" |> to_int = 3
            && report |> member "prepared_default_bytes" |> to_int = 8
            && report |> member "executed_steps" |> to_int > 0)
            "native arithmetic failure retains earlier default preparation");
      with_file ".hc" "I64 F(I64 n=42){return n;} @invalid" (fun source ->
          let report = native_json ~status:1 ~mode source in
          require
            (report |> member "compiled_initializer_steps" |> to_int = 3
            && report |> member "prepared_default_bytes" |> to_int = 8
            && member "executed_steps" report = `Null)
            "later parse failure retains earlier default preparation");
      with_file ".hc" "@invalid" (fun source ->
          List.iter
            (fun option ->
              let report =
                native_json ~status:1 ~mode ~options:[ option ] source
              in
              require
                (report_error_code report = "HCIRVM0001"
                && report |> member "compiled_initializer_steps" |> to_int = 0
                && report |> member "prepared_default_bytes" |> to_int = 0
                && member "executed_steps" report = `Null)
                "invalid preparation configuration precedes parsing")
            [
              "--initializer-step-limit=0";
              "--default-byte-limit=0";
              "--default-byte-limit=-1";
            ]))
    [ "jit"; "aot" ]

let exact_meter_and_empty () =
  with_file ".hc" "6*7;" (fun source ->
      let exact = native_json ~options:[ "--step-limit=5" ] source in
      check_success exact;
      require
        (exact |> member "executed_steps" |> to_int = 5)
        "6*7 must execute exactly five IR instructions";
      check_word exact "i64" "42" "0x000000000000002a";
      let one_below =
        native_json ~status:1 ~options:[ "--step-limit=4" ] source
      in
      require
        (one_below |> member "outcome" |> to_string = "error"
        && one_below |> member "executed_steps" |> to_int = 4
        && member "termination" one_below = `Null
        && member "final_value" one_below = `Null
        && one_below |> member "native" |> member "image" = `Null
        && first_code one_below = "HCIRVM0007")
        "one-below meter must be a native execution fault, not fallback");
  with_file ".hc" "" (fun source ->
      let report = native_json ~options:[ "--step-limit=1" ] source in
      check_success report;
      require
        (report |> member "executed_steps" |> to_int = 1
        && member "final_value" report = `Null)
        "empty stream charges only its END instruction")

let control_fixture_matches_ir_contract () =
  let native = native_json control_fixture in
  check_success native;
  require
    (native |> member "executed_steps" |> to_int = 23)
    "control fixture host-jit executes the established 23 steps";
  check_word native "i64" "0" "0x0000000000000000";
  let ir =
    checked_json 0
      [
        "run";
        "--target=ir";
        "--report-version=2";
        "--format=json";
        control_fixture;
      ]
  in
  require
    (ir |> member "schema" |> to_string = "holyc-integer-program-v2"
    && ir |> member "target" |> to_string = "ir"
    && ir |> member "arithmetic" |> to_string = "runtime-ir"
    && ir |> member "executed_steps" |> to_int = 23
    && ir |> member "final_value" |> member "value" |> to_string = "0"
    && member "native" ir = `Null)
    "existing IR v2 contract remains unchanged by host-jit"

let full_width_values () =
  List.iter
    (fun (contents, type_, value, bits) ->
      with_file ".hc" contents (fun source ->
          List.iter
            (fun mode ->
              let report = native_json ~mode source in
              check_success report;
              check_word report type_ value bits)
            [ "jit"; "aot" ];
          let status, stdout, stderr =
            invoke [ "run"; "--target=host-jit"; source ]
          in
          require
            (status = Unix.WEXITED 0 && stderr = "")
            "full-width human report succeeds";
          let expected =
            Printf.sprintf "final-value=%s type=%s bits=%s" value type_ bits
          in
          require
            (stdout |> String.split_on_char '\n'
            |> List.exists (fun line -> String.trim line = expected))
            ("human report retains all bits: " ^ expected)))
    [
      ( "0xffffffffffffffff;",
        "u64",
        "18446744073709551615",
        "0xffffffffffffffff" );
      ( "0x8000000000000000(I64i);",
        "i64",
        "-9223372036854775808",
        "0x8000000000000000" );
      ("9007199254740993;", "i64", "9007199254740993", "0x0020000000000001");
    ]

let faults_and_loops () =
  let cases =
    [
      ("84/0;", "HCIRVM0009");
      ("85%0;", "HCIRVM0009");
      ("0x8000000000000000(I64i)/-1;", "HCIRVM0010");
      ("0x8000000000000000(I64i)%-1;", "HCIRVM0010");
    ]
  in
  List.iter
    (fun (contents, code) ->
      with_file ".hc" contents (fun source ->
          let report = native_json ~status:1 source in
          require
            (report |> member "outcome" |> to_string = "error"
            && report |> member "executed_steps" <> `Null
            && member "final_value" report = `Null
            && first_code report = code)
            (contents ^ " native arithmetic fault report")))
    cases;
  List.iter
    (fun contents ->
      with_file ".hc" contents (fun source ->
          let report =
            native_json ~status:1 ~options:[ "--step-limit=17" ] source
          in
          require
            (report |> member "executed_steps" |> to_int = 17
            && first_code report = "HCIRVM0007")
            (contents ^ " exact infinite-loop budget")))
    [ "while(1);"; "for(0;1;0);" ]

let source_rejection_has_no_native_outcome () =
  List.iter
    (fun contents ->
      with_file ".hc" contents (fun source ->
          let report = native_json ~status:1 source in
          require
            (report |> member "executed_steps" = `Null
            && report |> member "final_value" = `Null
            && report |> member "native" |> member "image" = `Null
            && diagnostics report <> [])
            (contents ^ " rejects before native entry")))
    [
      "I64 x=42;";
      "I64 Bad(){F64 x=1.0;return 0;} 42;";
      "1/0; I64 F(I64 n=42){return n;} F();";
      "I64 F(I64 n=1<<3){return n;} F();";
      "extern I64 Add(I64 x); 42;";
      "\"output\";";
      "#exe {42;}\n42;";
    ]

let callable_resource_limits () =
  let recursion =
    "I64 Recur(I64 n){if(n)return Recur(n-1);return 42;}\nRecur(3);"
  in
  with_file ".hc" recursion (fun source ->
      let exact =
        native_json
          ~options:[ "--frame-byte-limit=32"; "--call-depth-limit=4" ]
          source
      in
      check_success exact;
      check_word exact "i64" "42" "0x000000000000002a";
      let depth =
        native_json ~status:1
          ~options:[ "--frame-byte-limit=32"; "--call-depth-limit=3" ]
          source
      in
      require
        (first_code depth = "HCIRVM0015"
        && depth |> member "executed_steps" <> `Null)
        "call-depth one-below is a native execution fault";
      let frame =
        native_json ~status:1
          ~options:[ "--frame-byte-limit=31"; "--call-depth-limit=4" ]
          source
      in
      require
        (first_code frame = "HCIRVM0011"
        && frame |> member "executed_steps" <> `Null)
        "active-frame one-below is a native execution fault")

let active_stack_root_boundary () =
  let contents = "42;" in
  let compiled = checked_source_compile contents in
  let root = Program.entry_stack_bytes compiled in
  require (root > 1) "root physical stack fixture requires a nontrivial charge";
  with_file ".hc" contents (fun source ->
      let exact =
        native_json
          ~options:[ "--active-stack-byte-limit=" ^ string_of_int root ]
          source
      in
      check_success exact;
      let one_below =
        native_json ~status:1
          ~options:[ "--active-stack-byte-limit=" ^ string_of_int (root - 1) ]
          source
      in
      require
        (one_below |> member "executed_steps" = `Null
        && one_below |> member "final_value" = `Null
        && one_below |> member "native" |> member "image" = `Null
        && first_code one_below = "HCNATIVE0002")
        "root one-below physical stack budget fails before native entry")

let exact_native_input_limits () =
  let contents = "if(0) 1; else 1+(2+(3+(4+(5+6))));" in
  let compiled = checked_source_compile contents in
  let ir = Program.ir_instructions compiled in
  let bytes = String.length (Program.code compiled) in
  let frame = Program.frame_bytes compiled in
  let blocks = Program.block_count compiled in
  require (frame > 0) "limit fixture must force a program spill";
  with_file ".hc" contents (fun source ->
      let options =
        [
          "--ir-instruction-limit=" ^ string_of_int ir;
          "--code-byte-limit=" ^ string_of_int bytes;
          "--stack-byte-limit=" ^ string_of_int frame;
          "--block-limit=" ^ string_of_int blocks;
        ]
      in
      let exact = native_json ~options source in
      check_success exact;
      let one_below name value code =
        let replacement = "--" ^ name ^ "=" ^ string_of_int (value - 1) in
        let filtered =
          List.filter
            (fun option ->
              not (String.starts_with ~prefix:("--" ^ name ^ "=") option))
            options
        in
        let report =
          native_json ~status:1 ~options:(replacement :: filtered) source
        in
        require
          (report_error_code report = code)
          (name ^ " one-below diagnostic")
      in
      one_below "ir-instruction-limit" ir "HCBACK0001";
      one_below "code-byte-limit" bytes "HCBACK0005";
      one_below "stack-byte-limit" frame "HCBACK0004";
      one_below "block-limit" blocks "HCBACK0001")

let host_jit_v1_rejected () =
  let status, stdout, stderr =
    invoke [ "run"; "--target=host-jit"; "--report-version=1"; native_fixture ]
  in
  require
    (status = Unix.WEXITED 1 && stdout = ""
    && String.starts_with ~prefix:"holyc: run: HCRUN0005" stderr)
    "host-jit must not impersonate the legacy IR v1 report"

let () =
  fixture_modes ();
  function_fixture_modes ();
  default_fixture_modes ();
  default_failure_reporting ();
  exact_meter_and_empty ();
  control_fixture_matches_ir_contract ();
  full_width_values ();
  faults_and_loops ();
  source_rejection_has_no_native_outcome ();
  callable_resource_limits ();
  active_stack_root_boundary ();
  exact_native_input_limits ();
  host_jit_v1_rejected ()
