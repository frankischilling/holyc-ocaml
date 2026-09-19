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
  let path = Filename.temp_file "holyc native goto cli " suffix in
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
    "usage: test_native_goto_cli.exe <holyc.exe> <integer-goto.hc>"

let compiler = Sys.argv.(1)
let goto_fixture = Sys.argv.(2)

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
    "host-jit goto report identity";
  let expected_platform =
    match Runtime.platform () with
    | Runtime.Windows_x86_64 -> "windows-x86_64"
    | Runtime.Linux_x86_64 -> "linux-x86_64"
    | Runtime.Unsupported -> failwith "native goto CLI tests require x86-64"
  in
  require
    (report |> member "native" |> member "platform" |> to_string
   = expected_platform)
    "host-jit goto report platform";
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

let first_diagnostic report =
  match diagnostics report with
  | first :: _ -> first
  | [] -> failwith "expected a goto diagnostic"

let first_code report = first_diagnostic report |> member "code" |> to_string

let first_message report =
  first_diagnostic report |> member "message" |> to_string

let check_success report =
  require
    (report |> member "outcome" |> to_string = "success"
    && report |> member "termination" |> to_string = "stream-end"
    && diagnostics report = []
    && member "command_error" report = `Null)
    "successful goto report"

let check_word report type_ value bits =
  let word = member "final_value" report in
  require
    (word |> member "type" |> to_string = type_
    && word |> member "value" |> to_string = value
    && word |> member "bits" |> to_string = bits)
    ("unexpected goto final word: " ^ Yojson.Safe.to_string word)

let preparation report = report |> member "compiled_initializer_steps" |> to_int
let default_bytes report = report |> member "prepared_default_bytes" |> to_int
let executed_steps report = report |> member "executed_steps" |> to_int

let function_count report =
  report |> member "native" |> member "image" |> member "function_count"
  |> to_int

let mode_value = function
  | "jit" -> Holyc_lib.Preprocessor.Jit
  | "aot" -> Holyc_lib.Preprocessor.Aot
  | mode -> failwith ("unknown goto CLI mode: " ^ mode)

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

let contains text fragment =
  let text_length = String.length text in
  let fragment_length = String.length fragment in
  let rec search index =
    if fragment_length = 0 then true
    else if index + fragment_length > text_length then false
    else if String.sub text index fragment_length = fragment then true
    else search (index + 1)
  in
  search 0

let substring_start contents fragment occurrence =
  let rec search index remaining =
    if index + String.length fragment > String.length contents then
      failwith
        (Printf.sprintf "missing occurrence %d of %S" occurrence fragment)
    else if String.sub contents index (String.length fragment) = fragment then
      if remaining = 1 then index
      else search (index + String.length fragment) (remaining - 1)
    else search (index + 1) remaining
  in
  search 0 occurrence

let check_primary source contents fragment occurrence report =
  let primary = first_diagnostic report |> member "primary" in
  let start = substring_start contents fragment occurrence in
  require
    (primary |> member "source_id" |> to_int = 0
    && Filename.basename (primary |> member "path" |> to_string)
       = Filename.basename source
    && primary |> member "start" |> to_int = start
    && primary |> member "stop" |> to_int = start + String.length fragment)
    ("goto diagnostic lost its original source location: "
    ^ Yojson.Safe.to_string primary)

let check_no_native_entry report =
  require
    (member "executed_steps" report = `Null
    && member "final_value" report = `Null
    && report |> member "native" |> member "image" = `Null)
    "invalid goto source must reject before native entry"

let maintained_fixture_contract () =
  List.iter
    (fun mode ->
      (* Public source IR remains the independent value/preparation oracle. JIT
         may account source task units differently, so runtime budgets below come
         only from explicit execution of the exact isolated checked batch. *)
      let source_ir = ir_json ~mode goto_fixture in
      check_success source_ir;
      check_word source_ir "i64" "42" "0x000000000000002a";
      require
        (preparation source_ir = 3)
        "maintained goto fixture has one three-step literal default";
      (* Saved-default bytes are exposed by the native report. The public IR
         report supplies preparation work; exact retained bytes are checked
         against the original isolated preparation and native report below. *)
      let fixture = batch_fixture ~mode goto_fixture in
      require
        (fixture.preparation_steps = 3 && fixture.default_bytes = 8)
        "isolated goto fixture preparation differs from source contract";
      let batch = batch_execution ~max_steps:100_000 fixture in
      let batch_steps = VM.executed_steps batch in
      require (batch_steps > 1)
        "goto fixture needs a one-below runtime boundary";
      (match VM.final_value batch with
      | Some word ->
          require
            (word.type_ = VM.I64 && word.bits = 42L)
            "checked goto fixture IR did not return I64 42"
      | None -> failwith "checked goto fixture IR has no final word");
      (match
         Native_scalar_fixture.execute ~max_steps:(batch_steps - 1) fixture
       with
      | Error (first :: _) ->
          require
            (first.code = "HCIRVM0007" && first.executed_steps = batch_steps - 1)
            "checked goto fixture IR one-below runtime boundary"
      | Error [] -> failwith "checked goto fixture one-below returned no fault"
      | Ok _ -> failwith "checked goto fixture accepted a one-below allowance");
      let native = host_json ~mode goto_fixture in
      check_success native;
      check_word native "i64" "42" "0x000000000000002a";
      require
        (executed_steps native = batch_steps
        && preparation native = fixture.preparation_steps
        && default_bytes native = fixture.default_bytes)
        "host-jit goto meters differ from its exact checked batch";
      require
        (function_count native = 11)
        "maintained goto fixture must retain eleven named functions";
      let exact =
        host_json ~mode
          ~options:[ "--step-limit=" ^ string_of_int batch_steps ]
          goto_fixture
      in
      check_success exact;
      check_word exact "i64" "42" "0x000000000000002a";
      require
        (executed_steps exact = batch_steps)
        "goto fixture exact native runtime allowance";
      let one_below =
        host_json ~status:1 ~mode
          ~options:[ "--step-limit=" ^ string_of_int (batch_steps - 1) ]
          goto_fixture
      in
      require
        (first_code one_below = "HCIRVM0007"
        && executed_steps one_below = batch_steps - 1
        && member "final_value" one_below = `Null)
        "goto fixture one-below native runtime allowance";
      let exact_resources =
        host_json ~mode
          ~options:[ "--initializer-step-limit=3"; "--default-byte-limit=8" ]
          goto_fixture
      in
      check_success exact_resources;
      require
        (preparation exact_resources = 3 && default_bytes exact_resources = 8)
        "goto fixture exact default preparation bounds";
      let prep_one_below =
        host_json ~status:1 ~mode
          ~options:[ "--initializer-step-limit=2"; "--default-byte-limit=8" ]
          goto_fixture
      in
      require
        (first_code prep_one_below = "HCIRVM0007"
        && preparation prep_one_below = 2
        && default_bytes prep_one_below = 0
        && member "executed_steps" prep_one_below = `Null
        && prep_one_below |> member "native" |> member "image" = `Null)
        "goto fixture one-below default preparation";
      let byte_one_below =
        host_json ~status:1 ~mode
          ~options:[ "--default-byte-limit=7" ]
          goto_fixture
      in
      require
        (first_code byte_one_below = "HCIRVM0011"
        && preparation byte_one_below = 0
        && default_bytes byte_one_below = 0
        && member "executed_steps" byte_one_below = `Null
        && byte_one_below |> member "native" |> member "image" = `Null)
        "goto fixture one-below saved-default bytes")
    [ "jit"; "aot" ]

let skipped_initialization_faults () =
  List.iter
    (fun (source_text, owner) ->
      with_file ".hc" source_text (fun source ->
          List.iter
            (fun mode ->
              let fixture = batch_fixture ~mode source in
              let batch_error =
                match
                  Native_scalar_fixture.execute ~max_steps:10_000 fixture
                with
                | Ok _ ->
                    failwith "checked skipped-local goto unexpectedly succeeded"
                | Error [] -> failwith "checked skipped-local goto has no fault"
                | Error (first :: _) -> first
              in
              require
                (batch_error.code = "HCIRVM0012"
                && batch_error.function_name = Some owner)
                "checked skipped-local goto fault";
              let native = host_json ~status:1 ~mode source in
              let notes =
                first_diagnostic native |> member "notes" |> to_list
                |> List.map to_string
              in
              require
                (first_code native = "HCIRVM0012"
                && executed_steps native = batch_error.executed_steps
                && List.mem "stage=execution" notes
                && List.mem ("function_name=" ^ owner) notes
                && member "final_value" native = `Null
                && native |> member "native" |> member "image" = `Null)
                (Printf.sprintf
                   "native skipped-local goto fault differs from checked IR \
                    (%d steps): %s"
                   batch_error.executed_steps
                   (Yojson.Safe.to_string native)))
            [ "jit"; "aot" ]))
    [
      ( "I64 SkipRead(){goto read;I64 n=42;read:return n;}SkipRead();",
        "SkipRead" );
      ( "I64 SkipUpdate(){goto update;U8 n=41;update:return ++n;}SkipUpdate();",
        "SkipUpdate" );
    ]

let infinite_goto_budget_and_recovery () =
  with_file ".hc" "U0 Spin(){again:goto again;}Spin();" (fun infinite ->
      with_file ".hc"
        "I64 Healthy(){goto done;return 7;done:return 42;}Healthy();"
        (fun healthy ->
          List.iter
            (fun mode ->
              let fixture = batch_fixture ~mode infinite in
              let batch_error =
                match Native_scalar_fixture.execute ~max_steps:17 fixture with
                | Ok _ ->
                    failwith "checked infinite goto unexpectedly completed"
                | Error [] -> failwith "checked infinite goto has no fault"
                | Error (first :: _) -> first
              in
              require
                (batch_error.code = "HCIRVM0007"
                && batch_error.executed_steps = 17)
                "checked infinite goto exact 17-step boundary";
              let native =
                host_json ~status:1 ~mode ~options:[ "--step-limit=17" ]
                  infinite
              in
              require
                (first_code native = "HCIRVM0007" && executed_steps native = 17)
                "native infinite goto exact 17-step boundary";
              let recovered = host_json ~mode healthy in
              check_success recovered;
              check_word recovered "i64" "42" "0x000000000000002a")
            [ "jit"; "aot" ]))

let invalid_label_gates () =
  let cases =
    [
      ( "I64 Bad(){goto missing;return 42;}Bad();",
        "not defined",
        "goto missing;",
        1,
        "HCEVAL0003" );
      ( "I64 Bad(){goto missing;return 42;}42;",
        "not defined",
        "goto missing;",
        1,
        "HCEVAL0003" );
      ( "I64 Bad(){return 42;goto missing;}42;",
        "not defined",
        "goto missing;",
        1,
        "HCEVAL0003" );
      ( "I64 Bad(){same:same:return 42;}42;",
        "defined more than once",
        "same:",
        2,
        "HCEVAL0003" );
      ( "I64 A(){goto shared;return 1;}I64 B(){shared:return 42;}42;",
        "not defined",
        "goto shared;",
        1,
        "HCEVAL0003" );
      ( "goto missing;",
        "outside the closed native program",
        "goto missing;",
        1,
        "HCRUN0001" );
      ( "outside:42;",
        "outside the closed native program",
        "outside:",
        1,
        "HCRUN0001" );
    ]
  in
  List.iter
    (fun (contents, native_fragment, primary, occurrence, native_code) ->
      with_file ".hc" contents (fun source ->
          List.iter
            (fun mode ->
              let interpreted = ir_json ~status:1 ~mode source in
              require
                (first_code interpreted = "HCEVAL0003")
                "invalid public goto/label diagnostic";
              check_primary source contents primary occurrence interpreted;
              let report = host_json ~status:1 ~mode source in
              require
                (first_code report = native_code
                && contains (first_message report) native_fragment)
                "invalid native goto/label diagnostic and explanation";
              check_primary source contents primary occurrence report;
              check_no_native_entry report)
            [ "jit"; "aot" ]))
    cases

let goto_return_completeness () =
  let contents = "I64 Bad(){goto tail;tail:}Bad();" in
  with_file ".hc" contents (fun source ->
      List.iter
        (fun mode ->
          let interpreted = ir_json ~status:1 ~mode source in
          require
            (first_code interpreted = "HCIRVM0013"
            && executed_steps interpreted > 0
            && member "final_value" interpreted = `Null)
            "interpreted goto must fault when a word return is missing";
          let native = host_json ~status:1 ~mode source in
          require
            (first_code native = "HCBACK0002"
            && contains (first_message native)
                 "reachable return without its own word value")
            "native goto must retain word-return preflight";
          check_primary source contents "I64 Bad(){goto tail;tail:}" 1 native;
          check_no_native_entry native)
        [ "jit"; "aot" ]);
  List.iter
    (fun contents ->
      with_file ".hc" contents (fun source ->
          List.iter
            (fun mode ->
              let interpreted = ir_json ~mode source in
              let native = host_json ~mode source in
              List.iter
                (fun report ->
                  check_success report;
                  check_word report "i64" "42" "0x000000000000002a")
                [ interpreted; native ])
            [ "jit"; "aot" ]))
    [
      "I64 Good(){goto tail;return 7;tail:return 42;}Good();";
      "U0 V(){goto tail;return;tail:}V();42;";
    ]

let unsupported_goto_regions () =
  List.iter
    (fun (contents, primary) ->
      with_file ".hc" contents (fun source ->
          List.iter
            (fun mode ->
              let interpreted = ir_json ~status:1 ~mode source in
              let native = host_json ~status:1 ~mode source in
              List.iter
                (fun report ->
                  require
                    (first_code report = "HCRUN0001")
                    "unsupported goto region must reach the execution source \
                     gate";
                  check_primary source contents primary 1 report)
                [ interpreted; native ];
              check_no_native_entry native)
            [ "jit"; "aot" ]))
    [
      ("U0 F(){goto done;asm {} done:return;}F();", "asm {}");
      ("U0 F(){done:lock goto done;}F();", "lock goto done;");
      ( "U0 F(){done:try goto done;catch return;}F();",
        "try goto done;catch return;" );
      ( "U0 F(I64 n){switch[n]{case 0:goto done;}done:return;}F(0);",
        "switch[n]{case 0:goto done;}" );
      ( "U0 F(I64 n){switch(n){start:case 0:goto done;end:}done:return;}F(0);",
        "switch(n){start:case 0:goto done;end:}" );
    ]

let () =
  maintained_fixture_contract ();
  skipped_initialization_faults ();
  infinite_goto_budget_and_recovery ();
  invalid_label_gates ();
  goto_return_completeness ();
  unsupported_goto_regions ()
