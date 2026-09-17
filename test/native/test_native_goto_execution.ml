open Holyc_lib
module Program = X86_64_program
module Runtime = Native_program_execution
module VM = Ir_integer_interpreter

let require_ok show = function
  | Ok value -> value
  | Error error -> Alcotest.fail (show error)

let diagnostics_text diagnostics =
  diagnostics
  |> List.map (fun (error : Diagnostic.t) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let vm_errors_text errors =
  errors
  |> List.map (fun (error : VM.error) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let source_inputs ~mode contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-goto-execution.hc" ~contents
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> require_ok Fun.id
  in
  (session, config, source)

let public_run ?(max_steps = 10_000) ~mode contents =
  let session, config, source = source_inputs ~mode contents in
  run_integer_program session ~config ~source ~max_steps

let public_run_with_source ?(max_steps = 10_000) ~mode contents =
  let session, config, source = source_inputs ~mode contents in
  (source, run_integer_program session ~config ~source ~max_steps)

let native_report ?(max_steps = 10_000) ~mode contents =
  let session, config, source = source_inputs ~mode contents in
  Native_program.evaluate session ~config ~source ~max_steps

let native_report_with_source ?(max_steps = 10_000) ~mode contents =
  let session, config, source = source_inputs ~mode contents in
  (source, Native_program.evaluate session ~config ~source ~max_steps)

let native_success ?max_steps ~mode contents =
  let report = native_report ?max_steps ~mode contents in
  match Native_program.outcome report with
  | Ok checked -> (report, checked.value)
  | Error diagnostics -> Alcotest.fail (diagnostics_text diagnostics)

let batch_fixture ~mode contents =
  Native_scalar_fixture.compile ~mode ~path:"native-goto-batch.hc" ~contents ()
  |> require_ok diagnostics_text

let batch_success ?(max_steps = 10_000) ~mode contents =
  let fixture = batch_fixture ~mode contents in
  let execution =
    Native_scalar_fixture.execute ~max_steps fixture
    |> require_ok vm_errors_text
  in
  (fixture, execution)

let first_public_error = function
  | Ok _ -> Alcotest.fail "public goto execution unexpectedly succeeded"
  | Error [] -> Alcotest.fail "public goto execution returned no diagnostic"
  | Error (first :: _) -> first

let first_native_diagnostic report =
  match Native_program.outcome report with
  | Ok _ -> Alcotest.fail "native goto execution unexpectedly succeeded"
  | Error [] -> Alcotest.fail "native goto execution returned no diagnostic"
  | Error (first :: _) -> first

let vm_type_name = function
  | VM.I64 -> "I64"
  | VM.U64 -> "U64"

let program_type_name = function
  | Program.I64 -> "I64"
  | Program.U64 -> "U64"

let check_vm_word label expected_type expected_bits result =
  match VM.final_value result with
  | None -> Alcotest.failf "%s: missing checked-IR final value" label
  | Some word ->
      Alcotest.(check string)
        (label ^ " checked-IR type")
        expected_type (vm_type_name word.type_);
      Alcotest.(check int64)
        (label ^ " checked-IR bits")
        expected_bits word.bits

let check_native_word label expected_type expected_bits = function
  | None -> Alcotest.failf "%s: missing native final value" label
  | Some (word : Program.word) ->
      Alcotest.(check string)
        (label ^ " native type") expected_type
        (program_type_name word.type_);
      Alcotest.(check int64) (label ^ " native bits") expected_bits word.bits

let check_public_word label expected_type expected_bits = function
  | Error diagnostics -> Alcotest.fail (diagnostics_text diagnostics)
  | Ok checked ->
      check_vm_word (label ^ " public source") expected_type expected_bits
        checked.value

let compare_word ?(preparation = 0) ?(default_bytes = 0) ~mode ~label
    ~expected_type ~expected_bits contents =
  let public = public_run ~mode contents in
  check_public_word label expected_type expected_bits public;
  let fixture, batch = batch_success ~mode contents in
  check_vm_word label expected_type expected_bits batch;
  Alcotest.(check int)
    (label ^ " checked batch preparation")
    preparation fixture.preparation_steps;
  Alcotest.(check int)
    (label ^ " checked batch saved default bytes")
    default_bytes fixture.default_bytes;
  let report, native = native_success ~mode contents in
  check_native_word label expected_type expected_bits
    native.execution.final_value;
  Alcotest.(check int)
    (label ^ " exact checked-IR/native runtime meter")
    (VM.executed_steps batch) native.execution.executed_steps;
  Alcotest.(check int)
    (label ^ " native preparation meter")
    fixture.preparation_steps
    (Native_program.preparation_steps report);
  Alcotest.(check int)
    (label ^ " native saved-default meter")
    fixture.default_bytes
    (Native_program.default_bytes report)

let modes = [ Preprocessor.Jit; Preprocessor.Aot ]

let successful_goto_differentials () =
  let cases =
    [
      ( "split definition supplies the goto target",
        "#define DEST done\nU0 F(){goto DEST;DEST:}F();42;",
        0,
        0 );
      ( "split definition supplies the goto keyword",
        "#define GO goto\nU0 F(){GO done;done:}F();42;",
        0,
        0 );
      ( "forward goto and source side effects",
        "I64 F(){I64 n=0;goto done;n=99;done:n+=42;return n;}F();",
        0,
        0 );
      ( "backward goto",
        "I64 F(I64 n){I64 sum=0;again:sum+=n;n--;if(n)goto again;return \
         sum*2;}F(6);",
        0,
        0 );
      ( "same-name labels in separate functions",
        "I64 A(){same:return 20;}I64 B(){same:return 22;}A()+B();",
        0,
        0 );
      ( "earlier label does not shadow a later local declaration",
        "I64 F(){goto done;done:I64 done=42;return done;}F();",
        0,
        0 );
      ( "consecutive labels and later goto",
        "I64 F(){goto third;first:second:return 7;third:return 42;}F();",
        0,
        0 );
      ( "labels after return and goto",
        "I64 F(){goto next;return 7;after:return 8;next:goto done;done:return \
         42;}F();",
        0,
        0 );
      ( "nested branch loop break and goto",
        "I64 F(){I64 n=0;while(1){if(n==2)break;n++;}goto \
         done;n=99;done:return n+40;}F();",
        0,
        0 );
      ( "goto enters a nested conditional without its condition",
        "I64 F(){I64 n=2;goto branch;if(0){branch:n+=40;}else n=7;return \
         n;}F();",
        0,
        0 );
      ( "goto enters a for body and retains the update continuation",
        "I64 F(){I64 n=0;goto body;for(n=99;0;n++){body:n++;}return n+40;}F();",
        0,
        0 );
      ( "for-update goto uses its original source occurrence",
        "I64 F(){I64 n=0;for(n=0;n<2;goto again){n++;again:if(n==1)n++;else \
         goto done;}done:return n+40;}F();",
        0,
        0 );
      ( "recursive local frames survive goto transfers",
        "I64 R(I64 n){I64 saved=n;if(n)goto recurse;return 0;recurse:return \
         saved+R(n-1);}R(6)*2;",
        0,
        0 );
      ( "narrow default and U0 goto calls",
        "I8 D(I8 n=298){goto done;n=0;done:return n;}U0 V(U8 \
         n){again:if(!n)goto done;n--;goto again;done:return;}I64 \
         F(){V(2);return D();}F();",
        3,
        8 );
      ( "trailing U0 label falls through",
        "U0 V(){goto tail;first:second:return;tail:}V();42;",
        0,
        0 );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, source, preparation, default_bytes) ->
          compare_word ~preparation ~default_bytes ~mode ~label
            ~expected_type:"I64" ~expected_bits:42L source)
        cases)
    modes

let u0_last_expression_has_no_word () =
  let source = "U0 V(){goto done;1/0;done:return;}42;V();" in
  List.iter
    (fun mode ->
      let public =
        public_run ~mode source |> require_ok diagnostics_text |> fun checked ->
        checked.value
      in
      Alcotest.(check bool)
        "public U0 goto clears the prior word" true
        (Option.is_none (VM.final_value public));
      let _, batch = batch_success ~mode source in
      Alcotest.(check bool)
        "checked batch U0 goto has no final word" true
        (Option.is_none (VM.final_value batch));
      let _, native = native_success ~mode source in
      Alcotest.(check bool)
        "native U0 goto has no final word" true
        (Option.is_none native.execution.final_value);
      Alcotest.(check int)
        "U0 checked-IR/native runtime meter" (VM.executed_steps batch)
        native.execution.executed_steps)
    modes

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

let nth_substring_start text fragment occurrence =
  let fragment_length = String.length fragment in
  let rec find_from start remaining =
    let rec search index =
      if index + fragment_length > String.length text then
        Alcotest.failf "could not find occurrence %d of %S" occurrence fragment
      else if String.sub text index fragment_length = fragment then
        if remaining = 1 then index
        else find_from (index + fragment_length) (remaining - 1)
      else search (index + 1)
    in
    search start
  in
  find_from 0 occurrence

let check_primary label source contents fragment occurrence
    (error : Diagnostic.t) =
  let start = nth_substring_start contents fragment occurrence in
  Alcotest.(check bool)
    (label ^ " source id") true
    (Source_id.equal error.primary.source (Source_file.id source));
  Alcotest.(check int) (label ^ " primary start") start error.primary.start;
  Alcotest.(check int)
    (label ^ " primary stop")
    (start + String.length fragment)
    error.primary.stop

let invalid_labels_reject_before_native_entry () =
  let label_cases =
    [
      ( "missing called target",
        "I64 Bad(){goto missing;return 42;}Bad();",
        "not defined",
        "goto missing;",
        1,
        "HCEVAL0003" );
      ( "missing unused target",
        "I64 Bad(){goto missing;return 42;}42;",
        "not defined",
        "goto missing;",
        1,
        "HCEVAL0003" );
      ( "unreachable missing target",
        "I64 Bad(){return 42;goto missing;}42;",
        "not defined",
        "goto missing;",
        1,
        "HCEVAL0003" );
      ( "duplicate target",
        "I64 Bad(){same:same:return 42;}42;",
        "defined more than once",
        "same:",
        2,
        "HCEVAL0003" );
      ( "cross-function target",
        "I64 A(){goto shared;return 1;}I64 B(){shared:return 42;}42;",
        "not defined",
        "goto shared;",
        1,
        "HCEVAL0003" );
      ( "top-level goto",
        "goto missing;",
        "outside the closed native program execution domain",
        "goto missing;",
        1,
        "HCRUN0001" );
      ( "top-level label",
        "outside:42;",
        "outside the closed native program execution domain",
        "outside:",
        1,
        "HCRUN0001" );
    ]
  in
  let parser_cases =
    [
      ( "visible parameter cannot start a label definition",
        "I64 F(I64 done){goto done;done:return done;}F(42);",
        "expected ';' or ','" );
      ( "visible callable cannot start a label definition",
        "I64 Target(){return 42;}I64 F(){goto Target;Target:return \
         Target();}F();",
        "expected ';' or ','" );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, contents, fragment, primary, occurrence, native_code) ->
          let public_source, public_result =
            public_run_with_source ~mode contents
          in
          let public_error = first_public_error public_result in
          Alcotest.(check string)
            (label ^ " public diagnostic")
            "HCEVAL0003" public_error.code;
          Alcotest.(check bool)
            (label ^ " public source reports a real label error")
            true
            (public_error.message <> "");
          check_primary (label ^ " public") public_source contents primary
            occurrence public_error;
          let native_source, report =
            native_report_with_source ~mode contents
          in
          let native_error = first_native_diagnostic report in
          Alcotest.(check string)
            (label ^ " native diagnostic")
            native_code native_error.code;
          Alcotest.(check bool)
            (label ^ " native semantic explanation")
            true
            (contains native_error.message fragment);
          check_primary (label ^ " native") native_source contents primary
            occurrence native_error;
          Alcotest.(check bool)
            (label ^ " has no native image")
            true
            (Option.is_none (Native_program.image report));
          Alcotest.(check bool)
            (label ^ " has no native outcome")
            true
            (Option.is_none (Native_program.native_outcome report));
          Alcotest.(check (option int))
            (label ^ " has no native progress")
            None
            (Native_program.executed_steps report))
        label_cases;
      List.iter
        (fun (label, contents, fragment) ->
          let public_error = first_public_error (public_run ~mode contents) in
          Alcotest.(check bool)
            (label ^ " public parser classification")
            true
            (contains public_error.message fragment);
          let report = native_report ~mode contents in
          let native_error = first_native_diagnostic report in
          Alcotest.(check bool)
            (label ^ " native parser classification")
            true
            (contains native_error.message fragment);
          Alcotest.(check bool)
            (label ^ " parser failure has no native image")
            true
            (Option.is_none (Native_program.image report)))
        parser_cases)
    modes

let goto_word_return_completeness () =
  let missing = "I64 Bad(){goto tail;tail:}Bad();" in
  let returning = "I64 Good(){goto tail;return 7;tail:return 42;}Good();" in
  let void = "U0 V(){goto tail;return;tail:}V();42;" in
  List.iter
    (fun mode ->
      let public_error = first_public_error (public_run ~mode missing) in
      Alcotest.(check string)
        "public goto word fallthrough fault" "HCIRVM0013" public_error.code;
      Alcotest.(check bool)
        "public goto word fallthrough identifies Bad" true
        (List.mem "function=Bad" public_error.notes);
      let fixture = batch_fixture ~mode missing in
      let batch_error =
        match Native_scalar_fixture.execute ~max_steps:10_000 fixture with
        | Ok _ ->
            Alcotest.fail "checked missing-return goto unexpectedly succeeded"
        | Error [] -> Alcotest.fail "checked missing-return goto has no fault"
        | Error (first :: _) -> first
      in
      Alcotest.(check string)
        "isolated checked goto word fallthrough fault" "HCIRVM0013"
        batch_error.code;
      let report = native_report ~mode missing in
      let native_error = first_native_diagnostic report in
      Alcotest.(check string)
        "native goto word fallthrough preflight" "HCBACK0002" native_error.code;
      Alcotest.(check bool)
        "native goto word fallthrough names missing value return" true
        (contains native_error.message
           "reachable return without its own word value");
      Alcotest.(check bool)
        "native goto word fallthrough has no image" true
        (Option.is_none (Native_program.image report));
      Alcotest.(check (option int))
        "native goto word fallthrough has no runtime progress" None
        (Native_program.executed_steps report);
      compare_word ~mode ~label:"goto word-returning control"
        ~expected_type:"I64" ~expected_bits:42L returning;
      compare_word ~mode ~label:"goto U0 fallthrough control"
        ~expected_type:"I64" ~expected_bits:42L void)
    modes

let unsupported_regions_reject_before_native_entry () =
  let cases =
    [
      ("assembly block", "U0 F(){goto done;asm {} done:return;}F();", "asm {}");
      ("lock region", "U0 F(){done:lock goto done;}F();", "lock goto done;");
      ( "try/catch region",
        "U0 F(){done:try goto done;catch return;}F();",
        "try goto done;catch return;" );
      ( "switch region",
        "U0 F(I64 n){switch(n){case 0:goto done;}done:return;}F(0);",
        "switch(n){case 0:goto done;}" );
      ( "sub-switch region",
        "U0 F(I64 n){switch(n){start:case 0:goto done;end:}done:return;}F(0);",
        "switch(n){start:case 0:goto done;end:}" );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, contents, primary) ->
          let public_source, public_result =
            public_run_with_source ~mode contents
          in
          let public_error = first_public_error public_result in
          Alcotest.(check string)
            (label ^ " public source gate")
            "HCRUN0001" public_error.code;
          check_primary (label ^ " public") public_source contents primary 1
            public_error;
          let native_source, report =
            native_report_with_source ~mode contents
          in
          let native_error = first_native_diagnostic report in
          Alcotest.(check string)
            (label ^ " native source gate")
            "HCRUN0001" native_error.code;
          check_primary (label ^ " native") native_source contents primary 1
            native_error;
          Alcotest.(check bool)
            (label ^ " has no native image")
            true
            (Option.is_none (Native_program.image report));
          Alcotest.(check (option int))
            (label ^ " has no native progress")
            None
            (Native_program.executed_steps report))
        cases)
    modes

let skipped_initialization_faults_match_checked_ir () =
  let cases =
    [
      ( "SkipRead",
        "I64 SkipRead(){goto read;I64 n=42;read:return n;}SkipRead();" );
      ( "SkipUpdate",
        "I64 SkipUpdate(){goto update;U8 n=41;update:return ++n;}SkipUpdate();"
      );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (owner, source) ->
          let public_error = first_public_error (public_run ~mode source) in
          Alcotest.(check string)
            "public skipped-initialization fault" "HCIRVM0012" public_error.code;
          let fixture = batch_fixture ~mode source in
          let batch_error =
            match Native_scalar_fixture.execute ~max_steps:10_000 fixture with
            | Ok _ ->
                Alcotest.fail "checked goto IR unexpectedly read skipped local"
            | Error [] -> Alcotest.fail "checked goto IR returned no fault"
            | Error (first :: _) -> first
          in
          Alcotest.(check string)
            "checked IR skipped-initialization fault" "HCIRVM0012"
            batch_error.code;
          let report = native_report ~mode source in
          let diagnostic = first_native_diagnostic report in
          Alcotest.(check bool)
            "native API retains its compiled image after the reached goto fault"
            true
            (Option.is_some (Native_program.image report));
          Alcotest.(check string)
            "native skipped-initialization diagnostic" "HCIRVM0012"
            diagnostic.code;
          let fault =
            match Native_program.native_outcome report with
            | Some (Program.Fault fault) -> fault
            | Some (Program.Completed _) ->
                Alcotest.fail "native skipped-local source completed"
            | None -> Alcotest.fail "native skipped-local source has no fault"
          in
          Alcotest.(check int)
            "checked IR/native skipped-local step" batch_error.executed_steps
            fault.executed_steps;
          Alcotest.(check (option string))
            "skipped-local fault owner" (Some owner) fault.function_name)
        cases)
    modes

let exact_infinite_goto_budget_and_recovery () =
  let infinite = "U0 Spin(){again:goto again;}Spin();" in
  let healthy = "I64 Healthy(){goto done;return 7;done:return 42;}Healthy();" in
  List.iter
    (fun mode ->
      let public_error =
        first_public_error (public_run ~max_steps:17 ~mode infinite)
      in
      Alcotest.(check string)
        "public infinite goto budget" "HCIRVM0007" public_error.code;
      let fixture = batch_fixture ~mode infinite in
      let batch_error =
        match Native_scalar_fixture.execute ~max_steps:17 fixture with
        | Ok _ -> Alcotest.fail "checked infinite goto unexpectedly completed"
        | Error [] -> Alcotest.fail "checked infinite goto returned no fault"
        | Error (first :: _) -> first
      in
      Alcotest.(check string)
        "checked infinite goto budget" "HCIRVM0007" batch_error.code;
      Alcotest.(check int)
        "checked infinite goto consumes exact allowance" 17
        batch_error.executed_steps;
      let report = native_report ~max_steps:17 ~mode infinite in
      let diagnostic = first_native_diagnostic report in
      Alcotest.(check string)
        "native infinite goto budget" "HCIRVM0007" diagnostic.code;
      let native_fault =
        match Native_program.native_outcome report with
        | Some (Program.Fault fault) -> fault
        | Some (Program.Completed _) ->
            Alcotest.fail "native infinite goto completed"
        | None -> Alcotest.fail "native infinite goto has no runtime fault"
      in
      Alcotest.(check int)
        "native infinite goto consumes exact allowance" 17
        native_fault.executed_steps;
      compare_word ~mode ~label:"fresh healthy goto after budget fault"
        ~expected_type:"I64" ~expected_bits:42L healthy)
    modes

let () =
  match Runtime.platform () with
  | Runtime.Unsupported ->
      Alcotest.fail "native goto execution tests require an x86-64 host"
  | Runtime.Windows_x86_64 | Runtime.Linux_x86_64 ->
      Alcotest.run "holyc native goto execution"
        [
          ( "goto execution",
            [
              Alcotest.test_case "successful goto programs match checked IR"
                `Quick successful_goto_differentials;
              Alcotest.test_case "U0 goto retains no-value completion" `Quick
                u0_last_expression_has_no_word;
              Alcotest.test_case "invalid labels reject before native entry"
                `Quick invalid_labels_reject_before_native_entry;
              Alcotest.test_case
                "goto preserves native word-return completeness" `Quick
                goto_word_return_completeness;
              Alcotest.test_case
                "unsupported goto regions reject before native entry" `Quick
                unsupported_regions_reject_before_native_entry;
              Alcotest.test_case
                "skipped initialization faults match exact checked IR" `Quick
                skipped_initialization_faults_match_checked_ir;
              Alcotest.test_case "infinite goto meter is exact and recovers"
                `Quick exact_infinite_goto_budget_and_recovery;
            ] );
        ]
