open Holyc_lib
module VM = Ir_integer_interpreter
module Labels = Semantic_label_resolution
module Symbol = Semantic_symbol

let require_ok show = function
  | Ok value -> value
  | Error error -> Alcotest.fail (show error)

let diagnostics_text diagnostics =
  diagnostics
  |> List.map (fun (error : Diagnostic.t) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let config ?working_directory mode =
  Preprocessor.Config.create ?working_directory ~compilation_mode:mode ()
  |> require_ok Fun.id

let run ?working_directory ?(max_steps = 10_000) ~mode ~path contents =
  let session = Session.create () in
  let source = Session.add_source session ~path ~contents in
  let config = config ?working_directory mode in
  (session, source, run_integer_program session ~config ~source ~max_steps)

let first_error = function
  | Ok _ -> Alcotest.fail "goto source unexpectedly succeeded"
  | Error [] -> Alcotest.fail "goto source returned no diagnostic"
  | Error (first :: _) -> first

let expect_word label expected = function
  | Error diagnostics -> Alcotest.fail (diagnostics_text diagnostics)
  | Ok checked -> (
      let result = checked.value in
      Alcotest.(check bool)
        (label ^ " reaches stream end")
        true
        (VM.termination result = VM.Stream_end);
      match VM.final_value result with
      | Some word ->
          Alcotest.(check bool)
            (label ^ " result class") true (word.type_ = VM.I64);
          Alcotest.(check int64) (label ^ " result bits") expected word.bits;
          result
      | None -> Alcotest.failf "%s produced no final word" label)

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
    if remaining = 0 then start
    else
      let rec search index =
        if index + fragment_length > String.length text then
          Alcotest.failf "could not find occurrence %d of %S" occurrence
            fragment
        else if String.sub text index fragment_length = fragment then
          if remaining = 1 then index
          else find_from (index + fragment_length) (remaining - 1)
        else search (index + 1)
      in
      search start
  in
  find_from 0 occurrence

let check_original_primary label source contents fragment occurrence
    (error : Diagnostic.t) =
  let start = nth_substring_start contents fragment occurrence in
  Alcotest.(check bool)
    (label ^ " keeps the original source id")
    true
    (Source_id.equal error.primary.source (Source_file.id source));
  Alcotest.(check int) (label ^ " primary start") start error.primary.start;
  Alcotest.(check int)
    (label ^ " primary stop")
    (start + String.length fragment)
    error.primary.stop

let modes = [ Preprocessor.Jit; Preprocessor.Aot ]

let valid_control_flow () =
  let cases =
    [
      ( "definition supplies only the goto target",
        "#define DEST done\nU0 F(){goto DEST;DEST:}F();42;",
        42L );
      ( "definition supplies only the goto keyword",
        "#define GO goto\nU0 F(){GO done;done:}F();42;",
        42L );
      ( "forward goto skips source side effects",
        "I64 F(){I64 n=0;goto done;n=99;done:n+=42;return n;}F();",
        42L );
      ( "backward goto loops to a prior label",
        "I64 F(I64 n){I64 sum=0;again:sum+=n;n--;if(n)goto again;return \
         sum*2;}F(6);",
        42L );
      ( "same label spelling is function scoped",
        "I64 A(){same:return 20;}I64 B(){same:return 22;}A()+B();",
        42L );
      ( "earlier label does not shadow a later local declaration",
        "I64 F(){goto done;done:I64 done=42;return done;}F();",
        42L );
      ( "consecutive language labels retain one destination each",
        "I64 F(){goto third;first:second:return 7;third:return 42;}F();",
        42L );
      ( "trailing U0 label falls through without a value",
        "U0 V(){goto tail;first:second:return;tail:}V();42;",
        42L );
      ( "label after return is reachable from an earlier goto",
        "I64 F(){goto after;return 7;after:return 42;}F();",
        42L );
      ( "labels after goto preserve later transfer order",
        "I64 F(){goto next;dead:return 7;next:goto done;done:return 42;}F();",
        42L );
      ( "nested branch loop break and goto compose",
        "I64 F(){I64 n=0;while(1){if(n==2)break;n++;}goto \
         done;n=99;done:return n+40;}F();",
        42L );
      ( "goto enters a nested conditional without evaluating its condition",
        "I64 F(){I64 n=2;goto branch;if(0){branch:n+=40;}else n=7;return \
         n;}F();",
        42L );
      ( "goto enters a for body and retains its update continuation",
        "I64 F(){I64 n=0;goto body;for(n=99;0;n++){body:n++;}return n+40;}F();",
        42L );
      ( "for update goto resolves by source identity despite emitted order",
        "I64 F(){I64 n=0;for(n=0;n<2;goto again){n++;again:if(n==1)n++;else \
         goto done;}done:return n+40;}F();",
        42L );
      ( "recursive goto keeps each local frame isolated",
        "I64 R(I64 n){I64 saved=n;if(n)goto recurse;return 0;recurse:return \
         saved+R(n-1);}R(6)*2;",
        42L );
      ( "narrow default and U0 goto calls compose",
        "I8 D(I8 n=298){goto done;n=0;done:return n;}U0 V(U8 \
         n){again:if(!n)goto done;n--;goto again;done:return;}I64 \
         F(){V(2);return D();}F();",
        42L );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, source, expected) ->
          let _, _, result = run ~mode ~path:"integer-goto-valid.hc" source in
          ignore (expect_word label expected result))
        cases)
    modes

let resolution_failures_cover_every_definition () =
  let label_cases =
    [
      ( "missing target in called function",
        "I64 Bad(){goto missing;return 42;}Bad();",
        "not defined",
        "goto missing;",
        1 );
      ( "missing target in unused definition",
        "I64 Bad(){goto missing;return 42;}42;",
        "not defined",
        "goto missing;",
        1 );
      ( "missing target after unreachable return",
        "I64 Bad(){return 42;goto missing;}42;",
        "not defined",
        "goto missing;",
        1 );
      ( "duplicate labels in unused function",
        "I64 Bad(){same:same:return 42;}42;",
        "defined more than once",
        "same:",
        2 );
      ( "duplicate label after transfer remains invalid",
        "I64 Bad(){goto done;done:done:return 42;}42;",
        "defined more than once",
        "done:",
        2 );
      ( "goto cannot cross function ownership",
        "I64 A(){goto shared;return 1;}I64 B(){shared:return 42;}42;",
        "not defined",
        "goto shared;",
        1 );
      ( "top-level goto stays invalid",
        "goto nowhere;",
        "outside a function body",
        "goto nowhere;",
        1 );
      ( "top-level label stays invalid",
        "outside:42;",
        "outside a function body",
        "outside:",
        1 );
    ]
  in
  let parser_cases =
    [
      ( "visible parameter starts an expression rather than a label",
        "I64 F(I64 done){goto done;done:return done;}F(42);",
        "expected ';' or ','" );
      ( "visible callable starts an expression rather than a label",
        "I64 Target(){return 42;}I64 F(){goto Target;Target:return \
         Target();}F();",
        "expected ';' or ','" );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, contents, fragment, primary, occurrence) ->
          let _, source, result =
            run ~mode ~path:"integer-goto-invalid.hc" contents
          in
          let error = first_error result in
          Alcotest.(check string)
            (label ^ " uses the semantic-label source diagnostic")
            "HCEVAL0003" error.code;
          Alcotest.(check bool)
            (label ^ " explains the label failure")
            true
            (contains error.message fragment);
          check_original_primary label source contents primary occurrence error)
        label_cases;
      List.iter
        (fun (label, contents, fragment) ->
          let _, _, result =
            run ~mode ~path:"integer-goto-invalid-syntax.hc" contents
          in
          let error = first_error result in
          Alcotest.(check bool)
            (label ^ " preserves parser classification")
            true
            (contains error.message fragment))
        parser_cases)
    modes

let word_return_completeness_across_goto () =
  let missing = "I64 Bad(){goto tail;tail:}Bad();" in
  let returning = "I64 Good(){goto tail;return 7;tail:return 42;}Good();" in
  let void = "U0 V(){goto tail;return;tail:}V();42;" in
  List.iter
    (fun mode ->
      let _, _, missing_result =
        run ~mode ~path:"integer-goto-missing-return.hc" missing
      in
      let error = first_error missing_result in
      Alcotest.(check string)
        "goto word fallthrough retains reached missing-return fault"
        "HCIRVM0013" error.code;
      Alcotest.(check bool)
        "goto missing return identifies the active function" true
        (List.mem "function=Bad" error.notes);
      let _, _, returning_result =
        run ~mode ~path:"integer-goto-returning-control.hc" returning
      in
      ignore (expect_word "goto word-returning control" 42L returning_result);
      let _, _, void_result =
        run ~mode ~path:"integer-goto-u0-control.hc" void
      in
      ignore (expect_word "goto U0 fallthrough control" 42L void_result))
    modes

let unsupported_regions_remain_outside_execution_gate () =
  let cases =
    [
      ("assembly block", "U0 F(){goto done;asm {} done:return;}F();", "asm {}");
      ("lock region", "U0 F(){done:lock goto done;}F();", "lock goto done;");
      ( "try/catch region",
        "U0 F(){done:try goto done;catch return;}F();",
        "try goto done;catch return;" );
      ( "no-bound switch region",
        "U0 F(I64 n){switch[n]{case 0:goto done;}done:return;}F(0);",
        "switch[n]{case 0:goto done;}" );
      ( "sub-switch region",
        "U0 F(I64 n){switch(n){start:case 0:goto done;end:}done:return;}F(0);",
        "switch(n){start:case 0:goto done;end:}" );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, contents, primary) ->
          let _, source, result =
            run ~mode ~path:"integer-goto-unsupported-region.hc" contents
          in
          let error = first_error result in
          Alcotest.(check string)
            (label ^ " source gate") "HCRUN0001" error.code;
          check_original_primary label source contents primary 1 error)
        cases)
    modes

let skipped_initialization_faults () =
  let cases =
    [
      ( "goto skips a scalar initializer before read",
        "I64 SkipRead(){goto read;I64 n=42;read:return n;}SkipRead();",
        "SkipRead" );
      ( "goto skips a narrow initializer before update",
        "I64 SkipUpdate(){goto update;U8 n=41;update:return ++n;}SkipUpdate();",
        "SkipUpdate" );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, source, owner) ->
          let _, _, result =
            run ~mode ~path:"integer-goto-uninitialized.hc" source
          in
          let error = first_error result in
          Alcotest.(check string)
            (label ^ " diagnostic") "HCIRVM0012" error.code;
          Alcotest.(check bool)
            (label ^ " identifies its active function")
            true
            (List.mem ("function=" ^ owner) error.notes))
        cases)
    modes

let exact_infinite_budget_and_recovery () =
  let infinite = "U0 Spin(){again:goto again;}Spin();" in
  let healthy = "I64 Healthy(){goto done;return 7;done:return 42;}Healthy();" in
  List.iter
    (fun mode ->
      let _, _, exhausted =
        run ~mode ~max_steps:17 ~path:"integer-goto-infinite.hc" infinite
      in
      let error = first_error exhausted in
      Alcotest.(check string)
        "infinite goto exact budget code" "HCIRVM0007" error.code;
      Alcotest.(check bool)
        "infinite goto consumes the complete public budget" true
        (List.mem "executed_steps=17" error.notes);
      let _, _, recovered =
        run ~mode ~path:"integer-goto-recovery.hc" healthy
      in
      ignore
        (expect_word "fresh execution after goto budget fault" 42L recovered))
    modes

let rec remove_tree path =
  match (Unix.lstat path).st_kind with
  | Unix.S_DIR ->
      Sys.readdir path |> Array.to_list |> List.sort String.compare
      |> List.iter (fun name -> remove_tree (Filename.concat path name));
      Unix.rmdir path
  | _ -> Unix.unlink path

let with_temp_directory run =
  let path = Filename.temp_dir "holyc-goto-execution-" "" in
  Fun.protect ~finally:(fun () -> remove_tree path) (fun () -> run path)

let write_file path contents =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let resolved_function session ~config source name =
  let ast =
    Holyc_lib.parse_with_config session ~config ~source
    |> require_ok diagnostics_text
  in
  let declarations = collect_declarations session ast |> require_ok Fun.id in
  let functions =
    collect_functions session ~declarations ast |> require_ok Fun.id
  in
  let labels = resolve_labels session ~functions ast |> require_ok Fun.id in
  Labels.functions labels
  |> List.find (fun function_ ->
      function_ |> Labels.function_symbol |> Symbol.name |> String.equal name)

let provenance_definitions_and_includes () =
  let defined =
    "#define JUMP goto done\n\
     #define DEST done:\n\
     I64 Generated(){JUMP;return 7;DEST return 42;}Generated();"
  in
  List.iter
    (fun mode ->
      let _, _, result = run ~mode ~path:"goto-definition.hc" defined in
      ignore (expect_word "definition-backed goto execution" 42L result);
      let session = Session.create () in
      let source =
        Session.add_source session ~path:"goto-definition-origin.hc"
          ~contents:defined
      in
      let function_ =
        resolved_function session ~config:(config mode) source "Generated"
      in
      Alcotest.(check bool)
        "definition-backed goto and label retain definition provenance" true
        (Labels.function_occurrences function_
        |> List.for_all (fun occurrence ->
            match Labels.occurrence_origin occurrence with
            | Symbol.Source_location location ->
                Option.is_some location.defined_at
            | Symbol.Pinned_source _ | Symbol.Synthesized _ -> false)))
    modes;
  with_temp_directory (fun root ->
      let root_file = Filename.concat root "root.HC" in
      let include_file = Filename.concat root "flow.HC" in
      let root_contents = "#include \"flow\"\nIncluded();" in
      let include_contents =
        "I64 Included(){goto done;return 7;done:return 42;}"
      in
      write_file root_file root_contents;
      write_file include_file include_contents;
      List.iter
        (fun mode ->
          let session = Session.create () in
          let source =
            Session.add_source session ~path:root_file ~contents:root_contents
          in
          let execution_config = config ~working_directory:root mode in
          let result =
            run_integer_program session ~config:execution_config ~source
              ~max_steps:10_000
          in
          ignore (expect_word "included goto execution" 42L result);
          let provenance_session = Session.create () in
          let provenance_source =
            Session.add_source provenance_session ~path:root_file
              ~contents:root_contents
          in
          let provenance_config = config ~working_directory:root mode in
          let function_ =
            resolved_function provenance_session ~config:provenance_config
              provenance_source "Included"
          in
          Alcotest.(check bool)
            "included goto and label retain the included physical source" true
            (Labels.function_occurrences function_
            |> List.for_all (fun occurrence ->
                match Labels.occurrence_origin occurrence with
                | Symbol.Source_location location -> (
                    match
                      Source_manager.find
                        (Session.sources provenance_session)
                        location.span.source
                    with
                    | Some file ->
                        String.equal
                          (Filename.basename (Source_file.display_path file))
                          "flow"
                        && String.equal
                             (Filename.basename (Source_file.path file))
                             "flow.HC"
                    | None -> false)
                | Symbol.Pinned_source _ | Symbol.Synthesized _ -> false)))
        modes)

let included_label_failure_keeps_physical_source () =
  with_temp_directory (fun root ->
      let root_file = Filename.concat root "root.HC" in
      let include_file = Filename.concat root "bad.HC" in
      let root_contents = "#include \"bad\"\n42;" in
      let include_contents = "I64 Bad(){goto missing;return 42;}" in
      write_file root_file root_contents;
      write_file include_file include_contents;
      List.iter
        (fun mode ->
          let session = Session.create () in
          let root_source =
            Session.add_source session ~path:root_file ~contents:root_contents
          in
          let result =
            run_integer_program session
              ~config:(config ~working_directory:root mode)
              ~source:root_source ~max_steps:10_000
          in
          let error = first_error result in
          Alcotest.(check string)
            "included missing goto uses semantic-label diagnostic" "HCEVAL0003"
            error.code;
          Alcotest.(check bool)
            "included missing goto does not point at the root include directive"
            false
            (Source_id.equal error.primary.source (Source_file.id root_source));
          let physical =
            Source_manager.find (Session.sources session) error.primary.source
            |> Option.get
          in
          Alcotest.(check string)
            "included missing goto keeps included source path" "bad.HC"
            (Filename.basename (Source_file.path physical));
          let start = nth_substring_start include_contents "goto missing;" 1 in
          Alcotest.(check int)
            "included missing goto primary start" start error.primary.start;
          Alcotest.(check int)
            "included missing goto primary stop"
            (start + String.length "goto missing;")
            error.primary.stop)
        modes)

let tests =
  [
    Alcotest.test_case "goto control flow executes in both public modes" `Quick
      valid_control_flow;
    Alcotest.test_case "label resolution rejects every invalid definition"
      `Quick resolution_failures_cover_every_definition;
    Alcotest.test_case "goto preserves word-return completeness" `Quick
      word_return_completeness_across_goto;
    Alcotest.test_case "unsupported goto regions remain outside execution"
      `Quick unsupported_regions_remain_outside_execution_gate;
    Alcotest.test_case "goto can expose skipped local initialization faults"
      `Quick skipped_initialization_faults;
    Alcotest.test_case "infinite goto has an exact budget and fresh recovery"
      `Quick exact_infinite_budget_and_recovery;
    Alcotest.test_case "goto definitions and includes retain stable provenance"
      `Quick provenance_definitions_and_includes;
    Alcotest.test_case "invalid included goto keeps its physical source" `Quick
      included_label_failure_keeps_physical_source;
  ]
