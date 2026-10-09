open Holyc_lib

let checked = Test_declaration_collection.checked

let observed = function
  | Ok value -> value
  | Error diagnostics ->
      Alcotest.fail
        (diagnostics
        |> List.map (fun d -> d.Diagnostic.message)
        |> String.concat "; ")

let control = checked
let modes = [ Preprocessor.Jit; Preprocessor.Aot ]

let duplicate_warnings diagnostics =
  List.filter (fun d -> d.Diagnostic.code = "HCSEMA0076") diagnostics

let names diagnostics =
  duplicate_warnings diagnostics |> List.map (fun d -> d.Diagnostic.message)

let warning name function_name =
  Printf.sprintf "duplicate local-variable type for %S in function %S" name
    function_name

let fixture ?(enabled = true) ?(before = fun _ -> ()) ?(after = fun _ _ -> ())
    text =
  let session = Session.create () in
  let ledger = Task_declarations.create session |> checked in
  let receipts = ref [] in
  let checkpoint event =
    Task_declarations.observe_command ledger event |> observed;
    (match event with
    | Parser.Sequence_started context ->
        ignore
          (Parser.context_set_option context ~bit_index:18L enabled |> control)
    | _ -> ());
    Ok ()
  in
  let declaration event =
    before event;
    Task_declarations.observe ledger event |> observed;
    (match event with
    | Parser.Function_local_allocated receipt ->
        receipts := receipt :: !receipts
    | _ -> ());
    after ledger event;
    Ok ()
  in
  let commands : Parser.command_sink =
    {
      lexical_lookup = None;
      checkpoint = Some checkpoint;
      reference = None;
      call = None;
      implicit_output = None;
      query = Some (Task_declarations.observe_query ledger);
      declaration = Some declaration;
      dimension_count = Some (Task_declarations.grammar_dimension_count ledger);
      command = (fun _ -> Ok ());
      resume = (fun () -> Ok ());
    }
  in
  let source =
    Session.add_source session ~path:"duplicate-types.hc" ~contents:text
  in
  let config = Preprocessor.Config.create () |> control in
  let output =
    Parser.parse ~commands ~sources:(Session.sources session)
      ~definitions:(Session.definitions session)
      ~symbols:(Session.symbols session) ~config source
  in
  (output, List.rev !receipts)

let success output =
  if Parser.has_errors output then
    Alcotest.fail
      (output.Parser.diagnostics
      |> List.map (fun d -> d.Diagnostic.message)
      |> String.concat "; ")

let check_parse ?enabled text expected =
  let output, _ = fixture ?enabled text in
  success output;
  Alcotest.(check (list string))
    "original duplicate type warnings" expected (names output.diagnostics)

let default_off () =
  check_parse ~enabled:false "I64 F(){I64 a;I64 b;return 42;}" []

let first_declarator_and_original_phase () =
  let counts = ref [] in
  let output, receipts =
    fixture "I64 F(){I64 a,b;I64 c=42,d;I64 *e;return 42;}"
      ~after:(fun ledger event ->
        match event with
        | Parser.Function_local_allocated receipt ->
            let context =
              receipt.allocation_local.local_command.command_context
            in
            let count = Parser.context_warning_count context |> control in
            counts := count :: !counts;
            Alcotest.(check bool)
              "allocation callback cannot replay" true
              (Result.is_error (Task_declarations.observe ledger event));
            Alcotest.(check int64)
              "replay leaves warning count unchanged" count
              (Parser.context_warning_count context |> control)
        | _ -> ())
  in
  success output;
  Alcotest.(check (list string))
    "only first comma declarators warn"
    [ warning "c" "F"; warning "e" "F" ]
    (names output.diagnostics);
  Alcotest.(check (list bool))
    "original first-declarator receipts"
    [ true; false; true; false; true ]
    (List.map (fun r -> r.Parser.allocation_first_in_declaration) receipts);
  Alcotest.(check (list int64))
    "warnings increment before initializer" [ 0L; 0L; 1L; 1L; 2L ]
    (List.rev !counts);
  let warned = [ List.nth receipts 2; List.nth receipts 4 ] in
  List.iter2
    (fun diagnostic receipt ->
      Alcotest.(check bool)
        "warning retains original lookahead span" true
        (diagnostic.Diagnostic.primary
       == receipt.Parser.allocation_lookahead.span))
    (duplicate_warnings output.diagnostics)
    warned

let index_while_disabled () =
  let output, _ =
    fixture ~enabled:false "I64 F(){I64 a;I64 b;I64 c;I64 d;return 42;}"
      ~after:(fun _ -> function
      | Parser.Function_local_allocated receipt -> (
          match receipt.allocation_local.local_source with
          | Parser.Local_variable local ->
              let context =
                receipt.allocation_local.local_command.command_context
              in
              if local.local_name.spelling = "a" then
                ignore
                  (Parser.context_set_option context ~bit_index:18L true
                  |> control)
              else if local.local_name.spelling = "b" then
                ignore
                  (Parser.context_set_option context ~bit_index:18L false
                  |> control)
              else if local.local_name.spelling = "c" then
                ignore
                  (Parser.context_set_option context ~bit_index:18L true
                  |> control)
          | _ -> ())
      | _ -> ())
  in
  success output;
  Alcotest.(check (list string))
    "off declarations populate the same index"
    [ warning "b" "F"; warning "d" "F" ]
    (names output.diagnostics)

let automatic_mode_and_function_scope () =
  check_parse
    "I64 F(I64 parameter){static I64 s;I64 a;I64 reg b;I64 noreg c;static I64 \
     t;return 42;}I64 G(){I64 a;return 42;}"
    [ warning "b" "F"; warning "c" "F" ]

let exact_primitive_identity () =
  check_parse
    "I64 F(){I64 a;U64 b;I64i c;Bool d;I8 e;I64 *f;I64i **g;Bool h;I8 i;return \
     42;}"
    [ warning "f" "F"; warning "g" "F"; warning "h" "F"; warning "i" "F" ];
  check_parse "I64 F(){I64 a[2];I64 *b;I64 **c[3];return 42;}"
    [ warning "b" "F"; warning "c" "F" ]

let callback_intrinsic_base () =
  check_parse
    "I64 F(){I64 a;I64 (*first)(I64 n);U8 (*second)(U8 n);I64i third;I64 \
     *fourth;return 42;}"
    [ warning "second" "F"; warning "third" "F"; warning "fourth" "F" ];
  check_parse
    "class I64i{I64 field;};I64 F(){I64i *a;I64 (*first)(I64 n);U8 \
     (*second)(U8 n);I64i *b;return 42;}"
    [ warning "second" "F"; warning "b" "F" ]

let exact_aggregate_identity () =
  check_parse
    "extern class T;class T{I64 field;};class U{I64 field;};I64 F(){T *a;U \
     *b;T **c;return 42;}"
    [ warning "c" "F" ];
  check_parse
    "class T{I64 field;};I64 F(){T *a;class T{I64 other;};T *b;T **c;return \
     42;}"
    [ warning "c" "F" ]

let early_warning_survives_parser_failure () =
  let output, receipts = fixture "I64 F(){I64 a;I64 b=;}" in
  Alcotest.(check bool)
    "initializer fails after MemberAdd phase" true (Parser.has_errors output);
  Alcotest.(check int) "both allocations reached" 2 (List.length receipts);
  Alcotest.(check (list string))
    "warning survives unfinished initializer"
    [ warning "b" "F" ]
    (names output.diagnostics)

let run mode text =
  let session = Session.create () in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> control
  in
  let source =
    Session.add_source session ~path:"duplicate-source-types.hc" ~contents:text
  in
  run_integer_program_report ~max_steps:10000 session ~config ~source

let report_diagnostics report =
  match integer_program_report_outcome report with
  | Ok checked -> checked.diagnostics
  | Error diagnostics -> diagnostics

let source_controls_and_children () =
  List.iter
    (fun mode ->
      let cases =
        [
          ( "#exe {Option(16,0);Option(18,1);}I64 F(){I64 a=40;I64 b=2;return \
             a+b;}F();",
            [ warning "b" "F" ] );
          ( "#exe {Option(16,0);Option(18,0);I64 F(){I64 a[Option(18,1)+1];I64 \
             b;return 42;}}42;",
            [ warning "b" "F" ] );
          ( {|#exe {Option(16,0);Option(18,1);StreamExePrint("I64 Child(){I64 a;I64 b;return 42;}Child();");I64 Parent(){I64 a;I64 b;return 42;}Parent();}42;|},
            [ warning "b" "Child"; warning "b" "Parent" ] );
          ( "#exe {Option(16,0);Option(18,1);extern class T;I64 F(){T *a;#exe \
             {class T{I64 field;};}T *b;return 42;}}42;",
            [ warning "b" "F" ] );
          ( "#exe {Option(16,0);Option(18,1);class T{I64 field;};I64 F(){T \
             *a;T *b[1 #exe {class T{I64 other;};}];T *c;T *d;return 42;}}42;",
            [ warning "b" "F"; warning "d" "F" ] );
          ( "#exe {Option(16,0);Option(18,1);I64 F(){I64 a;I64 b;return \
             missing;}}42;",
            [ warning "b" "F" ] );
          ( "#exe {Option(16,0);Option(18,1);I64 F(){I64 a;I64 b;return \
             42;}I64 zero=0;42/zero;}42;",
            [ warning "b" "F" ] );
        ]
      in
      List.iteri
        (fun index (text, expected) ->
          let report = run mode text in
          Alcotest.(check bool)
            "source success or reached failure" (index < 4)
            (Result.is_ok (integer_program_report_outcome report));
          Alcotest.(check (list string))
            "source warnings retain phase and order" expected
            (names (report_diagnostics report)))
        cases)
    modes

let retained_pointer_seed () =
  let session = Session.create () in
  let pointer = Session.pointer_primitive session in
  Alcotest.(check string)
    "original RT_PTR seed is I64i" "I64i"
    (Semantic_symbol.name (Session.primitive_symbol pointer));
  Alcotest.(check bool)
    "task view retains original pointer binding" true
    (pointer == Session.pointer_primitive (Session.task_frontend session));
  let fork = Session.fork_frontend session in
  Alcotest.(check bool)
    "frontend fork has its own canonical semantic symbol" true
    (Session.primitive_symbol pointer
    != Session.primitive_symbol (Session.pointer_primitive fork));
  let entry =
    match
      Symbol_visibility.Environment.find_preprocessor (Session.symbols fork)
        "I64i"
    with
    | Symbol_visibility.Present entry -> entry
    | Symbol_visibility.Absent | Symbol_visibility.Shadowed_by_local ->
        Alcotest.fail "missing intrinsic seed"
  in
  Alcotest.(check bool)
    "fork associates the same seeded frontend entry" true
    (Session.pointer_primitive fork
    == Option.get (Session.primitive_for fork entry));
  ignore
    (Symbol_visibility.Environment.add (Session.symbols session) ~name:"I64i"
       ~kind:Symbol_visibility.Class ());
  Alcotest.(check bool)
    "later spelling shadow leaves RT_PTR binding intact" true
    (pointer == Session.pointer_primitive session)

let member_failure_precedes_type_warning () =
  List.iter
    (fun mode ->
      List.iter
        (fun text ->
          let report =
            run mode ("#exe {Option(16,0);Option(18,1);" ^ text ^ "}42;")
          in
          let diagnostics = report_diagnostics report in
          Alcotest.(check bool)
            "duplicate member fails at insertion" true
            (List.exists
               (fun d -> d.Diagnostic.code = "HCSEMA0015")
               diagnostics);
          Alcotest.(check (list string))
            "member failure precedes duplicate type warning" []
            (names diagnostics))
        [
          "I64 F(I64 a){I64 a;I64 b;return 42;}";
          "I64 F(){I64 a,a;I64 b;return 42;}";
          "I64 F(...){I64 argc;I64 b;return 42;}";
        ];
      let report =
        run mode
          "#exe {Option(16,0);Option(18,1);I64 F(){I64 pad;I64 pad;return \
           42;}}42;"
      in
      Alcotest.(check bool)
        "original duplicate name exemption is accepted" true
        (Result.is_ok (integer_program_report_outcome report));
      Alcotest.(check (list string))
        "exempt member still takes type warning path"
        [ warning "pad" "F" ]
        (names (report_diagnostics report)))
    modes

let reentrant_native_type_index () =
  List.iter
    (fun mode ->
      let report =
        run mode
          "#exe {Option(16,0);Option(18,1);Option(19,0);I64 F(){I64 a;#exe \
           {extern I64 F();}I64 b;I64 c;return 42;}}42;"
      in
      (match integer_program_report_outcome report with
      | Ok _ -> ()
      | Error diagnostics ->
          Alcotest.fail
            (String.concat "; "
               (List.map (fun d -> d.Diagnostic.message) diagnostics)));
      Alcotest.(check (list string))
        "reused header clears the actual native class index"
        [ warning "c" "F" ]
        (names (report_diagnostics report)))
    modes

let unavailable_cursor_does_not_assume_empty_index () =
  let session = Session.create () in
  let config = Preprocessor.Config.create () |> control in
  let source =
    Session.add_source session ~path:"repeat-duplicate-types.hc"
      ~contents:"I64 F(){I64 a;I64 b;return 42;}F();"
  in
  List.iter
    (fun () ->
      let report =
        run_integer_program_report ~max_steps:10000 session ~config ~source
      in
      Alcotest.(check bool)
        "warning-off repeated reports retain supported execution" true
        (Result.is_ok (integer_program_report_outcome report)))
    [ (); () ];
  let source =
    Session.add_source session ~path:"untracked-duplicate-types.hc"
      ~contents:
        "#exe {Option(16,0);Option(18,1);}I64 F(){I64 a;I64 b;return 42;}F();"
  in
  let report =
    run_integer_program_report ~max_steps:10000 session ~config ~source
  in
  let diagnostics = report_diagnostics report in
  Alcotest.(check bool)
    "untracked predecessor cannot supply a native type index" true
    (List.exists
       (fun d ->
         d.Diagnostic.code = "HCRUN0004"
         && d.message = "previous native function record is untracked")
       diagnostics);
  Alcotest.(check (list string))
    "unavailable cursor supplies no assumed duplicate warning" []
    (names diagnostics)

let tests =
  [
    Alcotest.test_case "duplicate warning defaults off" `Quick default_off;
    Alcotest.test_case "first declarator and original warning phase" `Quick
      first_declarator_and_original_phase;
    Alcotest.test_case "type index populated while warning disabled" `Quick
      index_while_disabled;
    Alcotest.test_case "automatic locals and per-function index" `Quick
      automatic_mode_and_function_scope;
    Alcotest.test_case "physical primitive bases and pointer depths" `Quick
      exact_primitive_identity;
    Alcotest.test_case "callback locals retain intrinsic RT_PTR base" `Quick
      callback_intrinsic_base;
    Alcotest.test_case "canonical aggregate bases and spelling shadows" `Quick
      exact_aggregate_identity;
    Alcotest.test_case "warning precedes failed initializer" `Quick
      early_warning_survives_parser_failure;
    Alcotest.test_case "live options, ordinary source and child warnings" `Quick
      source_controls_and_children;
    Alcotest.test_case "pointer seed retained across task views and forks"
      `Quick retained_pointer_seed;
    Alcotest.test_case "member collision fails before duplicate type warning"
      `Quick member_failure_precedes_type_warning;
    Alcotest.test_case "reentrant headers use the actual native type index"
      `Quick reentrant_native_type_index;
    Alcotest.test_case
      "unavailable cursor rejects warning evidence without breaking off path"
      `Quick unavailable_cursor_does_not_assume_empty_index;
  ]
