open Holyc_lib

let checked = Test_declaration_collection.checked
let observed = Test_duplicate_local_types.observed

let warnings diagnostics =
  List.filter (fun d -> d.Diagnostic.code = "HCSEMA0078") diagnostics

let fixture ?(after = fun _ _ -> ()) text =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"return-warnings.hc" ~contents:text
  in
  let ledger = Task_declarations.create_source session ~source |> checked in
  let phases = ref [] and counts = ref [] in
  let checkpoint event =
    Task_declarations.observe_command ledger event |> observed;
    (match event with
    | Parser.Sequence_started context ->
        List.iter
          (fun bit_index ->
            ignore
              (Parser.context_set_option context ~bit_index false |> checked))
          [ 16L; 17L; 18L; 19L ]
    | _ -> ());
    Ok ()
  in
  let declaration event =
    let result = Task_declarations.observe ledger event in
    (match (event, result) with
    | Parser.Function_return_phase receipt, Ok () ->
        phases := receipt :: !phases;
        let context =
          receipt.return_header.function_publication.function_header
            .declaration_command
            .command_context
        in
        let count = Parser.context_warning_count context |> checked in
        counts := count :: !counts;
        Alcotest.(check bool)
          "live callback is original" true
          (Parser.function_return_phase_is_current receipt);
        Alcotest.(check bool)
          "consumer replay rejects" true
          (Result.is_error (Task_declarations.observe ledger event));
        Alcotest.(check bool)
          "native field replay rejects" true
          (Result.is_error (Parser.consume_function_return_phase receipt));
        Alcotest.(check int64)
          "replay preserves warning field" count
          (Parser.context_warning_count context |> checked)
    | _ -> ());
    after ledger event;
    result
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
  let output =
    Parser.parse ~commands ~sources:(Session.sources session)
      ~definitions:(Session.definitions session)
      ~symbols:(Session.symbols session)
      ~config:(Preprocessor.Config.create () |> checked)
      source
  in
  List.iter
    (fun receipt ->
      Alcotest.(check bool)
        "receipt expires after callback" false
        (Parser.function_return_phase_is_current receipt);
      Alcotest.(check bool)
        "expired field operation rejects" true
        (Result.is_error (Parser.consume_function_return_phase receipt)))
    !phases;
  (output, List.rev !phases, List.rev !counts)

let should = "Function should return val"
let should_not = "Function should NOT return val"

let check text expected =
  let output, _, _ = fixture text in
  Test_duplicate_local_types.success output;
  Alcotest.(check (list string))
    text expected
    (List.map (fun d -> d.Diagnostic.message) (warnings output.diagnostics))

let statement_and_body () =
  List.iter
    (fun (text, expected) -> check text expected)
    [
      ("I64 F(){}", [ should ]);
      ("U0 F(){}", []);
      ("I64 F(){return;}", [ should; should ]);
      ("U0 F(){return;}", []);
      ("U0 F(){return 42;}", [ should_not ]);
      ("I64 F(){return 42;}", []);
      ("I64 F(){return;return 42;}", [ should ]);
      ("I64 F(){if(0)return 42;}", []);
      ("I64 F(){if(1)return;}", [ should; should ]);
      ("I64 F(){return 42;}I64 G(){}", [ should ]);
      ("I64 *F(){return;}", [ should; should ]);
      ("U0 *F(){}", [ should ]);
    ]

let counts_and_phases () =
  let output, phases, counts =
    fixture "I64 F(){return;return 42;}U0 G(){return 1;}I64 H(){}"
  in
  Test_duplicate_local_types.success output;
  Alcotest.(check (list int64))
    "unconditional native warning increments"
    [ 0L; 1L; 1L; 1L; 1L; 1L; 2L; 2L; 2L; 2L; 3L ]
    counts;
  Alcotest.(check (list string))
    "original warning order"
    [ should; should_not; should ]
    (List.map (fun d -> d.Diagnostic.message) (warnings output.diagnostics));
  Alcotest.(check int)
    "five source return phases retained" 11 (List.length phases)

let fault_order () =
  List.iter
    (fun text ->
      let output, phases, _ = fixture text in
      Alcotest.(check bool)
        "later grammar fails" true (Parser.has_errors output);
      Alcotest.(check (list string))
        "warning reached before failed value" [ should_not ]
        (List.map (fun d -> d.Diagnostic.message) (warnings output.diagnostics));
      Alcotest.(check bool)
        "failed body does not reach end warning" false
        (List.exists
           (fun r -> r.Parser.return_step = Parser.Check_function_body_return)
           phases))
    [ "U0 F(){return );}"; "U0 F(){return 1+;}"; "U0 F(){return 42 43;}" ];
  let output, phases, _ = fixture "I64 F(){return 42 43;}" in
  Alcotest.(check bool) "terminator fails" true (Parser.has_errors output);
  Alcotest.(check bool)
    "parsed value phase precedes terminator failure" true
    (List.exists
       (fun r -> r.Parser.return_step = Parser.Value_return_parsed)
       phases)

let exact_classes () =
  List.iter
    (fun (text, expected) -> check text expected)
    [
      ("class Empty{};Empty F(){}", []);
      ("class Full{I64 n;};Full F(){}", [ should ]);
      ("class Empty{};Empty F(){return 42;}", [ should_not ]);
      ("class Full{I64 n;};Full F(){return;}", [ should; should ]);
      ("extern class T;class T{I64 n;};T F(){}", [ should ]);
      ("extern class T;T *F(){return;}", [ should; should ]);
      ("class B{I64 n;};class D:B{};D F(){}", [ should ]);
      ("U16 class B{U16 low;};B class D{U8 byte;};D F(){}", [ should ]);
      ("class B{};B class D{};D F(){return 42;}", [ should_not ]);
      ("union T{I64 n;U8 b;};T F(){}", [ should ]);
      ("class T{};T F(){}class T{I64 n;};T G(){}", [ should ]);
    ];
  let output, _, _ = fixture "extern class T;T F(){}" in
  (* A forward's original reached size is zero, unlike an absent layout. *)
  Test_duplicate_local_types.success output;
  Alcotest.(check int)
    "retained zero forward class" 0
    (List.length (warnings output.diagnostics))

let run mode text =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"return-source.hc" ~contents:text
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> checked
  in
  run_integer_program_report ~max_steps:10000 session ~config ~source

let source_phases () =
  List.iter
    (fun mode ->
      List.iter
        (fun (text, expected) ->
          let report = run mode text in
          Alcotest.(check (list string))
            (text ^ " -> "
            ^ String.concat "; "
                (List.map
                   (fun d -> d.Diagnostic.message)
                   (Test_duplicate_local_types.report_diagnostics report)))
            expected
            (Test_duplicate_local_types.report_diagnostics report
            |> warnings
            |> List.map (fun d -> d.Diagnostic.message)))
        [
          ("#exe {Option(16,0);I64 F(){return;}}42;", [ should; should ]);
          ("#exe {Option(16,0);U0 F(){return missing;}}42;", [ should_not ]);
          ("#exe {Option(16,0);I64 F(){}I64 zero=0;42/zero;}42;", [ should ]);
          ( "#exe {Option(16,0);\n#define BAD return );\nU0 F(){BAD}}42;",
            [ should_not ] );
          ("#exe {Option(16,0);I64 F(){if(0)return 42;}}42;", []);
          ( {|#exe {Option(16,0);I64 Parent(){return 42;}StreamExePrint("I64 Child(){}");}42;|},
            [ should ] );
          ( "#exe {Option(16,0);I64 Outer(){return 42;#exe {I64 Inner(){}}}}42;",
            [ should; should ] );
          ( {|#exe {Option(16,0);Option(19,0);extern I64 F();I64 F()#exe {extern U0 F();}{return 42;}}42;|},
            [ should_not ] );
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let tests =
  [
    Alcotest.test_case "statement and body source rules" `Quick
      statement_and_body;
    Alcotest.test_case "counts, original receipts and replay" `Quick
      counts_and_phases;
    Alcotest.test_case "warning and flag before later grammar faults" `Quick
      fault_order;
    Alcotest.test_case "exact aggregate sizes and pointer classes" `Quick
      exact_classes;
    Alcotest.test_case "source, macros, children and shared flags" `Quick
      source_phases;
  ]
