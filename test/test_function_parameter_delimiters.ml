open Holyc_lib
module T = Test_function_type_resolution
module H = Semantic_function_type_resolution
module O = Test_integer_output

let parse_types mode source =
  let session = Session.create () in
  let ast = T.parse ~mode session ~path:"parameter-delimiters.HC" source in
  (ast, T.resolve session ast)

let trailing_semicolons () =
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          let _, results = parse_types mode source in
          let function_ = T.function_named results "F" in
          Alcotest.(check int)
            "only concrete parameters occupy slots" 1
            (List.length (T.parameters function_));
          Alcotest.(check bool)
            "final source delimiter survives typing" true
            (Option.is_some
               (H.parameter_delimiter_origin (T.parameter_at function_ 0))))
        [ "extern I64 F(I64 n;);"; "I64 F(;;I64 n;;;){return n;}" ])
    Test_integer_globals.modes

let comma_and_empty_entries () =
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          let ast, results = parse_types mode source in
          let function_ = T.function_named results "F" in
          Alcotest.(check int)
            "trailing comma adds no parameter" 1
            (List.length (T.parameters function_));
          let prototype = Test_parser.expect_one_prototype ast in
          Alcotest.(check bool)
            "comma remains attached to its original parameter" true
            ((Option.get (List.hd prototype.parameters).delimiter).kind
           = Ast.Comma);
          List.iter
            (fun (entry : Ast.empty_parameter_entry) ->
              Alcotest.(check int)
                "empty entries follow the concrete slot" 1
                entry.preceding_parameter_count;
              Alcotest.(check bool)
                "empty entries remain semicolons" true
                (entry.empty_parameter_delimiter.kind = Ast.Semicolon))
            prototype.empty_parameter_entries)
        [ "extern I64 F(I64 n,);"; "extern I64 F(I64 n,;;;);" ])
    Test_integer_globals.modes

let recursive_signatures () =
  List.iter
    (fun mode ->
      let _, results =
        parse_types mode "extern I64 F(I64 (*callback)(;;U8 n,;;),;I64 m=2,;);"
      in
      let function_ = T.function_named results "F" in
      Alcotest.(check int)
        "outer signature has two slots" 2
        (List.length (T.parameters function_));
      match H.parameter_declarator_kind (T.parameter_at function_ 0) with
      | H.Function_pointer pointer ->
          let nested =
            H.function_pointer_signature pointer |> H.signature_parameters
          in
          Alcotest.(check int) "callback has one slot" 1 (List.length nested);
          Alcotest.(check bool)
            "callback retains trailing delimiter" true
            (Option.is_some (H.parameter_delimiter_origin (List.hd nested)))
      | _ -> Alcotest.fail "expected callback signature")
    Test_integer_globals.modes

let completed_headers () =
  let rec check_children signature sources =
    List.iter2
      (fun (source : Ast.function_parameter) parameter ->
        Alcotest.(check bool)
          "typed header retains exact original parameter" true
          (Option.get (H.parameter_source parameter) == source);
        match
          (source.function_pointer, H.parameter_declarator_kind parameter)
        with
        | Some original, H.Function_pointer pointer ->
            check_children
              (H.function_pointer_signature pointer)
              original.signature_parameters
        | None, H.Object -> ()
        | _ -> Alcotest.fail "callback shape changed")
      sources
      (H.signature_parameters signature)
  in
  List.iter
    (fun source ->
      let _, _, headers = Test_completed_function_header.parse source in
      let original, _, _, typed = List.hd headers in
      check_children (H.function_signature typed) original.parameters;
      Alcotest.(check int)
        "empty entries remain in original header" 2
        (List.length original.empty_parameter_entries))
    [
      "I64 F(I64 n=42;;;){return n;}";
      "I64 F(I64 n=42,;;){return n;}";
      "I64 F(I64 (*callback)(;;U8 n,;;),;;){return 42;}";
    ]

let generated_delimiters () =
  List.iter
    (fun mode ->
      let ast, results =
        parse_types mode "#define END ,;;\nextern I64 F(I64 n END);"
      in
      let prototype = Test_parser.expect_one_prototype ast in
      let delimiter = Option.get (List.hd prototype.parameters).delimiter in
      Alcotest.(check bool)
        "generated comma retains invocation provenance" true
        (Option.is_some delimiter.location.generated_from);
      Alcotest.(check bool)
        "generated comma retains definition provenance" true
        (Option.is_some delimiter.location.defined_at);
      Alcotest.(check int)
        "generated empty entries remain visible" 2
        (List.length prototype.empty_parameter_entries);
      List.iter
        (fun (entry : Ast.empty_parameter_entry) ->
          Alcotest.(check bool)
            "generated empty entry keeps its origin" true
            (Option.is_some entry.empty_parameter_delimiter.location.defined_at))
        prototype.empty_parameter_entries;
      Alcotest.(check int)
        "generated delimiters add no typed slots" 1
        (List.length (T.parameters (T.function_named results "F"))))
    Test_integer_globals.modes

let malformed_delimiters () =
  List.iter
    (fun compilation_mode ->
      List.iter
        (fun parameters ->
          let _, _, output =
            Test_parser.parse_string ~compilation_mode
              ("extern I64 F(" ^ parameters ^ ");")
          in
          Alcotest.(check string)
            "malformed list retains parameter diagnostic" "HCPARSE0009"
            (Test_parser.first_diagnostic output).code)
        [
          ",";
          "I64 n,,";
          "I64 n;,";
          "I64 n,;,";
          "I64 n,reg";
          "I64 n,reg;";
          "I64 n,noreg;";
        ])
    Test_integer_globals.modes

let execution () =
  List.iter
    (fun mode ->
      List.iter
        (fun source -> ignore (O.run ~mode source |> O.expect ""))
        [
          "I64 F(;;I64 n=40;;;I64 m=2;;){return n+m;};F();";
          "I64 F(I64 n=42,){return n;};F();";
          "I64 F(I64 n=40,;;...){return n+argc+argv[0];};F(,1);";
          "I64 F(;;){return 42;};F();";
          "I64 F(U8 n,;){return n;};F(298);";
          "#exe {I64 Out=0;U0 Print(I64 \
           n,;;){Out=n;}\"\"(42);StreamPrint(\"%d;\",Out);}";
          "#exe {I64 F(I64 n=40,){return n+2;}#exe {I64 Saved(){return \
           F();}I64 F(I64 n=99,;){return 7;}}StreamPrint(\"%d;\",Saved());}";
        ])
    Test_integer_globals.modes

let exact_resources () =
  let original = "I64 F(I64 n=40,I64 m=2){return n+m;};F();" in
  let delimited = "I64 F(;;I64 n=40,;;I64 m=2,;;){return n+m;};F();" in
  List.iter
    (fun mode ->
      let baseline = O.run ~mode original in
      ignore (O.expect "" baseline);
      let steps =
        (Option.get (integer_program_report_progress baseline)).runtime
      in
      ignore
        (O.run ~mode ~max_steps:steps.executed_steps
           ~max_initializer_steps:steps.initializer_steps ~max_frame_bytes:16
           ~max_call_depth:1 delimited
        |> O.expect "");
      ignore (O.run ~mode ~max_frame_bytes:15 delimited |> O.fault "HCIRVM0011");
      ignore
        (O.run ~mode ~max_steps:(steps.executed_steps - 1) delimited
        |> O.fault "HCIRVM0007"))
    Test_integer_globals.modes

let tests =
  [
    Alcotest.test_case "trailing semicolons type without extra slots" `Quick
      trailing_semicolons;
    Alcotest.test_case "comma continuation retains empty semicolon entries"
      `Quick comma_and_empty_entries;
    Alcotest.test_case "recursive callback signatures retain delimiters" `Quick
      recursive_signatures;
    Alcotest.test_case "completed headers retain original delimiter children"
      `Quick completed_headers;
    Alcotest.test_case "generated delimiters retain source provenance" `Quick
      generated_delimiters;
    Alcotest.test_case "missing parameters and dangling qualifiers still reject"
      `Quick malformed_delimiters;
    Alcotest.test_case
      "ordinary and retained calls execute with native delimiters" `Quick
      execution;
    Alcotest.test_case "delimiters leave frame and execution budgets unchanged"
      `Quick exact_resources;
  ]
