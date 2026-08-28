module Lowering = Holyc_lib.Ir_top_level_statement_lowering
module Sequence = Holyc_lib.Ir_instruction_sequence
module Graph = Holyc_lib.Ir_block_graph
module Top_level = Holyc_lib.Ir_top_level_body
module Opcode = Holyc_lib.Ir_opcode
module Option_ = Holyc_lib.Compiler_option
module Semantic_result = Holyc_lib.Semantic_function_call_expression_result
module Tree = Holyc_lib.Semantic_top_level_expression_tree
module Binding = Holyc_lib.Semantic_top_level_outer_expression_binding
module Target = Holyc_lib.Semantic_top_level_function_call_target_classification
module Preprocessor = Holyc_lib.Preprocessor

let require_ok show = function
  | Ok value -> value
  | Error error -> Alcotest.fail (show error)

let show_sequence_error (error : Sequence.error) =
  error.code ^ ": " ^ error.message

let show_top_level_error (error : Top_level.error) =
  error.code ^ ": " ^ error.message

let show_lowering_error (error : Lowering.error) =
  error.code ^ ": " ^ error.message

let show_lowering_errors errors =
  String.concat "; " (List.map show_lowering_error errors)

let instruction_id value =
  Sequence.Instruction_id.of_int value |> require_ok show_sequence_error

let value_id value =
  Sequence.Value_id.of_int value |> require_ok show_sequence_error

let block_id value =
  Sequence.Block_id.of_int value |> require_ok show_sequence_error

let stream_id value =
  Top_level.Stream_id.of_int value |> require_ok show_top_level_error

let inputs ~mode ~path source =
  let prepared = Test_top_level_expression_result.prepared ~mode ~path source in
  let _, _, _, results = Test_top_level_expression_result.analyze prepared in
  let records =
    Holyc_lib.classify_function_records prepared.session
      ~resolution:prepared.functions prepared.ast
    |> require_ok Fun.id
  in
  ( records,
    Semantic_result.top_level_direct_calls results,
    Semantic_result.top_level_statements results )

let target records call =
  Target.classify ~records call |> require_ok Target.error_to_string

let lower ?(stream = 7) ?(block = 30) ?(instruction = 10) ?(value = 20)
    ?(compiler_options = 0L) records call statement =
  Lowering.lower_direct_call ~stream_id:(stream_id stream)
    ~block_id:(block_id block)
    ~instruction_id:(instruction_id instruction)
    ~value_id:(value_id value) ~compiler_options ~target:(target records call)
    statement

let ordinary_statements ~mode ~path source =
  let prepared = Test_top_level_expression_result.prepared ~mode ~path source in
  let _, _, _, results = Test_top_level_expression_result.analyze prepared in
  Semantic_result.top_level_statements results

let lower_expression ?(stream = 7) ?(block = 30) ?(instruction = 10)
    ?(value = 20) ?(compiler_options = 0L) statement =
  Lowering.lower_expression ~stream_id:(stream_id stream)
    ~block_id:(block_id block)
    ~instruction_id:(instruction_id instruction)
    ~value_id:(value_id value) ~compiler_options statement

let require_lowered = function
  | Lowering.Lowered lowered -> lowered
  | Lowering.Unsupported_statement ->
      Alcotest.fail "expected a checked top-level statement body"

let descriptions lowered =
  let graph = lowered |> Lowering.body |> Top_level.body in
  let block = List.hd (Graph.blocks graph) in
  block |> Graph.instructions |> Sequence.instructions
  |> List.map Sequence.description

let opcode_names lowered =
  descriptions lowered
  |> List.map (fun (description : Sequence.description) ->
      Opcode.to_source_name description.opcode)

let statement_metadata statement =
  let source =
    statement |> Semantic_result.top_level_statement_source
    |> Tree.statement_source
  in
  (Binding.statement_item_index source, Binding.statement_origin source)

let span_of_origin = function
  | Holyc_lib.Semantic_symbol.Source_location location -> location.span
  | Holyc_lib.Semantic_symbol.Pinned_source _
  | Holyc_lib.Semantic_symbol.Synthesized _ ->
      Alcotest.fail "expected source-backed top-level statement provenance"

let one_block_bodies_cover_modes_and_call_access () =
  let run mode source expected_opcodes =
    let records, calls, statements =
      inputs ~mode ~path:"ir-top-level-statement-body.HC" source
    in
    Alcotest.(check int)
      "one statement per call" (List.length calls) (List.length statements);
    let options = Option_.mask Option_.Trace in
    List.iter2
      (fun expected_opcode (call, statement) ->
        let lowered =
          lower ~compiler_options:options records call statement
          |> require_ok show_lowering_errors
          |> require_lowered
        in
        Alcotest.(check (list string))
          "the call statement and stream marker share one block"
          [
            "IC_CALL_START";
            "IC_IMM_I64";
            "IC_IMM_I64";
            "IC_IMM_I64";
            expected_opcode;
            "IC_ADD_RSP";
            "IC_CALL_END";
            "IC_END_EXP";
            "IC_END";
          ]
          (opcode_names lowered);
        let body = Lowering.body lowered in
        let expected_position, origin = statement_metadata statement in
        Alcotest.(check int)
          "retained item position" expected_position
          (Top_level.item_position body);
        Alcotest.(check bool)
          "retained statement span" true
          (Top_level.span body = Some (span_of_origin origin));
        Alcotest.(check int64)
          "explicit compiler options" options
          (Top_level.compiler_options body);
        let graph = Top_level.body body in
        Alcotest.(check int)
          "one entry block" 1
          (List.length (Graph.blocks graph));
        Alcotest.(check int)
          "caller block identity" 30
          (Graph.entry graph |> Graph.block_id |> Sequence.Block_id.to_int);
        let final = List.hd (List.rev (descriptions lowered)) in
        Alcotest.(check bool)
          "the structural end marker has no source span" true
          (Option.is_none final.span);
        Alcotest.(check (pair int int))
          "the body advances past IC_END without consuming a value" (19, 24)
          ( Lowering.next_instruction_id lowered
            |> Sequence.Instruction_id.to_int,
            Lowering.next_value_id lowered |> Sequence.Value_id.to_int ))
      expected_opcodes
      (List.combine calls statements)
  in
  run Preprocessor.Jit
    "_extern _BOUND I64 Direct(I64 first,...);\n\
     extern I64 External(I64 first,...);\n\
     Direct(1,2);External(3,4);"
    [ "IC_CALL"; "IC_CALL_INDIRECT2" ];
  run Preprocessor.Aot
    "_extern _BOUND I64 Direct(I64 first,...);\n\
     import I64 Imported(I64 first,...);\n\
     Direct(1,2);Imported(3,4);"
    [ "IC_CALL"; "IC_CALL_IMPORT" ]

let ordinary_expression_bodies_cover_supported_families () =
  let source =
    "class Base {I8 inherited;};class Box : Base {I16 prefix;};Box \
     global;1+2;1.0+2;$$;defined(missing);sizeof(I64);offset(global.prefix);"
  in
  let expected =
    [
      [ "IC_IMM_I64"; "IC_IMM_I64"; "IC_ADD"; "IC_END_EXP"; "IC_END" ];
      [ "IC_IMM_F64"; "IC_IMM_I64"; "IC_ADD"; "IC_END_EXP"; "IC_END" ];
      [ "IC_RIP"; "IC_END_EXP"; "IC_END" ];
      [ "IC_IMM_I64"; "IC_END_EXP"; "IC_END" ];
      [ "IC_IMM_I64"; "IC_END_EXP"; "IC_END" ];
      [ "IC_IMM_I64"; "IC_END_EXP"; "IC_END" ];
    ]
  in
  List.iter
    (fun mode ->
      let statements =
        ordinary_statements ~mode ~path:"ir-top-level-expression-body.HC" source
      in
      Alcotest.(check int) "six supported statements" 6 (List.length statements);
      List.iter2
        (fun expected statement ->
          let lowered =
            lower_expression statement
            |> require_ok show_lowering_errors
            |> require_lowered
          in
          Alcotest.(check (list string))
            "ordinary expression closes one checked body" expected
            (opcode_names lowered);
          let body = Lowering.body lowered in
          let expected_position, origin = statement_metadata statement in
          Alcotest.(check int)
            "ordinary statement item position" expected_position
            (Top_level.item_position body);
          Alcotest.(check bool)
            "ordinary statement span" true
            (Top_level.span body = Some (span_of_origin origin)))
        expected statements)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let ordinary_expression_metadata_and_dumps_are_deterministic () =
  let statement =
    ordinary_statements ~mode:Preprocessor.Jit
      ~path:"ir-top-level-expression-body-dump.HC" "1+2;"
    |> List.hd
  in
  let options = Option_.mask Option_.No_reg_var in
  let lower_once () =
    lower_expression ~compiler_options:options statement
    |> require_ok show_lowering_errors
    |> require_lowered
  in
  let lowered = lower_once () in
  let body = Lowering.body lowered in
  Alcotest.(check int64)
    "explicit compiler options" options
    (Top_level.compiler_options body);
  Alcotest.(check int)
    "stream-end identity" 14
    (Lowering.end_instruction_id lowered |> Sequence.Instruction_id.to_int);
  Alcotest.(check (pair int int))
    "ordinary expression cursors" (15, 23)
    ( Lowering.next_instruction_id lowered |> Sequence.Instruction_id.to_int,
      Lowering.next_value_id lowered |> Sequence.Value_id.to_int );
  Alcotest.(check string)
    "ordinary body dumps replay deterministically" (Lowering.human lowered)
    (lower_once () |> Lowering.human)

let ordinary_expression_failures_expose_no_partial_body () =
  let unsupported =
    ordinary_statements ~mode:Preprocessor.Jit
      ~path:"ir-top-level-expression-body-unsupported.HC" "I64 value;value;"
    |> List.hd
  in
  (match lower_expression unsupported |> require_ok show_lowering_errors with
  | Lowering.Unsupported_statement -> ()
  | Lowering.Lowered _ ->
      Alcotest.fail "an unsupported identifier value produced a body");
  let invalid =
    ordinary_statements ~mode:Preprocessor.Jit
      ~path:"ir-top-level-expression-body-role.HC" "if(1) 2;"
    |> List.hd
  in
  (match lower_expression invalid with
  | Error [ error ] ->
      Alcotest.(check string) "root diagnostic" "HCIRL0004" error.code
  | Error errors ->
      Alcotest.failf "expected one root diagnostic, got %d" (List.length errors)
  | Ok _ -> Alcotest.fail "a multi-root control statement produced a body");
  let literal =
    ordinary_statements ~mode:Preprocessor.Jit
      ~path:"ir-top-level-expression-body-failure.HC" "1;"
    |> List.hd
  in
  (match
     lower_expression ~compiler_options:(Int64.shift_left 1L 63) literal
   with
  | Error [ error ] ->
      Alcotest.(check string) "option diagnostic" "HCIR0037" error.code
  | Error errors ->
      Alcotest.failf "expected one option diagnostic, got %d"
        (List.length errors)
  | Ok _ -> Alcotest.fail "unknown options produced an ordinary body");
  match lower_expression ~instruction:(Int.max_int - 2) literal with
  | Error [ error ] ->
      Alcotest.(check string) "identity diagnostic" "HCIRL0005" error.code;
      Alcotest.(check bool)
        "identity failure keeps statement span" true
        (Option.is_some error.span)
  | Error errors ->
      Alcotest.failf "expected one identity diagnostic, got %d"
        (List.length errors)
  | Ok _ -> Alcotest.fail "exhausted identities produced an ordinary body"

let failures_expose_no_partial_body () =
  let records, calls, statements =
    inputs ~mode:Preprocessor.Jit ~path:"ir-top-level-body-owner.HC"
      "I64 First(){return 1;}I64 Second(){return 2;}First();Second();"
  in
  (match lower records (List.hd calls) (List.nth statements 1) with
  | Error [ error ] ->
      Alcotest.(check string) "ownership diagnostic" "HCIRL0004" error.code;
      Alcotest.(check (option int))
        "caller block context" (Some 30) error.block_id
  | Error errors ->
      Alcotest.failf "expected one ownership diagnostic, got %d"
        (List.length errors)
  | Ok _ -> Alcotest.fail "a foreign call target produced a top-level body");
  (match
     lower ~compiler_options:(Int64.shift_left 1L 63) records (List.hd calls)
       (List.hd statements)
   with
  | Error [ error ] ->
      Alcotest.(check string) "option diagnostic" "HCIR0037" error.code;
      Alcotest.(check int) "stream context" 7 error.stream_id
  | Error errors ->
      Alcotest.failf "expected one option diagnostic, got %d"
        (List.length errors)
  | Ok _ -> Alcotest.fail "unknown compiler options produced a top-level body");
  let unsupported source =
    let records, calls, statements =
      inputs ~mode:Preprocessor.Jit ~path:"ir-top-level-body-unsupported.HC"
        source
    in
    match
      lower records (List.hd calls) (List.hd statements)
      |> require_ok show_lowering_errors
    with
    | Lowering.Unsupported_statement -> ()
    | Lowering.Lowered _ ->
        Alcotest.fail "an unsupported call produced a top-level body"
  in
  unsupported "_intern 42 I64 Internal();Internal();";
  unsupported "I64 Defaulted(I64 value=1);Defaulted();"

let dumps_and_identity_exhaustion_are_deterministic () =
  let records, calls, statements =
    inputs ~mode:Preprocessor.Jit ~path:"ir-top-level-body-dump.HC"
      "I64 Callee(){return 1;}Callee();"
  in
  let call = List.hd calls in
  let statement = List.hd statements in
  let lower_once () =
    lower records call statement
    |> require_ok show_lowering_errors
    |> require_lowered
  in
  let first = lower_once () in
  Alcotest.(check string)
    "checked body dumps replay deterministically" (Lowering.human first)
    (lower_once () |> Lowering.human);
  Alcotest.(check int)
    "stream-end identity" 15
    (Lowering.end_instruction_id first |> Sequence.Instruction_id.to_int);
  match lower ~instruction:(Int.max_int - 5) records call statement with
  | Error [ error ] ->
      Alcotest.(check string) "identity diagnostic" "HCIRL0005" error.code;
      Alcotest.(check string)
        "identity message"
        "cannot allocate the top-level stream-end instruction because the host \
         integer range is exhausted"
        error.message;
      Alcotest.(check bool)
        "statement span retained" true
        (Option.is_some error.span)
  | Error errors ->
      Alcotest.failf "expected one identity diagnostic, got %d"
        (List.length errors)
  | Ok _ -> Alcotest.fail "an exhausted stream identity produced a body"

let tests =
  [
    Alcotest.test_case "ordinary expression bodies" `Quick
      ordinary_expression_bodies_cover_supported_families;
    Alcotest.test_case "ordinary body metadata and dumps" `Quick
      ordinary_expression_metadata_and_dumps_are_deterministic;
    Alcotest.test_case "ordinary body failure atomicity" `Quick
      ordinary_expression_failures_expose_no_partial_body;
    Alcotest.test_case "one-block direct-call bodies" `Quick
      one_block_bodies_cover_modes_and_call_access;
    Alcotest.test_case "failure atomicity" `Quick
      failures_expose_no_partial_body;
    Alcotest.test_case "determinism and identity exhaustion" `Quick
      dumps_and_identity_exhaustion_are_deterministic;
  ]
