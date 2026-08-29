module Batch = Holyc_lib.Ir_top_level_batch_lowering
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

let show_batch_error (error : Batch.error) = error.code ^ ": " ^ error.message

let show_batch_errors errors =
  String.concat "; " (List.map show_batch_error errors)

let instruction_id value =
  Sequence.Instruction_id.of_int value |> require_ok show_sequence_error

let value_id value =
  Sequence.Value_id.of_int value |> require_ok show_sequence_error

let block_id value =
  Sequence.Block_id.of_int value |> require_ok show_sequence_error

let stream_id value =
  Top_level.Stream_id.of_int value |> require_ok show_top_level_error

let analyzed_inputs ~mode ~path source =
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

let inputs ~mode ~path source =
  let records, calls, statements = analyzed_inputs ~mode ~path source in
  let targets =
    calls
    |> List.map (fun call ->
        Target.classify ~records call |> require_ok Target.error_to_string)
  in
  (targets, statements)

let ordinary_statements ~mode ~path source =
  let prepared = Test_top_level_expression_result.prepared ~mode ~path source in
  let _, _, _, results = Test_top_level_expression_result.analyze prepared in
  Semantic_result.top_level_statements results

let lower ?(stream = 7) ?(block = 30) ?(instruction = 10) ?(value = 20)
    ~compiler_options ~targets statements =
  Batch.lower_direct_calls ~stream_id:(stream_id stream)
    ~block_id:(block_id block)
    ~instruction_id:(instruction_id instruction)
    ~value_id:(value_id value) ~compiler_options ~targets statements

let lower_expressions ?(stream = 7) ?(block = 30) ?(instruction = 10)
    ?(value = 20) ~compiler_options statements =
  Batch.lower_expressions ~stream_id:(stream_id stream)
    ~block_id:(block_id block)
    ~instruction_id:(instruction_id instruction)
    ~value_id:(value_id value) ~compiler_options statements

let lower_mixed ?(stream = 7) ?(block = 30) ?(instruction = 10) ?(value = 20)
    ~compiler_options ~targets statements =
  Batch.lower_mixed ~stream_id:(stream_id stream) ~block_id:(block_id block)
    ~instruction_id:(instruction_id instruction)
    ~value_id:(value_id value) ~compiler_options ~targets statements

let require_lowered = function
  | Batch.Lowered lowered -> lowered
  | Batch.Unsupported_batch ->
      Alcotest.fail "expected a checked top-level body batch"

let descriptions body =
  let block = body |> Top_level.body |> Graph.blocks |> List.hd in
  block |> Graph.instructions |> Sequence.instructions
  |> List.map Sequence.description

let opcode_names body =
  descriptions body
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

let batch_cursors lowered =
  [
    Batch.next_stream_id lowered |> Top_level.Stream_id.to_int;
    Batch.next_block_id lowered |> Sequence.Block_id.to_int;
    Batch.next_instruction_id lowered |> Sequence.Instruction_id.to_int;
    Batch.next_value_id lowered |> Sequence.Value_id.to_int;
  ]

let body_stream_ids bodies =
  List.map
    (fun body -> Top_level.stream_id body |> Top_level.Stream_id.to_int)
    bodies

let body_block_ids bodies =
  List.map
    (fun body ->
      Top_level.body body |> Graph.entry |> Graph.block_id
      |> Sequence.Block_id.to_int)
    bodies

let direct_call_opcodes access =
  [
    "IC_CALL_START";
    "IC_IMM_I64";
    "IC_IMM_I64";
    "IC_IMM_I64";
    access;
    "IC_ADD_RSP";
    "IC_CALL_END";
    "IC_END_EXP";
    "IC_END";
  ]

let addition_opcodes =
  [ "IC_IMM_I64"; "IC_IMM_I64"; "IC_ADD"; "IC_END_EXP"; "IC_END" ]

let expect_mixed_error label expected_code expected_statement_index = function
  | Error [ (error : Batch.error) ] ->
      Alcotest.(check string) (label ^ " code") expected_code error.code;
      Alcotest.(check (option int))
        (label ^ " statement") expected_statement_index error.statement_index
  | Error errors ->
      Alcotest.failf "%s: expected one diagnostic, got %d" label
        (List.length errors)
  | Ok Batch.Unsupported_batch ->
      Alcotest.failf "%s: expected a diagnostic, got unsupported" label
  | Ok (Batch.Lowered _) ->
      Alcotest.failf "%s: expected a diagnostic, got bodies" label

let expect_mixed_unsupported label = function
  | Ok Batch.Unsupported_batch -> ()
  | Ok (Batch.Lowered _) ->
      Alcotest.failf "%s: unsupported input produced bodies" label
  | Error errors ->
      Alcotest.failf "%s: expected unsupported, got %s" label
        (show_batch_errors errors)

let empty_and_single_batches_thread_identity () =
  let empty =
    lower ~compiler_options:[] ~targets:[] []
    |> require_ok show_batch_errors
    |> require_lowered
  in
  Alcotest.(check int) "empty body count" 0 (List.length (Batch.bodies empty));
  Alcotest.(check (list int))
    "empty batch leaves every cursor unchanged" [ 7; 30; 10; 20 ]
    [
      Batch.next_stream_id empty |> Top_level.Stream_id.to_int;
      Batch.next_block_id empty |> Sequence.Block_id.to_int;
      Batch.next_instruction_id empty |> Sequence.Instruction_id.to_int;
      Batch.next_value_id empty |> Sequence.Value_id.to_int;
    ];
  let targets, statements =
    inputs ~mode:Preprocessor.Jit ~path:"ir-top-level-batch-single.HC"
      "I64 Callee(){return 1;}Callee();"
  in
  let single =
    lower ~compiler_options:[ 0L ] ~targets statements
    |> require_ok show_batch_errors
    |> require_lowered
  in
  Alcotest.(check int) "single body count" 1 (List.length (Batch.bodies single));
  Alcotest.(check (list int))
    "single statement advances every cursor exactly" [ 8; 31; 16; 21 ]
    [
      Batch.next_stream_id single |> Top_level.Stream_id.to_int;
      Batch.next_block_id single |> Sequence.Block_id.to_int;
      Batch.next_instruction_id single |> Sequence.Instruction_id.to_int;
      Batch.next_value_id single |> Sequence.Value_id.to_int;
    ];
  let body = List.hd (Batch.bodies single) in
  Alcotest.(check int)
    "starting stream identity" 7
    (Top_level.stream_id body |> Top_level.Stream_id.to_int);
  Alcotest.(check int)
    "starting block identity" 30
    (Top_level.body body |> Graph.entry |> Graph.block_id
   |> Sequence.Block_id.to_int)

let multi_statement_batches_cover_modes_and_options () =
  let run mode source expected_calls =
    let targets, statements =
      inputs ~mode ~path:"ir-top-level-body-batch.HC" source
    in
    let options =
      [ Option_.mask Option_.Trace; Option_.mask Option_.No_reg_var ]
    in
    let lower_once () =
      lower ~compiler_options:options ~targets statements
      |> require_ok show_batch_errors
      |> require_lowered
    in
    let lowered = lower_once () in
    let bodies = Batch.bodies lowered in
    Alcotest.(check int) "two checked bodies" 2 (List.length bodies);
    Alcotest.(check (list int))
      "consecutive stream identities" [ 7; 8 ]
      (List.map
         (fun body -> Top_level.stream_id body |> Top_level.Stream_id.to_int)
         bodies);
    Alcotest.(check (list int))
      "consecutive entry-block identities" [ 30; 31 ]
      (List.map
         (fun body ->
           Top_level.body body |> Graph.entry |> Graph.block_id
           |> Sequence.Block_id.to_int)
         bodies);
    Alcotest.(check (list int64))
      "source-ordered compiler options" options
      (List.map Top_level.compiler_options bodies);
    Alcotest.(check (list string))
      "mode-specific call access survives each body" expected_calls
      (List.map
         (fun body ->
           let descriptions = descriptions body in
           List.nth descriptions 4 |> fun description ->
           Opcode.to_source_name description.opcode)
         bodies);
    List.iter
      (fun body ->
        Alcotest.(check string)
          "every body closes at IC_END" "IC_END"
          (List.hd (List.rev (opcode_names body))))
      bodies;
    Alcotest.(check (list int))
      "multi-statement cursors" [ 9; 32; 28; 28 ]
      [
        Batch.next_stream_id lowered |> Top_level.Stream_id.to_int;
        Batch.next_block_id lowered |> Sequence.Block_id.to_int;
        Batch.next_instruction_id lowered |> Sequence.Instruction_id.to_int;
        Batch.next_value_id lowered |> Sequence.Value_id.to_int;
      ];
    Alcotest.(check string)
      "batch dumps replay deterministically" (Batch.human lowered)
      (lower_once () |> Batch.human)
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

let invalid_unsupported_and_exhausted_batches_are_atomic () =
  let targets, statements =
    inputs ~mode:Preprocessor.Jit ~path:"ir-top-level-batch-failure.HC"
      "I64 First(){return 1;}I64 Second(){return 2;}First();Second();"
  in
  (match
     lower ~compiler_options:[ 0L; 0L ] ~targets:[ List.hd targets ] statements
   with
  | Error [ error ] ->
      Alcotest.(check string) "count diagnostic" "HCIRL0004" error.code;
      Alcotest.(check (option int))
        "no statement was entered" None error.statement_index
  | Error errors ->
      Alcotest.failf "expected one count diagnostic, got %d"
        (List.length errors)
  | Ok _ -> Alcotest.fail "mismatched batch counts produced bodies");
  (match
     lower ~compiler_options:[ 0L; 0L ] ~targets:(List.rev targets) statements
   with
  | Error [ error ] ->
      Alcotest.(check string) "ownership diagnostic" "HCIRL0004" error.code;
      Alcotest.(check (option int))
        "first statement context" (Some 0) error.statement_index
  | Error errors ->
      Alcotest.failf "expected one ownership diagnostic, got %d"
        (List.length errors)
  | Ok _ -> Alcotest.fail "foreign batch targets produced bodies");
  (match
     lower ~compiler_options:[ 0L; Int64.shift_left 1L 63 ] ~targets statements
   with
  | Error [ error ] ->
      Alcotest.(check string) "option diagnostic" "HCIR0037" error.code;
      Alcotest.(check (option int))
        "second statement context" (Some 1) error.statement_index
  | Error errors ->
      Alcotest.failf "expected one option diagnostic, got %d"
        (List.length errors)
  | Ok _ -> Alcotest.fail "a partial batch escaped before an option failure");
  let internal_targets, internal_statements =
    inputs ~mode:Preprocessor.Jit ~path:"ir-top-level-batch-unsupported.HC"
      "_intern 42 I64 Internal();Internal();"
  in
  (match
     lower ~compiler_options:[ 0L ] ~targets:internal_targets
       internal_statements
     |> require_ok show_batch_errors
   with
  | Batch.Unsupported_batch -> ()
  | Batch.Lowered _ -> Alcotest.fail "an unsupported batch produced bodies");
  let one_target = [ List.hd targets ] in
  let one_statement = [ List.hd statements ] in
  match
    lower ~stream:Int.max_int ~block:Int.max_int ~compiler_options:[ 0L ]
      ~targets:one_target one_statement
  with
  | Error errors ->
      Alcotest.(check (list string))
        "both owner cursors report exhaustion"
        [ "HCIRL0005"; "HCIRL0005" ]
        (List.map (fun (error : Batch.error) -> error.code) errors);
      List.iter
        (fun (error : Batch.error) ->
          Alcotest.(check (option int))
            "exhausted statement context" (Some 0) error.statement_index)
        errors
  | Ok _ -> Alcotest.fail "exhausted owner identities produced a batch"

let expression_empty_and_single_batches_thread_identity () =
  let empty =
    lower_expressions ~compiler_options:[] []
    |> require_ok show_batch_errors
    |> require_lowered
  in
  Alcotest.(check int)
    "empty expression body count" 0
    (List.length (Batch.bodies empty));
  Alcotest.(check (list int))
    "empty expression batch leaves every cursor unchanged" [ 7; 30; 10; 20 ]
    [
      Batch.next_stream_id empty |> Top_level.Stream_id.to_int;
      Batch.next_block_id empty |> Sequence.Block_id.to_int;
      Batch.next_instruction_id empty |> Sequence.Instruction_id.to_int;
      Batch.next_value_id empty |> Sequence.Value_id.to_int;
    ];
  let statement =
    ordinary_statements ~mode:Preprocessor.Jit
      ~path:"ir-top-level-expression-batch-single.HC" "1+2;"
    |> List.hd
  in
  let single =
    lower_expressions ~compiler_options:[ 0L ] [ statement ]
    |> require_ok show_batch_errors
    |> require_lowered
  in
  Alcotest.(check int)
    "single expression body count" 1
    (List.length (Batch.bodies single));
  Alcotest.(check (list int))
    "single expression advances every cursor exactly" [ 8; 31; 15; 23 ]
    [
      Batch.next_stream_id single |> Top_level.Stream_id.to_int;
      Batch.next_block_id single |> Sequence.Block_id.to_int;
      Batch.next_instruction_id single |> Sequence.Instruction_id.to_int;
      Batch.next_value_id single |> Sequence.Value_id.to_int;
    ];
  let body = List.hd (Batch.bodies single) in
  let item_position, origin = statement_metadata statement in
  Alcotest.(check int)
    "single expression item position" item_position
    (Top_level.item_position body);
  Alcotest.(check bool)
    "single expression span" true
    (Top_level.span body = Some (span_of_origin origin));
  Alcotest.(check (list string))
    "single expression body boundary"
    [ "IC_IMM_I64"; "IC_IMM_I64"; "IC_ADD"; "IC_END_EXP"; "IC_END" ]
    (opcode_names body)

let expression_batches_cover_supported_families () =
  let source =
    "class Base {I8 inherited;};class Box : Base {I16 prefix;};Box \
     global;1+2;1.0+2;$$;defined(missing);sizeof(I64);offset(global.prefix);"
  in
  let expected_opcodes =
    [
      [ "IC_IMM_I64"; "IC_IMM_I64"; "IC_ADD"; "IC_END_EXP"; "IC_END" ];
      [ "IC_IMM_F64"; "IC_IMM_I64"; "IC_ADD"; "IC_END_EXP"; "IC_END" ];
      [ "IC_RIP"; "IC_END_EXP"; "IC_END" ];
      [ "IC_IMM_I64"; "IC_END_EXP"; "IC_END" ];
      [ "IC_IMM_I64"; "IC_END_EXP"; "IC_END" ];
      [ "IC_IMM_I64"; "IC_END_EXP"; "IC_END" ];
    ]
  in
  let options =
    [
      Option_.mask Option_.Trace;
      Option_.mask Option_.No_reg_var;
      0L;
      Option_.mask Option_.Trace;
      Option_.mask Option_.No_reg_var;
      0L;
    ]
  in
  List.iter
    (fun mode ->
      let statements =
        ordinary_statements ~mode ~path:"ir-top-level-expression-batch.HC"
          source
      in
      let lower_once () =
        lower_expressions ~compiler_options:options statements
        |> require_ok show_batch_errors
        |> require_lowered
      in
      let lowered = lower_once () in
      let bodies = Batch.bodies lowered in
      Alcotest.(check int) "six expression bodies" 6 (List.length bodies);
      Alcotest.(check (list int))
        "consecutive expression stream identities" [ 7; 8; 9; 10; 11; 12 ]
        (List.map
           (fun body -> Top_level.stream_id body |> Top_level.Stream_id.to_int)
           bodies);
      Alcotest.(check (list int))
        "consecutive expression entry blocks" [ 30; 31; 32; 33; 34; 35 ]
        (List.map
           (fun body ->
             Top_level.body body |> Graph.entry |> Graph.block_id
             |> Sequence.Block_id.to_int)
           bodies);
      Alcotest.(check (list int64))
        "source-ordered expression options" options
        (List.map Top_level.compiler_options bodies);
      Alcotest.(check (list (list string)))
        "supported expression families" expected_opcodes
        (List.map opcode_names bodies);
      Alcotest.(check bool)
        "every expression body retains its statement metadata" true
        (List.for_all2
           (fun statement body ->
             let item_position, origin = statement_metadata statement in
             Top_level.item_position body = item_position
             && Top_level.span body = Some (span_of_origin origin))
           statements bodies);
      Alcotest.(check (list int))
        "multi-expression cursors" [ 13; 36; 32; 30 ]
        [
          Batch.next_stream_id lowered |> Top_level.Stream_id.to_int;
          Batch.next_block_id lowered |> Sequence.Block_id.to_int;
          Batch.next_instruction_id lowered |> Sequence.Instruction_id.to_int;
          Batch.next_value_id lowered |> Sequence.Value_id.to_int;
        ];
      Alcotest.(check string)
        "expression batch dumps replay deterministically" (Batch.human lowered)
        (lower_once () |> Batch.human))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let expression_batch_failures_are_atomic () =
  let statements =
    ordinary_statements ~mode:Preprocessor.Jit
      ~path:"ir-top-level-expression-batch-failure.HC" "1;2;"
  in
  (match lower_expressions ~compiler_options:[ 0L ] statements with
  | Error [ error ] ->
      Alcotest.(check string)
        "expression count diagnostic" "HCIRL0004" error.code;
      Alcotest.(check (option int))
        "no expression statement was entered" None error.statement_index
  | Error errors ->
      Alcotest.failf "expected one expression count diagnostic, got %d"
        (List.length errors)
  | Ok _ -> Alcotest.fail "mismatched expression counts produced bodies");
  (match
     lower_expressions
       ~compiler_options:[ 0L; Int64.shift_left 1L 63 ]
       statements
   with
  | Error [ error ] ->
      Alcotest.(check string)
        "expression option diagnostic" "HCIR0037" error.code;
      Alcotest.(check (option int))
        "second expression statement context" (Some 1) error.statement_index
  | Error errors ->
      Alcotest.failf "expected one expression option diagnostic, got %d"
        (List.length errors)
  | Ok _ ->
      Alcotest.fail
        "a partial expression batch escaped before an option failure");
  let unsupported =
    ordinary_statements ~mode:Preprocessor.Jit
      ~path:"ir-top-level-expression-batch-unsupported.HC" "1;I64 value;value;"
  in
  (match
     lower_expressions ~compiler_options:[ 0L; 0L ] unsupported
     |> require_ok show_batch_errors
   with
  | Batch.Unsupported_batch -> ()
  | Batch.Lowered _ ->
      Alcotest.fail "an unsupported expression batch produced bodies");
  let invalid =
    ordinary_statements ~mode:Preprocessor.Jit
      ~path:"ir-top-level-expression-batch-role.HC" "1;if(1) 2;"
  in
  (match lower_expressions ~compiler_options:[ 0L; 0L ] invalid with
  | Error [ error ] ->
      Alcotest.(check string)
        "expression role diagnostic" "HCIRL0004" error.code;
      Alcotest.(check (option int))
        "invalid second expression context" (Some 1) error.statement_index
  | Error errors ->
      Alcotest.failf "expected one expression role diagnostic, got %d"
        (List.length errors)
  | Ok _ -> Alcotest.fail "a multi-root expression batch produced bodies");
  let literal = [ List.hd statements ] in
  (match
     lower_expressions ~instruction:(Int.max_int - 2) ~compiler_options:[ 0L ]
       literal
   with
  | Error [ error ] ->
      Alcotest.(check string)
        "expression identity diagnostic" "HCIRL0005" error.code;
      Alcotest.(check (option int))
        "expression identity statement context" (Some 0) error.statement_index
  | Error errors ->
      Alcotest.failf "expected one expression identity diagnostic, got %d"
        (List.length errors)
  | Ok _ -> Alcotest.fail "exhausted expression identities produced a batch");
  match
    lower_expressions ~stream:Int.max_int ~block:Int.max_int
      ~compiler_options:[ 0L ] literal
  with
  | Error errors ->
      Alcotest.(check (list string))
        "both expression owner cursors report exhaustion"
        [ "HCIRL0005"; "HCIRL0005" ]
        (List.map (fun (error : Batch.error) -> error.code) errors);
      List.iter
        (fun (error : Batch.error) ->
          Alcotest.(check (option int))
            "exhausted expression statement context" (Some 0)
            error.statement_index)
        errors
  | Ok _ ->
      Alcotest.fail "exhausted expression owner identities produced a batch"

let mixed_empty_and_homogeneous_batches_preserve_behavior () =
  let empty =
    lower_mixed ~compiler_options:[] ~targets:[] []
    |> require_ok show_batch_errors
    |> require_lowered
  in
  Alcotest.(check int)
    "empty mixed body count" 0
    (List.length (Batch.bodies empty));
  Alcotest.(check (list int))
    "empty mixed batch leaves every cursor unchanged" [ 7; 30; 10; 20 ]
    (batch_cursors empty);
  let expression_options =
    [ Option_.mask Option_.Trace; Option_.mask Option_.No_reg_var ]
  in
  List.iter
    (fun mode ->
      let statements =
        ordinary_statements ~mode ~path:"ir-top-level-mixed-expressions.HC"
          "1+2;3+4;"
      in
      let mixed =
        lower_mixed ~compiler_options:expression_options ~targets:[] statements
        |> require_ok show_batch_errors
        |> require_lowered
      in
      let homogeneous =
        lower_expressions ~compiler_options:expression_options statements
        |> require_ok show_batch_errors
        |> require_lowered
      in
      Alcotest.(check string)
        "all-expression mixed lowering matches the homogeneous API"
        (Batch.human homogeneous) (Batch.human mixed))
    [ Preprocessor.Jit; Preprocessor.Aot ];
  let run_calls mode path source expected_access =
    let targets, statements = inputs ~mode ~path source in
    let options =
      [ Option_.mask Option_.Trace; Option_.mask Option_.No_reg_var ]
    in
    let mixed =
      lower_mixed ~compiler_options:options ~targets statements
      |> require_ok show_batch_errors
      |> require_lowered
    in
    let homogeneous =
      lower ~compiler_options:options ~targets statements
      |> require_ok show_batch_errors
      |> require_lowered
    in
    Alcotest.(check string)
      "all-call mixed lowering matches the homogeneous API"
      (Batch.human homogeneous) (Batch.human mixed);
    Alcotest.(check (list string))
      "mixed calls retain mode-specific access" expected_access
      (mixed |> Batch.bodies
      |> List.map (fun body ->
             let description = List.nth (descriptions body) 4 in
             Opcode.to_source_name description.opcode))
  in
  run_calls Preprocessor.Jit "ir-top-level-mixed-calls-jit.HC"
    "_extern _BOUND I64 Direct(I64 first,...);\n\
     extern I64 External(I64 first,...);\n\
     Direct(1,2);External(3,4);"
    [ "IC_CALL"; "IC_CALL_INDIRECT2" ];
  run_calls Preprocessor.Aot "ir-top-level-mixed-calls-aot.HC"
    "_extern _BOUND I64 Direct(I64 first,...);\n\
     import I64 Imported(I64 first,...);\n\
     Direct(1,2);Imported(3,4);"
    [ "IC_CALL"; "IC_CALL_IMPORT" ]

let mixed_orders_preserve_source_metadata_and_cursors () =
  let run path source expected_opcodes =
    let targets, statements = inputs ~mode:Preprocessor.Jit ~path source in
    let options =
      [ Option_.mask Option_.Trace; Option_.mask Option_.No_reg_var ]
    in
    let lower_once () =
      lower_mixed ~compiler_options:options ~targets statements
      |> require_ok show_batch_errors
      |> require_lowered
    in
    let lowered = lower_once () in
    let bodies = Batch.bodies lowered in
    Alcotest.(check int) "two mixed bodies" 2 (List.length bodies);
    Alcotest.(check (list int))
      "mixed source-order stream identities" [ 7; 8 ]
      (body_stream_ids bodies);
    Alcotest.(check (list int))
      "mixed source-order block identities" [ 30; 31 ]
      (body_block_ids bodies);
    Alcotest.(check (list int64))
      "mixed source-order compiler options" options
      (List.map Top_level.compiler_options bodies);
    Alcotest.(check (list (list string)))
      "mixed source-order body shapes" expected_opcodes
      (List.map opcode_names bodies);
    Alcotest.(check (list int))
      "mixed source-order item positions"
      (List.map (fun statement -> fst (statement_metadata statement)) statements)
      (List.map Top_level.item_position bodies);
    Alcotest.(check bool)
      "mixed source-order spans" true
      (List.for_all2
         (fun statement body ->
           let _, origin = statement_metadata statement in
           Top_level.span body = Some (span_of_origin origin))
         statements bodies);
    Alcotest.(check (list int))
      "two-item mixed cursors" [ 9; 32; 24; 27 ]
      (batch_cursors lowered);
    Alcotest.(check string)
      "two-item mixed dump is deterministic" (Batch.human lowered)
      (lower_once () |> Batch.human)
  in
  run "ir-top-level-mixed-call-expression.HC"
    "_extern _BOUND I64 Direct(I64 first,...);Direct(1,2);1+2;"
    [ direct_call_opcodes "IC_CALL"; addition_opcodes ];
  run "ir-top-level-mixed-expression-call.HC"
    "_extern _BOUND I64 Direct(I64 first,...);1+2;Direct(1,2);"
    [ addition_opcodes; direct_call_opcodes "IC_CALL" ]

let mixed_alternating_batch_ignores_target_order () =
  let source =
    "_extern _BOUND I64 First(I64 first,...);\n\
     extern I64 Second(I64 first,...);\n\
     _extern _BOUND I64 Third(I64 first,...);\n\
     First(1,2);1+2;Second(3,4);3+4;Third(5,6);5+6;"
  in
  let targets, statements =
    inputs ~mode:Preprocessor.Jit ~path:"ir-top-level-mixed-alternating.HC"
      source
  in
  Alcotest.(check int) "three classified calls" 3 (List.length targets);
  Alcotest.(check int) "six executable statements" 6 (List.length statements);
  let options =
    [
      Option_.mask Option_.Trace;
      0L;
      Option_.mask Option_.No_reg_var;
      Option_.mask Option_.Trace;
      0L;
      Option_.mask Option_.No_reg_var;
    ]
  in
  let lower_once () =
    lower_mixed ~compiler_options:options ~targets:(List.rev targets) statements
    |> require_ok show_batch_errors
    |> require_lowered
  in
  let lowered = lower_once () in
  let bodies = Batch.bodies lowered in
  Alcotest.(check (list int))
    "alternating source-order streams" [ 7; 8; 9; 10; 11; 12 ]
    (body_stream_ids bodies);
  Alcotest.(check (list int))
    "alternating source-order blocks" [ 30; 31; 32; 33; 34; 35 ]
    (body_block_ids bodies);
  Alcotest.(check (list int64))
    "alternating source-order options" options
    (List.map Top_level.compiler_options bodies);
  Alcotest.(check (list (list string)))
    "alternating bodies ignore target-list order"
    [
      direct_call_opcodes "IC_CALL";
      addition_opcodes;
      direct_call_opcodes "IC_CALL_INDIRECT2";
      addition_opcodes;
      direct_call_opcodes "IC_CALL";
      addition_opcodes;
    ]
    (List.map opcode_names bodies);
  Alcotest.(check bool)
    "alternating bodies retain statement metadata" true
    (List.for_all2
       (fun statement body ->
         let item_position, origin = statement_metadata statement in
         Top_level.item_position body = item_position
         && Top_level.span body = Some (span_of_origin origin))
       statements bodies);
  Alcotest.(check (list int))
    "alternating mixed cursors" [ 13; 36; 52; 41 ]
    (batch_cursors lowered);
  Alcotest.(check string)
    "alternating mixed dump is deterministic" (Batch.human lowered)
    (lower_once () |> Batch.human)

let mixed_target_validation_is_atomic () =
  let source =
    "_extern _BOUND I64 Direct(I64 first,...);Direct(1,2);"
  in
  let records, calls, statements =
    analyzed_inputs ~mode:Preprocessor.Jit
      ~path:"ir-top-level-mixed-target-validation.HC" source
  in
  Alcotest.(check int) "one target-validation call" 1 (List.length calls);
  Alcotest.(check int)
    "one target-validation statement" 1 (List.length statements);
  let call = List.hd calls in
  let statement = List.hd statements in
  let classify () =
    Target.classify ~records call |> require_ok Target.error_to_string
  in
  let target = classify () in
  let separately_classified = classify () in
  expect_mixed_error "mixed count mismatch" "HCIRL0004" None
    (lower_mixed ~compiler_options:[] ~targets:[ target ] [ statement ]);
  expect_mixed_error "repeated mixed target" "HCIRL0004" (Some 0)
    (lower_mixed ~compiler_options:[ 0L ] ~targets:[ target; target ]
       [ statement ]);
  expect_mixed_error "separately classified duplicate target" "HCIRL0004"
    (Some 0)
    (lower_mixed ~compiler_options:[ 0L ]
       ~targets:[ target; separately_classified ] [ statement ]);
  expect_mixed_error "missing standalone target" "HCIRL0004" (Some 0)
    (lower_mixed ~compiler_options:[ 0L ] ~targets:[] [ statement ]);
  expect_mixed_error "ambiguous duplicated statement" "HCIRL0004" None
    (lower_mixed ~compiler_options:[ 0L; 0L ] ~targets:[ target ]
       [ statement; statement ]);
  expect_mixed_error "target without statements" "HCIRL0004" None
    (lower_mixed ~compiler_options:[] ~targets:[ target ] []);
  let foreign_targets, _ =
    inputs ~mode:Preprocessor.Jit
      ~path:"ir-top-level-mixed-foreign-target.HC" source
  in
  expect_mixed_error "foreign mixed target" "HCIRL0004" None
    (lower_mixed ~compiler_options:[ 0L ] ~targets:foreign_targets [ statement ]);
  let unrelated =
    ordinary_statements ~mode:Preprocessor.Jit
      ~path:"ir-top-level-mixed-unrelated-target.HC" "1+2;"
  in
  expect_mixed_error "target with unrelated statement" "HCIRL0004" None
    (lower_mixed ~compiler_options:[ 0L ] ~targets:[ target ] unrelated)

let mixed_nested_calls_are_not_standalone_targets () =
  let targets, statements =
    inputs ~mode:Preprocessor.Jit ~path:"ir-top-level-mixed-nested-call.HC"
      "I64 Callee(){return 1;}1+Callee();"
  in
  Alcotest.(check int) "one nested classified call" 1 (List.length targets);
  Alcotest.(check int) "one nested expression statement" 1
    (List.length statements);
  expect_mixed_error "nested target is not a statement root" "HCIRL0004" None
    (lower_mixed ~compiler_options:[ 0L ] ~targets statements);
  expect_mixed_unsupported "nested call follows expression lowering"
    (lower_mixed ~compiler_options:[ 0L ] ~targets:[] statements)

let mixed_identifier_callbacks_are_not_direct_call_targets () =
  let _, calls, statements =
    analyzed_inputs ~mode:Preprocessor.Jit
      ~path:"ir-top-level-mixed-callback.HC"
      "I64 (*Callback)(I64 value);Callback(1);"
  in
  Alcotest.(check int)
    "identifier callback is not a classified direct call" 0
    (List.length calls);
  Alcotest.(check int) "one callback expression statement" 1
    (List.length statements);
  expect_mixed_unsupported "identifier callback follows expression lowering"
    (lower_mixed ~compiler_options:[ 0L ] ~targets:[] statements)

let mixed_post_prefix_failures_are_atomic () =
  let call_then_expression_source =
    "_extern _BOUND I64 Direct(I64 first,...);Direct(1,2);1+2;"
  in
  let targets, statements =
    inputs ~mode:Preprocessor.Jit
      ~path:"ir-top-level-mixed-invalid-option.HC"
      call_then_expression_source
  in
  expect_mixed_error "mixed option failure after a body" "HCIR0037" (Some 1)
    (lower_mixed
       ~compiler_options:[ 0L; Int64.shift_left 1L 63 ]
       ~targets statements);
  let targets, statements =
    inputs ~mode:Preprocessor.Jit
      ~path:"ir-top-level-mixed-unsupported-prefix.HC"
      "_extern _BOUND I64 Direct(I64 first,...);I64 value;Direct(1,2);value;"
  in
  expect_mixed_unsupported "unsupported mixed suffix"
    (lower_mixed ~compiler_options:[ 0L; 0L ] ~targets statements);
  let targets, statements =
    inputs ~mode:Preprocessor.Jit
      ~path:"ir-top-level-mixed-invalid-role-prefix.HC"
      "_extern _BOUND I64 Direct(I64 first,...);Direct(1,2);if(1) 2;"
  in
  expect_mixed_error "invalid mixed role after a body" "HCIRL0004" (Some 1)
    (lower_mixed ~compiler_options:[ 0L; 0L ] ~targets statements);
  let expressions =
    ordinary_statements ~mode:Preprocessor.Jit
      ~path:"ir-top-level-mixed-identity-prefix.HC" "1+2;3+4;"
  in
  expect_mixed_error "mixed instruction exhaustion after a body" "HCIRL0005"
    (Some 1)
    (lower_mixed ~instruction:(Int.max_int - 5)
       ~compiler_options:[ 0L; 0L ] ~targets:[] expressions);
  expect_mixed_error "mixed value exhaustion after a body" "HCIRL0005"
    (Some 1)
    (lower_mixed ~value:(Int.max_int - 3) ~compiler_options:[ 0L; 0L ]
       ~targets:[] expressions);
  match
    lower_mixed ~stream:(Int.max_int - 1) ~block:(Int.max_int - 1)
      ~compiler_options:[ 0L; 0L ] ~targets:[] expressions
  with
  | Error errors ->
      Alcotest.(check (list string))
        "mixed owner exhaustion after a body"
        [ "HCIRL0005"; "HCIRL0005" ]
        (List.map (fun (error : Batch.error) -> error.code) errors);
      Alcotest.(check bool)
        "mixed owner exhaustion identifies the second statement" true
        (List.for_all
           (fun (error : Batch.error) -> error.statement_index = Some 1)
           errors)
  | Ok Batch.Unsupported_batch ->
      Alcotest.fail "mixed owner exhaustion reported unsupported"
  | Ok (Batch.Lowered _) ->
      Alcotest.fail "mixed owner exhaustion exposed partial bodies"

let tests =
  [
    Alcotest.test_case "mixed empty and homogeneous batches" `Quick
      mixed_empty_and_homogeneous_batches_preserve_behavior;
    Alcotest.test_case "mixed source orders" `Quick
      mixed_orders_preserve_source_metadata_and_cursors;
    Alcotest.test_case "mixed alternating batches" `Quick
      mixed_alternating_batch_ignores_target_order;
    Alcotest.test_case "mixed target validation" `Quick
      mixed_target_validation_is_atomic;
    Alcotest.test_case "mixed nested-call classification" `Quick
      mixed_nested_calls_are_not_standalone_targets;
    Alcotest.test_case "mixed callback classification" `Quick
      mixed_identifier_callbacks_are_not_direct_call_targets;
    Alcotest.test_case "mixed suffix failure atomicity" `Quick
      mixed_post_prefix_failures_are_atomic;
    Alcotest.test_case "empty and single expression batches" `Quick
      expression_empty_and_single_batches_thread_identity;
    Alcotest.test_case "multi-expression batches" `Quick
      expression_batches_cover_supported_families;
    Alcotest.test_case "expression batch failure atomicity" `Quick
      expression_batch_failures_are_atomic;
    Alcotest.test_case "empty and single batches" `Quick
      empty_and_single_batches_thread_identity;
    Alcotest.test_case "multi-statement batches" `Quick
      multi_statement_batches_cover_modes_and_options;
    Alcotest.test_case "batch failure atomicity" `Quick
      invalid_unsupported_and_exhausted_batches_are_atomic;
  ]
