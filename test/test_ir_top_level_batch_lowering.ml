module Batch = Holyc_lib.Ir_top_level_batch_lowering
module Sequence = Holyc_lib.Ir_instruction_sequence
module Graph = Holyc_lib.Ir_block_graph
module Top_level = Holyc_lib.Ir_top_level_body
module Opcode = Holyc_lib.Ir_opcode
module Option_ = Holyc_lib.Compiler_option
module Semantic_result = Holyc_lib.Semantic_function_call_expression_result
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

let inputs ~mode ~path source =
  let prepared = Test_top_level_expression_result.prepared ~mode ~path source in
  let _, _, _, results = Test_top_level_expression_result.analyze prepared in
  let records =
    Holyc_lib.classify_function_records prepared.session
      ~resolution:prepared.functions prepared.ast
    |> require_ok Fun.id
  in
  let targets =
    Semantic_result.top_level_direct_calls results
    |> List.map (fun call ->
        Target.classify ~records call |> require_ok Target.error_to_string)
  in
  (targets, Semantic_result.top_level_statements results)

let lower ?(stream = 7) ?(block = 30) ?(instruction = 10) ?(value = 20)
    ~compiler_options ~targets statements =
  Batch.lower_direct_calls ~stream_id:(stream_id stream)
    ~block_id:(block_id block)
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

let tests =
  [
    Alcotest.test_case "empty and single batches" `Quick
      empty_and_single_batches_thread_identity;
    Alcotest.test_case "multi-statement batches" `Quick
      multi_statement_batches_cover_modes_and_options;
    Alcotest.test_case "batch failure atomicity" `Quick
      invalid_unsupported_and_exhausted_batches_are_atomic;
  ]
