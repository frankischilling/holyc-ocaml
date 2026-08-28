open Holyc_lib
module Lowering = Ir_direct_call_lowering
module Expression = Ir_expression_lowering
module Sequence = Ir_instruction_sequence
module Span = Span
module Target = Semantic_function_call_target_classification
module Top_target = Semantic_top_level_function_call_target_classification

let checked = function
  | Ok value -> value
  | Error (error : Sequence.error) ->
      Alcotest.fail (error.code ^ ": " ^ error.message)

let instruction_id value = Sequence.Instruction_id.of_int value |> checked
let value_id value = Sequence.Value_id.of_int value |> checked

let prepared mode source =
  Test_function_call_target_classification.prepare ~mode
    ~path:"ir-direct-call.HC" source

let analyze prepared = Test_function_call_target_classification.analyze prepared

let function_named results name =
  results |> Semantic_function_call_expression_result.functions
  |> List.find (fun function_ ->
      function_ |> Semantic_function_call_expression_result.function_symbol
      |> Semantic_symbol.name |> String.equal name)

let return_root results owner =
  let return_ =
    function_named results owner
    |> Semantic_function_call_expression_result.function_returns |> List.hd
  in
  match Semantic_function_call_expression_result.return_value return_ with
  | Some result -> result
  | None -> Alcotest.fail "expected a valued return"

let direct_call results owner =
  Test_function_call_target_classification.direct_calls results owner |> List.hd

let canonical_symbol call =
  call |> Semantic_function_call_expression_result.direct_source
  |> Semantic_function_call_conversion_policy.direct_source
  |> Semantic_function_call_resolution.direct_target_symbol

let source_span result =
  match Semantic_function_call_expression_result.result_origin result with
  | Semantic_symbol.Source_location location -> location.span
  | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
      Alcotest.fail "expected a complete source span"

let provided_argument call =
  match
    call |> Semantic_function_call_expression_result.direct_fixed_results
    |> List.hd |> Semantic_function_call_expression_result.fixed_path
  with
  | Semantic_function_call_expression_result.Provided_result result -> result
  | Semantic_function_call_expression_result.Declared_default_result _ ->
      Alcotest.fail "expected a provided argument"

let target records call =
  match Target.classify ~records call with
  | Ok target -> target
  | Error error -> error |> Target.error_to_string |> Alcotest.fail

let lower ?(instruction = 0) ?(value = 0) records results owner =
  let target = target records (direct_call results owner) in
  Lowering.lower
    ~instruction_id:(instruction_id instruction)
    ~value_id:(value_id value) ~target
    (return_root results owner)

let lowered = function
  | Ok (Lowering.Lowered lowered) -> lowered
  | Ok Lowering.Unsupported_call -> Alcotest.fail "expected a lowered call"
  | Error errors ->
      errors
      |> List.map (fun error -> error.Sequence.message)
      |> String.concat ", " |> Alcotest.fail

let descriptions lowered =
  lowered |> Lowering.sequence |> Sequence.instructions
  |> List.map Sequence.description

let opcode_names descriptions =
  descriptions
  |> List.map (fun description ->
      description.Sequence.opcode |> Ir_opcode.to_source_name)

let analyze_top_level prepared =
  let _, _, _, results = Test_top_level_expression_result.analyze prepared in
  let records =
    Holyc_lib.classify_function_records prepared.session
      ~resolution:prepared.functions prepared.ast
    |> function
    | Ok records -> records
    | Error message -> Alcotest.fail message
  in
  (results, records)

let top_level_result results call =
  let id =
    Semantic_function_call_expression_result.top_level_direct_result_id call
  in
  results |> Semantic_function_call_expression_result.top_level_all_results
  |> List.find (fun result ->
      Semantic_function_call_expression_result.Id.equal id
        (Semantic_function_call_expression_result.result_id result))

let top_level_target records call =
  match Top_target.classify ~records call with
  | Ok target -> target
  | Error error -> error |> Top_target.error_to_string |> Alcotest.fail

let lower_top_level ?(instruction = 0) ?(value = 0) records results call =
  Lowering.lower_top_level
    ~instruction_id:(instruction_id instruction)
    ~value_id:(value_id value)
    ~target:(top_level_target records call)
    (top_level_result results call)

let direct_calls_emit_complete_sequences () =
  List.iter
    (fun (mode, return_type) ->
      let source =
        Printf.sprintf "%s Callee(){return 7;}%s Caller(){return Callee();}"
          return_type return_type
      in
      let prepared = prepared mode source in
      let results, records = analyze prepared in
      let expected_symbol_id =
        direct_call results "Caller"
        |> canonical_symbol |> Semantic_symbol.id |> Semantic_symbol.Id.to_int
      in
      let lowered =
        lower ~instruction:10 ~value:20 records results "Caller" |> lowered
      in
      let items = descriptions lowered in
      Alcotest.(check (list string))
        "complete direct-call sequence"
        [ "IC_CALL_START"; "IC_CALL"; "IC_ADD_RSP"; "IC_CALL_END" ]
        (opcode_names items);
      Alcotest.(check (list int))
        "instruction identities are consecutive" [ 10; 11; 12; 13 ]
        (List.map
           (fun item ->
             Sequence.Instruction_id.to_int item.Sequence.instruction_id)
           items);
      Alcotest.(check int)
        "the call result uses the caller identity" 20
        (Lowering.result_value lowered |> Sequence.Value_id.to_int);
      Alcotest.(check (pair int int))
        "identity cursors advance by four and one" (14, 21)
        ( Lowering.next_instruction_id lowered |> Sequence.Instruction_id.to_int,
          Lowering.next_value_id lowered |> Sequence.Value_id.to_int );
      Alcotest.(check string)
        "the checked result type is retained" ("public:" ^ return_type)
        (Lowering.result_type lowered |> Sequence.type_name);
      let call_span =
        return_root results "Caller"
        |> Semantic_function_call_expression_result.result_origin
        |> function
        | Semantic_symbol.Source_location location -> location.span
        | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
            Alcotest.fail "expected a call span"
      in
      Alcotest.(check (list (option (pair int int))))
        "every instruction keeps the complete call span"
        (List.init 4 (fun _ -> Some (call_span.start, call_span.stop)))
        (List.map
           (fun item ->
             Option.map
               (fun (span : Span.t) -> (span.start, span.stop))
               item.Sequence.span)
           items);
      let symbol_ids =
        items
        |> List.filter_map (fun item ->
            match item.Sequence.payload with
            | Some (Sequence.Symbol symbol) ->
                Some (Semantic_symbol.id symbol |> Semantic_symbol.Id.to_int)
            | Some (Sequence.Integer _) -> None
            | Some
                ( Sequence.Float_bits _
                | Sequence.Bytes _
                | Sequence.Block _
                | Sequence.Block_targets _ )
            | None -> Alcotest.fail "unexpected call payload")
      in
      Alcotest.(check bool)
        "start, call, and end carry the canonical symbol" true
        (match symbol_ids with
        | [ first; second; third ] ->
            first = expected_symbol_id
            && second = expected_symbol_id
            && third = expected_symbol_id
        | _ -> false);
      Alcotest.(check (option int64))
        "cleanup carries a zero-byte immediate" (Some 0L)
        (match (List.nth items 2).Sequence.payload with
        | Some (Sequence.Integer value) -> Some value
        | _ -> None))
    [ (Preprocessor.Jit, "I64"); (Preprocessor.Aot, "F64") ]

let cleanup_opcode_follows_checked_flags () =
  [ ("argpop", "IC_ADD_RSP1"); ("argpop noargpop", "IC_ADD_RSP") ]
  |> List.iter (fun (modifiers, expected) ->
      let source =
        Printf.sprintf
          "%s I64 Callee(){return 1;}I64 Caller(){return Callee();}" modifiers
      in
      let prepared = prepared Preprocessor.Jit source in
      let results, records = analyze prepared in
      let items = lower records results "Caller" |> lowered |> descriptions in
      Alcotest.(check string)
        "cleanup uses the checked caller/callee predicate" expected
        ((List.nth items 2).Sequence.opcode |> Ir_opcode.to_source_name))

let one_argument_sequences_preserve_expression_ir () =
  [
    ( Preprocessor.Jit,
      "I64 Callee(I64 value){return value;}I64 Caller(){return Callee(1+2);}",
      0x000002000L,
      "public:I64" );
    ( Preprocessor.Aot,
      "F64 Callee(F64 value){return value;}F64 Caller(){return Callee(1+2);}",
      0x000002001L,
      "public:F64" );
  ]
  |> List.iter (fun (mode, source, root_flags, result_type) ->
      let prepared = prepared mode source in
      let results, records = analyze prepared in
      let call = direct_call results "Caller" in
      let argument = provided_argument call in
      let call_span = return_root results "Caller" |> source_span in
      let expected_argument_descriptions =
        match
          Expression.lower_typed_result ~instruction_id:(instruction_id 11)
            ~value_id:(value_id 20) argument
        with
        | Ok (Expression.Lowered lowered) ->
            lowered |> Expression.sequence |> Sequence.instructions
            |> List.map Sequence.description
        | Ok Expression.Unsupported_expression ->
            Alcotest.fail "expected a supported argument expression"
        | Error errors ->
            errors
            |> List.map (fun error -> error.Sequence.message)
            |> String.concat ", " |> Alcotest.fail
      in
      let lowered =
        lower ~instruction:10 ~value:20 records results "Caller" |> lowered
      in
      let items = descriptions lowered in
      Alcotest.(check (list string))
        "the argument is appended between start and call"
        [
          "IC_CALL_START";
          "IC_IMM_I64";
          "IC_IMM_I64";
          "IC_ADD";
          "IC_CALL";
          "IC_ADD_RSP1";
          "IC_CALL_END";
        ]
        (opcode_names items);
      Alcotest.(check (list int))
        "instruction identities include the argument tree"
        [ 10; 11; 12; 13; 14; 15; 16 ]
        (List.map
           (fun item ->
             Sequence.Instruction_id.to_int item.Sequence.instruction_id)
           items);
      Alcotest.(check (list int64))
        "only the argument root gains push-result intent"
        [ 0L; 0L; 0L; root_flags; 0L; 0L; 0L ]
        (List.map (fun item -> item.Sequence.flags) items);
      let span_pair span =
        Option.map (fun (span : Span.t) -> (span.start, span.stop)) span
      in
      Alcotest.(check (list (option (pair int int))))
        "the argument tree keeps the canonical expression spans"
        (List.map
           (fun item -> span_pair item.Sequence.span)
           expected_argument_descriptions)
        ([ 1; 2; 3 ]
        |> List.map (fun index ->
            span_pair (List.nth items index).Sequence.span));
      Alcotest.(check (list (option (pair int int))))
        "the call records keep the complete call span"
        (List.init 4 (fun _ -> Some (call_span.start, call_span.stop)))
        ([ 0; 4; 5; 6 ]
        |> List.map (fun index ->
            span_pair (List.nth items index).Sequence.span));
      Alcotest.(check int)
        "the call result follows all argument values" 23
        (Lowering.result_value lowered |> Sequence.Value_id.to_int);
      Alcotest.(check (pair int int))
        "both identity cursors include the argument tree" (17, 24)
        ( Lowering.next_instruction_id lowered |> Sequence.Instruction_id.to_int,
          Lowering.next_value_id lowered |> Sequence.Value_id.to_int );
      Alcotest.(check string)
        "the call result keeps its checked type" result_type
        (Lowering.result_type lowered |> Sequence.type_name);
      Alcotest.(check (option int64))
        "one fixed argument cleans eight bytes" (Some 8L)
        (match (List.nth items 5).Sequence.payload with
        | Some (Sequence.Integer value) -> Some value
        | _ -> None))

let one_argument_cleanup_and_dump () =
  let source =
    "noargpop I64 Callee(I64 value){return value;}I64 Caller(){return \
     Callee(1);}"
  in
  let prepared = prepared Preprocessor.Jit source in
  let results, records = analyze prepared in
  let first =
    lower ~instruction:3 ~value:5 records results "Caller" |> lowered
  in
  let second =
    lower ~instruction:3 ~value:5 records results "Caller" |> lowered
  in
  let items = descriptions first in
  Alcotest.(check string)
    "noargpop selects caller cleanup" "IC_ADD_RSP"
    ((List.nth items 3).Sequence.opcode |> Ir_opcode.to_source_name);
  Alcotest.(check (option int64))
    "caller cleanup retains eight bytes" (Some 8L)
    (match (List.nth items 3).Sequence.payload with
    | Some (Sequence.Integer value) -> Some value
    | _ -> None);
  Alcotest.(check string)
    "one-argument dumps replay exactly" (Lowering.human first)
    (Lowering.human second)

let multiple_arguments_preserve_order () =
  let source =
    "I64 Callee(I64 first,F64 second,I64 third){return first;}I64 \
     Caller(){return Callee(1+2,3,4*5);}"
  in
  [ Preprocessor.Jit; Preprocessor.Aot ]
  |> List.iter (fun mode ->
      let prepared = prepared mode source in
      let results, records = analyze prepared in
      let first =
        lower ~instruction:10 ~value:20 records results "Caller" |> lowered
      in
      let second =
        lower ~instruction:10 ~value:20 records results "Caller" |> lowered
      in
      let items = descriptions first in
      Alcotest.(check (list string))
        "fixed argument trees retain parameter order"
        [
          "IC_CALL_START";
          "IC_IMM_I64";
          "IC_IMM_I64";
          "IC_ADD";
          "IC_IMM_I64";
          "IC_IMM_I64";
          "IC_IMM_I64";
          "IC_MUL";
          "IC_CALL";
          "IC_ADD_RSP1";
          "IC_CALL_END";
        ]
        (opcode_names items);
      Alcotest.(check (list int64))
        "every fixed root gains one push-result bit"
        [
          0L;
          0L;
          0L;
          0x000002000L;
          0x000002001L;
          0L;
          0L;
          0x000002000L;
          0L;
          0L;
          0L;
        ]
        (List.map (fun item -> item.Sequence.flags) items);
      Alcotest.(check (list int))
        "instruction identities span every argument"
        [ 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20 ]
        (List.map
           (fun item ->
             Sequence.Instruction_id.to_int item.Sequence.instruction_id)
           items);
      Alcotest.(check int)
        "the call result follows every argument value" 27
        (Lowering.result_value first |> Sequence.Value_id.to_int);
      Alcotest.(check (pair int int))
        "identity cursors include all fixed arguments" (21, 28)
        ( Lowering.next_instruction_id first |> Sequence.Instruction_id.to_int,
          Lowering.next_value_id first |> Sequence.Value_id.to_int );
      Alcotest.(check (option int64))
        "three fixed arguments clean 24 bytes" (Some 24L)
        (match (List.nth items 9).Sequence.payload with
        | Some (Sequence.Integer value) -> Some value
        | _ -> None);
      Alcotest.(check string)
        "multiple-argument dumps replay exactly" (Lowering.human first)
        (Lowering.human second))

let empty_variadic_tail_emits_hidden_count () =
  [ Preprocessor.Jit; Preprocessor.Aot ]
  |> List.iter (fun mode ->
      let prepared =
        prepared mode
          "I64 Callee(...){return argc;}I64 Caller(){return Callee();}"
      in
      let results, records = analyze prepared in
      let call_span = return_root results "Caller" |> source_span in
      let first =
        lower ~instruction:10 ~value:20 records results "Caller" |> lowered
      in
      let second =
        lower ~instruction:10 ~value:20 records results "Caller" |> lowered
      in
      let items = descriptions first in
      Alcotest.(check (list string))
        "the hidden count precedes the call"
        [
          "IC_CALL_START"; "IC_IMM_I64"; "IC_CALL"; "IC_ADD_RSP"; "IC_CALL_END";
        ]
        (opcode_names items);
      Alcotest.(check (list int64))
        "only the hidden count is pushed"
        [ 0L; 0x000002000L; 0L; 0L; 0L ]
        (List.map (fun item -> item.Sequence.flags) items);
      let hidden = List.nth items 1 in
      Alcotest.(check (option int64))
        "the hidden count is zero" (Some 0L)
        (match hidden.Sequence.payload with
        | Some (Sequence.Integer value) -> Some value
        | _ -> None);
      Alcotest.(check (option string))
        "the hidden count uses the synthesized argc type" (Some "internal:I64")
        (Option.map Sequence.type_name hidden.Sequence.target_type);
      Alcotest.(check (option (pair int int)))
        "the hidden count keeps the complete call span"
        (Some (call_span.start, call_span.stop))
        (Option.map
           (fun (span : Span.t) -> (span.start, span.stop))
           hidden.Sequence.span);
      Alcotest.(check (list int))
        "the hidden slot consumes an instruction identity"
        [ 10; 11; 12; 13; 14 ]
        (List.map
           (fun item ->
             Sequence.Instruction_id.to_int item.Sequence.instruction_id)
           items);
      Alcotest.(check int)
        "the call result follows the hidden count" 21
        (Lowering.result_value first |> Sequence.Value_id.to_int);
      Alcotest.(check (pair int int))
        "both cursors include the hidden slot" (15, 22)
        ( Lowering.next_instruction_id first |> Sequence.Instruction_id.to_int,
          Lowering.next_value_id first |> Sequence.Value_id.to_int );
      Alcotest.(check (option int64))
        "the hidden slot cleans eight bytes" (Some 8L)
        (match (List.nth items 3).Sequence.payload with
        | Some (Sequence.Integer value) -> Some value
        | _ -> None);
      Alcotest.(check string)
        "empty variadic dumps replay exactly" (Lowering.human first)
        (Lowering.human second))

let fixed_arguments_precede_hidden_variadic_count () =
  let source =
    "I64 Callee(I64 first,F64 second,...){return first;}I64 Caller(){return \
     Callee(1+2,3);}"
  in
  [ Preprocessor.Jit; Preprocessor.Aot ]
  |> List.iter (fun mode ->
      let prepared = prepared mode source in
      let results, records = analyze prepared in
      let items =
        lower ~instruction:10 ~value:20 records results "Caller"
        |> lowered |> descriptions
      in
      Alcotest.(check (list string))
        "fixed trees precede the hidden count"
        [
          "IC_CALL_START";
          "IC_IMM_I64";
          "IC_IMM_I64";
          "IC_ADD";
          "IC_IMM_I64";
          "IC_IMM_I64";
          "IC_CALL";
          "IC_ADD_RSP";
          "IC_CALL_END";
        ]
        (opcode_names items);
      Alcotest.(check (list int64))
        "fixed roots and hidden count are pushed once"
        [ 0L; 0L; 0L; 0x000002000L; 0x000002001L; 0x000002000L; 0L; 0L; 0L ]
        (List.map (fun item -> item.Sequence.flags) items);
      Alcotest.(check (option int64))
        "two fixed slots plus argc clean 24 bytes" (Some 24L)
        (match (List.nth items 7).Sequence.payload with
        | Some (Sequence.Integer value) -> Some value
        | _ -> None))

let one_variadic_argument_follows_hidden_count () =
  [ Preprocessor.Jit; Preprocessor.Aot ]
  |> List.iter (fun mode ->
      let prepared =
        prepared mode
          "I64 Callee(...){return argc;}I64 Caller(){return Callee(1.5+2.5);}"
      in
      let results, records = analyze prepared in
      let call = direct_call results "Caller" in
      let argument =
        call |> Semantic_function_call_expression_result.direct_variadic_results
        |> List.hd
      in
      let call_span = return_root results "Caller" |> source_span in
      let expected_argument_descriptions =
        match
          Expression.lower_typed_result ~instruction_id:(instruction_id 12)
            ~value_id:(value_id 21) argument
        with
        | Ok (Expression.Lowered lowered) ->
            lowered |> Expression.sequence |> Sequence.instructions
            |> List.map Sequence.description
        | Ok Expression.Unsupported_expression ->
            Alcotest.fail "expected a supported variadic expression"
        | Error errors ->
            errors
            |> List.map (fun error -> error.Sequence.message)
            |> String.concat ", " |> Alcotest.fail
      in
      let first =
        lower ~instruction:10 ~value:20 records results "Caller" |> lowered
      in
      let second =
        lower ~instruction:10 ~value:20 records results "Caller" |> lowered
      in
      let items = descriptions first in
      Alcotest.(check (list string))
        "the variadic tree follows its hidden count"
        [
          "IC_CALL_START";
          "IC_IMM_I64";
          "IC_IMM_F64";
          "IC_IMM_F64";
          "IC_ADD";
          "IC_CALL";
          "IC_ADD_RSP";
          "IC_CALL_END";
        ]
        (opcode_names items);
      Alcotest.(check (list int64))
        "the hidden count and variadic root are pushed"
        [ 0L; 0x000002000L; 0L; 0L; 0x000002000L; 0L; 0L; 0L ]
        (List.map (fun item -> item.Sequence.flags) items);
      let hidden = List.nth items 1 in
      Alcotest.(check (option int64))
        "the hidden count records one supplied value" (Some 1L)
        (match hidden.Sequence.payload with
        | Some (Sequence.Integer value) -> Some value
        | _ -> None);
      Alcotest.(check (option string))
        "the nonempty count keeps the argc type" (Some "internal:I64")
        (Option.map Sequence.type_name hidden.Sequence.target_type);
      let span_pair span =
        Option.map (fun (span : Span.t) -> (span.start, span.stop)) span
      in
      Alcotest.(check (option (pair int int)))
        "the count keeps the complete call span"
        (Some (call_span.start, call_span.stop))
        (span_pair hidden.Sequence.span);
      Alcotest.(check (list (option (pair int int))))
        "the supplied tree keeps canonical expression spans"
        (List.map
           (fun item -> span_pair item.Sequence.span)
           expected_argument_descriptions)
        ([ 2; 3; 4 ]
        |> List.map (fun index ->
            span_pair (List.nth items index).Sequence.span));
      Alcotest.(check (list int))
        "variadic identities are consecutive"
        [ 10; 11; 12; 13; 14; 15; 16; 17 ]
        (List.map
           (fun item ->
             Sequence.Instruction_id.to_int item.Sequence.instruction_id)
           items);
      Alcotest.(check int)
        "the call result follows the variadic tree" 24
        (Lowering.result_value first |> Sequence.Value_id.to_int);
      Alcotest.(check (pair int int))
        "both cursors include count and supplied tree" (18, 25)
        ( Lowering.next_instruction_id first |> Sequence.Instruction_id.to_int,
          Lowering.next_value_id first |> Sequence.Value_id.to_int );
      Alcotest.(check (option int64))
        "count and one supplied slot clean 16 bytes" (Some 16L)
        (match (List.nth items 6).Sequence.payload with
        | Some (Sequence.Integer value) -> Some value
        | _ -> None);
      Alcotest.(check string)
        "nonempty variadic dumps replay exactly" (Lowering.human first)
        (Lowering.human second))

let multiple_variadic_arguments_preserve_order () =
  let source =
    "I64 Callee(I64 fixed,...){return fixed;}I64 Caller(){return \
     Callee(1,2,3*4);}"
  in
  [ Preprocessor.Jit; Preprocessor.Aot ]
  |> List.iter (fun mode ->
      let prepared = prepared mode source in
      let results, records = analyze prepared in
      let lowered =
        lower ~instruction:10 ~value:20 records results "Caller" |> lowered
      in
      let items = descriptions lowered in
      Alcotest.(check (list string))
        "fixed, count, and variadic trees retain source order"
        [
          "IC_CALL_START";
          "IC_IMM_I64";
          "IC_IMM_I64";
          "IC_IMM_I64";
          "IC_IMM_I64";
          "IC_IMM_I64";
          "IC_MUL";
          "IC_CALL";
          "IC_ADD_RSP";
          "IC_CALL_END";
        ]
        (opcode_names items);
      Alcotest.(check (list int64))
        "every logical argument root is pushed once"
        [
          0L;
          0x000002000L;
          0x000002000L;
          0x000002000L;
          0L;
          0L;
          0x000002000L;
          0L;
          0L;
          0L;
        ]
        (List.map (fun item -> item.Sequence.flags) items);
      Alcotest.(check (option int64))
        "the hidden count records both supplied values" (Some 2L)
        (match (List.nth items 2).Sequence.payload with
        | Some (Sequence.Integer value) -> Some value
        | _ -> None);
      Alcotest.(check int)
        "the call result follows every variadic value" 26
        (Lowering.result_value lowered |> Sequence.Value_id.to_int);
      Alcotest.(check (pair int int))
        "all argument identities advance the cursors" (20, 27)
        ( Lowering.next_instruction_id lowered |> Sequence.Instruction_id.to_int,
          Lowering.next_value_id lowered |> Sequence.Value_id.to_int );
      Alcotest.(check (option int64))
        "fixed, count, and two supplied slots clean 32 bytes" (Some 32L)
        (match (List.nth items 8).Sequence.payload with
        | Some (Sequence.Integer value) -> Some value
        | _ -> None))

let extern_and_import_calls_select_checked_opcodes () =
  [
    (Preprocessor.Jit, "extern", "IC_CALL_INDIRECT2");
    (Preprocessor.Aot, "import", "IC_CALL_IMPORT");
    (Preprocessor.Aot, "extern", "IC_CALL_EXTERN");
  ]
  |> List.iter (fun (mode, declaration, expected_call) ->
      let source =
        Printf.sprintf
          "%s I64 Callee(I64 fixed,...);I64 Caller(){return Callee(1,2);}"
          declaration
      in
      let prepared = prepared mode source in
      let results, records = analyze prepared in
      let expected_symbol_id =
        direct_call results "Caller"
        |> canonical_symbol |> Semantic_symbol.id |> Semantic_symbol.Id.to_int
      in
      let first =
        lower ~instruction:10 ~value:20 records results "Caller" |> lowered
      in
      let second =
        lower ~instruction:10 ~value:20 records results "Caller" |> lowered
      in
      let items = descriptions first in
      Alcotest.(check (list string))
        "the classified access selects its call opcode"
        [
          "IC_CALL_START";
          "IC_IMM_I64";
          "IC_IMM_I64";
          "IC_IMM_I64";
          expected_call;
          "IC_ADD_RSP";
          "IC_CALL_END";
        ]
        (opcode_names items);
      let symbol_ids =
        items
        |> List.filter_map (fun item ->
            match item.Sequence.payload with
            | Some (Sequence.Symbol symbol) ->
                Some (Semantic_symbol.id symbol |> Semantic_symbol.Id.to_int)
            | Some (Sequence.Integer _)
            | Some
                ( Sequence.Float_bits _
                | Sequence.Bytes _
                | Sequence.Block _
                | Sequence.Block_targets _ )
            | None -> None)
      in
      Alcotest.(check (list int))
        "start, selected call, and end keep the canonical symbol"
        [ expected_symbol_id; expected_symbol_id; expected_symbol_id ]
        symbol_ids;
      Alcotest.(check (list int))
        "extern and import instruction identities are consecutive"
        [ 10; 11; 12; 13; 14; 15; 16 ]
        (List.map
           (fun item ->
             Sequence.Instruction_id.to_int item.Sequence.instruction_id)
           items);
      Alcotest.(check int)
        "the extern or import result follows all argument values" 23
        (Lowering.result_value first |> Sequence.Value_id.to_int);
      Alcotest.(check (pair int int))
        "extern and import cursors include the complete call" (17, 24)
        ( Lowering.next_instruction_id first |> Sequence.Instruction_id.to_int,
          Lowering.next_value_id first |> Sequence.Value_id.to_int );
      Alcotest.(check (option int64))
        "fixed, hidden, and supplied slots clean 24 bytes" (Some 24L)
        (match (List.nth items 5).Sequence.payload with
        | Some (Sequence.Integer value) -> Some value
        | _ -> None);
      Alcotest.(check string)
        "extern and import dumps replay exactly" (Lowering.human first)
        (Lowering.human second))

let unsupported_argument_expression_returns_no_ir () =
  let prepared =
    prepared Preprocessor.Jit
      "I64 Callee(I64 first,I64 second,I64 third){return first;}I64 \
       Caller(){I64 local=1;return Callee(1,local,3);}"
  in
  let results, records = analyze prepared in
  match lower records results "Caller" with
  | Ok Lowering.Unsupported_call -> ()
  | Ok (Lowering.Lowered _) -> Alcotest.fail "expected an unsupported argument"
  | Error errors ->
      errors
      |> List.map (fun error -> error.Sequence.message)
      |> String.concat ", " |> Alcotest.fail

let deterministic_dump () =
  let prepared =
    prepared Preprocessor.Jit
      "I64 Callee(){return 1;}I64 Caller(){return Callee();}"
  in
  let results, records = analyze prepared in
  let first =
    lower ~instruction:3 ~value:5 records results "Caller" |> lowered
  in
  let second =
    lower ~instruction:3 ~value:5 records results "Caller" |> lowered
  in
  Alcotest.(check string)
    "direct-call dumps replay exactly" (Lowering.human first)
    (Lowering.human second)

let unsupported_call_boundaries () =
  let expect_unsupported mode source =
    let prepared = prepared mode source in
    let results, records = analyze prepared in
    match lower records results "Caller" with
    | Ok Lowering.Unsupported_call -> ()
    | Ok (Lowering.Lowered _) -> Alcotest.fail "expected an unsupported call"
    | Error errors ->
        errors
        |> List.map (fun error -> error.Sequence.message)
        |> String.concat ", " |> Alcotest.fail
  in
  expect_unsupported Preprocessor.Jit
    "_intern 42 I64 Callee();I64 Caller(){return Callee();}";
  expect_unsupported Preprocessor.Jit
    "I64 Callee(I64 value=1){return value;}I64 Caller(){return Callee();}";
  expect_unsupported Preprocessor.Jit
    "I64 Callee(...){return argc;}I64 Caller(){I64 local=1;return \
     Callee(local);}"

let inconsistent_and_exhausted_inputs_fail_without_ir () =
  let prepared =
    prepared Preprocessor.Jit
      "I64 First(){return 1;}I64 Second(){return 2;}\n\
       I64 Caller(){First();return Second();}"
  in
  let results, records = analyze prepared in
  let calls =
    Test_function_call_target_classification.direct_calls results "Caller"
  in
  let wrong_target = target records (List.hd calls) in
  (match
     Lowering.lower ~instruction_id:(instruction_id 0) ~value_id:(value_id 0)
       ~target:wrong_target
       (return_root results "Caller")
   with
  | Error [ error ] ->
      Alcotest.(check string)
        "inconsistent evidence code" "HCIRL0004" error.Sequence.code
  | Error _ -> Alcotest.fail "expected one metadata error"
  | Ok _ -> Alcotest.fail "expected inconsistent evidence to fail");
  let right_target = target records (List.nth calls 1) in
  match
    Lowering.lower
      ~instruction_id:(instruction_id Int.max_int)
      ~value_id:(value_id 0) ~target:right_target
      (return_root results "Caller")
  with
  | Error [ error ] ->
      Alcotest.(check string)
        "instruction exhaustion code" "HCIRL0005" error.Sequence.code
  | Error _ -> Alcotest.fail "expected one exhaustion error"
  | Ok _ -> Alcotest.fail "expected identity exhaustion to fail"

let top_level_calls_reuse_complete_composition () =
  let run mode source expected =
    let prepared =
      Test_top_level_expression_result.prepared ~mode
        ~path:"ir-top-level-direct-call.HC" source
    in
    let results, records = analyze_top_level prepared in
    let calls =
      Semantic_function_call_expression_result.top_level_direct_calls results
    in
    Alcotest.(check int)
      "every expected top-level call is typed" (List.length expected)
      (List.length calls);
    List.iter2
      (fun call expected_opcode ->
        let first =
          lower_top_level ~instruction:10 ~value:20 records results call
          |> lowered
        in
        let second =
          lower_top_level ~instruction:10 ~value:20 records results call
          |> lowered
        in
        let items = descriptions first in
        Alcotest.(check (list string))
          "top-level call uses the shared complete sequence"
          [
            "IC_CALL_START";
            "IC_IMM_I64";
            "IC_IMM_I64";
            "IC_IMM_I64";
            expected_opcode;
            "IC_ADD_RSP";
            "IC_CALL_END";
          ]
          (opcode_names items);
        Alcotest.(check (list int64))
          "fixed, count, and variadic values are pushed"
          [ 0x2000L; 0x2000L; 0x2000L ]
          (items
          |> List.filter_map (fun item ->
              item.Sequence.result |> Option.map (fun _ -> item.Sequence.flags))
          |> List.rev |> List.tl |> List.rev);
        Alcotest.(check (option int64))
          "top-level cleanup includes fixed, hidden, and variadic slots"
          (Some 24L)
          (match (List.nth items 5).Sequence.payload with
          | Some (Sequence.Integer bytes) -> Some bytes
          | _ -> None);
        Alcotest.(check (pair int int))
          "top-level identities include the complete call" (17, 24)
          ( Lowering.next_instruction_id first |> Sequence.Instruction_id.to_int,
            Lowering.next_value_id first |> Sequence.Value_id.to_int );
        Alcotest.(check string)
          "top-level call composition is deterministic" (Lowering.human first)
          (Lowering.human second))
      calls expected
  in
  run Preprocessor.Jit
    "extern I64 External(I64 first,...);\n\
     _extern _BOUND I64 Bound(I64 first,...);\n\
     External(1,2);Bound(3,4);"
    [ "IC_CALL_INDIRECT2"; "IC_CALL" ];
  run Preprocessor.Aot
    "extern I64 External(I64 first,...);\n\
     import I64 Imported(I64 first,...);\n\
     _extern _BOUND I64 Bound(I64 first,...);\n\
     External(1,2);Imported(3,4);Bound(5,6);"
    [ "IC_CALL_EXTERN"; "IC_CALL_IMPORT"; "IC_CALL" ]

let top_level_call_ownership_is_exact () =
  let source =
    "I64 First(){return 1;}I64 Second(){return 2;}First();Second();"
  in
  let prepared =
    Test_top_level_expression_result.prepared ~mode:Preprocessor.Jit
      ~path:"ir-top-level-direct-call-owner.HC" source
  in
  let results, records = analyze_top_level prepared in
  let calls =
    Semantic_function_call_expression_result.top_level_direct_calls results
  in
  let first_target = top_level_target records (List.hd calls) in
  (match
     Lowering.lower_top_level ~instruction_id:(instruction_id 0)
       ~value_id:(value_id 0) ~target:first_target
       (top_level_result results (List.nth calls 1))
   with
  | Error [ error ] ->
      Alcotest.(check string)
        "mismatched top-level evidence code" "HCIRL0004" error.Sequence.code
  | Error _ -> Alcotest.fail "expected one top-level ownership error"
  | Ok _ -> Alcotest.fail "expected mismatched top-level evidence to fail");
  let replay =
    Test_top_level_expression_result.prepared ~mode:Preprocessor.Jit
      ~path:"ir-top-level-direct-call-owner.HC" source
  in
  let replay_results, _ = analyze_top_level replay in
  let replay_call =
    replay_results
    |> Semantic_function_call_expression_result.top_level_direct_calls
    |> List.hd
  in
  match
    Lowering.lower_top_level ~instruction_id:(instruction_id 0)
      ~value_id:(value_id 0) ~target:first_target
      (top_level_result replay_results replay_call)
  with
  | Error [ error ] ->
      Alcotest.(check string)
        "replayed top-level result code" "HCIRL0004" error.Sequence.code
  | Error _ -> Alcotest.fail "expected one replay ownership error"
  | Ok _ -> Alcotest.fail "expected replayed top-level evidence to fail"

let unsupported_top_level_call_boundaries () =
  let expect source =
    let prepared =
      Test_top_level_expression_result.prepared ~mode:Preprocessor.Jit
        ~path:"ir-top-level-direct-call-unsupported.HC" source
    in
    let results, records = analyze_top_level prepared in
    let call =
      results |> Semantic_function_call_expression_result.top_level_direct_calls
      |> List.hd
    in
    match lower_top_level records results call with
    | Ok Lowering.Unsupported_call -> ()
    | Ok (Lowering.Lowered _) ->
        Alcotest.fail "expected an unsupported top-level call"
    | Error errors ->
        errors
        |> List.map (fun error -> error.Sequence.message)
        |> String.concat ", " |> Alcotest.fail
  in
  expect "_intern 42 I64 Internal();Internal();";
  expect "I64 Defaulted(I64 value=1);Defaulted();"

let tests =
  [
    Alcotest.test_case "complete direct-call sequences" `Quick
      direct_calls_emit_complete_sequences;
    Alcotest.test_case "checked cleanup opcode" `Quick
      cleanup_opcode_follows_checked_flags;
    Alcotest.test_case "one provided argument sequence" `Quick
      one_argument_sequences_preserve_expression_ir;
    Alcotest.test_case "one argument cleanup and dump" `Quick
      one_argument_cleanup_and_dump;
    Alcotest.test_case "multiple provided argument order" `Quick
      multiple_arguments_preserve_order;
    Alcotest.test_case "empty variadic hidden count" `Quick
      empty_variadic_tail_emits_hidden_count;
    Alcotest.test_case "fixed arguments before variadic count" `Quick
      fixed_arguments_precede_hidden_variadic_count;
    Alcotest.test_case "one supplied variadic argument" `Quick
      one_variadic_argument_follows_hidden_count;
    Alcotest.test_case "multiple supplied variadic arguments" `Quick
      multiple_variadic_arguments_preserve_order;
    Alcotest.test_case "extern and import call opcodes" `Quick
      extern_and_import_calls_select_checked_opcodes;
    Alcotest.test_case "unsupported argument expression" `Quick
      unsupported_argument_expression_returns_no_ir;
    Alcotest.test_case "deterministic direct-call dump" `Quick
      deterministic_dump;
    Alcotest.test_case "unsupported direct-call boundaries" `Quick
      unsupported_call_boundaries;
    Alcotest.test_case "inconsistent and exhausted inputs" `Quick
      inconsistent_and_exhausted_inputs_fail_without_ir;
    Alcotest.test_case "top-level complete call composition" `Quick
      top_level_calls_reuse_complete_composition;
    Alcotest.test_case "top-level exact call ownership" `Quick
      top_level_call_ownership_is_exact;
    Alcotest.test_case "unsupported top-level call boundaries" `Quick
      unsupported_top_level_call_boundaries;
  ]
