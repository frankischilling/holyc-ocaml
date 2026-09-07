open Holyc_lib
module Frame = Semantic_function_frame_layout
module Lowering = Ir_frame_address_lowering
module Result = Semantic_function_call_expression_result
module Sequence = Ir_instruction_sequence
module Type = Semantic_type

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let checked_policy = function
  | Ok value -> value
  | Error error ->
      error |> Semantic_function_call_conversion_policy.error_to_string
      |> Alcotest.fail

let checked_results = function
  | Ok value -> value
  | Error error -> error |> Result.error_to_string |> Alcotest.fail

let checked_id = function
  | Ok value -> value
  | Error (error : Sequence.error) ->
      Alcotest.fail (error.code ^ ": " ^ error.message)

let instruction_id value = Sequence.Instruction_id.of_int value |> checked_id
let value_id value = Sequence.Value_id.of_int value |> checked_id

let analyze ?(compilation_mode = Preprocessor.Jit) source =
  let prepared =
    Test_function_frame_layout.prepare ~mode:compilation_mode
      ~path:"ir-frame-address.HC" source
  in
  let members =
    Holyc_lib.index_aggregate_members prepared.session
      ~declarations:prepared.declarations ~headers:prepared.aggregate_headers
      ~members:prepared.aggregate_members ~layouts:prepared.aggregate_layouts
    |> checked
  in
  let global_types =
    Holyc_lib.resolve_global_types prepared.session
      ~declarations:prepared.declarations ~aggregates:prepared.aggregates
      prepared.ast
    |> checked
  in
  let functions =
    Holyc_lib.resolve_function_identities prepared.session
      ~declarations:prepared.declarations ~functions:prepared.function_types
      ~compilation_mode prepared.ast
    |> checked
  in
  let globals =
    Holyc_lib.resolve_global_records prepared.session
      ~declarations:prepared.declarations ~globals:global_types
      ~compilation_mode prepared.ast
    |> checked
  in
  let expressions =
    Holyc_lib.resolve_function_expressions prepared.session
      ~declarations:prepared.declarations ~functions:prepared.functions
      ~local_types:prepared.local_types ~bindings:prepared.bindings prepared.ast
    |> checked
  in
  let module_expressions =
    Holyc_lib.resolve_module_expressions prepared.session
      ~declarations:prepared.declarations ~aggregates:prepared.aggregates
      ~functions ~globals ~expressions
    |> checked
  in
  let calls =
    Holyc_lib.resolve_function_calls prepared.session
      ~declarations:prepared.declarations ~members
      ~function_types:prepared.function_types ~local_types:prepared.local_types
      ~global_types ~functions ~expressions:module_expressions prepared.ast
    |> checked
  in
  let policies =
    Holyc_lib.analyze_function_call_conversions prepared.session
      ~declarations:prepared.declarations ~headers:prepared.aggregate_headers
      ~calls
    |> checked_policy
  in
  let results =
    Holyc_lib.type_function_call_expressions prepared.session ~members ~policies
    |> checked_results
  in
  (Test_function_frame_layout.layout prepared, results)

let function_named results name =
  Result.functions results
  |> List.find (fun function_ ->
      function_ |> Result.function_symbol |> Semantic_symbol.name
      |> String.equal name)

let return_value function_ =
  function_ |> Result.function_returns |> List.hd |> Result.return_value
  |> function
  | Some result -> result
  | None -> Alcotest.fail "expected a valued return"

let expression_values function_ =
  function_ |> Result.function_expression_statements
  |> List.map Result.expression_statement_value

let frame_for frames function_ =
  match Frame.find_function frames (Result.function_symbol function_) with
  | Some frame -> frame
  | None -> Alcotest.fail "expected the function frame"

let location_for_result frame result =
  let source = Result.result_source result in
  match Semantic_function_call_resolution.argument_expression_kind source with
  | Semantic_function_call_resolution.Bound_identifier_expression identifier
    -> (
      let occurrence =
        Semantic_function_call_resolution.bound_identifier_occurrence identifier
      in
      match
        Semantic_module_expression_binding.occurrence_resolution occurrence
      with
      | Semantic_module_expression_binding.Local_binding binding -> (
          match Frame.find_binding_location frame binding with
          | Some location -> location
          | None -> Alcotest.fail "expected the exact frame location")
      | Semantic_module_expression_binding.Module_binding _
      | Semantic_module_expression_binding.Outer_candidate ->
          Alcotest.fail "expected a function-local binding")
  | _ -> Alcotest.fail "expected a bound identifier result"

let require_lowered = function
  | Ok (Lowering.Lowered lowered) -> lowered
  | Ok Lowering.Unsupported_location ->
      Alcotest.fail "expected a frame-backed identifier address"
  | Error errors ->
      errors
      |> List.map (fun (error : Sequence.error) -> error.message)
      |> String.concat ", " |> Alcotest.fail

let result_id description =
  description.Sequence.result
  |> Option.map (fun result ->
      Sequence.Value_id.to_int result.Sequence.value_id)

let operand_ids description =
  description.Sequence.operands |> List.map Sequence.Value_id.to_int

let integer_payload description =
  match description.Sequence.payload with
  | None -> None
  | Some (Sequence.Integer value) -> Some value
  | Some
      ( Sequence.Float_bits _
      | Sequence.Bytes _
      | Sequence.Symbol _
      | Sequence.Block _
      | Sequence.Block_targets _ ) ->
      Alcotest.fail "expected only an integer displacement payload"

let source_span result =
  match Result.result_origin result with
  | Semantic_symbol.Source_location location -> location.span
  | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
      Alcotest.fail "expected a complete identifier span"

let lower_result ?(instruction = 0) ?(value = 0) frame result =
  Lowering.lower
    ~instruction_id:(instruction_id instruction)
    ~value_id:(value_id value) ~frame result
  |> require_lowered

let lowered_displacement lowered =
  let descriptions =
    lowered |> Lowering.sequence |> Sequence.instructions
    |> List.map Sequence.description
  in
  match (List.nth descriptions 1).Sequence.payload with
  | Some (Sequence.Integer displacement) -> displacement
  | Some
      ( Sequence.Float_bits _
      | Sequence.Bytes _
      | Sequence.Symbol _
      | Sequence.Block _
      | Sequence.Block_targets _ )
  | None -> Alcotest.fail "expected a signed frame displacement"

let named_parameter_address_uses_positive_rbp_displacement () =
  let frames, results = analyze "I64 Read(I64 value){return value;}" in
  let function_ = function_named results "Read" in
  let root = return_value function_ in
  let frame = frame_for frames function_ in
  let lowered =
    Lowering.lower ~instruction_id:(instruction_id 10) ~value_id:(value_id 20)
      ~frame root
    |> require_lowered
  in
  let descriptions =
    lowered |> Lowering.sequence |> Sequence.instructions
    |> List.map Sequence.description
  in
  Alcotest.(check (list string))
    "RBP-relative address sequence"
    [ "IC_RBP"; "IC_IMM_I64"; "IC_ADD" ]
    (descriptions
    |> List.map (fun description ->
        description.Sequence.opcode |> Ir_opcode.to_source_name));
  Alcotest.(check (list int))
    "instruction identities" [ 10; 11; 12 ]
    (descriptions
    |> List.map (fun description ->
        description.Sequence.instruction_id |> Sequence.Instruction_id.to_int));
  Alcotest.(check (list (option int)))
    "result identities"
    [ Some 20; Some 21; Some 22 ]
    (List.map result_id descriptions);
  Alcotest.(check (list (list int)))
    "address operands"
    [ []; []; [ 20; 21 ] ]
    (List.map operand_ids descriptions);
  Alcotest.(check (list (option int64)))
    "parameter displacement" [ None; Some 16L; None ]
    (List.map integer_payload descriptions);
  Alcotest.(check (list int64))
    "address flags" [ 0L; 0L; 0L ]
    (List.map (fun description -> description.Sequence.flags) descriptions);
  let value_type =
    match Result.result_type root with
    | Some value_type -> value_type
    | None -> Alcotest.fail "expected a checked identifier type"
  in
  let address_type = Type.pointer_to value_type |> checked in
  Alcotest.(check bool)
    "every producer carries the pointer type" true
    (List.for_all
       (fun description -> description.Sequence.target_type = Some address_type)
       descriptions);
  Alcotest.(check bool)
    "the lowered result keeps the address type" true
    (Lowering.result_type lowered = address_type);
  let span = source_span root in
  Alcotest.(check (list (option (pair int int))))
    "every producer keeps the identifier span"
    (List.init 3 (fun _ -> Some (span.start, span.stop)))
    (descriptions
    |> List.map (fun description ->
        Option.map
          (fun (span : Span.t) -> (span.start, span.stop))
          description.Sequence.span));
  Alcotest.(check int)
    "address result identity" 22
    (Lowering.result_value lowered |> Sequence.Value_id.to_int);
  Alcotest.(check (pair int int))
    "identity cursors advance by three" (13, 23)
    ( Lowering.next_instruction_id lowered |> Sequence.Instruction_id.to_int,
      Lowering.next_value_id lowered |> Sequence.Value_id.to_int );
  let dump = Lowering.human lowered in
  let prefix =
    Printf.sprintf "holyc-ir-frame-address-v1 reference=%s\n"
      Lowering.reference_commit
  in
  Alcotest.(check bool)
    "the dump has its own versioned schema" true
    (String.starts_with ~prefix dump);
  let repeated =
    Lowering.lower ~instruction_id:(instruction_id 10) ~value_id:(value_id 20)
      ~frame root
    |> require_lowered |> Lowering.human
  in
  Alcotest.(check string) "the dump is deterministic" dump repeated;
  let aot_frames, aot_results =
    analyze ~compilation_mode:Preprocessor.Aot
      "I64 Read(I64 value){return value;}"
  in
  let aot_function = function_named aot_results "Read" in
  let aot_root = return_value aot_function in
  let aot_frame = frame_for aot_frames aot_function in
  let aot_dump =
    Lowering.lower ~instruction_id:(instruction_id 10) ~value_id:(value_id 20)
      ~frame:aot_frame aot_root
    |> require_lowered |> Lowering.human
  in
  Alcotest.(check string)
    "JIT and AOT keep the same address fragment" dump aot_dump

let positive_parameters_and_variadic_slots_keep_source_offsets () =
  let frames, results =
    analyze "U0 Slots(I8 first,I64 second,...){first;second;argc;argv;}"
  in
  let function_ = function_named results "Slots" in
  let frame = frame_for frames function_ in
  let lowered =
    function_ |> expression_values |> List.map (lower_result frame)
  in
  Alcotest.(check (list int64))
    "fixed and synthetic parameter displacements" [ 16L; 24L; 32L; 40L ]
    (List.map lowered_displacement lowered);
  Alcotest.(check (list int))
    "each location emits only the address triple" [ 3; 3; 3; 3 ]
    (lowered
    |> List.map (fun lowered -> lowered |> Lowering.sequence |> Sequence.length)
    );
  Alcotest.(check string)
    "argv keeps one address layer over internal I64" "internal:I64*"
    (List.nth lowered 3 |> Lowering.result_type |> Sequence.type_name);
  let argv = List.nth (expression_values function_) 3 in
  let argv_location = location_for_result frame argv in
  Alcotest.(check (list int64))
    "argv keeps its placeholder extent" [ 127L ]
    (argv_location |> Frame.location_dimensions
    |> List.map Frame.dimension_value)

let shaped_automatic_locals_keep_signed_frame_locations () =
  let source =
    "class Box {I16 field;};U0 Locals(){I8 byte;I32 word;I64 array[2];Box \
     object;I0 zero;I8 *pointer;byte;word;array;object;zero;pointer;}U0 \
     Empty(){I0 empty;empty;}"
  in
  let frames, results = analyze source in
  let function_ = function_named results "Locals" in
  let frame = frame_for frames function_ in
  let values = expression_values function_ in
  let lowered = List.map (lower_result frame) values in
  Alcotest.(check (list int64))
    "automatic local displacements"
    [ -1L; -8L; -24L; -26L; -26L; -40L ]
    (List.map lowered_displacement lowered);
  Alcotest.(check bool)
    "scalar, array, aggregate, and zero-sized locations add one address layer"
    true
    (List.for_all2
       (fun result lowered ->
         let location = location_for_result frame result in
         let expected =
           Frame.location_checked_type location |> Type.pointer_to |> checked
         in
         Lowering.result_type lowered = expected
         && Sequence.length (Lowering.sequence lowered) = 3)
       values lowered);
  let empty = function_named results "Empty" in
  let empty_frame = frame_for frames empty in
  let empty_value = empty |> expression_values |> List.hd in
  Alcotest.(check int64)
    "zero-sized first local keeps a zero displacement" 0L
    (lower_result empty_frame empty_value |> lowered_displacement);
  let replay_frames, replay_results = analyze source in
  let replay_function = function_named replay_results "Locals" in
  let replay_frame = frame_for replay_frames replay_function in
  let object_type =
    List.nth values 3 |> location_for_result frame
    |> Frame.location_checked_type
  in
  let result_object_type =
    match Result.result_type (List.nth values 3) with
    | Some type_ -> type_
    | None -> Alcotest.fail "expected a checked aggregate result type"
  in
  Alcotest.(check bool)
    "types over the same canonical aggregate compare equal" true
    (Type.equal object_type result_object_type);
  let replay_values = expression_values replay_function in
  let replay_object_type =
    List.nth replay_values 3
    |> location_for_result replay_frame
    |> Frame.location_checked_type
  in
  let aggregate_id type_ =
    match Type.base type_ with
    | Type.Aggregate symbol ->
        symbol |> Semantic_symbol.id |> Semantic_symbol.Id.to_int
    | Type.Primitive _ -> Alcotest.fail "expected an aggregate object type"
  in
  Alcotest.(check int)
    "separate sessions reuse the table-local aggregate ID"
    (aggregate_id object_type)
    (aggregate_id replay_object_type);
  Alcotest.(check bool)
    "same-ID aggregates from separate sessions remain distinct" false
    (Type.equal object_type replay_object_type)

let unsupported_locations_never_expose_an_address_fragment () =
  let frames, results =
    analyze
      "I64 global;I64 Callee(){return 0;}U0 Unsupported(){static I64 \
       stored;I64 (*callback)();stored;callback;global;&Callee;7;}"
  in
  let function_ = function_named results "Unsupported" in
  let frame = frame_for frames function_ in
  let values = expression_values function_ in
  let outer_prepared =
    Test_function_call_conversion_policy.prepare
      ~path:"ir-frame-address-outer.HC"
      "I64 Template;I64 Outer(){return outside;}"
  in
  let outer_entry =
    Test_function_call_expression_result.make_outer_global_entry outer_prepared
      ~entry_index:0 ~name:"outside"
      ~metadata:
        (Test_function_call_expression_result.outer_global_metadata
           outer_prepared "Template")
      ()
  in
  let _, outer_results =
    Test_function_call_expression_result.type_with_outer outer_prepared
      [ outer_entry ]
  in
  let outer = function_named outer_results "Outer" |> return_value in
  Alcotest.(check bool)
    "the unresolved source retains its selected outer binding" true
    (Option.is_some (Result.result_outer_binding outer));
  let direct_function =
    match Result.result_operand (List.nth values 3) with
    | Some operand -> operand
    | None -> Alcotest.fail "expected the retained direct-function operand"
  in
  let candidates =
    [
      List.nth values 0;
      List.nth values 1;
      List.nth values 2;
      List.nth values 3;
      direct_function;
      List.nth values 4;
      outer;
    ]
  in
  Alcotest.(check (list bool))
    "static, callback, module, direct-function, literal, and outer paths are \
     explicit"
    [ true; true; true; true; true; true; true ]
    (candidates
    |> List.map (fun result ->
        match
          Lowering.lower ~instruction_id:(instruction_id 4)
            ~value_id:(value_id 7) ~frame result
        with
        | Ok Lowering.Unsupported_location -> true
        | Ok (Lowering.Lowered _) | Error _ -> false))

let same_spelling_bindings_cannot_exchange_function_frames () =
  let frames, results =
    analyze "U0 First(){I8 value;value;}U0 Second(){I64 value;value;}"
  in
  let first = function_named results "First" in
  let second = function_named results "Second" in
  let first_value = first |> expression_values |> List.hd in
  let second_value = second |> expression_values |> List.hd in
  let first_frame = frame_for frames first in
  let second_frame = frame_for frames second in
  Alcotest.(check (pair int64 int64))
    "each exact binding keeps its own displacement" (-1L, -8L)
    ( lower_result first_frame first_value |> lowered_displacement,
      lower_result second_frame second_value |> lowered_displacement );
  let check_cross label frame result =
    match
      Lowering.lower ~instruction_id:(instruction_id 0) ~value_id:(value_id 0)
        ~frame result
    with
    | Error [ error ] ->
        Alcotest.(check string) label "HCIRL0004" error.Sequence.code;
        Alcotest.(check bool)
          (label ^ " span") true
          (Option.is_some error.Sequence.span)
    | Error _ -> Alcotest.fail (label ^ " returned several errors")
    | Ok Lowering.Unsupported_location | Ok (Lowering.Lowered _) ->
        Alcotest.fail (label ^ " accepted the wrong function frame")
  in
  check_cross "first result in second frame" second_frame first_value;
  check_cross "second result in first frame" first_frame second_value

let pointer_and_identity_limits_fail_without_partial_ir () =
  let frames, results =
    analyze "U0 Limits(){I64 value;I64 ****deep;value;deep;}"
  in
  let function_ = function_named results "Limits" in
  let frame = frame_for frames function_ in
  let values = expression_values function_ in
  let ordinary = List.nth values 0 in
  let maximum_pointer = List.nth values 1 in
  (match
     Lowering.lower ~instruction_id:(instruction_id 0) ~value_id:(value_id 0)
       ~frame maximum_pointer
   with
  | Ok Lowering.Unsupported_location -> ()
  | Ok (Lowering.Lowered _) | Error _ ->
      Alcotest.fail "maximum pointer depth must stay unsupported");
  let near_limit = Int.max_int - 2 in
  let check_exhaustion label instruction value =
    match
      Lowering.lower
        ~instruction_id:(instruction_id instruction)
        ~value_id:(value_id value) ~frame ordinary
    with
    | Error [ error ] ->
        Alcotest.(check string) label "HCIRL0005" error.Sequence.code;
        Alcotest.(check bool)
          (label ^ " span") true
          (Option.is_some error.Sequence.span)
    | Error _ -> Alcotest.fail (label ^ " returned several errors")
    | Ok Lowering.Unsupported_location | Ok (Lowering.Lowered _) ->
        Alcotest.fail (label ^ " exposed an address fragment")
  in
  check_exhaustion "instruction identity exhaustion" near_limit 0;
  check_exhaustion "value identity exhaustion" 0 near_limit

let tests =
  [
    Alcotest.test_case "named parameter RBP-relative address" `Quick
      named_parameter_address_uses_positive_rbp_displacement;
    Alcotest.test_case "positive parameter and variadic slots" `Quick
      positive_parameters_and_variadic_slots_keep_source_offsets;
    Alcotest.test_case "shaped automatic local addresses" `Quick
      shaped_automatic_locals_keep_signed_frame_locations;
    Alcotest.test_case "unsupported location boundaries" `Quick
      unsupported_locations_never_expose_an_address_fragment;
    Alcotest.test_case "exact binding and frame identity" `Quick
      same_spelling_bindings_cannot_exchange_function_frames;
    Alcotest.test_case "pointer and identity limits" `Quick
      pointer_and_identity_limits_fail_without_partial_ir;
  ]
