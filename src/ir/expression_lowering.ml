module Sequence = Instruction_sequence
module Literal = Literal_lowering
module Semantic_result = Sema.Function_call_expression_result
module Semantic_source = Sema.Function_call_resolution
module Function_resolution = Sema.Function_resolution
module Function_type_resolution = Sema.Function_type_resolution
module Module_binding = Sema.Module_expression_binding
module Type = Sema.Type
module Type_reference = Sema.Type_reference
module Int_map = Map.Make (Int)

type t = {
  sequence_ : Sequence.t;
  result_value_ : Sequence.Value_id.t;
  result_type_ : Type.t;
  next_instruction_id_ : Sequence.Instruction_id.t;
  next_value_id_ : Sequence.Value_id.t;
}

type lowering_result = Lowered of t | Unsupported_expression

type call_lowerer =
  instruction_id:Sequence.Instruction_id.t ->
  value_id:Sequence.Value_id.t ->
  Semantic_result.expression_result ->
  (Sequence.t option, Sequence.error list) result

type checked_type = Checked_type of Type.t | Unsupported_type
type result_conversion = Keep_result | Result_to_f64 | Result_to_int

type checked_constant =
  | Deferred_constant
  | Checked_constant of Common.Span.t * Type.t * int64

type binary_validation =
  | Unsupported_binary
  | Supported_binary of {
      left_conversion : result_conversion;
      right_conversion : result_conversion;
      operation_flags : int64;
    }

type cancellation =
  | No_cancellation
  | Canceled_dereference of Semantic_result.expression_result

type storage_address =
  | Frame_slot of Frame_address_lowering.prepared_address
  | Global_slot of Global_address_lowering.prepared_address

type assignment_address =
  | Direct_address of storage_address
  | Indirect_address_value of Semantic_result.expression_result
  | Pointer_base of Semantic_result.expression_result
  | Indexed_address of {
      base : Semantic_result.expression_result;
      address : assignment_address;
      index : Semantic_result.expression_result;
      stride : int64;
      pointer_type : Type.t;
      span : Common.Span.t;
    }

type index_step = {
  indexed_result : Semantic_result.expression_result;
  indexed_base : Semantic_result.expression_result;
  index_value : Semantic_result.expression_result;
  index_stride : int64;
  index_type : Type.t;
  index_span : Common.Span.t;
}

type plan_node =
  | Index_stride of index_step
  | Index_address of index_step
  | Materialize_array of {
      result : Semantic_result.expression_result;
      operand : Semantic_result.expression_result;
      pointer_type : Type.t;
      span : Common.Span.t;
    }
  | Call of {
      result : Semantic_result.expression_result;
      conversion : result_conversion;
    }
  | Storage_address of {
      result : Semantic_result.expression_result;
      address : storage_address;
    }
  | Indirect_address of {
      result : Semantic_result.expression_result;
      pointer : Semantic_result.expression_result;
    }
  | Storage_load of {
      result : Semantic_result.expression_result;
      address : storage_address;
      result_type : Type.t;
      span : Common.Span.t;
      conversion : result_conversion;
    }
  | Literal of {
      result : Semantic_result.expression_result;
      conversion : result_conversion;
    }
  | Current_position of {
      result : Semantic_result.expression_result;
      span : Common.Span.t;
      result_type : Type.t;
      conversion : result_conversion;
    }
  | Integer_constant of {
      result : Semantic_result.expression_result;
      span : Common.Span.t;
      result_type : Type.t;
      value : int64;
      conversion : result_conversion;
    }
  | Direct_function_address of {
      result : Semantic_result.expression_result;
      span : Common.Span.t;
      result_type : Type.t;
      symbol : Sema.Symbol.t;
      path : Semantic_source.direct_function_address_path;
      conversion : result_conversion;
    }
  | Alias of {
      result : Semantic_result.expression_result;
      operand : Semantic_result.expression_result;
    }
  | Unary of {
      result : Semantic_result.expression_result;
      opcode : Opcode.t;
      span : Common.Span.t;
      operand : Semantic_result.expression_result;
      conversion : result_conversion;
    }
  | Cast of {
      result : Semantic_result.expression_result;
      span : Common.Span.t;
      operand : Semantic_result.expression_result;
      was_parenthesized : bool;
      conversion : result_conversion;
    }
  | Binary of {
      result : Semantic_result.expression_result;
      opcode : Opcode.t;
      span : Common.Span.t;
      left : Semantic_result.expression_result;
      right : Semantic_result.expression_result;
      conversion : result_conversion;
      operation_flags : int64;
    }
  | Chain_link of {
      result : Semantic_result.expression_result;
      previous : Semantic_result.expression_result;
      middle : Semantic_result.expression_result;
      right : Semantic_result.expression_result;
      opcode : Opcode.t;
      span : Common.Span.t;
      conversion : result_conversion;
    }

type task =
  | Emit_index_stride of index_step
  | Finish_index_address of index_step
  | Finish_materialize_array of {
      result : Semantic_result.expression_result;
      operand : Semantic_result.expression_result;
      pointer_type : Type.t;
      span : Common.Span.t;
    }
  | Visit of {
      result : Semantic_result.expression_result;
      conversion : result_conversion;
    }
  | Finish_indirect_address of {
      result : Semantic_result.expression_result;
      pointer : Semantic_result.expression_result;
    }
  | Finish_alias of {
      result : Semantic_result.expression_result;
      operand : Semantic_result.expression_result;
    }
  | Finish_unary of {
      result : Semantic_result.expression_result;
      opcode : Opcode.t;
      span : Common.Span.t;
      operand : Semantic_result.expression_result;
      conversion : result_conversion;
    }
  | Finish_cast of {
      result : Semantic_result.expression_result;
      span : Common.Span.t;
      operand : Semantic_result.expression_result;
      was_parenthesized : bool;
      conversion : result_conversion;
    }
  | Finish_binary of {
      result : Semantic_result.expression_result;
      opcode : Opcode.t;
      span : Common.Span.t;
      left : Semantic_result.expression_result;
      right : Semantic_result.expression_result;
      conversion : result_conversion;
      operation_flags : int64;
    }
  | Finish_chain_link of {
      result : Semantic_result.expression_result;
      previous : Semantic_result.expression_result;
      middle : Semantic_result.expression_result;
      right : Semantic_result.expression_result;
      opcode : Opcode.t;
      span : Common.Span.t;
      conversion : result_conversion;
    }

type planned = Planned of plan_node list | Unsupported_plan

type lowered_node = {
  lowered_value : Sequence.Value_id.t;
  lowered_type : Type.t;
}

type allocator = { mutable instruction : int; mutable value : int }

let reference_commit = Opcode.reference_commit
let result_to_f64_flag = 0x000000001L
let result_to_int_flag = 0x000000002L
let use_f64_flag = 0x000000040L

let conversion_flags = function
  | Keep_result -> 0L
  | Result_to_f64 -> result_to_f64_flag
  | Result_to_int -> result_to_int_flag

let result_span result =
  match Semantic_result.result_origin result with
  | Sema.Symbol.Source_location location -> Some location.span
  | Sema.Symbol.Pinned_source _ | Sema.Symbol.Synthesized _ -> None

let lowering_error ?span code message =
  { Sequence.code; message; instruction_id = None; span }

let metadata_error ?span message = lowering_error ?span "HCIRL0004" message

let checked_integer_type result =
  match Semantic_result.result_type result with
  | None ->
      Error
        (metadata_error ?span:(result_span result)
           "typed semantic expression does not have a checked result type")
  | Some type_ -> (
      match
        ( Semantic_result.result_class result,
          Type.pointer_depth type_,
          Type.base type_ )
      with
      | Semantic_result.Integer_result, 0, Type.Primitive (_, primitive)
        when (Sema.Primitive_type.info primitive).category
             <> Sema.Primitive_type.Floating
             && not (Sema.Primitive_type.is_zero_sized primitive) ->
          Ok (Checked_type type_)
      | Semantic_result.Integer_result, _, (Type.Primitive _ | Type.Aggregate _)
      | ( (Semantic_result.F64_result | Semantic_result.Unresolved_actual_class),
          _,
          (Type.Primitive _ | Type.Aggregate _) ) -> Ok Unsupported_type)

let checked_frame_integer result =
  match checked_integer_type result with
  | Ok (Checked_type type_) when Semantic_result.result_array_rank result = 0 ->
      if Option.is_some (Integer_scalar_storage.of_type type_) then
        Ok (Checked_type type_)
      else Ok Unsupported_type
  | Ok (Checked_type _) -> Ok Unsupported_type
  | other -> other

let checked_frame_word result =
  match checked_frame_integer result with
  | Ok (Checked_type type_) -> (
      match Type.base type_ with
      | Type.Primitive (_, (Sema.Primitive_type.I64 | U64)) ->
          Ok (Checked_type type_)
      | _ -> Ok Unsupported_type)
  | other -> other

let scalar_pointer_type type_ =
  match Type.dereference type_ with
  | Ok pointee -> Option.is_some (Integer_scalar_storage.of_type pointee)
  | Error _ -> false

let storage_element_size type_ =
  Option.map
    (fun scalar -> Int64.of_int (Integer_scalar_storage.byte_size scalar))
    (Integer_scalar_storage.of_type type_)

let pointer_element_size type_ =
  match Type.dereference type_ with
  | Ok pointee -> storage_element_size pointee
  | Error _ -> None

let checked_frame_value result =
  match Semantic_result.result_type result with
  | Some type_ when Semantic_result.result_is_array_address result -> (
      match Type.pointer_to type_ with
      | Ok pointer when scalar_pointer_type pointer -> Ok (Checked_type pointer)
      | _ -> Ok Unsupported_type)
  | Some type_
    when scalar_pointer_type type_
         && Semantic_result.result_array_rank result = 0
         && Semantic_result.result_class result = Semantic_result.Integer_result
    -> Ok (Checked_type type_)
  | _ -> checked_frame_integer result

let checked_frame_scalar result =
  match Semantic_result.result_category result with
  | Semantic_result.Object_value | Semantic_result.Lvalue ->
      checked_frame_value result
  | _ -> Ok Unsupported_type

let checked_f64_type result =
  match Semantic_result.result_type result with
  | None ->
      Error
        (metadata_error ?span:(result_span result)
           "typed semantic expression does not have a checked result type")
  | Some type_ -> (
      match
        ( Semantic_result.result_class result,
          Type.pointer_depth type_,
          Type.base type_ )
      with
      | ( Semantic_result.F64_result,
          0,
          Type.Primitive
            ( (Type.Public_spelling | Type.Internal_storage),
              Sema.Primitive_type.F64 ) ) -> Ok (Checked_type type_)
      | ( ( Semantic_result.Integer_result
          | Semantic_result.F64_result
          | Semantic_result.Unresolved_actual_class ),
          _,
          (Type.Primitive _ | Type.Aggregate _) ) -> Ok Unsupported_type)

let checked_string_type result =
  let invalid () =
    Error
      (metadata_error ?span:(result_span result)
         "typed semantic string literal does not retain the checked internal \
          U8 pointer address result")
  in
  match
    ( Semantic_result.result_type result,
      Semantic_result.result_class result,
      Semantic_result.result_category result,
      Semantic_result.result_array_rank result )
  with
  | Some type_, Semantic_result.Integer_result, Semantic_result.Address_value, 0
    -> (
      match (Type.pointer_depth type_, Type.base type_) with
      | 1, Type.Primitive (Type.Internal_storage, Sema.Primitive_type.U8) ->
          Ok (Checked_type type_)
      | _, (Type.Primitive _ | Type.Aggregate _) -> invalid ())
  | ( (None | Some _),
      ( Semantic_result.Integer_result
      | Semantic_result.F64_result
      | Semantic_result.Unresolved_actual_class ),
      ( Semantic_result.Object_value
      | Semantic_result.Address_value
      | Semantic_result.Array_value
      | Semantic_result.Callback_value
      | Semantic_result.Function_value
      | Semantic_result.Offset_value
      | Semantic_result.Lvalue
      | Semantic_result.Unavailable ),
      _ ) -> invalid ()

let checked_numeric_type result =
  match checked_integer_type result with
  | Error _ as error -> error
  | Ok (Checked_type _) as checked -> checked
  | Ok Unsupported_type -> checked_f64_type result

let requested_conversion result =
  match Semantic_result.result_intrinsic_conversion result with
  | Semantic_result.No_intrinsic_conversion -> Keep_result
  | Semantic_result.Result_to_f64 -> Result_to_f64
  | Semantic_result.Result_to_int -> Result_to_int

let validate_conversion result = function
  | Keep_result -> Ok true
  | Result_to_f64 -> (
      match checked_integer_type result with
      | Error _ as error -> error
      | Ok (Checked_type _) -> Ok true
      | Ok Unsupported_type -> Ok false)
  | Result_to_int -> (
      match checked_f64_type result with
      | Error _ as error -> error
      | Ok (Checked_type _) -> Ok true
      | Ok Unsupported_type -> Ok false)

let checked_current_position result =
  let invalid ?span () =
    Error
      (metadata_error ?span
         "current-position expression does not retain the checked RT_PTR \
          address result")
  in
  match result_span result with
  | None ->
      Error
        (metadata_error
           "current-position expression does not have a source location")
  | Some span -> (
      match
        ( Semantic_result.result_type result,
          Semantic_result.result_class result,
          Semantic_result.result_category result,
          Semantic_result.result_array_rank result )
      with
      | ( Some result_type,
          Semantic_result.Integer_result,
          Semantic_result.Address_value,
          0 ) -> (
          match (Type.base result_type, Type.pointer_depth result_type) with
          | Type.Primitive (Type.Internal_storage, primitive), 0
            when Sema.Primitive_type.equal primitive Sema.Primitive_type.I64 ->
              Ok (span, result_type)
          | Type.Primitive _, _ | Type.Aggregate _, _ -> invalid ~span ())
      | ( (None | Some _),
          ( Semantic_result.Integer_result
          | Semantic_result.F64_result
          | Semantic_result.Unresolved_actual_class ),
          ( Semantic_result.Object_value
          | Semantic_result.Address_value
          | Semantic_result.Array_value
          | Semantic_result.Callback_value
          | Semantic_result.Function_value
          | Semantic_result.Offset_value
          | Semantic_result.Lvalue
          | Semantic_result.Unavailable ),
          _ ) -> invalid ~span ())

let checked_internal_i64_constant result ~description known_value =
  let invalid ?span () =
    Error
      (metadata_error ?span
         (description
        ^ " does not retain the checked internal I64 object result"))
  in
  match known_value with
  | None -> Ok Deferred_constant
  | Some known_value -> (
      match result_span result with
      | None ->
          Error
            (metadata_error (description ^ " does not have a source location"))
      | Some span -> (
          match
            ( Semantic_result.result_type result,
              Semantic_result.result_class result,
              Semantic_result.result_category result,
              Semantic_result.result_array_rank result )
          with
          | ( Some result_type,
              Semantic_result.Integer_result,
              Semantic_result.Object_value,
              0 ) -> (
              match (Type.base result_type, Type.pointer_depth result_type) with
              | Type.Primitive (Type.Internal_storage, primitive), 0
                when Sema.Primitive_type.equal primitive Sema.Primitive_type.I64
                -> Ok (Checked_constant (span, result_type, known_value))
              | Type.Primitive _, _ | Type.Aggregate _, _ -> invalid ~span ())
          | ( (None | Some _),
              ( Semantic_result.Integer_result
              | Semantic_result.F64_result
              | Semantic_result.Unresolved_actual_class ),
              ( Semantic_result.Object_value
              | Semantic_result.Address_value
              | Semantic_result.Array_value
              | Semantic_result.Callback_value
              | Semantic_result.Function_value
              | Semantic_result.Offset_value
              | Semantic_result.Lvalue
              | Semantic_result.Unavailable ),
              _ ) -> invalid ~span ()))

let checked_defined result defined =
  let known_value =
    Semantic_source.defined_known_value defined
    |> Option.map (fun value -> if value then 1L else 0L)
  in
  checked_internal_i64_constant result ~description:"defined expression"
    known_value

let checked_sizeof result sizeof =
  checked_internal_i64_constant result ~description:"sizeof expression"
    (Semantic_source.sizeof_known_value sizeof)

let checked_aggregate_offset result =
  let invalid ?span message =
    Error
      (metadata_error ?span
         ("aggregate offset expression does not retain " ^ message))
  in
  match Semantic_result.result_aggregate_offset_path result with
  | None -> Ok Deferred_constant
  | Some path -> (
      match result_span result with
      | None -> invalid "a source location"
      | Some span -> (
          match
            ( Semantic_result.result_type result,
              Semantic_result.result_class result,
              Semantic_result.result_category result,
              Semantic_result.result_array_rank result )
          with
          | ( Some result_type,
              Semantic_result.Integer_result,
              Semantic_result.Offset_value,
              0 ) -> (
              match (Type.base result_type, Type.pointer_depth result_type) with
              | Type.Primitive (Type.Internal_storage, primitive), 0
                when Sema.Primitive_type.equal primitive Sema.Primitive_type.I64
                -> (
                  let value = Semantic_result.aggregate_offset_value path in
                  match
                    Semantic_result.aggregate_offset_segments path |> List.rev
                  with
                  | [] -> invalid ~span "a resolved member path"
                  | final_segment :: _ ->
                      if
                        Int64.equal value
                          (Semantic_result
                           .aggregate_offset_segment_cumulative_offset
                             final_segment)
                      then Ok (Checked_constant (span, result_type, value))
                      else invalid ~span "its final cumulative member offset")
              | Type.Primitive _, _ | Type.Aggregate _, _ ->
                  invalid ~span "the checked internal I64 offset result")
          | ( (None | Some _),
              ( Semantic_result.Integer_result
              | Semantic_result.F64_result
              | Semantic_result.Unresolved_actual_class ),
              ( Semantic_result.Object_value
              | Semantic_result.Address_value
              | Semantic_result.Array_value
              | Semantic_result.Callback_value
              | Semantic_result.Function_value
              | Semantic_result.Offset_value
              | Semantic_result.Lvalue
              | Semantic_result.Unavailable ),
              _ ) -> invalid ~span "the checked internal I64 offset result"))

let checked_integer_or_pointer_type result =
  match Semantic_result.result_type result with
  | None ->
      Error
        (metadata_error ?span:(result_span result)
           "typed semantic expression does not have a checked result type")
  | Some type_ -> (
      match (Semantic_result.result_class result, Type.base type_) with
      | Semantic_result.Integer_result, Type.Primitive (_, primitive)
        when (Sema.Primitive_type.info primitive).category
             <> Sema.Primitive_type.Floating
             && not (Sema.Primitive_type.is_zero_sized primitive) ->
          Ok (Checked_type type_)
      | Semantic_result.Integer_result, (Type.Primitive _ | Type.Aggregate _)
      | ( (Semantic_result.F64_result | Semantic_result.Unresolved_actual_class),
          (Type.Primitive _ | Type.Aggregate _) ) -> Ok Unsupported_type)

let accepted_binary_opcode = function
  | Opcode.Ic_shl
  | Opcode.Ic_shr
  | Opcode.Ic_mul
  | Opcode.Ic_div
  | Opcode.Ic_mod
  | Opcode.Ic_and
  | Opcode.Ic_or
  | Opcode.Ic_xor
  | Opcode.Ic_add
  | Opcode.Ic_sub
  | Opcode.Ic_equ_equ
  | Opcode.Ic_not_equ
  | Opcode.Ic_less
  | Opcode.Ic_greater_equ
  | Opcode.Ic_greater
  | Opcode.Ic_less_equ
  | Opcode.Ic_and_and
  | Opcode.Ic_or_or
  | Opcode.Ic_xor_xor
  | Opcode.Ic_power -> true
  | _ -> false

let accepted_f64_arithmetic_opcode = function
  | Opcode.Ic_mul
  | Opcode.Ic_div
  | Opcode.Ic_mod
  | Opcode.Ic_add
  | Opcode.Ic_sub -> true
  | _ -> false

let accepted_f64_bitwise_opcode = function
  | Opcode.Ic_and | Opcode.Ic_or | Opcode.Ic_xor -> true
  | _ -> false

let accepted_f64_shift_opcode = function
  | Opcode.Ic_shl | Opcode.Ic_shr -> true
  | _ -> false

let accepted_f64_comparison_opcode = function
  | Opcode.Ic_equ_equ
  | Opcode.Ic_not_equ
  | Opcode.Ic_less
  | Opcode.Ic_greater_equ
  | Opcode.Ic_greater
  | Opcode.Ic_less_equ -> true
  | _ -> false

let is_comparison_result result =
  match
    Semantic_result.result_source result
    |> Semantic_source.argument_expression_kind
  with
  | Semantic_source.Binary_expression binary ->
      accepted_f64_comparison_opcode (Semantic_source.binary_operator binary)
  | _ -> false

let unsigned_integer_type type_ =
  match Type.base type_ with
  | Type.Primitive (_, primitive) ->
      (Sema.Primitive_type.info primitive).raw_is_unsigned
  | Type.Aggregate _ -> false

let accepted_f64_logical_opcode = function
  | Opcode.Ic_and_and | Opcode.Ic_or_or | Opcode.Ic_xor_xor -> true
  | _ -> false

let accepted_prefix = function
  | Semantic_source.Unary_minus ->
      Some (Opcode.Ic_unary_minus, "unary-minus expression")
  | Semantic_source.Logical_not -> Some (Opcode.Ic_not, "logical-not expression")
  | Semantic_source.Bitwise_not ->
      Some (Opcode.Ic_com, "bitwise-complement expression")
  | Semantic_source.Unary_plus
  | Semantic_source.Dereference
  | Semantic_source.Address_of
  | Semantic_source.Pre_increment
  | Semantic_source.Pre_decrement -> None

let same_offset_publication actual expected =
  match (actual, expected) with
  | None, None -> true
  | Some actual, Some expected -> actual == expected
  | None, Some _ | Some _, None -> false

let same_offset_root_query actual expected =
  match (actual, expected) with
  | None, None -> true
  | ( Some (Semantic_source.Module_query actual),
      Some (Semantic_source.Module_query expected) ) -> actual == expected
  | ( Some (Semantic_source.Outer_query actual),
      Some (Semantic_source.Outer_query expected) ) -> actual == expected
  | None, Some _
  | Some _, None
  | Some (Semantic_source.Module_query _), Some (Semantic_source.Outer_query _)
  | Some (Semantic_source.Outer_query _), Some (Semantic_source.Module_query _)
    -> false

let same_offset_bound_target actual expected =
  match (actual, expected) with
  | None, None -> true
  | Some actual, Some expected -> actual == expected
  | None, Some _ | Some _, None -> false

let same_offset_top_level_query actual expected =
  match (actual, expected) with
  | None, None -> true
  | Some actual, Some expected -> actual == expected
  | None, Some _ | Some _, None -> false

let same_offset_member_source actual expected =
  Semantic_source.offset_member_dot_origin actual
  = Semantic_source.offset_member_dot_origin expected
  && String.equal
       (Semantic_source.offset_member_name actual)
       (Semantic_source.offset_member_name expected)
  && Semantic_source.offset_member_name_origin actual
     = Semantic_source.offset_member_name_origin expected
  && Semantic_source.offset_member_origin actual
     = Semantic_source.offset_member_origin expected
  &&
  match Semantic_source.offset_member_lookup expected with
  | None -> Option.is_some (Semantic_source.offset_member_lookup actual)
  | Some expected_lookup -> (
      match Semantic_source.offset_member_lookup actual with
      | Some actual_lookup -> actual_lookup == expected_lookup
      | None -> false)

let same_offset_source actual expected =
  String.equal
    (Semantic_source.offset_keyword_spelling actual)
    (Semantic_source.offset_keyword_spelling expected)
  && Semantic_source.offset_keyword_origin actual
     = Semantic_source.offset_keyword_origin expected
  && Semantic_source.offset_opening_origins actual
     = Semantic_source.offset_opening_origins expected
  && String.equal
       (Semantic_source.offset_target_spelling actual)
       (Semantic_source.offset_target_spelling expected)
  && Semantic_source.offset_target_origin actual
     = Semantic_source.offset_target_origin expected
  && same_offset_publication
       (Semantic_source.offset_publication actual)
       (Semantic_source.offset_publication expected)
  && same_offset_root_query
       (Semantic_source.offset_root_query actual)
       (Semantic_source.offset_root_query expected)
  && same_offset_top_level_query
       (Semantic_source.offset_top_level_query actual)
       (Semantic_source.offset_top_level_query expected)
  && same_offset_bound_target
       (Semantic_source.offset_bound_target actual)
       (Semantic_source.offset_bound_target expected)
  && List.compare_lengths
       (Semantic_source.offset_members actual)
       (Semantic_source.offset_members expected)
     = 0
  && List.for_all2 same_offset_member_source
       (Semantic_source.offset_members actual)
       (Semantic_source.offset_members expected)
  && Semantic_source.offset_closing_origins actual
     = Semantic_source.offset_closing_origins expected

let same_source_expression actual expected =
  actual == expected
  || Semantic_source.argument_expression_origin actual
     = Semantic_source.argument_expression_origin expected
     &&
     match
       ( Semantic_source.argument_expression_kind actual,
         Semantic_source.argument_expression_kind expected )
     with
     | ( Semantic_source.Standalone_offset_expression actual,
         Semantic_source.Standalone_offset_expression expected ) ->
         same_offset_source actual expected
     | _ -> false

let checked_standalone_offset result source =
  let invalid () =
    Error
      (metadata_error ?span:(result_span result)
         "aggregate offset expression does not retain checked standalone \
          source evidence")
  in
  match Semantic_result.result_aggregate_offset_path result with
  | None -> checked_aggregate_offset result
  | Some path -> (
      match Semantic_source.offset_publication source with
      | Some publication
        when publication == Semantic_result.aggregate_offset_base path ->
          let members = Semantic_source.offset_members source in
          let segments = Semantic_result.aggregate_offset_segments path in
          let valid_bound_target =
            match Semantic_source.offset_bound_target source with
            | None -> true
            | Some target -> (
                (Option.is_some (Semantic_source.offset_root_query source)
                || Option.is_some
                     (Semantic_source.offset_top_level_query source))
                && Semantic_source.identifier_value_shape target
                   = Semantic_source.Object_value
                && Semantic_source.identifier_value_array_rank target = 0
                &&
                let target_type =
                  Semantic_source.identifier_value_type target
                in
                Type.pointer_depth target_type = 0
                &&
                match Type.base target_type with
                | Type.Aggregate symbol ->
                    symbol
                    == Sema.Module_expression_binding
                       .publication_canonical_symbol publication
                | Type.Primitive _ -> false)
          in
          if
            valid_bound_target
            && List.compare_lengths members segments = 0
            && List.for_all2
                 (fun member segment ->
                   match Semantic_source.offset_member_lookup member with
                   | Some lookup ->
                       lookup
                       == Semantic_result.aggregate_offset_segment_lookup
                            segment
                   | None -> false)
                 members segments
          then checked_aggregate_offset result
          else invalid ()
      | None | Some _ -> invalid ())

let checked_operand result expected_source description =
  match Semantic_result.result_operand result with
  | None ->
      Error
        (metadata_error ?span:(result_span result)
           (Printf.sprintf
              "typed semantic %s does not retain its checked operand"
              description))
  | Some operand
    when not
           (same_source_expression
              (Semantic_result.result_source operand)
              expected_source) ->
      Error
        (metadata_error ?span:(result_span result)
           (Printf.sprintf
              "typed semantic %s operand does not match its source expression"
              description))
  | Some operand -> Ok operand

let checked_binary_operands result binary =
  match Semantic_result.result_binary_operands result with
  | None ->
      Error
        (metadata_error ?span:(result_span result)
           "typed semantic binary expression does not retain its checked \
            children")
  | Some (left, right)
    when (not
            (same_source_expression
               (Semantic_result.result_source left)
               (Semantic_source.binary_left binary)))
         || not
              (same_source_expression
                 (Semantic_result.result_source right)
                 (Semantic_source.binary_right binary)) ->
      Error
        (metadata_error ?span:(result_span result)
           "typed semantic binary children do not match their source \
            expressions")
  | Some operands -> Ok operands

let operator_span result description = function
  | Sema.Symbol.Source_location location -> Ok location.span
  | Sema.Symbol.Pinned_source _ | Sema.Symbol.Synthesized _ ->
      Error
        (metadata_error ?span:(result_span result)
           (Printf.sprintf "typed semantic %s does not have a source location"
              description))

let binary_span result binary =
  Semantic_source.binary_operator_origin binary
  |> operator_span result "binary operator"

let unary_span result description prefix =
  Semantic_source.prefix_operator_origin prefix
  |> operator_span result description

let internal_scalar primitive type_ =
  Type.pointer_depth type_ = 0
  &&
  match Type.base type_ with
  | Type.Primitive (Type.Internal_storage, actual) ->
      Sema.Primitive_type.equal actual primitive
  | Type.Primitive (Type.Public_spelling, _) | Type.Aggregate _ -> false

let internal_i64 = internal_scalar Sema.Primitive_type.I64

let same_symbol left right =
  Sema.Symbol.Id.equal (Sema.Symbol.id left) (Sema.Symbol.id right)

let function_publication_matches declaration publication =
  let site = Function_resolution.resolved_declaration_site declaration in
  let source_symbol =
    site |> Function_resolution.declaration_site_function
    |> Function_type_resolution.function_symbol
  in
  Module_binding.publication_kind publication = Module_binding.Function
  && same_symbol
       (Module_binding.publication_source_symbol publication)
       source_symbol
  && same_symbol
       (Module_binding.publication_canonical_symbol publication)
       (Function_resolution.resolved_declaration_identity_symbol declaration)

let direct_function_path_matches_site declaration path =
  let site = Function_resolution.resolved_declaration_site declaration in
  if
    Function_resolution.declaration_site_source_kind site
    = Function_resolution.Intern
  then false
  else
    match (path, Function_resolution.declaration_site_state site) with
    | Semantic_source.Jit_extern_slot, Function_resolution.Unresolved_extern
    | Semantic_source.Jit_immediate, Function_resolution.Resolved
    | Semantic_source.Aot_absolute, Function_resolution.Resolved -> true
    | ( ( Semantic_source.Jit_extern_slot
        | Semantic_source.Jit_immediate
        | Semantic_source.Aot_absolute
        | Semantic_source.Reject_aot_extern
        | Semantic_source.Reject_aot_import
        | Semantic_source.Reject_internal ),
        ( Function_resolution.Unresolved_extern
        | Function_resolution.Imported
        | Function_resolution.Resolved ) ) -> false

let direct_function_metadata result =
  ( Semantic_result.result_function_declaration result,
    Semantic_result.result_function_address_path result )

let direct_function_metadata_absent result =
  match direct_function_metadata result with
  | None, None -> true
  | (None | Some _), (None | Some _) -> false

let checked_bound_direct_function_source operand operand_type identifier
    declaration path =
  let invalid message =
    Error (metadata_error ?span:(result_span operand) message)
  in
  let occurrence = Semantic_source.bound_identifier_occurrence identifier in
  let source = Semantic_result.result_source operand in
  if
    Semantic_result.result_origin operand
    <> Semantic_source.argument_expression_origin source
    || Module_binding.occurrence_origin occurrence
       <> Semantic_result.result_origin operand
  then invalid "direct function operand origins disagree"
  else if
    Semantic_source.bound_identifier_shape identifier
    <> Semantic_source.Direct_function_value
    || Semantic_source.bound_identifier_array_rank identifier <> 0
  then invalid "direct function operand has an inconsistent source shape"
  else if
    not
      (Sema.Type.equal
         (Semantic_source.bound_identifier_type identifier)
         operand_type)
  then invalid "direct function operand source and result types disagree"
  else
    match
      ( Semantic_source.bound_identifier_function_declaration identifier,
        Semantic_source.bound_identifier_function_address_path identifier,
        Module_binding.occurrence_resolution occurrence )
    with
    | ( Some source_declaration,
        Some source_path,
        Module_binding.Module_binding publication )
      when source_declaration == declaration && source_path = path ->
        let site = Function_resolution.resolved_declaration_site declaration in
        let source_symbol =
          site |> Function_resolution.declaration_site_function
          |> Function_type_resolution.function_symbol
        in
        if
          (not (function_publication_matches declaration publication))
          || not
               (String.equal
                  (Module_binding.occurrence_name occurrence)
                  (Sema.Symbol.name source_symbol))
        then invalid "direct function operand and module publication disagree"
        else Ok ()
    | _ -> invalid "direct function operand has inconsistent retained metadata"

let checked_top_level_direct_function_source operand identifier declaration =
  let invalid message =
    Error (metadata_error ?span:(result_span operand) message)
  in
  let occurrence =
    Semantic_source.top_level_bound_identifier_occurrence identifier
  in
  let site = Function_resolution.resolved_declaration_site declaration in
  let source_symbol =
    site |> Function_resolution.declaration_site_function
    |> Function_type_resolution.function_symbol
  in
  let invalid_metadata =
    Semantic_result.result_origin operand
    <> Semantic_source.argument_expression_origin
         (Semantic_result.result_source operand)
    || Sema.Top_level_outer_expression_binding.occurrence_origin occurrence
       <> Semantic_result.result_origin operand
    || (not
          (String.equal
             (Sema.Top_level_outer_expression_binding.occurrence_name occurrence)
             (Sema.Symbol.name source_symbol)))
    || Sema.Symbol.kind source_symbol <> Sema.Symbol.Function
    || Option.is_some
         (Semantic_result.result_top_level_outer_occurrence operand)
  in
  if invalid_metadata then
    invalid "top-level direct function operand metadata disagrees"
  else
    match
      Sema.Top_level_outer_expression_binding.occurrence_resolution occurrence
    with
    | Sema.Top_level_outer_expression_binding.Module_binding publication
      when function_publication_matches declaration publication -> Ok ()
    | Sema.Top_level_outer_expression_binding.Module_binding _
    | Sema.Top_level_outer_expression_binding.Outer_binding _ ->
        invalid "top-level direct function publication disagrees"

let checked_direct_function_address result prefix operand =
  let invalid message =
    Error (metadata_error ?span:(result_span result) message)
  in
  let operand_source = Semantic_result.result_source operand in
  let direct_source =
    match Semantic_source.argument_expression_kind operand_source with
    | Semantic_source.Bound_identifier_expression identifier
      when Semantic_source.bound_identifier_shape identifier
           = Semantic_source.Direct_function_value -> Some (`Bound identifier)
    | Semantic_source.Top_level_bound_identifier_expression identifier
      when Semantic_result.result_category operand
           = Semantic_result.Function_value -> Some (`Top_level identifier)
    | _ -> None
  in
  match direct_source with
  | None ->
      if
        direct_function_metadata_absent result
        && direct_function_metadata_absent operand
      then Ok None
      else
        invalid
          "nonfunction address expression retains direct-function metadata"
  | Some source_kind -> (
      match
        ( direct_function_metadata result,
          direct_function_metadata operand,
          Semantic_result.result_type result,
          Semantic_result.result_type operand )
      with
      | ( (Some declaration, Some path),
          (Some operand_declaration, Some operand_path),
          Some result_type,
          Some operand_type )
        when declaration == operand_declaration && path = operand_path -> (
          if
            Semantic_result.result_origin result
            <> Semantic_source.argument_expression_origin
                 (Semantic_result.result_source result)
            || Semantic_result.result_category result
               <> Semantic_result.Address_value
            || Semantic_result.result_category operand
               <> Semantic_result.Function_value
            || Semantic_result.result_class result
               <> Semantic_result.Integer_result
            || Semantic_result.result_class operand
               <> Semantic_result.Integer_result
            || Semantic_result.result_array_rank result <> 0
            || Semantic_result.result_array_rank operand <> 0
            || Semantic_result.result_intrinsic_conversion operand
               <> Semantic_result.No_intrinsic_conversion
            || (not (Sema.Type.equal result_type operand_type))
            || not (internal_i64 result_type)
          then
            invalid "direct function address has inconsistent checked metadata"
          else if not (direct_function_path_matches_site declaration path) then
            invalid
              "direct function address path disagrees with its declaration"
          else
            match source_kind with
            | `Bound identifier -> (
                match
                  checked_bound_direct_function_source operand operand_type
                    identifier declaration path
                with
                | Error _ as error -> error
                | Ok () -> (
                    match
                      unary_span result "direct function address" prefix
                    with
                    | Error _ as error -> error
                    | Ok span ->
                        Ok
                          (Some
                             ( span,
                               result_type,
                               Function_resolution
                               .resolved_declaration_identity_symbol declaration,
                               path ))))
            | `Top_level identifier -> (
                match
                  checked_top_level_direct_function_source operand identifier
                    declaration
                with
                | Error _ as error -> error
                | Ok () -> (
                    match
                      unary_span result "direct function address" prefix
                    with
                    | Error _ as error -> error
                    | Ok span ->
                        Ok
                          (Some
                             ( span,
                               result_type,
                               Function_resolution
                               .resolved_declaration_identity_symbol declaration,
                               path )))))
      | _ -> invalid "direct function address has incomplete retained metadata")

let checked_numeric_unary_types result opcode operand =
  match
    (Semantic_result.result_type result, Semantic_result.result_type operand)
  with
  | None, _ | _, None ->
      Error
        (metadata_error ?span:(result_span result)
           "typed semantic unary expression does not have complete checked \
            types")
  | Some result_type, Some _ ->
      let valid =
        match opcode with
        | Opcode.Ic_unary_minus | Opcode.Ic_not ->
            Option.fold ~none:false
              ~some:(fun operand_type ->
                let expected =
                  if opcode = Opcode.Ic_unary_minus then
                    Sema.Integer_computation_class.negate operand_type
                  else Sema.Integer_computation_class.forward operand_type
                in
                Type.equal result_type expected)
              (Semantic_result.result_computation_type operand)
        | Opcode.Ic_com -> internal_i64 result_type
        | _ -> false
      in
      if valid then Ok ()
      else
        Error
          (metadata_error ?span:(result_span result)
             "typed semantic unary result type does not match the audited \
              operator rule")

let checked_pointer_unary_types result opcode operand =
  match
    (Semantic_result.result_type result, Semantic_result.result_type operand)
  with
  | None, _ | _, None ->
      Error
        (metadata_error ?span:(result_span result)
           "typed semantic pointer expression does not have complete checked \
            types")
  | Some result_type, Some operand_type -> (
      let expected =
        match opcode with
        | Opcode.Ic_deref when Semantic_result.result_is_array_address operand
          -> Ok operand_type
        | Opcode.Ic_deref -> (
            match Type.dereference operand_type with
            | Ok type_ -> Ok type_
            | Error _ -> Ok operand_type)
        | Opcode.Ic_addr ->
            let pointer = Type.pointer_to operand_type in
            if Semantic_result.result_is_array_address operand then
              Result.bind pointer Type.pointer_to
            else pointer
        | _ -> Error "not a pointer prefix opcode"
      in
      match expected with
      | Error message ->
          Error (metadata_error ?span:(result_span result) message)
      | Ok expected_type when Type.equal result_type expected_type -> Ok ()
      | Ok _ ->
          Error
            (metadata_error ?span:(result_span result)
               "typed semantic pointer result type does not match the audited \
                operator rule"))

let checked_alias_types result operand =
  match
    (Semantic_result.result_type result, Semantic_result.result_type operand)
  with
  | Some result_type, Some operand_type when Type.equal result_type operand_type
    -> Ok ()
  | Some _, Some _ ->
      Error
        (metadata_error ?span:(result_span result)
           "transparent expression changes its checked operand type")
  | None, _ | _, None ->
      Error
        (metadata_error ?span:(result_span result)
           "transparent expression does not have complete checked types")

let checked_cast_types result operand target =
  match Semantic_result.result_type result with
  | None ->
      Error
        (metadata_error ?span:(result_span result)
           "postfix cast does not have a checked target type")
  | Some result_type -> (
      let target_type = Type_reference.resolved_type target in
      if not (Type.equal result_type target_type) then
        Error
          (metadata_error ?span:(result_span result)
             "postfix cast result type does not match its checked target")
      else
        match (checked_numeric_type operand, checked_numeric_type result) with
        | Error item, _ | _, Error item -> Error item
        | Ok Unsupported_type, _ | _, Ok Unsupported_type -> Ok false
        | Ok (Checked_type _), Ok (Checked_type _) -> Ok true)

let cast_span result =
  match result_span result with
  | Some span -> Ok span
  | None ->
      Error
        (metadata_error
           "typed semantic postfix cast does not have a source location")

let cast_was_parenthesized operand =
  match Semantic_source.argument_expression_kind operand with
  | Semantic_source.Parenthesized_expression _ -> true
  | Semantic_source.Integer_literal _
  | Semantic_source.Float_literal _
  | Semantic_source.Character_literal _
  | Semantic_source.String_literal _
  | Semantic_source.Prefix_expression _
  | Semantic_source.Postfix_expression _
  | Semantic_source.Postfix_cast_expression _
  | Semantic_source.Binary_expression _
  | Semantic_source.Index_expression _
  | Semantic_source.Member_access_expression _
  | Semantic_source.Bound_identifier_expression _
  | Semantic_source.Aggregate_offset_base_expression _
  | Semantic_source.Top_level_bound_identifier_expression _
  | Semantic_source.Sizeof_expression _
  | Semantic_source.Standalone_offset_expression _
  | Semantic_source.Defined_expression _
  | Semantic_source.Unresolved_expression _ -> false

let validate_numeric_unary result opcode operand =
  let checked_type =
    if Opcode.equal opcode Opcode.Ic_com then checked_integer_type
    else checked_numeric_type
  in
  match checked_type operand with
  | Error item -> Error item
  | Ok Unsupported_type -> Ok false
  | Ok (Checked_type _) -> (
      match checked_type result with
      | Error item -> Error item
      | Ok Unsupported_type -> Ok false
      | Ok (Checked_type _) ->
          Result.map
            (fun () -> true)
            (checked_numeric_unary_types result opcode operand))

let validate_pointer_unary result opcode operand =
  if
    Semantic_result.result_array_rank operand > 0
    && not (Semantic_result.result_is_array_address operand)
  then Ok false
  else
    match checked_integer_or_pointer_type operand with
    | Error item -> Error item
    | Ok Unsupported_type -> Ok false
    | Ok (Checked_type _) -> (
        match checked_integer_or_pointer_type result with
        | Error item -> Error item
        | Ok Unsupported_type -> Ok false
        | Ok (Checked_type _) ->
            Result.map
              (fun () -> true)
              (checked_pointer_unary_types result opcode operand))

let cancellable_dereference operand =
  let current = ref operand in
  let result = ref No_cancellation in
  let error = ref None in
  let searching = ref true in
  while !searching && Option.is_none !error do
    match
      Semantic_result.result_source !current
      |> Semantic_source.argument_expression_kind
    with
    | Semantic_source.Parenthesized_expression source -> (
        match checked_operand !current source "parenthesized expression" with
        | Error item -> error := Some item
        | Ok next -> (
            match checked_alias_types !current next with
            | Error item -> error := Some item
            | Ok () -> current := next))
    | Semantic_source.Prefix_expression prefix
      when Semantic_source.prefix_operator prefix = Semantic_source.Unary_plus
      -> (
        match
          checked_operand !current
            (Semantic_source.prefix_operand prefix)
            "unary-plus expression"
        with
        | Error item -> error := Some item
        | Ok next -> (
            match checked_alias_types !current next with
            | Error item -> error := Some item
            | Ok () -> current := next))
    | Semantic_source.Prefix_expression prefix
      when Semantic_source.prefix_operator prefix = Semantic_source.Dereference
      -> (
        match
          ( checked_operand !current
              (Semantic_source.prefix_operand prefix)
              "dereference expression",
            unary_span !current "dereference expression" prefix )
        with
        | Error item, _ | _, Error item -> error := Some item
        | Ok next, Ok _ -> (
            match checked_pointer_unary_types !current Opcode.Ic_deref next with
            | Error item -> error := Some item
            | Ok () ->
                result := Canceled_dereference next;
                searching := false))
    | Semantic_source.Integer_literal _
    | Semantic_source.Float_literal _
    | Semantic_source.Character_literal _
    | Semantic_source.String_literal _
    | Semantic_source.Prefix_expression _
    | Semantic_source.Postfix_expression _
    | Semantic_source.Postfix_cast_expression _
    | Semantic_source.Binary_expression _
    | Semantic_source.Index_expression _
    | Semantic_source.Member_access_expression _
    | Semantic_source.Bound_identifier_expression _
    | Semantic_source.Aggregate_offset_base_expression _
    | Semantic_source.Top_level_bound_identifier_expression _
    | Semantic_source.Sizeof_expression _
    | Semantic_source.Standalone_offset_expression _
    | Semantic_source.Defined_expression _
    | Semantic_source.Unresolved_expression _ -> searching := false
  done;
  match !error with
  | Some item -> Error item
  | None -> Ok !result

let validate_binary_with checked_type result left right =
  match (checked_type left, checked_type right) with
  | Error item, _ | _, Error item -> Error item
  | Ok Unsupported_type, _ | _, Ok Unsupported_type -> Ok false
  | Ok (Checked_type _), Ok (Checked_type _) -> (
      match checked_type result with
      | Error item -> Error item
      | Ok Unsupported_type -> Ok false
      | Ok (Checked_type _) -> Ok true)

let numeric_conversion result =
  match checked_integer_type result with
  | Error _ as error -> error
  | Ok (Checked_type _) -> Ok (Some Result_to_f64)
  | Ok Unsupported_type -> (
      match checked_f64_type result with
      | Error _ as error -> error
      | Ok (Checked_type _) -> Ok (Some Keep_result)
      | Ok Unsupported_type -> Ok None)

let validate_f64_binary_with checked_result_type ~allow_integer_pair
    ~operation_flags result left right =
  match (numeric_conversion left, numeric_conversion right) with
  | Error item, _ | _, Error item -> Error item
  | Ok None, _ | _, Ok None -> Ok Unsupported_binary
  | Ok (Some left_conversion), Ok (Some right_conversion) -> (
      if
        (not allow_integer_pair)
        && left_conversion = Result_to_f64
        && right_conversion = Result_to_f64
      then Ok Unsupported_binary
      else
        match checked_result_type result with
        | Error item -> Error item
        | Ok Unsupported_type -> Ok Unsupported_binary
        | Ok (Checked_type _) ->
            Ok
              (Supported_binary
                 { left_conversion; right_conversion; operation_flags }))

let validate_binary result opcode left right =
  match validate_binary_with checked_integer_type result left right with
  | Error _ as error -> error
  | Ok true ->
      Ok
        (Supported_binary
           {
             left_conversion = Keep_result;
             right_conversion = Keep_result;
             operation_flags = 0L;
           })
  | Ok false when accepted_f64_arithmetic_opcode opcode ->
      validate_f64_binary_with checked_f64_type ~allow_integer_pair:false
        ~operation_flags:0L result left right
  | Ok false when accepted_f64_bitwise_opcode opcode ->
      validate_f64_binary_with checked_f64_type ~allow_integer_pair:false
        ~operation_flags:0L result left right
  | Ok false when accepted_f64_shift_opcode opcode ->
      validate_f64_binary_with checked_f64_type ~allow_integer_pair:false
        ~operation_flags:0L result left right
  | Ok false when accepted_f64_comparison_opcode opcode ->
      validate_f64_binary_with checked_integer_type ~allow_integer_pair:false
        ~operation_flags:use_f64_flag result left right
  | Ok false when accepted_f64_logical_opcode opcode ->
      validate_f64_binary_with checked_integer_type ~allow_integer_pair:false
        ~operation_flags:0L result left right
  | Ok false when opcode = Opcode.Ic_power ->
      validate_f64_binary_with checked_f64_type ~allow_integer_pair:true
        ~operation_flags:0L result left right
  | Ok false -> Ok Unsupported_binary

let prepare_storage_address ?frame ?globals result =
  let ( let* ) = Result.bind in
  let* address =
    match frame with
    | None -> Ok None
    | Some frame -> Frame_address_lowering.prepare ~frame result
  in
  match (address, globals) with
  | Some address, _ -> Ok (Some (Frame_slot address))
  | None, None -> Ok None
  | None, Some globals ->
      Global_address_lowering.prepare ?frame ~globals result
      |> Result.map (Option.map (fun address -> Global_slot address))

let rec prepare_index_address ?frame ?globals result =
  let ( let* ) = Result.bind in
  let invalid message =
    Error [ metadata_error ?span:(result_span result) message ]
  in
  let* value_type =
    checked_frame_value result |> Result.map_error (fun e -> [ e ])
  in
  match value_type with
  | Unsupported_type -> Ok None
  | Checked_type _ -> (
      match
        Semantic_source.argument_expression_kind
          (Semantic_result.result_source result)
      with
      | Semantic_source.Bound_identifier_expression _
      | Semantic_source.Top_level_bound_identifier_expression _
      | Semantic_source.Unresolved_expression
          Semantic_source.Identifier_expression
        when Semantic_result.result_is_array_address result
             && Option.is_some globals -> (
          let* prepared =
            Global_address_lowering.prepare ?frame ~globals:(Option.get globals)
              result
          in
          match prepared with
          | Some address ->
              Ok
                (Some
                   ( Direct_address (Global_slot address),
                     Global_address_lowering.strides address ))
          | None -> prepare_index_address ?frame result)
      | Semantic_source.Bound_identifier_expression identifier
        when Semantic_result.result_is_array_address result -> (
          match frame with
          | None -> Ok None
          | Some frame -> (
              let* prepared = Frame_address_lowering.prepare ~frame result in
              match prepared with
              | None -> Ok None
              | Some prepared -> (
                  let occurrence =
                    Semantic_source.bound_identifier_occurrence identifier
                  in
                  match Module_binding.occurrence_resolution occurrence with
                  | Module_binding.Local_binding binding -> (
                      match
                        Sema.Function_frame_layout.find_binding_location frame
                          binding
                      with
                      | Some location ->
                          let module F = Sema.Function_frame_layout in
                          if
                            F.location_kind location <> F.Automatic_local
                            || F.location_declarator_shape location <> F.Object
                            || storage_element_size
                                 (F.location_checked_type location)
                               <> Some (F.location_element_size location)
                          then Ok None
                          else
                            let rec strides = function
                              | [] -> Ok (F.location_element_size location, [])
                              | dimension :: rest ->
                                  let* bytes, tail = strides rest in
                                  let count = F.dimension_value dimension in
                                  if
                                    count <= 0L
                                    || count > Int64.div Int64.max_int bytes
                                  then
                                    invalid
                                      "array dimensions require positive \
                                       representable storage"
                                  else Ok (Int64.mul count bytes, bytes :: tail)
                            in
                            let* bytes, strides =
                              strides (F.location_dimensions location)
                            in
                            if
                              bytes <> F.location_allocated_size location
                              || List.length strides
                                 <> Semantic_result.result_array_rank result
                            then
                              invalid
                                "array dimensions disagree with the exact \
                                 frame extent"
                            else
                              Ok
                                (Some
                                   ( Direct_address (Frame_slot prepared),
                                     strides ))
                      | None ->
                          invalid "array root lost its exact frame location")
                  | _ -> Ok None)))
      | Semantic_source.Index_expression source -> (
          match Semantic_result.result_index_operands result with
          | None -> invalid "indexed expression lost its checked operands"
          | Some (base, index) -> (
              if
                Semantic_result.result_source base
                != Semantic_source.index_base source
                || Semantic_result.result_source index
                   != Semantic_source.index_value source
              then
                invalid
                  "indexed expression operands do not match their exact sources"
              else
                let* index_type =
                  checked_frame_word index |> Result.map_error (fun e -> [ e ])
                in
                match index_type with
                | Unsupported_type -> Ok None
                | Checked_type _
                  when Semantic_result.result_category index
                       <> Semantic_result.Object_value
                       && Semantic_result.result_category index
                          <> Semantic_result.Lvalue -> Ok None
                | Checked_type _ -> (
                    if
                      Semantic_result.result_intrinsic_conversion index
                      <> Semantic_result.Result_to_int
                    then
                      invalid "index operand lost its integer conversion intent"
                    else
                      let* base_address =
                        if Semantic_result.result_is_array_address base then
                          prepare_index_address ?frame ?globals base
                        else
                          match checked_frame_value base with
                          | Error e -> Error [ e ]
                          | Ok (Checked_type pointer)
                            when scalar_pointer_type pointer
                                 && Semantic_result.result_array_rank base = 0
                                 &&
                                 match Semantic_result.result_category base with
                                 | Semantic_result.Object_value
                                 | Lvalue
                                 | Address_value -> true
                                 | _ -> false ->
                              Ok
                                (Option.map
                                   (fun size -> (Pointer_base base, [ size ]))
                                   (pointer_element_size pointer))
                          | _ -> Ok None
                      in
                      match base_address with
                      | None -> Ok None
                      | Some (_, []) ->
                          invalid "index base has no checked remaining stride"
                      | Some (address, stride :: remaining) ->
                          let base_type =
                            Option.get (Semantic_result.result_type base)
                          in
                          let* element =
                            (if Semantic_result.result_is_array_address base
                             then Ok base_type
                             else Type.dereference base_type)
                            |> Result.map_error (fun message ->
                                [
                                  metadata_error ?span:(result_span result)
                                    message;
                                ])
                          in
                          let expected_rank = List.length remaining in
                          if
                            (not
                               (Option.fold ~none:false
                                  ~some:(Type.equal element)
                                  (Semantic_result.result_type result)))
                            || Semantic_result.result_array_rank result
                               <> expected_rank
                            || Semantic_result.result_is_array_address result
                               <> (expected_rank > 0)
                            ||
                            if expected_rank > 0 then
                              Semantic_result.result_category result
                              <> Semantic_result.Array_value
                            else
                              Semantic_result.result_category result
                              <> Semantic_result.Object_value
                              && Semantic_result.result_category result
                                 <> Semantic_result.Lvalue
                          then
                            invalid
                              "index result disagrees with its element type \
                               and remaining dimensions"
                          else
                            let* span =
                              operator_span result "array index"
                                (Semantic_source.index_opening_origin source)
                              |> Result.map_error (fun e -> [ e ])
                            in
                            let* pointer_type =
                              Type.pointer_to element
                              |> Result.map_error (fun message ->
                                  [ metadata_error ~span message ])
                            in
                            Ok
                              (Some
                                 ( Indexed_address
                                     {
                                       base;
                                       address;
                                       index;
                                       stride;
                                       pointer_type;
                                       span;
                                     },
                                   remaining )))))
      | _ -> Ok None)

let rec prepare_assignment_address ?frame ?globals result =
  match
    Semantic_source.argument_expression_kind
      (Semantic_result.result_source result)
  with
  | Semantic_source.Index_expression _ ->
      prepare_index_address ?frame ?globals result
      |> Result.map (Option.map fst)
  | Semantic_source.Parenthesized_expression source -> (
      match checked_operand result source "parenthesized assignment target" with
      | Error item -> Error [ item ]
      | Ok operand -> prepare_assignment_address ?frame ?globals operand)
  | Semantic_source.Prefix_expression prefix
    when Semantic_source.prefix_operator prefix = Semantic_source.Dereference ->
      let ( let* ) = Result.bind in
      let* pointer =
        checked_operand result
          (Semantic_source.prefix_operand prefix)
          "dereference destination"
        |> Result.map_error (fun e -> [ e ])
      in
      let* valid =
        validate_pointer_unary result Opcode.Ic_deref pointer
        |> Result.map_error (fun e -> [ e ])
      in
      if
        valid
        &&
        match checked_frame_value pointer with
        | Ok (Checked_type type_) -> scalar_pointer_type type_
        | _ -> false
      then Ok (Some (Indirect_address_value pointer))
      else Ok None
  | _ ->
      prepare_storage_address ?frame ?globals result
      |> Result.map (Option.map (fun a -> Direct_address a))

let validate_frame_assignment result left right =
  let ( let* ) = Result.bind in
  let* valid = validate_binary_with checked_frame_value result left right in
  if not valid then Ok false
  else
    let r = Option.get (Semantic_result.result_type result)
    and l = Option.get (Semantic_result.result_type left)
    and v =
      match checked_frame_value right with
      | Ok (Checked_type v) -> v
      | _ -> assert false
    in
    Ok
      (Type.pointer_depth r = 0
       && Type.pointer_depth l = 0
       && Type.pointer_depth v = 0
      || scalar_pointer_type r && Type.equal r l
         && Type.compatible_u8_pointer l v)

let compound_assignment = function
  | Opcode.Ic_add_equ
  | Ic_sub_equ
  | Ic_mul_equ
  | Ic_div_equ
  | Ic_mod_equ
  | Ic_and_equ
  | Ic_or_equ
  | Ic_xor_equ
  | Ic_shl_equ
  | Ic_shr_equ -> true
  | _ -> false

let prepare_update_address ?frame ?globals result operand =
  match (checked_frame_integer result, checked_frame_integer operand) with
  | Error item, _ | _, Error item -> Error [ item ]
  | Ok Unsupported_type, _ | _, Ok Unsupported_type -> Ok None
  | Ok (Checked_type result_type), Ok (Checked_type operand_type) ->
      if
        Semantic_result.result_category operand <> Semantic_result.Lvalue
        || Semantic_result.result_category result
           <> Semantic_result.Object_value
        || not (Type.equal result_type operand_type)
      then
        Error
          [
            metadata_error ?span:(result_span result)
              "scalar update does not retain its lvalue and destination result \
               type";
          ]
      else prepare_assignment_address ?frame ?globals operand

let plan ?frame ?globals ~allow_calls root =
  let root_conversion = requested_conversion root in
  let pending = ref [] in
  let reversed = ref [] in
  let unsupported = ref false in
  let error = ref None in
  let rec address_tasks result address after =
    match address with
    | Direct_address address ->
        reversed := Storage_address { result; address } :: !reversed;
        pending := after @ !pending
    | Indirect_address_value pointer ->
        pending :=
          Visit { result = pointer; conversion = Keep_result }
          :: Finish_indirect_address { result; pointer }
          :: after
          @ !pending
    | Pointer_base pointer ->
        pending :=
          (Visit { result = pointer; conversion = Keep_result } :: after)
          @ !pending
    | Indexed_address { base; address; index; stride; pointer_type; span } ->
        let step =
          {
            indexed_result = result;
            indexed_base = base;
            index_value = index;
            index_stride = stride;
            index_type = pointer_type;
            index_span = span;
          }
        in
        address_tasks base address
          (Emit_index_stride step
          :: Visit { result = index; conversion = Keep_result }
          :: Finish_index_address step :: after)
  in
  let array_value result operand conversion =
    match (checked_frame_value operand, result_span result) with
    | Error item, _ -> error := Some item
    | Ok (Checked_type pointer_type), Some span
      when scalar_pointer_type pointer_type && conversion = Keep_result -> (
        match prepare_index_address ?frame ?globals operand with
        | Error (item :: _) -> error := Some item
        | Ok (Some (address, _)) ->
            address_tasks operand address
              [
                Finish_materialize_array { result; operand; pointer_type; span };
              ]
        | _ -> unsupported := true)
    | _ -> unsupported := true
  in
  let update result source_operand opcode origin conversion =
    match
      ( checked_operand result source_operand "scalar update",
        operator_span result "scalar update" origin )
    with
    | Error item, _ | _, Error item -> error := Some item
    | Ok operand, Ok span -> (
        match prepare_update_address ?frame ?globals result operand with
        | Error (item :: _) -> error := Some item
        | Error [] ->
            error :=
              Some
                (metadata_error ~span "scalar update address validation failed")
        | Ok None -> unsupported := true
        | Ok (Some address) ->
            address_tasks operand address
              [ Finish_unary { result; opcode; span; operand; conversion } ])
  in
  (match validate_conversion root root_conversion with
  | Error item -> error := Some item
  | Ok false -> unsupported := true
  | Ok true ->
      pending := [ Visit { result = root; conversion = root_conversion } ]);
  while !pending <> [] && (not !unsupported) && Option.is_none !error do
    match !pending with
    | [] -> ()
    | task :: remaining -> (
        pending := remaining;
        match task with
        | Visit { result; conversion } -> (
            match
              Semantic_result.result_source result
              |> Semantic_source.argument_expression_kind
            with
            | _ when Semantic_result.result_is_array_address result ->
                array_value result result conversion
            | Semantic_source.Index_expression _ -> (
                match
                  ( prepare_index_address ?frame ?globals result,
                    result_span result )
                with
                | Error (item :: _), _ -> error := Some item
                | Ok (Some (address, [])), Some span ->
                    address_tasks result address
                      [
                        Finish_unary
                          {
                            result;
                            opcode = Opcode.Ic_deref;
                            span;
                            operand = result;
                            conversion;
                          };
                      ]
                | _ -> unsupported := true)
            | Semantic_source.Bound_identifier_expression _
            | Semantic_source.Top_level_bound_identifier_expression _
            | Semantic_source.Unresolved_expression
                Semantic_source.Identifier_expression -> (
                match (checked_frame_scalar result, result_span result) with
                | Error item, _ -> error := Some item
                | Ok (Checked_type result_type), Some span -> (
                    match prepare_storage_address ?frame ?globals result with
                    | Error (item :: _) -> error := Some item
                    | Error [] ->
                        error :=
                          Some
                            (metadata_error ~span
                               "storage address validation failed")
                    | Ok None -> unsupported := true
                    | Ok (Some address) ->
                        reversed :=
                          Storage_load
                            { result; address; result_type; span; conversion }
                          :: !reversed)
                | _ -> unsupported := true)
            | Semantic_source.Integer_literal _
            | Semantic_source.Character_literal _ -> (
                match checked_integer_type result with
                | Error item -> error := Some item
                | Ok Unsupported_type -> unsupported := true
                | Ok (Checked_type _) ->
                    reversed := Literal { result; conversion } :: !reversed)
            | Semantic_source.Float_literal _ -> (
                match checked_f64_type result with
                | Error item -> error := Some item
                | Ok Unsupported_type -> unsupported := true
                | Ok (Checked_type _) ->
                    if conversion <> Result_to_f64 then
                      reversed := Literal { result; conversion } :: !reversed
                    else unsupported := true)
            | Semantic_source.String_literal _ -> (
                match checked_string_type result with
                | Error item -> error := Some item
                | Ok Unsupported_type -> unsupported := true
                | Ok (Checked_type _) ->
                    if conversion = Keep_result then
                      reversed := Literal { result; conversion } :: !reversed
                    else unsupported := true)
            | Semantic_source.Unresolved_expression
                Semantic_source.Current_position_expression -> (
                match checked_current_position result with
                | Error item -> error := Some item
                | Ok (span, result_type) ->
                    reversed :=
                      Current_position { result; span; result_type; conversion }
                      :: !reversed)
            | Semantic_source.Defined_expression defined -> (
                match checked_defined result defined with
                | Error item -> error := Some item
                | Ok Deferred_constant -> unsupported := true
                | Ok (Checked_constant (span, result_type, value)) ->
                    reversed :=
                      Integer_constant
                        { result; span; result_type; value; conversion }
                      :: !reversed)
            | Semantic_source.Sizeof_expression sizeof -> (
                match checked_sizeof result sizeof with
                | Error item -> error := Some item
                | Ok Deferred_constant -> unsupported := true
                | Ok (Checked_constant (span, result_type, value)) ->
                    reversed :=
                      Integer_constant
                        { result; span; result_type; value; conversion }
                      :: !reversed)
            | Semantic_source.Member_access_expression _ -> (
                match checked_aggregate_offset result with
                | Error item -> error := Some item
                | Ok Deferred_constant -> unsupported := true
                | Ok (Checked_constant (span, result_type, value)) ->
                    reversed :=
                      Integer_constant
                        { result; span; result_type; value; conversion }
                      :: !reversed)
            | Semantic_source.Standalone_offset_expression source -> (
                match checked_standalone_offset result source with
                | Error item -> error := Some item
                | Ok Deferred_constant -> unsupported := true
                | Ok (Checked_constant (span, result_type, value)) ->
                    reversed :=
                      Integer_constant
                        { result; span; result_type; value; conversion }
                      :: !reversed)
            | Semantic_source.Parenthesized_expression source -> (
                match
                  checked_operand result source "parenthesized expression"
                with
                | Error item -> error := Some item
                | Ok operand
                  when Semantic_result.result_is_array_address operand -> (
                    match
                      ( Semantic_result.result_type result,
                        checked_frame_value operand )
                    with
                    | Some actual, Ok (Checked_type expected)
                      when Type.equal actual expected
                           && Semantic_result.result_array_rank result = 0 ->
                        array_value result operand conversion
                    | _ ->
                        error :=
                          Some
                            (metadata_error ?span:(result_span result)
                               "grouped array has a mismatched element pointer")
                    )
                | Ok operand ->
                    pending :=
                      Visit { result = operand; conversion }
                      :: Finish_alias { result; operand }
                      :: !pending)
            | Semantic_source.Prefix_expression prefix -> (
                let source_operand = Semantic_source.prefix_operand prefix in
                match Semantic_source.prefix_operator prefix with
                | (Semantic_source.Pre_increment | Semantic_source.Pre_decrement)
                  as operator ->
                    update result source_operand
                      (if operator = Semantic_source.Pre_increment then
                         Opcode.Ic_pp_
                       else Opcode.Ic_mm_)
                      (Semantic_source.prefix_operator_origin prefix)
                      conversion
                | Semantic_source.Unary_plus -> (
                    match
                      checked_operand result source_operand
                        "unary-plus expression"
                    with
                    | Error item -> error := Some item
                    | Ok operand ->
                        pending :=
                          Visit { result = operand; conversion }
                          :: Finish_alias { result; operand }
                          :: !pending)
                | (Semantic_source.Dereference | Semantic_source.Address_of) as
                  operator -> (
                    let opcode, description =
                      match operator with
                      | Semantic_source.Dereference ->
                          (Opcode.Ic_deref, "dereference expression")
                      | Semantic_source.Address_of ->
                          (Opcode.Ic_addr, "address-of expression")
                      | Semantic_source.Unary_plus
                      | Semantic_source.Unary_minus
                      | Semantic_source.Logical_not
                      | Semantic_source.Bitwise_not
                      | Semantic_source.Pre_increment
                      | Semantic_source.Pre_decrement -> assert false
                    in
                    match
                      ( checked_operand result source_operand description,
                        unary_span result description prefix )
                    with
                    | Error item, _ | _, Error item -> error := Some item
                    | Ok operand, Ok span -> (
                        let direct_address =
                          if Opcode.equal opcode Opcode.Ic_addr then
                            checked_direct_function_address result prefix
                              operand
                          else Ok None
                        in
                        match direct_address with
                        | Error item -> error := Some item
                        | Ok (Some (address_span, result_type, symbol, path)) ->
                            reversed :=
                              Direct_function_address
                                {
                                  result;
                                  span = address_span;
                                  result_type;
                                  symbol;
                                  path;
                                  conversion;
                                }
                              :: !reversed
                        | Ok None
                          when Option.is_some frame || Option.is_some globals
                          -> (
                            match
                              validate_pointer_unary result opcode operand
                            with
                            | Error item -> error := Some item
                            | Ok false -> unsupported := true
                            | Ok true -> (
                                if opcode = Opcode.Ic_addr then
                                  match Semantic_result.result_type result with
                                  | Some type_
                                    when scalar_pointer_type type_
                                         && conversion = Keep_result -> (
                                      match
                                        prepare_assignment_address ?frame
                                          ?globals operand
                                      with
                                      | Ok (Some address) ->
                                          address_tasks operand address
                                            [
                                              Finish_unary
                                                {
                                                  result;
                                                  opcode;
                                                  span;
                                                  operand;
                                                  conversion;
                                                };
                                            ]
                                      | Ok None -> unsupported := true
                                      | Error (item :: _) -> error := Some item
                                      | Error [] -> unsupported := true)
                                  | _ -> unsupported := true
                                else
                                  match
                                    ( checked_frame_integer result,
                                      checked_frame_value operand )
                                  with
                                  | Ok (Checked_type _), Ok (Checked_type type_)
                                    when scalar_pointer_type type_ ->
                                      pending :=
                                        Visit
                                          {
                                            result = operand;
                                            conversion = Keep_result;
                                          }
                                        :: Finish_unary
                                             {
                                               result;
                                               opcode;
                                               span;
                                               operand;
                                               conversion;
                                             }
                                        :: !pending
                                  | Error item, _ | _, Error item ->
                                      error := Some item
                                  | _ -> unsupported := true))
                        | Ok None -> (
                            match
                              validate_pointer_unary result opcode operand
                            with
                            | Error item -> error := Some item
                            | Ok false -> unsupported := true
                            | Ok true ->
                                if Opcode.equal opcode Opcode.Ic_addr then
                                  match cancellable_dereference operand with
                                  | Error item -> error := Some item
                                  | Ok No_cancellation ->
                                      pending :=
                                        Visit
                                          {
                                            result = operand;
                                            conversion = Keep_result;
                                          }
                                        :: Finish_unary
                                             {
                                               result;
                                               opcode;
                                               span;
                                               operand;
                                               conversion;
                                             }
                                        :: !pending
                                  | Ok (Canceled_dereference source_operand) ->
                                      pending :=
                                        Visit
                                          {
                                            result = source_operand;
                                            conversion = Keep_result;
                                          }
                                        :: Finish_unary
                                             {
                                               result;
                                               opcode;
                                               span;
                                               operand = source_operand;
                                               conversion;
                                             }
                                        :: !pending
                                else
                                  pending :=
                                    Visit
                                      {
                                        result = operand;
                                        conversion = Keep_result;
                                      }
                                    :: Finish_unary
                                         {
                                           result;
                                           opcode;
                                           span;
                                           operand;
                                           conversion;
                                         }
                                    :: !pending)))
                | operator -> (
                    match accepted_prefix operator with
                    | None -> unsupported := true
                    | Some (opcode, description) -> (
                        match
                          ( checked_operand result source_operand description,
                            unary_span result description prefix )
                        with
                        | Error item, _ | _, Error item -> error := Some item
                        | Ok operand, Ok span -> (
                            match
                              validate_numeric_unary result opcode operand
                            with
                            | Error item -> error := Some item
                            | Ok false -> unsupported := true
                            | Ok true ->
                                pending :=
                                  Visit
                                    {
                                      result = operand;
                                      conversion = Keep_result;
                                    }
                                  :: Finish_unary
                                       {
                                         result;
                                         opcode;
                                         span;
                                         operand;
                                         conversion;
                                       }
                                  :: !pending))))
            | Semantic_source.Binary_expression binary -> (
                let opcode = Semantic_source.binary_operator binary in
                if
                  not
                    (accepted_binary_opcode opcode
                    || (Option.is_some frame || Option.is_some globals)
                       && (Opcode.equal opcode Opcode.Ic_assign
                          || compound_assignment opcode))
                then unsupported := true
                else
                  match
                    ( checked_binary_operands result binary,
                      binary_span result binary )
                  with
                  | Error item, _ | _, Error item -> error := Some item
                  | Ok (left, right), Ok span -> (
                      if
                        Opcode.equal opcode Opcode.Ic_assign
                        || compound_assignment opcode
                      then
                        match
                          if opcode = Opcode.Ic_assign then
                            validate_frame_assignment result left right
                          else
                            validate_binary_with checked_frame_integer result
                              left right
                        with
                        | Error item -> error := Some item
                        | Ok true -> (
                            match
                              if compound_assignment opcode then
                                prepare_update_address ?frame ?globals result
                                  left
                              else
                                prepare_assignment_address ?frame ?globals left
                            with
                            | Error (item :: _) -> error := Some item
                            | Error [] ->
                                error :=
                                  Some
                                    (metadata_error ~span
                                       "assignment address validation failed")
                            | Ok None -> unsupported := true
                            | Ok (Some address) ->
                                address_tasks left address
                                  [
                                    Visit
                                      {
                                        result = right;
                                        conversion = Keep_result;
                                      };
                                    Finish_binary
                                      {
                                        result;
                                        opcode;
                                        span;
                                        left;
                                        right;
                                        conversion;
                                        operation_flags = 0L;
                                      };
                                  ])
                        | _ -> unsupported := true
                      else
                        let left_kind =
                          Semantic_result.result_source left
                          |> Semantic_source.argument_expression_kind
                        in
                        match left_kind with
                        | Semantic_source.Binary_expression previous
                          when accepted_f64_comparison_opcode opcode
                               && accepted_f64_comparison_opcode
                                    (Semantic_source.binary_operator previous)
                          -> (
                            match checked_binary_operands left previous with
                            | Error item -> error := Some item
                            | Ok (_, middle) when is_comparison_result middle ->
                                (* Multiple pending comparison reductions have a
                                 separate source stack shape. Parentheses end
                                 that stack before the outer comparison. *)
                                unsupported := true
                            | Ok (first, middle) -> (
                                match
                                  ( validate_binary_with checked_integer_type
                                      left first middle,
                                    validate_binary_with checked_integer_type
                                      result middle right )
                                with
                                | Error item, _ | _, Error item ->
                                    error := Some item
                                | Ok false, _ | _, Ok false ->
                                    unsupported := true
                                | Ok true, Ok true ->
                                    (* PrsExp.HC:225-230 keeps the previous right
                                     operand for the next comparison. Grouping
                                     and tighter right operands stay intact. *)
                                    pending :=
                                      Visit
                                        {
                                          result = left;
                                          conversion = Keep_result;
                                        }
                                      :: Visit
                                           {
                                             result = right;
                                             conversion = Keep_result;
                                           }
                                      :: Finish_chain_link
                                           {
                                             result;
                                             previous = left;
                                             middle;
                                             right;
                                             opcode;
                                             span;
                                             conversion;
                                           }
                                      :: !pending))
                        | _ -> (
                            match validate_binary result opcode left right with
                            | Error item -> error := Some item
                            | Ok Unsupported_binary -> unsupported := true
                            | Ok
                                (Supported_binary
                                   {
                                     left_conversion;
                                     right_conversion;
                                     operation_flags;
                                   }) ->
                                pending :=
                                  Visit
                                    {
                                      result = left;
                                      conversion = left_conversion;
                                    }
                                  :: Visit
                                       {
                                         result = right;
                                         conversion = right_conversion;
                                       }
                                  :: Finish_binary
                                       {
                                         result;
                                         opcode;
                                         span;
                                         left;
                                         right;
                                         conversion;
                                         operation_flags;
                                       }
                                  :: !pending)))
            | Semantic_source.Postfix_cast_expression (source_operand, target)
              -> (
                match
                  ( checked_operand result source_operand "postfix cast",
                    cast_span result )
                with
                | Error item, _ | _, Error item -> error := Some item
                | Ok operand, Ok span -> (
                    match checked_cast_types result operand target with
                    | Error item -> error := Some item
                    | Ok false -> unsupported := true
                    | Ok true ->
                        pending :=
                          Visit { result = operand; conversion = Keep_result }
                          :: Finish_cast
                               {
                                 result;
                                 span;
                                 operand;
                                 was_parenthesized =
                                   cast_was_parenthesized source_operand;
                                 conversion;
                               }
                          :: !pending))
            | Semantic_source.Unresolved_expression
                Semantic_source.Call_expression
              when allow_calls ->
                reversed := Call { result; conversion } :: !reversed
            | Semantic_source.Postfix_expression postfix ->
                update result
                  (Semantic_source.postfix_operand postfix)
                  (match Semantic_source.postfix_operator postfix with
                  | Semantic_source.Post_increment -> Opcode.Ic__pp
                  | Semantic_source.Post_decrement -> Opcode.Ic__mm)
                  (Semantic_source.postfix_operator_origin postfix)
                  conversion
            | Semantic_source.Aggregate_offset_base_expression _
            | Semantic_source.Unresolved_expression
                ( Semantic_source.Offset_expression
                | Semantic_source.Postfix_cast_expression
                | Semantic_source.Call_expression ) -> unsupported := true)
        | Emit_index_stride step -> reversed := Index_stride step :: !reversed
        | Finish_index_address step ->
            reversed := Index_address step :: !reversed
        | Finish_materialize_array { result; operand; pointer_type; span } ->
            reversed :=
              Materialize_array { result; operand; pointer_type; span }
              :: !reversed
        | Finish_indirect_address { result; pointer } ->
            reversed := Indirect_address { result; pointer } :: !reversed
        | Finish_alias { result; operand } ->
            reversed := Alias { result; operand } :: !reversed
        | Finish_unary { result; opcode; span; operand; conversion } ->
            reversed :=
              Unary { result; opcode; span; operand; conversion } :: !reversed
        | Finish_cast { result; span; operand; was_parenthesized; conversion }
          ->
            reversed :=
              Cast { result; span; operand; was_parenthesized; conversion }
              :: !reversed
        | Finish_binary
            { result; opcode; span; left; right; conversion; operation_flags }
          ->
            reversed :=
              Binary
                {
                  result;
                  opcode;
                  span;
                  left;
                  right;
                  conversion;
                  operation_flags;
                }
              :: !reversed
        | Finish_chain_link
            { result; previous; middle; right; opcode; span; conversion } ->
            reversed :=
              Chain_link
                { result; previous; middle; right; opcode; span; conversion }
              :: !reversed)
  done;
  match (!error, !unsupported) with
  | Some item, _ -> Error [ item ]
  | None, true -> Ok Unsupported_plan
  | None, false -> Ok (Planned (List.rev !reversed))

let result_key result =
  result |> Semantic_result.result_id |> Semantic_result.Id.to_int

let find_lowered lowered result description =
  match Int_map.find_opt (result_key result) lowered with
  | Some node -> Ok node
  | None ->
      Error
        (metadata_error ?span:(result_span result)
           (Printf.sprintf "%s was not lowered before its parent" description))

let take_identity allocator span =
  if allocator.instruction = Int.max_int || allocator.value = Int.max_int then
    Error
      (lowering_error ?span "HCIRL0005"
         "cannot allocate another expression identity because the host integer \
          range is exhausted")
  else
    match
      ( Sequence.Instruction_id.of_int allocator.instruction,
        Sequence.Value_id.of_int allocator.value )
    with
    | Ok instruction_id, Ok value_id ->
        allocator.instruction <- allocator.instruction + 1;
        allocator.value <- allocator.value + 1;
        Ok (instruction_id, value_id)
    | Error item, _ | _, Error item -> Error item

let one_literal_description lowered =
  match
    lowered |> Literal.sequence |> Sequence.instructions
    |> List.map Sequence.description
  with
  | [ description ] -> Ok description
  | _ ->
      Error
        (metadata_error
           "numeric literal lowering did not produce exactly one instruction")

let lower_literal allocator result =
  let span = result_span result in
  match take_identity allocator span with
  | Error item -> Error item
  | Ok (instruction_id, value_id) -> (
      match Literal.lower_typed_result ~instruction_id ~value_id result with
      | Error (item :: _) -> Error item
      | Error [] ->
          Error
            (metadata_error ?span
               "numeric literal lowering failed without a diagnostic")
      | Ok Literal.Not_literal ->
          Error
            (metadata_error ?span
               "checked numeric literal was not accepted by literal lowering")
      | Ok (Literal.Lowered lowered) -> (
          match one_literal_description lowered with
          | Error item -> Error item
          | Ok description ->
              Ok
                ( description,
                  {
                    lowered_value = value_id;
                    lowered_type = Literal.result_type lowered;
                  } )))

let direct_function_address_description ~instruction_id ~opcode ~operands
    ~value_id ~result_type ~payload ~flags ~span : Sequence.description =
  {
    instruction_id;
    opcode;
    operands;
    result = Some { value_id };
    target_type = Some result_type;
    payload;
    flags;
    span = Some span;
  }

let lower_direct_function_address allocator ~span ~result_type ~symbol ~path
    ~conversion =
  let symbol_payload = Some (Sequence.Symbol symbol) in
  let final_flags = conversion_flags conversion in
  match path with
  | Semantic_source.Jit_immediate | Semantic_source.Aot_absolute -> (
      match take_identity allocator (Some span) with
      | Error _ as error -> error
      | Ok (instruction_id, value_id) ->
          let opcode =
            match path with
            | Semantic_source.Jit_immediate -> Opcode.Ic_imm_i64
            | Semantic_source.Aot_absolute -> Opcode.Ic_abs_addr
            | Semantic_source.Jit_extern_slot
            | Semantic_source.Reject_aot_extern
            | Semantic_source.Reject_aot_import
            | Semantic_source.Reject_internal -> assert false
          in
          Ok
            ( [
                direct_function_address_description ~instruction_id ~opcode
                  ~operands:[] ~value_id ~result_type ~payload:symbol_payload
                  ~flags:final_flags ~span;
              ],
              { lowered_value = value_id; lowered_type = result_type } ))
  | Semantic_source.Jit_extern_slot -> (
      match take_identity allocator (Some span) with
      | Error _ as error -> error
      | Ok (slot_instruction_id, slot_value_id) -> (
          match take_identity allocator (Some span) with
          | Error _ as error -> error
          | Ok (deref_instruction_id, deref_value_id) ->
              Ok
                ( [
                    direct_function_address_description
                      ~instruction_id:slot_instruction_id
                      ~opcode:Opcode.Ic_imm_i64 ~operands:[]
                      ~value_id:slot_value_id ~result_type
                      ~payload:symbol_payload ~flags:0L ~span;
                    direct_function_address_description
                      ~instruction_id:deref_instruction_id
                      ~opcode:Opcode.Ic_deref ~operands:[ slot_value_id ]
                      ~value_id:deref_value_id ~result_type ~payload:None
                      ~flags:final_flags ~span;
                  ],
                  { lowered_value = deref_value_id; lowered_type = result_type }
                )))
  | Semantic_source.Reject_aot_extern
  | Semantic_source.Reject_aot_import
  | Semantic_source.Reject_internal ->
      Error
        (metadata_error ~span
           "rejected direct function address path reached IR emission")

let checked_call_fragment allocator result conversion sequence =
  let span = result_span result in
  let invalid message = metadata_error ?span message in
  let descriptions =
    Sequence.instructions sequence |> List.map Sequence.description
  in
  let instruction = ref allocator.instruction in
  let value = ref allocator.value in
  let error = ref None in
  let advance counter =
    if !counter = Int.max_int then
      error :=
        Some
          (lowering_error ?span "HCIRL0005"
             "call fragment exhausts expression identities")
    else incr counter
  in
  List.iter
    (fun (item : Sequence.description) ->
      if Option.is_none !error then (
        if Sequence.Instruction_id.to_int item.instruction_id <> !instruction
        then
          error :=
            Some
              (invalid
                 "call fragment instruction identities are not consecutive")
        else advance instruction;
        match item.result with
        | Some produced ->
            if Sequence.Value_id.to_int produced.value_id <> !value then
              error :=
                Some
                  (invalid "call fragment value identities are not consecutive")
            else advance value
        | None -> ()))
    descriptions;
  match !error with
  | Some error -> Error error
  | None -> (
      match
        (descriptions, List.rev descriptions, Semantic_result.result_type result)
      with
      | first :: _, last :: rest, Some expected -> (
          match
            (first.payload, last.payload, last.result, last.target_type)
          with
          | ( Some (Sequence.Symbol first_symbol),
              Some (Sequence.Symbol last_symbol),
              Some produced,
              Some actual )
            when first.opcode = Opcode.Ic_call_start
                 && last.opcode = Opcode.Ic_call_end
                 && first_symbol == last_symbol
                 && Type.equal actual expected && last.span = span
                 && last.flags = 0L ->
              allocator.instruction <- !instruction;
              allocator.value <- !value;
              let last =
                { last with Sequence.flags = conversion_flags conversion }
              in
              Ok
                ( List.rev (last :: rest),
                  { lowered_value = produced.value_id; lowered_type = actual }
                )
          | _ ->
              Error
                (invalid
                   "call fragment does not end with its checked call result"))
      | _ -> Error (invalid "call fragment has no checked call result"))

let emit_plan ?lower_call ~instruction_id ~value_id nodes =
  let allocator =
    {
      instruction = Sequence.Instruction_id.to_int instruction_id;
      value = Sequence.Value_id.to_int value_id;
    }
  in
  let lowered = ref Int_map.empty in
  let index_strides = ref Int_map.empty in
  let comparison_domains = ref Int_map.empty in
  let descriptions_rev = ref [] in
  let error = ref None in
  let unsupported = ref false in
  let emit_index_value ~opcode ~operands ~target_type ~payload ~span =
    match take_identity allocator (Some span) with
    | Error _ as error -> error
    | Ok (instruction_id, value_id) ->
        let description : Sequence.description =
          {
            instruction_id;
            opcode;
            operands;
            result = Some { value_id };
            target_type = Some target_type;
            payload;
            flags = 0L;
            span = Some span;
          }
        in
        descriptions_rev := description :: !descriptions_rev;
        Ok { lowered_value = value_id; lowered_type = target_type }
  in
  let storage_address address =
    match
      ( Sequence.Instruction_id.of_int allocator.instruction,
        Sequence.Value_id.of_int allocator.value )
    with
    | Error item, _ | _, Error item -> Error item
    | Ok instruction_id, Ok value_id -> (
        let fragment =
          match address with
          | Frame_slot address ->
              Frame_address_lowering.lower_prepared ~instruction_id ~value_id
                address
              |> Result.map (fun result ->
                  ( Frame_address_lowering.sequence result,
                    Frame_address_lowering.next_instruction_id result,
                    Frame_address_lowering.next_value_id result,
                    Frame_address_lowering.result_value result,
                    Frame_address_lowering.result_type result ))
          | Global_slot address ->
              Global_address_lowering.lower_prepared ~instruction_id ~value_id
                address
              |> Result.map (fun result ->
                  ( Global_address_lowering.sequence result,
                    Global_address_lowering.next_instruction_id result,
                    Global_address_lowering.next_value_id result,
                    Global_address_lowering.result_value result,
                    Global_address_lowering.result_type result ))
        in
        match fragment with
        | Error (item :: _) -> Error item
        | Error [] ->
            Error (metadata_error "prepared storage address emission failed")
        | Ok
            (sequence, next_instruction, next_value, lowered_value, lowered_type)
          ->
            allocator.instruction <-
              Sequence.Instruction_id.to_int next_instruction;
            allocator.value <- Sequence.Value_id.to_int next_value;
            Ok
              ( Sequence.instructions sequence |> List.map Sequence.description,
                { lowered_value; lowered_type } ))
  in
  List.iter
    (fun node ->
      if Option.is_none !error && not !unsupported then
        match node with
        | Index_stride step -> (
            match
              emit_index_value ~opcode:Opcode.Ic_imm_i64 ~operands:[]
                ~target_type:step.index_type
                ~payload:(Some (Sequence.Integer step.index_stride))
                ~span:step.index_span
            with
            | Error item -> error := Some item
            | Ok node ->
                index_strides :=
                  Int_map.add
                    (result_key step.indexed_result)
                    node !index_strides)
        | Index_address step -> (
            let ( let* ) = Result.bind in
            let emitted =
              let* base =
                find_lowered !lowered step.indexed_base "index base"
              in
              let* index =
                find_lowered !lowered step.index_value "index value"
              in
              let* stride =
                find_lowered !index_strides step.indexed_result "index stride"
              in
              if not (Type.equal base.lowered_type step.index_type) then
                Error
                  (metadata_error ~span:step.index_span
                     "indexed base has a mismatched pointer type")
              else
                let* scaled =
                  emit_index_value ~opcode:Opcode.Ic_mul
                    ~operands:[ stride.lowered_value; index.lowered_value ]
                    ~target_type:step.index_type ~payload:None
                    ~span:step.index_span
                in
                emit_index_value ~opcode:Opcode.Ic_add
                  ~operands:[ base.lowered_value; scaled.lowered_value ]
                  ~target_type:step.index_type ~payload:None
                  ~span:step.index_span
            in
            match emitted with
            | Error item -> error := Some item
            | Ok node ->
                lowered :=
                  Int_map.add (result_key step.indexed_result) node !lowered)
        | Materialize_array { result; operand; pointer_type; span } -> (
            match find_lowered !lowered operand "array address" with
            | Error item -> error := Some item
            | Ok address when Type.equal address.lowered_type pointer_type -> (
                match
                  emit_index_value ~opcode:Opcode.Ic_addr
                    ~operands:[ address.lowered_value ]
                    ~target_type:pointer_type ~payload:None ~span
                with
                | Error item -> error := Some item
                | Ok node ->
                    lowered := Int_map.add (result_key result) node !lowered)
            | Ok _ ->
                error :=
                  Some
                    (metadata_error ~span
                       "array materialization changes its checked pointer type")
            )
        | Call { result; conversion } -> (
            match
              ( lower_call,
                Sequence.Instruction_id.of_int allocator.instruction,
                Sequence.Value_id.of_int allocator.value )
            with
            | _, Error item, _ | _, _, Error item -> error := Some item
            | None, _, _ -> unsupported := true
            | Some lower_call, Ok instruction_id, Ok value_id -> (
                match lower_call ~instruction_id ~value_id result with
                | Error (item :: _) -> error := Some item
                | Error [] ->
                    error :=
                      Some
                        (metadata_error
                           "call callback failed without a diagnostic")
                | Ok None -> unsupported := true
                | Ok (Some sequence) -> (
                    match
                      checked_call_fragment allocator result conversion sequence
                    with
                    | Error item -> error := Some item
                    | Ok (descriptions, node) ->
                        descriptions_rev :=
                          List.rev_append descriptions !descriptions_rev;
                        lowered := Int_map.add (result_key result) node !lowered
                    )))
        | Storage_address { result; address } -> (
            match storage_address address with
            | Error item -> error := Some item
            | Ok (descriptions, node) ->
                descriptions_rev :=
                  List.rev_append descriptions !descriptions_rev;
                lowered := Int_map.add (result_key result) node !lowered)
        | Indirect_address { result; pointer } -> (
            match
              ( find_lowered !lowered pointer "indirect destination",
                Semantic_result.result_type result )
            with
            | Ok node, Some type_ -> (
                match Type.pointer_to type_ with
                | Ok expected when Type.equal expected node.lowered_type ->
                    lowered := Int_map.add (result_key result) node !lowered
                | _ ->
                    error :=
                      Some
                        (metadata_error ?span:(result_span result)
                           "indirect destination has a mismatched pointer type")
                )
            | Error item, _ -> error := Some item
            | _ ->
                error :=
                  Some
                    (metadata_error "indirect destination has no checked type"))
        | Storage_load { result; address; result_type; span; conversion } -> (
            match storage_address address with
            | Error item -> error := Some item
            | Ok (descriptions, address) -> (
                match take_identity allocator (Some span) with
                | Error item -> error := Some item
                | Ok (instruction_id, value_id) ->
                    let description : Sequence.description =
                      {
                        instruction_id;
                        opcode = Opcode.Ic_deref;
                        operands = [ address.lowered_value ];
                        result = Some { value_id };
                        target_type = Some result_type;
                        payload = None;
                        flags = conversion_flags conversion;
                        span = Some span;
                      }
                    in
                    descriptions_rev :=
                      description
                      :: List.rev_append descriptions !descriptions_rev;
                    lowered :=
                      Int_map.add (result_key result)
                        { lowered_value = value_id; lowered_type = result_type }
                        !lowered))
        | Literal { result; conversion } -> (
            match lower_literal allocator result with
            | Error item -> error := Some item
            | Ok (description, lowered_node) ->
                let description =
                  {
                    description with
                    flags =
                      Int64.logor description.flags
                        (conversion_flags conversion);
                  }
                in
                descriptions_rev := description :: !descriptions_rev;
                lowered := Int_map.add (result_key result) lowered_node !lowered
            )
        | Current_position { result; span; result_type; conversion } -> (
            match take_identity allocator (Some span) with
            | Error item -> error := Some item
            | Ok (instruction_id, value_id) ->
                let description : Sequence.description =
                  {
                    instruction_id;
                    opcode = Opcode.Ic_rip;
                    operands = [];
                    result = Some { value_id };
                    target_type = Some result_type;
                    payload = None;
                    flags = conversion_flags conversion;
                    span = Some span;
                  }
                in
                descriptions_rev := description :: !descriptions_rev;
                lowered :=
                  Int_map.add (result_key result)
                    { lowered_value = value_id; lowered_type = result_type }
                    !lowered)
        | Integer_constant { result; span; result_type; value; conversion } -> (
            match take_identity allocator (Some span) with
            | Error item -> error := Some item
            | Ok (instruction_id, value_id) ->
                let description : Sequence.description =
                  {
                    instruction_id;
                    opcode = Opcode.Ic_imm_i64;
                    operands = [];
                    result = Some { value_id };
                    target_type = Some result_type;
                    payload = Some (Sequence.Integer value);
                    flags = conversion_flags conversion;
                    span = Some span;
                  }
                in
                descriptions_rev := description :: !descriptions_rev;
                lowered :=
                  Int_map.add (result_key result)
                    { lowered_value = value_id; lowered_type = result_type }
                    !lowered)
        | Direct_function_address
            { result; span; result_type; symbol; path; conversion } -> (
            match
              lower_direct_function_address allocator ~span ~result_type ~symbol
                ~path ~conversion
            with
            | Error item -> error := Some item
            | Ok (descriptions, lowered_node) ->
                descriptions_rev :=
                  List.rev_append descriptions !descriptions_rev;
                lowered := Int_map.add (result_key result) lowered_node !lowered
            )
        | Alias { result; operand } -> (
            match find_lowered !lowered operand "transparent operand" with
            | Error item -> error := Some item
            | Ok lowered_operand -> (
                match Semantic_result.result_type result with
                | None ->
                    error :=
                      Some
                        (metadata_error ?span:(result_span result)
                           "transparent expression does not have a checked \
                            result type")
                | Some result_type ->
                    if not (Type.equal result_type lowered_operand.lowered_type)
                    then
                      error :=
                        Some
                          (metadata_error ?span:(result_span result)
                             "transparent expression changes its checked \
                              operand type")
                    else
                      lowered :=
                        Int_map.add (result_key result) lowered_operand !lowered
                ))
        | Unary { result; opcode; span; operand; conversion } -> (
            match
              ( find_lowered !lowered operand "unary operand",
                Semantic_result.result_type result )
            with
            | Error item, _ -> error := Some item
            | _, None ->
                error :=
                  Some
                    (metadata_error ~span
                       "unary expression does not have a checked result type")
            | Ok operand_node, Some result_type -> (
                match take_identity allocator (Some span) with
                | Error item -> error := Some item
                | Ok (instruction_id, value_id) ->
                    let description : Sequence.description =
                      {
                        instruction_id;
                        opcode;
                        operands = [ operand_node.lowered_value ];
                        result = Some { value_id };
                        target_type = Some result_type;
                        payload = None;
                        flags = conversion_flags conversion;
                        span = Some span;
                      }
                    in
                    descriptions_rev := description :: !descriptions_rev;
                    lowered :=
                      Int_map.add (result_key result)
                        { lowered_value = value_id; lowered_type = result_type }
                        !lowered))
        | Cast { result; span; operand; was_parenthesized; conversion } -> (
            match
              ( find_lowered !lowered operand "postfix-cast operand",
                Semantic_result.result_type result )
            with
            | Error item, _ -> error := Some item
            | _, None ->
                error :=
                  Some
                    (metadata_error ~span
                       "postfix cast does not have a checked target type")
            | Ok operand_node, Some result_type -> (
                match take_identity allocator (Some span) with
                | Error item -> error := Some item
                | Ok (instruction_id, value_id) ->
                    let description : Sequence.description =
                      {
                        instruction_id;
                        opcode = Opcode.Ic_holyc_typecast;
                        operands = [ operand_node.lowered_value ];
                        result = Some { value_id };
                        target_type = Some result_type;
                        payload =
                          Some
                            (Sequence.Integer
                               (if was_parenthesized then 1L else 0L));
                        flags = conversion_flags conversion;
                        span = Some span;
                      }
                    in
                    descriptions_rev := description :: !descriptions_rev;
                    lowered :=
                      Int_map.add (result_key result)
                        { lowered_value = value_id; lowered_type = result_type }
                        !lowered))
        | Binary
            { result; opcode; span; left; right; conversion; operation_flags }
          -> (
            match
              ( find_lowered !lowered left "left binary operand",
                find_lowered !lowered right "right binary operand",
                Semantic_result.result_type result )
            with
            | Error item, _, _ | _, Error item, _ -> error := Some item
            | _, _, None ->
                error :=
                  Some
                    (metadata_error ~span
                       "binary expression does not have a checked result type")
            | Ok left_node, Ok right_node, Some result_type -> (
                match take_identity allocator (Some span) with
                | Error item -> error := Some item
                | Ok (instruction_id, value_id) ->
                    let description : Sequence.description =
                      {
                        instruction_id;
                        opcode;
                        operands =
                          [ left_node.lowered_value; right_node.lowered_value ];
                        result = Some { value_id };
                        target_type = Some result_type;
                        payload = None;
                        flags =
                          Int64.logor operation_flags
                            (conversion_flags conversion);
                        span = Some span;
                      }
                    in
                    if accepted_f64_comparison_opcode opcode then
                      comparison_domains :=
                        Int_map.add (result_key result)
                          (unsigned_integer_type left_node.lowered_type
                          || unsigned_integer_type right_node.lowered_type)
                          !comparison_domains;
                    descriptions_rev := description :: !descriptions_rev;
                    lowered :=
                      Int_map.add (result_key result)
                        { lowered_value = value_id; lowered_type = result_type }
                        !lowered))
        | Chain_link
            { result; previous; middle; right; opcode; span; conversion } -> (
            match
              ( find_lowered !lowered previous "previous comparison",
                find_lowered !lowered middle "shared comparison operand",
                find_lowered !lowered right "right comparison operand",
                Semantic_result.result_type result,
                Int_map.find_opt (result_key previous) !comparison_domains )
            with
            | Error item, _, _, _, _
            | _, Error item, _, _, _
            | _, _, Error item, _, _ -> error := Some item
            | _, _, _, None, _ | _, _, _, _, None ->
                error :=
                  Some
                    (metadata_error ~span
                       "comparison chain does not have its checked type and \
                        prior domain")
            | ( Ok previous_node,
                Ok middle_node,
                Ok right_node,
                Some result_type,
                Some previous_unsigned ) -> (
                let shared =
                  (* OptPass012.HC:141-150,809-820 carries the promoted class
                     through PUSH_CMP. Keep the original producer's arithmetic
                     intact and reinterpret only its shared word. *)
                  if
                    previous_unsigned
                    && not (unsigned_integer_type middle_node.lowered_type)
                  then
                    match take_identity allocator (Some span) with
                    | Error item -> Error item
                    | Ok (instruction_id, value_id) -> (
                        match
                          Type.make_primitive ~form:Type.Internal_storage
                            ~primitive:Sema.Primitive_type.U64 ~pointer_depth:0
                        with
                        | Error message -> Error (metadata_error ~span message)
                        | Ok type_ ->
                            let description : Sequence.description =
                              {
                                instruction_id;
                                opcode = Opcode.Ic_holyc_typecast;
                                operands = [ middle_node.lowered_value ];
                                result = Some { value_id };
                                target_type = Some type_;
                                payload = Some (Sequence.Integer 0L);
                                flags = 0L;
                                span = Some span;
                              }
                            in
                            Ok
                              ( [ description ],
                                {
                                  lowered_value = value_id;
                                  lowered_type = type_;
                                } ))
                  else Ok ([], middle_node)
                in
                match shared with
                | Error item -> error := Some item
                | Ok (views, middle_node) -> (
                    match take_identity allocator (Some span) with
                    | Error item -> error := Some item
                    | Ok (comparison_id, comparison_value) -> (
                        match take_identity allocator (Some span) with
                        | Error item -> error := Some item
                        | Ok (instruction_id, value_id) ->
                            let comparison : Sequence.description =
                              {
                                instruction_id = comparison_id;
                                opcode;
                                operands =
                                  [
                                    middle_node.lowered_value;
                                    right_node.lowered_value;
                                  ];
                                result = Some { value_id = comparison_value };
                                target_type = Some result_type;
                                payload = None;
                                flags = 0L;
                                span = Some span;
                              }
                            in
                            let combination : Sequence.description =
                              {
                                instruction_id;
                                opcode = Opcode.Ic_and_and;
                                operands =
                                  [
                                    previous_node.lowered_value;
                                    comparison_value;
                                  ];
                                result = Some { value_id };
                                target_type = Some result_type;
                                payload = None;
                                flags = conversion_flags conversion;
                                span = Some span;
                              }
                            in
                            descriptions_rev :=
                              combination :: comparison
                              :: List.rev_append views !descriptions_rev;
                            comparison_domains :=
                              Int_map.add (result_key result)
                                (previous_unsigned
                                || unsigned_integer_type right_node.lowered_type
                                )
                                !comparison_domains;
                            lowered :=
                              Int_map.add (result_key result)
                                {
                                  lowered_value = value_id;
                                  lowered_type = result_type;
                                }
                                !lowered)))))
    nodes;
  match !error with
  | Some item -> Error [ item ]
  | None when !unsupported -> Ok None
  | None -> (
      match List.rev !descriptions_rev with
      | [] ->
          Error
            [ metadata_error "expression lowering produced no instructions" ]
      | descriptions -> (
          match Sequence.create descriptions with
          | Error items -> Error items
          | Ok sequence -> (
              let root =
                match List.rev nodes with
                | Call { result; _ } :: _
                | Storage_address { result; _ } :: _
                | Indirect_address { result; _ } :: _
                | Index_stride { indexed_result = result; _ } :: _
                | Index_address { indexed_result = result; _ } :: _
                | Materialize_array { result; _ } :: _
                | Storage_load { result; _ } :: _
                | Literal { result; _ } :: _
                | Current_position { result; _ } :: _
                | Integer_constant { result; _ } :: _
                | Direct_function_address { result; _ } :: _
                | Alias { result; _ } :: _
                | Unary { result; _ } :: _
                | Cast { result; _ } :: _
                | Binary { result; _ } :: _
                | Chain_link { result; _ } :: _ -> result
                | [] -> assert false
              in
              match find_lowered !lowered root "expression result" with
              | Error item -> Error [ item ]
              | Ok lowered_root -> (
                  match
                    ( Sequence.Instruction_id.of_int allocator.instruction,
                      Sequence.Value_id.of_int allocator.value )
                  with
                  | Ok next_instruction_id_, Ok next_value_id_ ->
                      Ok
                        (Some
                           {
                             sequence_ = sequence;
                             result_value_ = lowered_root.lowered_value;
                             result_type_ = lowered_root.lowered_type;
                             next_instruction_id_;
                             next_value_id_;
                           })
                  | Error item, _ | _, Error item -> Error [ item ]))))

let lower_typed_result ?frame ?globals ?lower_call ~instruction_id ~value_id
    result =
  match
    plan ?frame ?globals ~allow_calls:(Option.is_some lower_call) result
  with
  | Error items -> Error items
  | Ok Unsupported_plan -> Ok Unsupported_expression
  | Ok (Planned nodes) ->
      emit_plan ?lower_call ~instruction_id ~value_id nodes
      |> Result.map (function
        | Some t -> Lowered t
        | None -> Unsupported_expression)

let sequence lowered = lowered.sequence_

let lower_store_initializer ?frame ?globals ?lower_call ~lower_address
    ~target_type ~span ~instruction_id ~value_id value =
  let ( let* ) = Result.bind in
  let target_is_word =
    Option.is_some (Integer_scalar_storage.of_type target_type)
  in
  let* value_type =
    checked_frame_value value |> Result.map_error (fun error -> [ error ])
  in
  match value_type with
  | Checked_type value_type
    when (target_is_word && Type.pointer_depth value_type = 0)
         || Option.is_some frame
            && scalar_pointer_type target_type
            && Type.compatible_u8_pointer target_type value_type -> (
      let* address_sequence, address_value, next_instruction, next_value =
        lower_address ~instruction_id ~value_id
      in
      let* lowered =
        lower_typed_result ?frame ?globals ?lower_call
          ~instruction_id:next_instruction ~value_id:next_value value
      in
      match lowered with
      | Unsupported_expression -> Ok Unsupported_expression
      | Lowered value ->
          let allocator =
            {
              instruction =
                Sequence.Instruction_id.to_int value.next_instruction_id_;
              value = Sequence.Value_id.to_int value.next_value_id_;
            }
          in
          let* instruction_id, value_id =
            take_identity allocator span
            |> Result.map_error (fun error -> [ error ])
          in
          let store : Sequence.description =
            {
              instruction_id;
              opcode = Opcode.Ic_assign;
              operands = [ address_value; value.result_value_ ];
              result = Some { value_id };
              target_type = Some target_type;
              payload = None;
              flags = 0L;
              span;
            }
          in
          let descriptions sequence =
            Sequence.instructions sequence |> List.map Sequence.description
          in
          let* sequence_ =
            Sequence.create
              (descriptions address_sequence
              @ descriptions value.sequence_
              @ [ store ])
          in
          let* next_instruction_id_ =
            Sequence.Instruction_id.of_int allocator.instruction
            |> Result.map_error (fun error -> [ error ])
          in
          let* next_value_id_ =
            Sequence.Value_id.of_int allocator.value
            |> Result.map_error (fun error -> [ error ])
          in
          Ok
            (Lowered
               {
                 sequence_;
                 result_value_ = value_id;
                 result_type_ = target_type;
                 next_instruction_id_;
                 next_value_id_;
               }))
  | _ -> Ok Unsupported_expression

let lower_initializer ~frame ?globals ?lower_call ~instruction_id ~value_id
    initial =
  let ( let* ) = Result.bind in
  let* address = Frame_address_lowering.prepare_initializer ~frame initial in
  match address with
  | None -> Ok Unsupported_expression
  | Some prepared ->
      let lower_address ~instruction_id ~value_id =
        let* address =
          Frame_address_lowering.lower_prepared ~instruction_id ~value_id
            prepared
        in
        Ok
          ( Frame_address_lowering.sequence address,
            Frame_address_lowering.result_value address,
            Frame_address_lowering.next_instruction_id address,
            Frame_address_lowering.next_value_id address )
      in
      let span =
        match
          initial |> Semantic_result.initializer_source
          |> Semantic_source.initializer_origin
        with
        | Sema.Symbol.Source_location location -> Some location.span
        | _ -> None
      in
      lower_store_initializer ~frame ?globals ?lower_call ~lower_address
        ~target_type:(Semantic_result.initializer_target_type initial)
        ~span ~instruction_id ~value_id
        (Semantic_result.initializer_value initial)

let lower_global_initializer ~globals ?lower_call ~instruction_id ~value_id root
    =
  let ( let* ) = Result.bind in
  let* prepared = Global_address_lowering.prepare_initializer ~globals root in
  let* target_type, span =
    match
      root |> Semantic_result.top_level_root_source
      |> Sema.Top_level_expression_tree.root_role
    with
    | Sema.Top_level_expression_tree.Global_initializer owner ->
        let type_ =
          owner |> Sema.Global_initializer_binding.global_record
          |> Sema.Global_resolution.global_record_global
          |> Sema.Global_type_resolution.global_type_reference
          |> Sema.Type_reference.resolved_type
        in
        let span =
          match
            Sema.Global_initializer_binding.global_initializer_origin owner
          with
          | Some (Sema.Symbol.Source_location location) -> Some location.span
          | _ -> None
        in
        Ok (type_, span)
    | _ ->
        Error [ metadata_error "global initializer has no declaration owner" ]
  in
  let lower_address ~instruction_id ~value_id =
    let* address =
      Global_address_lowering.lower_prepared ~instruction_id ~value_id prepared
    in
    Ok
      ( Global_address_lowering.sequence address,
        Global_address_lowering.result_value address,
        Global_address_lowering.next_instruction_id address,
        Global_address_lowering.next_value_id address )
  in
  lower_store_initializer ~globals ?lower_call ~lower_address ~target_type ~span
    ~instruction_id ~value_id
    (Semantic_result.top_level_root_value root)

let lower_fragment_initializer ?lower_call ~instruction_id ~value_id destination
    =
  let ( let* ) = Result.bind in
  let module Destination = Initializer_fragment_destination in
  let* prepared =
    Global_address_lowering.prepare_fragment_initializer destination
  in
  let lower_address ~instruction_id ~value_id =
    let* address =
      Global_address_lowering.lower_prepared ~instruction_id ~value_id prepared
    in
    Ok
      ( Global_address_lowering.sequence address,
        Global_address_lowering.result_value address,
        Global_address_lowering.next_instruction_id address,
        Global_address_lowering.next_value_id address )
  in
  lower_store_initializer
    ~globals:(Destination.globals destination)
    ?lower_call ~lower_address
    ~target_type:
      (Integer_globals.storage_type (Destination.storage destination))
    ~span:(Some (Destination.span destination))
    ~instruction_id ~value_id
    (Semantic_result.top_level_root_value (Destination.root destination))

let lower_static_initializer ~globals ?root ?lower_call ~instruction_id
    ~value_id slot =
  let ( let* ) = Result.bind in
  let root =
    match root with
    | Some _ -> root
    | None -> Integer_globals.static_initializer slot
  in
  match root with
  | None ->
      Error
        [ metadata_error "static initializer has no checked declaration root" ]
  | Some root ->
      let* prepared =
        Global_address_lowering.prepare_static_initializer ~globals slot root
      in
      let lower_address ~instruction_id ~value_id =
        let* address =
          Global_address_lowering.lower_prepared ~instruction_id ~value_id
            prepared
        in
        Ok
          ( Global_address_lowering.sequence address,
            Global_address_lowering.result_value address,
            Global_address_lowering.next_instruction_id address,
            Global_address_lowering.next_value_id address )
      in
      let span =
        match
          Semantic_result.initializer_source root
          |> Semantic_source.initializer_origin
        with
        | Sema.Symbol.Source_location location -> Some location.span
        | _ -> None
      in
      lower_store_initializer
        ~frame:(Integer_globals.static_frame slot)
        ~globals ?lower_call ~lower_address
        ~target_type:(Semantic_result.initializer_target_type root)
        ~span ~instruction_id ~value_id
        (Semantic_result.initializer_value root)

let result_value lowered = lowered.result_value_
let result_type lowered = lowered.result_type_
let next_instruction_id lowered = lowered.next_instruction_id_
let next_value_id lowered = lowered.next_value_id_

let human lowered =
  Printf.sprintf
    "holyc-ir-expression-v1 reference=%s\n\
     result=%%v%d result-type=%s next-instruction=%d next-value=%d\n\
     %s"
    reference_commit
    (Sequence.Value_id.to_int lowered.result_value_)
    (Sequence.type_name lowered.result_type_)
    (Sequence.Instruction_id.to_int lowered.next_instruction_id_)
    (Sequence.Value_id.to_int lowered.next_value_id_)
    (Sequence.human_body lowered.sequence_)
