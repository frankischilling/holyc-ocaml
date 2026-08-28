module Sequence = Instruction_sequence
module Literal = Literal_lowering
module Semantic_result = Sema.Function_call_expression_result
module Semantic_source = Sema.Function_call_resolution
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

type plan_node =
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

type task =
  | Visit of {
      result : Semantic_result.expression_result;
      conversion : result_conversion;
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

let type_equal left right =
  Type.pointer_depth left = Type.pointer_depth right
  &&
  match (Type.base left, Type.base right) with
  | ( Type.Primitive (left_form, left_primitive),
      Type.Primitive (right_form, right_primitive) ) ->
      left_form = right_form
      && Sema.Primitive_type.equal left_primitive right_primitive
  | Type.Aggregate left_symbol, Type.Aggregate right_symbol ->
      Sema.Symbol.Id.equal
        (Sema.Symbol.id left_symbol)
        (Sema.Symbol.id right_symbol)
  | Type.Primitive _, Type.Aggregate _ | Type.Aggregate _, Type.Primitive _ ->
      false

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

let checked_operand result expected_source description =
  match Semantic_result.result_operand result with
  | None ->
      Error
        (metadata_error ?span:(result_span result)
           (Printf.sprintf
              "typed semantic %s does not retain its checked operand"
              description))
  | Some operand when Semantic_result.result_source operand != expected_source
    ->
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
    when Semantic_result.result_source left
         != Semantic_source.binary_left binary
         || Semantic_result.result_source right
            != Semantic_source.binary_right binary ->
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

let internal_i64 type_ =
  Type.pointer_depth type_ = 0
  &&
  match Type.base type_ with
  | Type.Primitive (Type.Internal_storage, Sema.Primitive_type.I64) -> true
  | Type.Primitive _ | Type.Aggregate _ -> false

let checked_numeric_unary_types result opcode operand =
  match
    (Semantic_result.result_type result, Semantic_result.result_type operand)
  with
  | None, _ | _, None ->
      Error
        (metadata_error ?span:(result_span result)
           "typed semantic unary expression does not have complete checked \
            types")
  | Some result_type, Some operand_type ->
      let valid =
        match opcode with
        | Opcode.Ic_unary_minus | Opcode.Ic_not ->
            type_equal result_type operand_type
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
        | Opcode.Ic_deref -> (
            match Type.dereference operand_type with
            | Ok type_ -> Ok type_
            | Error _ -> Ok operand_type)
        | Opcode.Ic_addr -> Type.pointer_to operand_type
        | _ -> Error "not a pointer prefix opcode"
      in
      match expected with
      | Error message ->
          Error (metadata_error ?span:(result_span result) message)
      | Ok expected_type when type_equal result_type expected_type -> Ok ()
      | Ok _ ->
          Error
            (metadata_error ?span:(result_span result)
               "typed semantic pointer result type does not match the audited \
                operator rule"))

let checked_alias_types result operand =
  match
    (Semantic_result.result_type result, Semantic_result.result_type operand)
  with
  | Some result_type, Some operand_type when type_equal result_type operand_type
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
      if not (type_equal result_type target_type) then
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

let plan root =
  let root_conversion = requested_conversion root in
  let pending = ref [] in
  let reversed = ref [] in
  let unsupported = ref false in
  let error = ref None in
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
            | Semantic_source.Parenthesized_expression source -> (
                match
                  checked_operand result source "parenthesized expression"
                with
                | Error item -> error := Some item
                | Ok operand ->
                    pending :=
                      Visit { result = operand; conversion }
                      :: Finish_alias { result; operand }
                      :: !pending)
            | Semantic_source.Prefix_expression prefix -> (
                let source_operand = Semantic_source.prefix_operand prefix in
                match Semantic_source.prefix_operator prefix with
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
                        match validate_pointer_unary result opcode operand with
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
                                  { result = operand; conversion = Keep_result }
                                :: Finish_unary
                                     {
                                       result;
                                       opcode;
                                       span;
                                       operand;
                                       conversion;
                                     }
                                :: !pending))
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
                if not (accepted_binary_opcode opcode) then unsupported := true
                else
                  match
                    ( checked_binary_operands result binary,
                      binary_span result binary )
                  with
                  | Error item, _ | _, Error item -> error := Some item
                  | Ok (left, right), Ok span -> (
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
                              { result = left; conversion = left_conversion }
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
                            :: !pending))
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
            | Semantic_source.Postfix_expression _
            | Semantic_source.Index_expression _
            | Semantic_source.Bound_identifier_expression _
            | Semantic_source.Aggregate_offset_base_expression _
            | Semantic_source.Top_level_bound_identifier_expression _
            | Semantic_source.Unresolved_expression
                ( Semantic_source.Identifier_expression
                | Semantic_source.Offset_expression
                | Semantic_source.Postfix_cast_expression
                | Semantic_source.Call_expression ) -> unsupported := true)
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

let emit_plan ~instruction_id ~value_id nodes =
  let allocator =
    {
      instruction = Sequence.Instruction_id.to_int instruction_id;
      value = Sequence.Value_id.to_int value_id;
    }
  in
  let lowered = ref Int_map.empty in
  let descriptions_rev = ref [] in
  let error = ref None in
  List.iter
    (fun node ->
      if Option.is_none !error then
        match node with
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
                    if not (type_equal result_type lowered_operand.lowered_type)
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
                    descriptions_rev := description :: !descriptions_rev;
                    lowered :=
                      Int_map.add (result_key result)
                        { lowered_value = value_id; lowered_type = result_type }
                        !lowered)))
    nodes;
  match !error with
  | Some item -> Error [ item ]
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
                | Literal { result; _ } :: _
                | Current_position { result; _ } :: _
                | Integer_constant { result; _ } :: _
                | Alias { result; _ } :: _
                | Unary { result; _ } :: _
                | Cast { result; _ } :: _
                | Binary { result; _ } :: _ -> result
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
                        {
                          sequence_ = sequence;
                          result_value_ = lowered_root.lowered_value;
                          result_type_ = lowered_root.lowered_type;
                          next_instruction_id_;
                          next_value_id_;
                        }
                  | Error item, _ | _, Error item -> Error [ item ]))))

let lower_typed_result ~instruction_id ~value_id result =
  match plan result with
  | Error items -> Error items
  | Ok Unsupported_plan -> Ok Unsupported_expression
  | Ok (Planned nodes) ->
      emit_plan ~instruction_id ~value_id nodes
      |> Result.map (fun t -> Lowered t)

let sequence lowered = lowered.sequence_
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
