module Typed = Sema.Function_call_expression_result

type numeric_conversion = Same_class | To_f64 | To_integer

let floating_destination scalar =
  Option.bind scalar (fun scalar ->
      let type_ = Sema.Initializer_type.scalar_value_type scalar in
      if
        Sema.Initializer_type.scalar_byte_size scalar = 8
        && Sema.Type.is_f64 type_
      then Some type_
      else None)

let expression_is_f64 expression =
  Typed.result_array_rank expression = 0
  && Typed.result_class expression = Typed.F64_result
  && Option.fold ~none:false ~some:Sema.Type.is_f64
       (Typed.result_value_type expression)

let same_f64 ~scalar expression =
  expression_is_f64 expression && Option.is_some (floating_destination scalar)

(* A U0 expression can supply effects to a zero-byte initializer leaf. It does
   not supply a word, and cannot authorize a later nonzero copy. *)
let is_void value =
  Typed.result_array_rank value = 0
  && Typed.result_class value = Typed.Integer_result
  &&
  match Typed.result_value_type value with
  | Some type_ -> (
      Sema.Type.pointer_depth type_ = 0
      &&
      match Sema.Type.base type_ with
      | Sema.Type.Primitive (_, Sema.Primitive_type.U0) -> true
      | _ -> false)
  | None -> false

let discards ~scalar value =
  is_void value
  && Option.fold ~none:false
       ~some:(fun scalar -> Sema.Initializer_type.scalar_byte_size scalar = 0)
       scalar

let pointer_destination scalar =
  Option.bind scalar (fun scalar ->
      let type_ = Sema.Initializer_type.scalar_value_type scalar in
      if
        Sema.Initializer_type.scalar_byte_size scalar = 8
        && Aggregate_member_projection.pointer_member_type type_
      then Some type_
      else None)

let source_pointer_type value =
  let source = Typed.result_storage_type value in
  if Typed.result_is_array_address value then
    Option.bind source (fun type_ ->
        Sema.Type.pointer_to type_ |> Result.to_option)
  else if Typed.result_array_rank value = 0 then source
  else None

let stores_pointer ~scalar value =
  Typed.result_class value = Typed.Integer_result
  && (not (Typed.result_is_callback_storage value))
  && Option.is_none (Typed.result_callback_pointer value)
  && Option.is_none (Typed.result_function_address_path value)
  && (not (Typed.result_is_numeric_callback value))
  &&
  match (pointer_destination scalar, source_pointer_type value) with
  | Some target, Some source ->
      Aggregate_member_projection.pointer_member_type source
      && (Sema.Type.equal target source
         || Sema.Type.compatible_u8_pointer target source)
  | _ -> false

let discards_reference ~scalar value =
  Option.fold ~none:false
    ~some:(fun scalar -> Sema.Initializer_type.scalar_byte_size scalar = 0)
    scalar
  && Typed.result_class value = Typed.Integer_result
  && (Option.fold ~none:false
        ~some:Aggregate_member_projection.pointer_member_type
        (source_pointer_type value)
     || Typed.result_array_rank value = 0
        && (Typed.result_is_callback_storage value
           || Typed.result_category value = Typed.Address_value
              && Option.is_some (Typed.result_function_address_path value)))

let has_integer_word value =
  Typed.result_class value = Typed.Integer_result
  && Typed.result_array_rank value = 0
  && (not (Typed.result_is_callback_storage value))
  && (not (Typed.result_is_numeric_callback value))
  && Option.is_none (Typed.result_function_address_path value)
  && Option.fold ~none:false
       ~some:(fun type_ ->
         Option.is_some (Integer_scalar_storage.of_type type_))
       (Typed.result_storage_type value)

let native_numeric_conversion ~scalar expression =
  let source_f64 = expression_is_f64 expression in
  Option.bind scalar (fun scalar ->
      let target_f64 = Option.is_some (floating_destination (Some scalar)) in
      if
        Sema.Type.pointer_depth (Sema.Initializer_type.scalar_type scalar) = 0
        && Sema.Initializer_type.scalar_byte_size scalar <= 8
        && (source_f64 || has_integer_word expression)
        && (target_f64
           || Option.is_some
                (Integer_scalar_storage.of_type
                   (Sema.Initializer_type.scalar_value_type scalar)))
      then
        Some
          (match (source_f64, target_f64) with
          | false, true -> To_f64
          | true, false -> To_integer
          | _ -> Same_class)
      else None)

let native_numeric ~scalar expression =
  (expression_is_f64 expression || Option.is_some (floating_destination scalar))
  && Option.is_some (native_numeric_conversion ~scalar expression)

let convert_native_word conversion bits =
  match conversion with
  | Same_class -> bits
  | To_f64 -> Common.Binary64_conversion.of_signed_word bits
  | To_integer -> Common.Binary64_conversion.to_signed_word bits
