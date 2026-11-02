module Typed = Sema.Function_call_expression_result

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
