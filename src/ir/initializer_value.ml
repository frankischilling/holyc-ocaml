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
