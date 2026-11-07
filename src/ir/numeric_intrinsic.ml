type conversion = To_integer | To_float

let conversion = function
  | Opcode.Ic_to_i64 -> Some To_integer
  | Opcode.Ic_to_f64 -> Some To_float
  | _ -> None

let arity opcode = Option.map (fun _ -> 1) (conversion opcode)
let supports opcode = Option.is_some (conversion opcode)

let scalar type_ expected =
  Sema.Type.pointer_depth type_ = 0
  &&
  match Sema.Type.base type_ with
  | Sema.Type.Primitive (_, actual) -> Sema.Primitive_type.equal actual expected
  | _ -> false

let argument_matches opcode ~index type_ =
  index = 0
  &&
  match conversion opcode with
  | Some To_integer -> scalar type_ Sema.Primitive_type.F64
  | Some To_float -> scalar type_ Sema.Primitive_type.I64
  | None -> false

let result_matches opcode type_ =
  match conversion opcode with
  | Some To_integer -> scalar type_ Sema.Primitive_type.I64
  | Some To_float -> scalar type_ Sema.Primitive_type.F64
  | None -> false

let runtime_supports opcode =
  supports opcode || Integer_intrinsic.supports opcode

let runtime_arity opcode =
  match arity opcode with
  | Some _ as count -> count
  | None -> Integer_intrinsic.arity opcode

let runtime_argument_matches opcode ~index type_ =
  if supports opcode then argument_matches opcode ~index type_
  else Integer_intrinsic.argument_matches opcode ~index type_

let runtime_result_matches opcode type_ =
  if supports opcode then result_matches opcode type_
  else Integer_intrinsic.result_matches opcode type_
