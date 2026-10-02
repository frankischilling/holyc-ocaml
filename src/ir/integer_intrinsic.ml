type unary = To_upper | To_bool | Absolute | Sign | Square_i64 | Square_u64

let unary = function
  | Opcode.Ic_toupper -> Some To_upper
  | Ic_to_bool -> Some To_bool
  | Ic_abs_i64 -> Some Absolute
  | Ic_sign_i64 -> Some Sign
  | Ic_sqr_i64 -> Some Square_i64
  | Ic_sqr_u64 -> Some Square_u64
  | _ -> None

let supports opcode = opcode = Opcode.Ic_strlen || Option.is_some (unary opcode)

let primitive type_ depth expected =
  Sema.Type.pointer_depth type_ = depth
  &&
  match Sema.Type.base type_ with
  | Sema.Type.Primitive (_, actual) -> Sema.Primitive_type.equal actual expected
  | _ -> false

let argument_matches opcode type_ =
  match opcode with
  | Opcode.Ic_strlen -> primitive type_ 1 Sema.Primitive_type.U8
  | Ic_toupper -> primitive type_ 0 Sema.Primitive_type.U8
  | Ic_sqr_u64 -> primitive type_ 0 Sema.Primitive_type.U64
  | Ic_to_bool | Ic_abs_i64 | Ic_sign_i64 | Ic_sqr_i64 ->
      primitive type_ 0 Sema.Primitive_type.I64
  | _ -> false

let result_matches opcode type_ =
  match opcode with
  | Opcode.Ic_to_bool -> primitive type_ 0 Sema.Primitive_type.U8
  | Ic_sqr_u64 -> primitive type_ 0 Sema.Primitive_type.U64
  | Ic_strlen | Ic_toupper | Ic_abs_i64 | Ic_sign_i64 | Ic_sqr_i64 ->
      primitive type_ 0 Sema.Primitive_type.I64
  | _ -> false

let apply operation bits =
  match operation with
  | To_upper -> if bits >= 97L && bits <= 122L then Int64.sub bits 32L else bits
  | To_bool -> if bits = 0L then 0L else 1L
  | Absolute -> if bits < 0L then Int64.neg bits else bits
  | Sign -> if bits < 0L then -1L else if bits = 0L then 0L else 1L
  | Square_i64 | Square_u64 -> Int64.mul bits bits
