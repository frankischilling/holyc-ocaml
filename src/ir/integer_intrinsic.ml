type unary =
  | To_upper
  | To_bool
  | Absolute
  | Sign
  | Square_i64
  | Square_u64
  | Scan_forward
  | Scan_reverse

type binary = Min_i64 | Min_u64 | Max_i64 | Max_u64

let unary = function
  | Opcode.Ic_bsf -> Some Scan_forward
  | Ic_bsr -> Some Scan_reverse
  | Ic_toupper -> Some To_upper
  | Ic_to_bool -> Some To_bool
  | Ic_abs_i64 -> Some Absolute
  | Ic_sign_i64 -> Some Sign
  | Ic_sqr_i64 -> Some Square_i64
  | Ic_sqr_u64 -> Some Square_u64
  | _ -> None

let binary = function
  | Opcode.Ic_min_i64 -> Some Min_i64
  | Ic_min_u64 -> Some Min_u64
  | Ic_max_i64 -> Some Max_i64
  | Ic_max_u64 -> Some Max_u64
  | _ -> None

let arity opcode =
  if opcode = Opcode.Ic_strlen || Option.is_some (unary opcode) then Some 1
  else if Option.is_some (binary opcode) then Some 2
  else None

let supports opcode = Option.is_some (arity opcode)

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
  | Ic_sqr_u64 | Ic_min_u64 | Ic_max_u64 ->
      primitive type_ 0 Sema.Primitive_type.U64
  | Ic_bsf
  | Ic_bsr
  | Ic_to_bool
  | Ic_abs_i64
  | Ic_sign_i64
  | Ic_sqr_i64
  | Ic_min_i64
  | Ic_max_i64 -> primitive type_ 0 Sema.Primitive_type.I64
  | _ -> false

let result_matches opcode type_ =
  match opcode with
  | Opcode.Ic_to_bool -> primitive type_ 0 Sema.Primitive_type.U8
  | Ic_sqr_u64 | Ic_min_u64 | Ic_max_u64 ->
      primitive type_ 0 Sema.Primitive_type.U64
  | Ic_bsf
  | Ic_bsr
  | Ic_strlen
  | Ic_toupper
  | Ic_abs_i64
  | Ic_sign_i64
  | Ic_sqr_i64
  | Ic_min_i64
  | Ic_max_i64 -> primitive type_ 0 Sema.Primitive_type.I64
  | _ -> false

let bit_scan ~forward bits =
  if bits = 0L then -1L
  else
    let rec find index =
      if index < 0 || index > 63 then -1L
      else if Int64.logand bits (Int64.shift_left 1L index) <> 0L then
        Int64.of_int index
      else find (index + if forward then 1 else -1)
    in
    find (if forward then 0 else 63)

let apply operation bits =
  match operation with
  | To_upper -> if bits >= 97L && bits <= 122L then Int64.sub bits 32L else bits
  | To_bool -> if bits = 0L then 0L else 1L
  | Absolute -> if bits < 0L then Int64.neg bits else bits
  | Sign -> if bits < 0L then -1L else if bits = 0L then 0L else 1L
  | Square_i64 | Square_u64 -> Int64.mul bits bits
  | Scan_forward -> bit_scan ~forward:true bits
  | Scan_reverse -> bit_scan ~forward:false bits

let apply_binary operation left right =
  let comparison =
    match operation with
    | Min_i64 | Max_i64 -> Int64.compare left right
    | Min_u64 | Max_u64 -> Int64.unsigned_compare left right
  in
  match operation with
  | Min_i64 | Min_u64 -> if comparison <= 0 then left else right
  | Max_i64 | Max_u64 -> if comparison >= 0 then left else right
