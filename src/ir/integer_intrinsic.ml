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
type bit = Test_bit | Set_bit | Reset_bit | Complement_bit

let swap_size = function
  | Opcode.Ic_swap_u8 -> Some 1
  | Ic_swap_u16 -> Some 2
  | Ic_swap_u32 -> Some 4
  | Ic_swap_i64 -> Some 8
  | _ -> None

let bit = function
  | Opcode.Ic_bt -> Some Test_bit
  | Ic_bts -> Some Set_bit
  | Ic_btr -> Some Reset_bit
  | Ic_btc -> Some Complement_bit
  | _ -> None

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
  else if
    opcode = Opcode.Ic_mod_u64
    || Option.is_some (binary opcode)
    || Option.is_some (bit opcode)
    || Option.is_some (swap_size opcode)
  then Some 2
  else None

let supports opcode = Option.is_some (arity opcode)

let primitive type_ depth expected =
  Sema.Type.pointer_depth type_ = depth
  &&
  match Sema.Type.base type_ with
  | Sema.Type.Primitive (_, actual) -> Sema.Primitive_type.equal actual expected
  | _ -> false

let mod_u64_pointer type_ =
  primitive type_ 1 Sema.Primitive_type.I64
  || primitive type_ 1 Sema.Primitive_type.U64

let bit_pointer type_ =
  Sema.Type.pointer_depth type_ = 1
  &&
  match Sema.Type.dereference type_ with
  | Ok pointee -> Option.is_some (Integer_scalar_storage.of_type pointee)
  | Error _ -> false

let swap_pointer opcode type_ =
  Sema.Type.pointer_depth type_ = 1
  &&
  match (swap_size opcode, Sema.Type.dereference type_) with
  | Some bytes, Ok pointee ->
      Option.fold ~none:false
        ~some:(fun scalar -> Integer_scalar_storage.byte_size scalar = bytes)
        (Integer_scalar_storage.of_type pointee)
  | _ -> false

let argument_matches opcode ~index type_ =
  match opcode with
  | Opcode.Ic_swap_u8 ->
      (index = 0 || index = 1) && primitive type_ 1 Sema.Primitive_type.U8
  | Ic_swap_u16 ->
      (index = 0 || index = 1) && primitive type_ 1 Sema.Primitive_type.U16
  | Ic_swap_u32 ->
      (index = 0 || index = 1) && primitive type_ 1 Sema.Primitive_type.U32
  | Ic_swap_i64 ->
      (index = 0 || index = 1) && primitive type_ 1 Sema.Primitive_type.I64
  | Opcode.Ic_bt | Ic_bts | Ic_btr | Ic_btc ->
      if index = 0 then primitive type_ 1 Sema.Primitive_type.U8
      else index = 1 && primitive type_ 0 Sema.Primitive_type.I64
  | Opcode.Ic_mod_u64 ->
      if index = 0 then primitive type_ 1 Sema.Primitive_type.U64
      else index = 1 && primitive type_ 0 Sema.Primitive_type.U64
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
  | Opcode.Ic_swap_u8 | Ic_swap_u16 | Ic_swap_u32 | Ic_swap_i64 ->
      primitive type_ 0 Sema.Primitive_type.U0
  | Opcode.Ic_bt | Ic_bts | Ic_btr | Ic_btc ->
      primitive type_ 0 Sema.Primitive_type.Bool
  | Opcode.Ic_mod_u64 -> primitive type_ 0 Sema.Primitive_type.U64
  | Opcode.Ic_to_bool ->
      primitive type_ 0 Sema.Primitive_type.U8
      || primitive type_ 0 Sema.Primitive_type.Bool
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

let apply_bit operation ~index bits =
  if index < 0 || index > 63 then
    invalid_arg "bit index must be within one word";
  let mask = Int64.shift_left 1L index in
  let previous = if Int64.logand bits mask = 0L then 0L else 1L in
  let updated =
    match operation with
    | Test_bit -> bits
    | Set_bit -> Int64.logor bits mask
    | Reset_bit -> Int64.logand bits (Int64.lognot mask)
    | Complement_bit -> Int64.logxor bits mask
  in
  (previous, updated)
