module Type = Sema.Type
module Primitive = Sema.Primitive_type

type t = Primitive.info

let of_type type_ =
  if Type.pointer_depth type_ <> 0 then None
  else
    match Type.base type_ with
    | Type.Primitive (_, primitive) ->
        let info = Primitive.info primitive in
        if info.category = Primitive.Integer && info.byte_size > 0 then
          Some info
        else None
    | Type.Aggregate _ -> None

let byte_size scalar = scalar.Primitive.byte_size

let compatible_pointer left right =
  Type.compatible_u8_pointer left right
  || Type.pointer_depth left = 1
     && Type.pointer_depth right = 1
     &&
     match (Type.base left, Type.base right) with
     | Type.Primitive (_, Primitive.I64), Type.Primitive (_, Primitive.I64) ->
         true
     | _ -> false

let is_unsigned scalar = scalar.Primitive.signedness = Primitive.Unsigned

let normalize scalar bits =
  let shift = 64 - (8 * byte_size scalar) in
  if shift = 0 then bits
  else
    let shifted = Int64.shift_left bits shift in
    if is_unsigned scalar then Int64.shift_right_logical shifted shift
    else Int64.shift_right shifted shift

let bounds scalar =
  let bits = 8 * byte_size scalar in
  if is_unsigned scalar then
    if bits = 64 then None
    else Some (0L, Int64.sub (Int64.shift_left 1L bits) 1L)
  else if bits = 64 then Some (Int64.min_int, Int64.max_int)
  else
    let sign = Int64.shift_left 1L (bits - 1) in
    Some (Int64.neg sign, Int64.pred sign)

let fits scalar bits = normalize scalar bits = bits

let public_byte_size type_ =
  match Type.base type_ with
  | Type.Primitive (Type.Public_spelling, _) ->
      Option.map byte_size (of_type type_)
  | _ -> None

let narrow_bits type_ bits =
  Option.fold ~none:bits
    ~some:(fun scalar -> normalize scalar bits)
    (of_type type_)
