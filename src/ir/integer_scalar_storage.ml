module Type = Sema.Type

let public_byte_size type_ =
  if Type.pointer_depth type_ <> 0 then None
  else
    match Type.base type_ with
    | Type.Primitive (Type.Public_spelling, Sema.Primitive_type.U8) -> Some 1
    | Type.Primitive (Type.Public_spelling, (Sema.Primitive_type.I64 | U64)) ->
        Some 8
    | _ -> None

let narrow_bits type_ bits =
  if public_byte_size type_ = Some 1 then Int64.logand bits 255L else bits
