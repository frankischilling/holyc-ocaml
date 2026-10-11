(* KernelA.HH:3271-3325, at the pinned TempleOS reference. The maintained
   source fixture independently lays out the original prefix and checks both
   offsets. Bytes outside these fields do not model the rest of CTask. *)
let code_offset = 816
let caught_offset = 888
let byte_count = caught_offset + 1

(* CExcept: two queue pointers and thirteen I64 fields, KernelA.HH:3116-3121. *)
let handler_byte_count = 15 * 8

let pointer_type type_ =
  Sema.Type.pointer_depth type_ = 1
  &&
  match Sema.Type.base type_ with
  | Sema.Type.Aggregate symbol -> Sema.Symbol.name symbol = "CTask"
  | Sema.Type.Primitive _ -> false
