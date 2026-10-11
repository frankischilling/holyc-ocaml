let flag = 0x10000000L
let has_flag flags = Int64.logand flags flag <> 0L
let clear_flag flags = Int64.logand flags (Int64.lognot flag)

let update_supported = function
  | Opcode.Ic_add_equ
  | Ic_sub_equ
  | Ic_and_equ
  | Ic_or_equ
  | Ic_xor_equ
  | Ic_pp_
  | Ic_mm_
  | Ic__pp
  | Ic__mm -> true
  | _ -> false

let is_implicit_bit = function
  | Opcode.Ic_lbts | Ic_lbtr | Ic_lbtc -> true
  | _ -> false

let scalar_prefix_invalid = function
  | Opcode.Ic_add | Ic_sub | Ic_and | Ic_or | Ic_xor -> true
  | _ -> false
