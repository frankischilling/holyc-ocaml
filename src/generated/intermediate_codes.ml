(* Generated from the pinned TempleOS intermediate-code definitions and metadata.
   Regenerate this file only as part of a reviewed reference or table update. *)

[@@@ocamlformat "disable"]

let reference_commit = "c26482bb6ad3f80106d28504ec5db3c6a360732c"

type source = { path : string; sha256 : string }

let sources =
  [
    { path = "Compiler/CompilerA.HH"; sha256 = "9eca54eff7d1c0803172e45e5483a57262e24f7b759a6a727c29beaf660967b2" };
    { path = "Compiler/CInit.HC"; sha256 = "f187d11043dcceb8791409a3e6809ea26e9c3b4f182fe2cbe5c5e644e6938b19" };
  ]

type t =
  | Ic_end
  | Ic_nop1
  | Ic_end_exp
  | Ic_nop2
  | Ic_label
  | Ic_call_start
  | Ic_call_end
  | Ic_call_end2
  | Ic_return_val
  | Ic_return_val2
  | Ic_imm_i64
  | Ic_imm_f64
  | Ic_str_const
  | Ic_abs_addr
  | Ic_addr_import
  | Ic_heap_glbl
  | Ic_sizeof
  | Ic_type
  | Ic_get_label
  | Ic_rbp
  | Ic_reg
  | Ic_fs
  | Ic_mov_fs
  | Ic_gs
  | Ic_mov_gs
  | Ic_lea
  | Ic_mov
  | Ic_to_i64
  | Ic_to_f64
  | Ic_to_bool
  | Ic_toupper
  | Ic_holyc_typecast
  | Ic_addr
  | Ic_com
  | Ic_not
  | Ic_unary_minus
  | Ic_deref
  | Ic_deref_pp
  | Ic_deref_mm
  | Ic__pp
  | Ic__mm
  | Ic_pp_
  | Ic_mm_
  | Ic_shl
  | Ic_shr
  | Ic_shl_const
  | Ic_shr_const
  | Ic_power
  | Ic_mul
  | Ic_div
  | Ic_mod
  | Ic_and
  | Ic_or
  | Ic_xor
  | Ic_add
  | Ic_sub
  | Ic_add_const
  | Ic_sub_const
  | Ic_equ_equ
  | Ic_not_equ
  | Ic_less
  | Ic_greater_equ
  | Ic_greater
  | Ic_less_equ
  | Ic_push_cmp
  | Ic_and_and
  | Ic_or_or
  | Ic_xor_xor
  | Ic_assign
  | Ic_assign_pp
  | Ic_assign_mm
  | Ic_shl_equ
  | Ic_shr_equ
  | Ic_mul_equ
  | Ic_div_equ
  | Ic_mod_equ
  | Ic_and_equ
  | Ic_or_equ
  | Ic_xor_equ
  | Ic_add_equ
  | Ic_sub_equ
  | Ic_jmp
  | Ic_sub_call
  | Ic_switch
  | Ic_nobound_switch
  | Ic_add_rsp
  | Ic_add_rsp1
  | Ic_enter
  | Ic_push_regs
  | Ic_pop_regs
  | Ic_leave
  | Ic_ret
  | Ic_call
  | Ic_call_indirect
  | Ic_call_indirect2
  | Ic_call_import
  | Ic_call_extern
  | Ic_asm
  | Ic_push
  | Ic_pop
  | Ic_clflush
  | Ic_invlpg
  | Ic_in_u8
  | Ic_in_u16
  | Ic_in_u32
  | Ic_out_u8
  | Ic_out_u16
  | Ic_out_u32
  | Ic_get_rflags
  | Ic_carry
  | Ic_set_rflags
  | Ic_get_rax
  | Ic_set_rax
  | Ic_get_rbp
  | Ic_set_rbp
  | Ic_get_rsp
  | Ic_set_rsp
  | Ic_rip
  | Ic_rdtsc
  | Ic_bt
  | Ic_bts
  | Ic_btr
  | Ic_btc
  | Ic_lbts
  | Ic_lbtr
  | Ic_lbtc
  | Ic_bsf
  | Ic_bsr
  | Ic_que_init
  | Ic_que_ins
  | Ic_que_ins_rev
  | Ic_que_rem
  | Ic_strlen
  | Ic_br_zero
  | Ic_br_not_zero
  | Ic_br_carry
  | Ic_br_not_carry
  | Ic_br_equ_equ
  | Ic_br_not_equ
  | Ic_br_less
  | Ic_br_greater_equ
  | Ic_br_greater
  | Ic_br_less_equ
  | Ic_br_equ_equ2
  | Ic_br_not_equ2
  | Ic_br_less2
  | Ic_br_greater_equ2
  | Ic_br_greater2
  | Ic_br_less_equ2
  | Ic_br_and_zero
  | Ic_br_and_not_zero
  | Ic_br_mm_zero
  | Ic_br_mm_not_zero
  | Ic_br_and_and_zero
  | Ic_br_and_and_not_zero
  | Ic_br_or_or_zero
  | Ic_br_or_or_not_zero
  | Ic_br_bt
  | Ic_br_bts
  | Ic_br_btr
  | Ic_br_btc
  | Ic_br_not_bt
  | Ic_br_not_bts
  | Ic_br_not_btr
  | Ic_br_not_btc
  | Ic_swap_u8
  | Ic_swap_u16
  | Ic_swap_u32
  | Ic_swap_i64
  | Ic_abs_i64
  | Ic_sign_i64
  | Ic_min_i64
  | Ic_min_u64
  | Ic_max_i64
  | Ic_max_u64
  | Ic_mod_u64
  | Ic_sqr_i64
  | Ic_sqr_u64
  | Ic_sqr
  | Ic_abs
  | Ic_sqrt
  | Ic_sin
  | Ic_cos
  | Ic_tan
  | Ic_atan

type argument_count = Zero | One | Two | Variable

type structural_type =
  | Null
  | Dereference
  | Assignment
  | Comparison

type info = {
  opcode : t;
  source_name : string;
  display_name : string;
  code : int;
  argument_count : argument_count;
  result_count : int;
  structural_type : structural_type;
  pops_float : bool;
  prevents_constant_folding : bool;
  definition_line : int;
  metadata_line : int;
}

let count = 0xB9

let all =
  [
    Ic_end;
    Ic_nop1;
    Ic_end_exp;
    Ic_nop2;
    Ic_label;
    Ic_call_start;
    Ic_call_end;
    Ic_call_end2;
    Ic_return_val;
    Ic_return_val2;
    Ic_imm_i64;
    Ic_imm_f64;
    Ic_str_const;
    Ic_abs_addr;
    Ic_addr_import;
    Ic_heap_glbl;
    Ic_sizeof;
    Ic_type;
    Ic_get_label;
    Ic_rbp;
    Ic_reg;
    Ic_fs;
    Ic_mov_fs;
    Ic_gs;
    Ic_mov_gs;
    Ic_lea;
    Ic_mov;
    Ic_to_i64;
    Ic_to_f64;
    Ic_to_bool;
    Ic_toupper;
    Ic_holyc_typecast;
    Ic_addr;
    Ic_com;
    Ic_not;
    Ic_unary_minus;
    Ic_deref;
    Ic_deref_pp;
    Ic_deref_mm;
    Ic__pp;
    Ic__mm;
    Ic_pp_;
    Ic_mm_;
    Ic_shl;
    Ic_shr;
    Ic_shl_const;
    Ic_shr_const;
    Ic_power;
    Ic_mul;
    Ic_div;
    Ic_mod;
    Ic_and;
    Ic_or;
    Ic_xor;
    Ic_add;
    Ic_sub;
    Ic_add_const;
    Ic_sub_const;
    Ic_equ_equ;
    Ic_not_equ;
    Ic_less;
    Ic_greater_equ;
    Ic_greater;
    Ic_less_equ;
    Ic_push_cmp;
    Ic_and_and;
    Ic_or_or;
    Ic_xor_xor;
    Ic_assign;
    Ic_assign_pp;
    Ic_assign_mm;
    Ic_shl_equ;
    Ic_shr_equ;
    Ic_mul_equ;
    Ic_div_equ;
    Ic_mod_equ;
    Ic_and_equ;
    Ic_or_equ;
    Ic_xor_equ;
    Ic_add_equ;
    Ic_sub_equ;
    Ic_jmp;
    Ic_sub_call;
    Ic_switch;
    Ic_nobound_switch;
    Ic_add_rsp;
    Ic_add_rsp1;
    Ic_enter;
    Ic_push_regs;
    Ic_pop_regs;
    Ic_leave;
    Ic_ret;
    Ic_call;
    Ic_call_indirect;
    Ic_call_indirect2;
    Ic_call_import;
    Ic_call_extern;
    Ic_asm;
    Ic_push;
    Ic_pop;
    Ic_clflush;
    Ic_invlpg;
    Ic_in_u8;
    Ic_in_u16;
    Ic_in_u32;
    Ic_out_u8;
    Ic_out_u16;
    Ic_out_u32;
    Ic_get_rflags;
    Ic_carry;
    Ic_set_rflags;
    Ic_get_rax;
    Ic_set_rax;
    Ic_get_rbp;
    Ic_set_rbp;
    Ic_get_rsp;
    Ic_set_rsp;
    Ic_rip;
    Ic_rdtsc;
    Ic_bt;
    Ic_bts;
    Ic_btr;
    Ic_btc;
    Ic_lbts;
    Ic_lbtr;
    Ic_lbtc;
    Ic_bsf;
    Ic_bsr;
    Ic_que_init;
    Ic_que_ins;
    Ic_que_ins_rev;
    Ic_que_rem;
    Ic_strlen;
    Ic_br_zero;
    Ic_br_not_zero;
    Ic_br_carry;
    Ic_br_not_carry;
    Ic_br_equ_equ;
    Ic_br_not_equ;
    Ic_br_less;
    Ic_br_greater_equ;
    Ic_br_greater;
    Ic_br_less_equ;
    Ic_br_equ_equ2;
    Ic_br_not_equ2;
    Ic_br_less2;
    Ic_br_greater_equ2;
    Ic_br_greater2;
    Ic_br_less_equ2;
    Ic_br_and_zero;
    Ic_br_and_not_zero;
    Ic_br_mm_zero;
    Ic_br_mm_not_zero;
    Ic_br_and_and_zero;
    Ic_br_and_and_not_zero;
    Ic_br_or_or_zero;
    Ic_br_or_or_not_zero;
    Ic_br_bt;
    Ic_br_bts;
    Ic_br_btr;
    Ic_br_btc;
    Ic_br_not_bt;
    Ic_br_not_bts;
    Ic_br_not_btr;
    Ic_br_not_btc;
    Ic_swap_u8;
    Ic_swap_u16;
    Ic_swap_u32;
    Ic_swap_i64;
    Ic_abs_i64;
    Ic_sign_i64;
    Ic_min_i64;
    Ic_min_u64;
    Ic_max_i64;
    Ic_max_u64;
    Ic_mod_u64;
    Ic_sqr_i64;
    Ic_sqr_u64;
    Ic_sqr;
    Ic_abs;
    Ic_sqrt;
    Ic_sin;
    Ic_cos;
    Ic_tan;
    Ic_atan;
  ]

let to_code = function
  | Ic_end -> 0x00
  | Ic_nop1 -> 0x01
  | Ic_end_exp -> 0x02
  | Ic_nop2 -> 0x03
  | Ic_label -> 0x04
  | Ic_call_start -> 0x05
  | Ic_call_end -> 0x06
  | Ic_call_end2 -> 0x07
  | Ic_return_val -> 0x08
  | Ic_return_val2 -> 0x09
  | Ic_imm_i64 -> 0x0A
  | Ic_imm_f64 -> 0x0B
  | Ic_str_const -> 0x0C
  | Ic_abs_addr -> 0x0D
  | Ic_addr_import -> 0x0E
  | Ic_heap_glbl -> 0x0F
  | Ic_sizeof -> 0x10
  | Ic_type -> 0x11
  | Ic_get_label -> 0x12
  | Ic_rbp -> 0x13
  | Ic_reg -> 0x14
  | Ic_fs -> 0x15
  | Ic_mov_fs -> 0x16
  | Ic_gs -> 0x17
  | Ic_mov_gs -> 0x18
  | Ic_lea -> 0x19
  | Ic_mov -> 0x1A
  | Ic_to_i64 -> 0x1B
  | Ic_to_f64 -> 0x1C
  | Ic_to_bool -> 0x1D
  | Ic_toupper -> 0x1E
  | Ic_holyc_typecast -> 0x1F
  | Ic_addr -> 0x20
  | Ic_com -> 0x21
  | Ic_not -> 0x22
  | Ic_unary_minus -> 0x23
  | Ic_deref -> 0x24
  | Ic_deref_pp -> 0x25
  | Ic_deref_mm -> 0x26
  | Ic__pp -> 0x27
  | Ic__mm -> 0x28
  | Ic_pp_ -> 0x29
  | Ic_mm_ -> 0x2A
  | Ic_shl -> 0x2B
  | Ic_shr -> 0x2C
  | Ic_shl_const -> 0x2D
  | Ic_shr_const -> 0x2E
  | Ic_power -> 0x2F
  | Ic_mul -> 0x30
  | Ic_div -> 0x31
  | Ic_mod -> 0x32
  | Ic_and -> 0x33
  | Ic_or -> 0x34
  | Ic_xor -> 0x35
  | Ic_add -> 0x36
  | Ic_sub -> 0x37
  | Ic_add_const -> 0x38
  | Ic_sub_const -> 0x39
  | Ic_equ_equ -> 0x3A
  | Ic_not_equ -> 0x3B
  | Ic_less -> 0x3C
  | Ic_greater_equ -> 0x3D
  | Ic_greater -> 0x3E
  | Ic_less_equ -> 0x3F
  | Ic_push_cmp -> 0x40
  | Ic_and_and -> 0x41
  | Ic_or_or -> 0x42
  | Ic_xor_xor -> 0x43
  | Ic_assign -> 0x44
  | Ic_assign_pp -> 0x45
  | Ic_assign_mm -> 0x46
  | Ic_shl_equ -> 0x47
  | Ic_shr_equ -> 0x48
  | Ic_mul_equ -> 0x49
  | Ic_div_equ -> 0x4A
  | Ic_mod_equ -> 0x4B
  | Ic_and_equ -> 0x4C
  | Ic_or_equ -> 0x4D
  | Ic_xor_equ -> 0x4E
  | Ic_add_equ -> 0x4F
  | Ic_sub_equ -> 0x50
  | Ic_jmp -> 0x51
  | Ic_sub_call -> 0x52
  | Ic_switch -> 0x53
  | Ic_nobound_switch -> 0x54
  | Ic_add_rsp -> 0x55
  | Ic_add_rsp1 -> 0x56
  | Ic_enter -> 0x57
  | Ic_push_regs -> 0x58
  | Ic_pop_regs -> 0x59
  | Ic_leave -> 0x5A
  | Ic_ret -> 0x5B
  | Ic_call -> 0x5C
  | Ic_call_indirect -> 0x5D
  | Ic_call_indirect2 -> 0x5E
  | Ic_call_import -> 0x5F
  | Ic_call_extern -> 0x60
  | Ic_asm -> 0x61
  | Ic_push -> 0x62
  | Ic_pop -> 0x63
  | Ic_clflush -> 0x64
  | Ic_invlpg -> 0x65
  | Ic_in_u8 -> 0x66
  | Ic_in_u16 -> 0x67
  | Ic_in_u32 -> 0x68
  | Ic_out_u8 -> 0x69
  | Ic_out_u16 -> 0x6A
  | Ic_out_u32 -> 0x6B
  | Ic_get_rflags -> 0x6C
  | Ic_carry -> 0x6D
  | Ic_set_rflags -> 0x6E
  | Ic_get_rax -> 0x6F
  | Ic_set_rax -> 0x70
  | Ic_get_rbp -> 0x71
  | Ic_set_rbp -> 0x72
  | Ic_get_rsp -> 0x73
  | Ic_set_rsp -> 0x74
  | Ic_rip -> 0x75
  | Ic_rdtsc -> 0x76
  | Ic_bt -> 0x77
  | Ic_bts -> 0x78
  | Ic_btr -> 0x79
  | Ic_btc -> 0x7A
  | Ic_lbts -> 0x7B
  | Ic_lbtr -> 0x7C
  | Ic_lbtc -> 0x7D
  | Ic_bsf -> 0x7E
  | Ic_bsr -> 0x7F
  | Ic_que_init -> 0x80
  | Ic_que_ins -> 0x81
  | Ic_que_ins_rev -> 0x82
  | Ic_que_rem -> 0x83
  | Ic_strlen -> 0x84
  | Ic_br_zero -> 0x85
  | Ic_br_not_zero -> 0x86
  | Ic_br_carry -> 0x87
  | Ic_br_not_carry -> 0x88
  | Ic_br_equ_equ -> 0x89
  | Ic_br_not_equ -> 0x8A
  | Ic_br_less -> 0x8B
  | Ic_br_greater_equ -> 0x8C
  | Ic_br_greater -> 0x8D
  | Ic_br_less_equ -> 0x8E
  | Ic_br_equ_equ2 -> 0x8F
  | Ic_br_not_equ2 -> 0x90
  | Ic_br_less2 -> 0x91
  | Ic_br_greater_equ2 -> 0x92
  | Ic_br_greater2 -> 0x93
  | Ic_br_less_equ2 -> 0x94
  | Ic_br_and_zero -> 0x95
  | Ic_br_and_not_zero -> 0x96
  | Ic_br_mm_zero -> 0x97
  | Ic_br_mm_not_zero -> 0x98
  | Ic_br_and_and_zero -> 0x99
  | Ic_br_and_and_not_zero -> 0x9A
  | Ic_br_or_or_zero -> 0x9B
  | Ic_br_or_or_not_zero -> 0x9C
  | Ic_br_bt -> 0x9D
  | Ic_br_bts -> 0x9E
  | Ic_br_btr -> 0x9F
  | Ic_br_btc -> 0xA0
  | Ic_br_not_bt -> 0xA1
  | Ic_br_not_bts -> 0xA2
  | Ic_br_not_btr -> 0xA3
  | Ic_br_not_btc -> 0xA4
  | Ic_swap_u8 -> 0xA5
  | Ic_swap_u16 -> 0xA6
  | Ic_swap_u32 -> 0xA7
  | Ic_swap_i64 -> 0xA8
  | Ic_abs_i64 -> 0xA9
  | Ic_sign_i64 -> 0xAA
  | Ic_min_i64 -> 0xAB
  | Ic_min_u64 -> 0xAC
  | Ic_max_i64 -> 0xAD
  | Ic_max_u64 -> 0xAE
  | Ic_mod_u64 -> 0xAF
  | Ic_sqr_i64 -> 0xB0
  | Ic_sqr_u64 -> 0xB1
  | Ic_sqr -> 0xB2
  | Ic_abs -> 0xB3
  | Ic_sqrt -> 0xB4
  | Ic_sin -> 0xB5
  | Ic_cos -> 0xB6
  | Ic_tan -> 0xB7
  | Ic_atan -> 0xB8

let of_code = function
  | 0x00 -> Some Ic_end
  | 0x01 -> Some Ic_nop1
  | 0x02 -> Some Ic_end_exp
  | 0x03 -> Some Ic_nop2
  | 0x04 -> Some Ic_label
  | 0x05 -> Some Ic_call_start
  | 0x06 -> Some Ic_call_end
  | 0x07 -> Some Ic_call_end2
  | 0x08 -> Some Ic_return_val
  | 0x09 -> Some Ic_return_val2
  | 0x0A -> Some Ic_imm_i64
  | 0x0B -> Some Ic_imm_f64
  | 0x0C -> Some Ic_str_const
  | 0x0D -> Some Ic_abs_addr
  | 0x0E -> Some Ic_addr_import
  | 0x0F -> Some Ic_heap_glbl
  | 0x10 -> Some Ic_sizeof
  | 0x11 -> Some Ic_type
  | 0x12 -> Some Ic_get_label
  | 0x13 -> Some Ic_rbp
  | 0x14 -> Some Ic_reg
  | 0x15 -> Some Ic_fs
  | 0x16 -> Some Ic_mov_fs
  | 0x17 -> Some Ic_gs
  | 0x18 -> Some Ic_mov_gs
  | 0x19 -> Some Ic_lea
  | 0x1A -> Some Ic_mov
  | 0x1B -> Some Ic_to_i64
  | 0x1C -> Some Ic_to_f64
  | 0x1D -> Some Ic_to_bool
  | 0x1E -> Some Ic_toupper
  | 0x1F -> Some Ic_holyc_typecast
  | 0x20 -> Some Ic_addr
  | 0x21 -> Some Ic_com
  | 0x22 -> Some Ic_not
  | 0x23 -> Some Ic_unary_minus
  | 0x24 -> Some Ic_deref
  | 0x25 -> Some Ic_deref_pp
  | 0x26 -> Some Ic_deref_mm
  | 0x27 -> Some Ic__pp
  | 0x28 -> Some Ic__mm
  | 0x29 -> Some Ic_pp_
  | 0x2A -> Some Ic_mm_
  | 0x2B -> Some Ic_shl
  | 0x2C -> Some Ic_shr
  | 0x2D -> Some Ic_shl_const
  | 0x2E -> Some Ic_shr_const
  | 0x2F -> Some Ic_power
  | 0x30 -> Some Ic_mul
  | 0x31 -> Some Ic_div
  | 0x32 -> Some Ic_mod
  | 0x33 -> Some Ic_and
  | 0x34 -> Some Ic_or
  | 0x35 -> Some Ic_xor
  | 0x36 -> Some Ic_add
  | 0x37 -> Some Ic_sub
  | 0x38 -> Some Ic_add_const
  | 0x39 -> Some Ic_sub_const
  | 0x3A -> Some Ic_equ_equ
  | 0x3B -> Some Ic_not_equ
  | 0x3C -> Some Ic_less
  | 0x3D -> Some Ic_greater_equ
  | 0x3E -> Some Ic_greater
  | 0x3F -> Some Ic_less_equ
  | 0x40 -> Some Ic_push_cmp
  | 0x41 -> Some Ic_and_and
  | 0x42 -> Some Ic_or_or
  | 0x43 -> Some Ic_xor_xor
  | 0x44 -> Some Ic_assign
  | 0x45 -> Some Ic_assign_pp
  | 0x46 -> Some Ic_assign_mm
  | 0x47 -> Some Ic_shl_equ
  | 0x48 -> Some Ic_shr_equ
  | 0x49 -> Some Ic_mul_equ
  | 0x4A -> Some Ic_div_equ
  | 0x4B -> Some Ic_mod_equ
  | 0x4C -> Some Ic_and_equ
  | 0x4D -> Some Ic_or_equ
  | 0x4E -> Some Ic_xor_equ
  | 0x4F -> Some Ic_add_equ
  | 0x50 -> Some Ic_sub_equ
  | 0x51 -> Some Ic_jmp
  | 0x52 -> Some Ic_sub_call
  | 0x53 -> Some Ic_switch
  | 0x54 -> Some Ic_nobound_switch
  | 0x55 -> Some Ic_add_rsp
  | 0x56 -> Some Ic_add_rsp1
  | 0x57 -> Some Ic_enter
  | 0x58 -> Some Ic_push_regs
  | 0x59 -> Some Ic_pop_regs
  | 0x5A -> Some Ic_leave
  | 0x5B -> Some Ic_ret
  | 0x5C -> Some Ic_call
  | 0x5D -> Some Ic_call_indirect
  | 0x5E -> Some Ic_call_indirect2
  | 0x5F -> Some Ic_call_import
  | 0x60 -> Some Ic_call_extern
  | 0x61 -> Some Ic_asm
  | 0x62 -> Some Ic_push
  | 0x63 -> Some Ic_pop
  | 0x64 -> Some Ic_clflush
  | 0x65 -> Some Ic_invlpg
  | 0x66 -> Some Ic_in_u8
  | 0x67 -> Some Ic_in_u16
  | 0x68 -> Some Ic_in_u32
  | 0x69 -> Some Ic_out_u8
  | 0x6A -> Some Ic_out_u16
  | 0x6B -> Some Ic_out_u32
  | 0x6C -> Some Ic_get_rflags
  | 0x6D -> Some Ic_carry
  | 0x6E -> Some Ic_set_rflags
  | 0x6F -> Some Ic_get_rax
  | 0x70 -> Some Ic_set_rax
  | 0x71 -> Some Ic_get_rbp
  | 0x72 -> Some Ic_set_rbp
  | 0x73 -> Some Ic_get_rsp
  | 0x74 -> Some Ic_set_rsp
  | 0x75 -> Some Ic_rip
  | 0x76 -> Some Ic_rdtsc
  | 0x77 -> Some Ic_bt
  | 0x78 -> Some Ic_bts
  | 0x79 -> Some Ic_btr
  | 0x7A -> Some Ic_btc
  | 0x7B -> Some Ic_lbts
  | 0x7C -> Some Ic_lbtr
  | 0x7D -> Some Ic_lbtc
  | 0x7E -> Some Ic_bsf
  | 0x7F -> Some Ic_bsr
  | 0x80 -> Some Ic_que_init
  | 0x81 -> Some Ic_que_ins
  | 0x82 -> Some Ic_que_ins_rev
  | 0x83 -> Some Ic_que_rem
  | 0x84 -> Some Ic_strlen
  | 0x85 -> Some Ic_br_zero
  | 0x86 -> Some Ic_br_not_zero
  | 0x87 -> Some Ic_br_carry
  | 0x88 -> Some Ic_br_not_carry
  | 0x89 -> Some Ic_br_equ_equ
  | 0x8A -> Some Ic_br_not_equ
  | 0x8B -> Some Ic_br_less
  | 0x8C -> Some Ic_br_greater_equ
  | 0x8D -> Some Ic_br_greater
  | 0x8E -> Some Ic_br_less_equ
  | 0x8F -> Some Ic_br_equ_equ2
  | 0x90 -> Some Ic_br_not_equ2
  | 0x91 -> Some Ic_br_less2
  | 0x92 -> Some Ic_br_greater_equ2
  | 0x93 -> Some Ic_br_greater2
  | 0x94 -> Some Ic_br_less_equ2
  | 0x95 -> Some Ic_br_and_zero
  | 0x96 -> Some Ic_br_and_not_zero
  | 0x97 -> Some Ic_br_mm_zero
  | 0x98 -> Some Ic_br_mm_not_zero
  | 0x99 -> Some Ic_br_and_and_zero
  | 0x9A -> Some Ic_br_and_and_not_zero
  | 0x9B -> Some Ic_br_or_or_zero
  | 0x9C -> Some Ic_br_or_or_not_zero
  | 0x9D -> Some Ic_br_bt
  | 0x9E -> Some Ic_br_bts
  | 0x9F -> Some Ic_br_btr
  | 0xA0 -> Some Ic_br_btc
  | 0xA1 -> Some Ic_br_not_bt
  | 0xA2 -> Some Ic_br_not_bts
  | 0xA3 -> Some Ic_br_not_btr
  | 0xA4 -> Some Ic_br_not_btc
  | 0xA5 -> Some Ic_swap_u8
  | 0xA6 -> Some Ic_swap_u16
  | 0xA7 -> Some Ic_swap_u32
  | 0xA8 -> Some Ic_swap_i64
  | 0xA9 -> Some Ic_abs_i64
  | 0xAA -> Some Ic_sign_i64
  | 0xAB -> Some Ic_min_i64
  | 0xAC -> Some Ic_min_u64
  | 0xAD -> Some Ic_max_i64
  | 0xAE -> Some Ic_max_u64
  | 0xAF -> Some Ic_mod_u64
  | 0xB0 -> Some Ic_sqr_i64
  | 0xB1 -> Some Ic_sqr_u64
  | 0xB2 -> Some Ic_sqr
  | 0xB3 -> Some Ic_abs
  | 0xB4 -> Some Ic_sqrt
  | 0xB5 -> Some Ic_sin
  | 0xB6 -> Some Ic_cos
  | 0xB7 -> Some Ic_tan
  | 0xB8 -> Some Ic_atan
  | _ -> None

let of_source_name = function
  | "IC_END" -> Some Ic_end
  | "IC_NOP1" -> Some Ic_nop1
  | "IC_END_EXP" -> Some Ic_end_exp
  | "IC_NOP2" -> Some Ic_nop2
  | "IC_LABEL" -> Some Ic_label
  | "IC_CALL_START" -> Some Ic_call_start
  | "IC_CALL_END" -> Some Ic_call_end
  | "IC_CALL_END2" -> Some Ic_call_end2
  | "IC_RETURN_VAL" -> Some Ic_return_val
  | "IC_RETURN_VAL2" -> Some Ic_return_val2
  | "IC_IMM_I64" -> Some Ic_imm_i64
  | "IC_IMM_F64" -> Some Ic_imm_f64
  | "IC_STR_CONST" -> Some Ic_str_const
  | "IC_ABS_ADDR" -> Some Ic_abs_addr
  | "IC_ADDR_IMPORT" -> Some Ic_addr_import
  | "IC_HEAP_GLBL" -> Some Ic_heap_glbl
  | "IC_SIZEOF" -> Some Ic_sizeof
  | "IC_TYPE" -> Some Ic_type
  | "IC_GET_LABEL" -> Some Ic_get_label
  | "IC_RBP" -> Some Ic_rbp
  | "IC_REG" -> Some Ic_reg
  | "IC_FS" -> Some Ic_fs
  | "IC_MOV_FS" -> Some Ic_mov_fs
  | "IC_GS" -> Some Ic_gs
  | "IC_MOV_GS" -> Some Ic_mov_gs
  | "IC_LEA" -> Some Ic_lea
  | "IC_MOV" -> Some Ic_mov
  | "IC_TO_I64" -> Some Ic_to_i64
  | "IC_TO_F64" -> Some Ic_to_f64
  | "IC_TO_BOOL" -> Some Ic_to_bool
  | "IC_TOUPPER" -> Some Ic_toupper
  | "IC_HOLYC_TYPECAST" -> Some Ic_holyc_typecast
  | "IC_ADDR" -> Some Ic_addr
  | "IC_COM" -> Some Ic_com
  | "IC_NOT" -> Some Ic_not
  | "IC_UNARY_MINUS" -> Some Ic_unary_minus
  | "IC_DEREF" -> Some Ic_deref
  | "IC_DEREF_PP" -> Some Ic_deref_pp
  | "IC_DEREF_MM" -> Some Ic_deref_mm
  | "IC__PP" -> Some Ic__pp
  | "IC__MM" -> Some Ic__mm
  | "IC_PP_" -> Some Ic_pp_
  | "IC_MM_" -> Some Ic_mm_
  | "IC_SHL" -> Some Ic_shl
  | "IC_SHR" -> Some Ic_shr
  | "IC_SHL_CONST" -> Some Ic_shl_const
  | "IC_SHR_CONST" -> Some Ic_shr_const
  | "IC_POWER" -> Some Ic_power
  | "IC_MUL" -> Some Ic_mul
  | "IC_DIV" -> Some Ic_div
  | "IC_MOD" -> Some Ic_mod
  | "IC_AND" -> Some Ic_and
  | "IC_OR" -> Some Ic_or
  | "IC_XOR" -> Some Ic_xor
  | "IC_ADD" -> Some Ic_add
  | "IC_SUB" -> Some Ic_sub
  | "IC_ADD_CONST" -> Some Ic_add_const
  | "IC_SUB_CONST" -> Some Ic_sub_const
  | "IC_EQU_EQU" -> Some Ic_equ_equ
  | "IC_NOT_EQU" -> Some Ic_not_equ
  | "IC_LESS" -> Some Ic_less
  | "IC_GREATER_EQU" -> Some Ic_greater_equ
  | "IC_GREATER" -> Some Ic_greater
  | "IC_LESS_EQU" -> Some Ic_less_equ
  | "IC_PUSH_CMP" -> Some Ic_push_cmp
  | "IC_AND_AND" -> Some Ic_and_and
  | "IC_OR_OR" -> Some Ic_or_or
  | "IC_XOR_XOR" -> Some Ic_xor_xor
  | "IC_ASSIGN" -> Some Ic_assign
  | "IC_ASSIGN_PP" -> Some Ic_assign_pp
  | "IC_ASSIGN_MM" -> Some Ic_assign_mm
  | "IC_SHL_EQU" -> Some Ic_shl_equ
  | "IC_SHR_EQU" -> Some Ic_shr_equ
  | "IC_MUL_EQU" -> Some Ic_mul_equ
  | "IC_DIV_EQU" -> Some Ic_div_equ
  | "IC_MOD_EQU" -> Some Ic_mod_equ
  | "IC_AND_EQU" -> Some Ic_and_equ
  | "IC_OR_EQU" -> Some Ic_or_equ
  | "IC_XOR_EQU" -> Some Ic_xor_equ
  | "IC_ADD_EQU" -> Some Ic_add_equ
  | "IC_SUB_EQU" -> Some Ic_sub_equ
  | "IC_JMP" -> Some Ic_jmp
  | "IC_SUB_CALL" -> Some Ic_sub_call
  | "IC_SWITCH" -> Some Ic_switch
  | "IC_NOBOUND_SWITCH" -> Some Ic_nobound_switch
  | "IC_ADD_RSP" -> Some Ic_add_rsp
  | "IC_ADD_RSP1" -> Some Ic_add_rsp1
  | "IC_ENTER" -> Some Ic_enter
  | "IC_PUSH_REGS" -> Some Ic_push_regs
  | "IC_POP_REGS" -> Some Ic_pop_regs
  | "IC_LEAVE" -> Some Ic_leave
  | "IC_RET" -> Some Ic_ret
  | "IC_CALL" -> Some Ic_call
  | "IC_CALL_INDIRECT" -> Some Ic_call_indirect
  | "IC_CALL_INDIRECT2" -> Some Ic_call_indirect2
  | "IC_CALL_IMPORT" -> Some Ic_call_import
  | "IC_CALL_EXTERN" -> Some Ic_call_extern
  | "IC_ASM" -> Some Ic_asm
  | "IC_PUSH" -> Some Ic_push
  | "IC_POP" -> Some Ic_pop
  | "IC_CLFLUSH" -> Some Ic_clflush
  | "IC_INVLPG" -> Some Ic_invlpg
  | "IC_IN_U8" -> Some Ic_in_u8
  | "IC_IN_U16" -> Some Ic_in_u16
  | "IC_IN_U32" -> Some Ic_in_u32
  | "IC_OUT_U8" -> Some Ic_out_u8
  | "IC_OUT_U16" -> Some Ic_out_u16
  | "IC_OUT_U32" -> Some Ic_out_u32
  | "IC_GET_RFLAGS" -> Some Ic_get_rflags
  | "IC_CARRY" -> Some Ic_carry
  | "IC_SET_RFLAGS" -> Some Ic_set_rflags
  | "IC_GET_RAX" -> Some Ic_get_rax
  | "IC_SET_RAX" -> Some Ic_set_rax
  | "IC_GET_RBP" -> Some Ic_get_rbp
  | "IC_SET_RBP" -> Some Ic_set_rbp
  | "IC_GET_RSP" -> Some Ic_get_rsp
  | "IC_SET_RSP" -> Some Ic_set_rsp
  | "IC_RIP" -> Some Ic_rip
  | "IC_RDTSC" -> Some Ic_rdtsc
  | "IC_BT" -> Some Ic_bt
  | "IC_BTS" -> Some Ic_bts
  | "IC_BTR" -> Some Ic_btr
  | "IC_BTC" -> Some Ic_btc
  | "IC_LBTS" -> Some Ic_lbts
  | "IC_LBTR" -> Some Ic_lbtr
  | "IC_LBTC" -> Some Ic_lbtc
  | "IC_BSF" -> Some Ic_bsf
  | "IC_BSR" -> Some Ic_bsr
  | "IC_QUE_INIT" -> Some Ic_que_init
  | "IC_QUE_INS" -> Some Ic_que_ins
  | "IC_QUE_INS_REV" -> Some Ic_que_ins_rev
  | "IC_QUE_REM" -> Some Ic_que_rem
  | "IC_STRLEN" -> Some Ic_strlen
  | "IC_BR_ZERO" -> Some Ic_br_zero
  | "IC_BR_NOT_ZERO" -> Some Ic_br_not_zero
  | "IC_BR_CARRY" -> Some Ic_br_carry
  | "IC_BR_NOT_CARRY" -> Some Ic_br_not_carry
  | "IC_BR_EQU_EQU" -> Some Ic_br_equ_equ
  | "IC_BR_NOT_EQU" -> Some Ic_br_not_equ
  | "IC_BR_LESS" -> Some Ic_br_less
  | "IC_BR_GREATER_EQU" -> Some Ic_br_greater_equ
  | "IC_BR_GREATER" -> Some Ic_br_greater
  | "IC_BR_LESS_EQU" -> Some Ic_br_less_equ
  | "IC_BR_EQU_EQU2" -> Some Ic_br_equ_equ2
  | "IC_BR_NOT_EQU2" -> Some Ic_br_not_equ2
  | "IC_BR_LESS2" -> Some Ic_br_less2
  | "IC_BR_GREATER_EQU2" -> Some Ic_br_greater_equ2
  | "IC_BR_GREATER2" -> Some Ic_br_greater2
  | "IC_BR_LESS_EQU2" -> Some Ic_br_less_equ2
  | "IC_BR_AND_ZERO" -> Some Ic_br_and_zero
  | "IC_BR_AND_NOT_ZERO" -> Some Ic_br_and_not_zero
  | "IC_BR_MM_ZERO" -> Some Ic_br_mm_zero
  | "IC_BR_MM_NOT_ZERO" -> Some Ic_br_mm_not_zero
  | "IC_BR_AND_AND_ZERO" -> Some Ic_br_and_and_zero
  | "IC_BR_AND_AND_NOT_ZERO" -> Some Ic_br_and_and_not_zero
  | "IC_BR_OR_OR_ZERO" -> Some Ic_br_or_or_zero
  | "IC_BR_OR_OR_NOT_ZERO" -> Some Ic_br_or_or_not_zero
  | "IC_BR_BT" -> Some Ic_br_bt
  | "IC_BR_BTS" -> Some Ic_br_bts
  | "IC_BR_BTR" -> Some Ic_br_btr
  | "IC_BR_BTC" -> Some Ic_br_btc
  | "IC_BR_NOT_BT" -> Some Ic_br_not_bt
  | "IC_BR_NOT_BTS" -> Some Ic_br_not_bts
  | "IC_BR_NOT_BTR" -> Some Ic_br_not_btr
  | "IC_BR_NOT_BTC" -> Some Ic_br_not_btc
  | "IC_SWAP_U8" -> Some Ic_swap_u8
  | "IC_SWAP_U16" -> Some Ic_swap_u16
  | "IC_SWAP_U32" -> Some Ic_swap_u32
  | "IC_SWAP_I64" -> Some Ic_swap_i64
  | "IC_ABS_I64" -> Some Ic_abs_i64
  | "IC_SIGN_I64" -> Some Ic_sign_i64
  | "IC_MIN_I64" -> Some Ic_min_i64
  | "IC_MIN_U64" -> Some Ic_min_u64
  | "IC_MAX_I64" -> Some Ic_max_i64
  | "IC_MAX_U64" -> Some Ic_max_u64
  | "IC_MOD_U64" -> Some Ic_mod_u64
  | "IC_SQR_I64" -> Some Ic_sqr_i64
  | "IC_SQR_U64" -> Some Ic_sqr_u64
  | "IC_SQR" -> Some Ic_sqr
  | "IC_ABS" -> Some Ic_abs
  | "IC_SQRT" -> Some Ic_sqrt
  | "IC_SIN" -> Some Ic_sin
  | "IC_COS" -> Some Ic_cos
  | "IC_TAN" -> Some Ic_tan
  | "IC_ATAN" -> Some Ic_atan
  | _ -> None

let of_display_name = function
  | "END" -> Some Ic_end
  | "NOP1" -> Some Ic_nop1
  | "END_EXP" -> Some Ic_end_exp
  | "NOP2" -> Some Ic_nop2
  | "LABEL" -> Some Ic_label
  | "CALL_START" -> Some Ic_call_start
  | "CALL_END" -> Some Ic_call_end
  | "CALL_END2" -> Some Ic_call_end2
  | "RETURN_VAL" -> Some Ic_return_val
  | "RETURN_VAL2" -> Some Ic_return_val2
  | "IMM_I64" -> Some Ic_imm_i64
  | "IMM_F64" -> Some Ic_imm_f64
  | "STR_CONST" -> Some Ic_str_const
  | "ABS_ADDR" -> Some Ic_abs_addr
  | "ADDR_IMPORT" -> Some Ic_addr_import
  | "HEAP_GLBL" -> Some Ic_heap_glbl
  | "SIZEOF" -> Some Ic_sizeof
  | "TYPE" -> Some Ic_type
  | "GET_LABEL" -> Some Ic_get_label
  | "RBP" -> Some Ic_rbp
  | "REG" -> Some Ic_reg
  | "FS" -> Some Ic_fs
  | "MOV_FS" -> Some Ic_mov_fs
  | "GS" -> Some Ic_gs
  | "MOV_GS" -> Some Ic_mov_gs
  | "LEA" -> Some Ic_lea
  | "MOV" -> Some Ic_mov
  | "TO_I64" -> Some Ic_to_i64
  | "TO_F64" -> Some Ic_to_f64
  | "TO_BOOL" -> Some Ic_to_bool
  | "TOUPPER" -> Some Ic_toupper
  | "HOLYC_TYPECAST" -> Some Ic_holyc_typecast
  | "ADDR" -> Some Ic_addr
  | "COM" -> Some Ic_com
  | "NOT" -> Some Ic_not
  | "UNARY_MINUS" -> Some Ic_unary_minus
  | "DEREF" -> Some Ic_deref
  | "DEREF_PP" -> Some Ic_deref_pp
  | "DEREF_MM" -> Some Ic_deref_mm
  | "_PP" -> Some Ic__pp
  | "_MM" -> Some Ic__mm
  | "PP_" -> Some Ic_pp_
  | "MM_" -> Some Ic_mm_
  | "SHL" -> Some Ic_shl
  | "SHR" -> Some Ic_shr
  | "SHL_CONST" -> Some Ic_shl_const
  | "SHR_CONST" -> Some Ic_shr_const
  | "POWER" -> Some Ic_power
  | "MUL" -> Some Ic_mul
  | "DIV" -> Some Ic_div
  | "MOD" -> Some Ic_mod
  | "AND" -> Some Ic_and
  | "OR" -> Some Ic_or
  | "XOR" -> Some Ic_xor
  | "ADD" -> Some Ic_add
  | "SUB" -> Some Ic_sub
  | "ADD_CONST" -> Some Ic_add_const
  | "SUB_CONST" -> Some Ic_sub_const
  | "EQU_EQU" -> Some Ic_equ_equ
  | "NOT_EQU" -> Some Ic_not_equ
  | "LESS" -> Some Ic_less
  | "GREATER_EQU" -> Some Ic_greater_equ
  | "GREATER" -> Some Ic_greater
  | "LESS_EQU" -> Some Ic_less_equ
  | "PUSH_CMP" -> Some Ic_push_cmp
  | "AND_AND" -> Some Ic_and_and
  | "OR_OR" -> Some Ic_or_or
  | "XOR_XOR" -> Some Ic_xor_xor
  | "ASSIGN" -> Some Ic_assign
  | "ASSIGN_PP" -> Some Ic_assign_pp
  | "ASSIGN_MM" -> Some Ic_assign_mm
  | "SHL_EQU" -> Some Ic_shl_equ
  | "SHR_EQU" -> Some Ic_shr_equ
  | "MUL_EQU" -> Some Ic_mul_equ
  | "DIV_EQU" -> Some Ic_div_equ
  | "MOD_EQU" -> Some Ic_mod_equ
  | "AND_EQU" -> Some Ic_and_equ
  | "OR_EQU" -> Some Ic_or_equ
  | "XOR_EQU" -> Some Ic_xor_equ
  | "ADD_EQU" -> Some Ic_add_equ
  | "SUB_EQU" -> Some Ic_sub_equ
  | "JMP" -> Some Ic_jmp
  | "SUB_CALL" -> Some Ic_sub_call
  | "SWITCH" -> Some Ic_switch
  | "NOBOUND_SWITCH" -> Some Ic_nobound_switch
  | "ADD_RSP" -> Some Ic_add_rsp
  | "ADD_RSP1" -> Some Ic_add_rsp1
  | "ENTER" -> Some Ic_enter
  | "PUSH_REGS" -> Some Ic_push_regs
  | "POP_REGS" -> Some Ic_pop_regs
  | "LEAVE" -> Some Ic_leave
  | "RET" -> Some Ic_ret
  | "CALL" -> Some Ic_call
  | "CALL_INDIRECT" -> Some Ic_call_indirect
  | "CALL_INDIRECT2" -> Some Ic_call_indirect2
  | "CALL_IMPORT" -> Some Ic_call_import
  | "CALL_EXTERN" -> Some Ic_call_extern
  | "ASM" -> Some Ic_asm
  | "PUSH" -> Some Ic_push
  | "POP" -> Some Ic_pop
  | "CLFLUSH" -> Some Ic_clflush
  | "INVLPG" -> Some Ic_invlpg
  | "IN_U8" -> Some Ic_in_u8
  | "IN_U16" -> Some Ic_in_u16
  | "IN_U32" -> Some Ic_in_u32
  | "OUT_U8" -> Some Ic_out_u8
  | "OUT_U16" -> Some Ic_out_u16
  | "OUT_U32" -> Some Ic_out_u32
  | "GET_RFLAGS" -> Some Ic_get_rflags
  | "CARRY" -> Some Ic_carry
  | "SET_RFLAGS" -> Some Ic_set_rflags
  | "GET_RAX" -> Some Ic_get_rax
  | "SET_RAX" -> Some Ic_set_rax
  | "GET_RBP" -> Some Ic_get_rbp
  | "SET_RBP" -> Some Ic_set_rbp
  | "GET_RSP" -> Some Ic_get_rsp
  | "SET_RSP" -> Some Ic_set_rsp
  | "RIP" -> Some Ic_rip
  | "RDTSC" -> Some Ic_rdtsc
  | "BT" -> Some Ic_bt
  | "BTS" -> Some Ic_bts
  | "BTR" -> Some Ic_btr
  | "BTC" -> Some Ic_btc
  | "LBTS" -> Some Ic_lbts
  | "LBTR" -> Some Ic_lbtr
  | "LBTC" -> Some Ic_lbtc
  | "BSF" -> Some Ic_bsf
  | "BSR" -> Some Ic_bsr
  | "QUE_INIT" -> Some Ic_que_init
  | "QUE_INS" -> Some Ic_que_ins
  | "QUE_INS_REV" -> Some Ic_que_ins_rev
  | "QUE_REM" -> Some Ic_que_rem
  | "STRLEN" -> Some Ic_strlen
  | "BR_ZERO" -> Some Ic_br_zero
  | "BR_NOT_ZERO" -> Some Ic_br_not_zero
  | "BR_CARRY" -> Some Ic_br_carry
  | "BR_NOT_CARRY" -> Some Ic_br_not_carry
  | "BR_EQU_EQU" -> Some Ic_br_equ_equ
  | "BR_NOT_EQU" -> Some Ic_br_not_equ
  | "BR_LESS" -> Some Ic_br_less
  | "BR_GREATER_EQU" -> Some Ic_br_greater_equ
  | "BR_GREATER" -> Some Ic_br_greater
  | "BR_LESS_EQU" -> Some Ic_br_less_equ
  | "BR_2EQU_EQU" -> Some Ic_br_equ_equ2
  | "BR_2NOT_EQU" -> Some Ic_br_not_equ2
  | "BR_2LESS" -> Some Ic_br_less2
  | "BR_2GREATER_EQU" -> Some Ic_br_greater_equ2
  | "BR_2GREATER" -> Some Ic_br_greater2
  | "BR_2LESS_EQU" -> Some Ic_br_less_equ2
  | "BR_AND_ZERO" -> Some Ic_br_and_zero
  | "BR_AND_NOT_ZERO" -> Some Ic_br_and_not_zero
  | "BR_MM_ZERO" -> Some Ic_br_mm_zero
  | "BR_MM_NOT_ZERO" -> Some Ic_br_mm_not_zero
  | "BR_AND_AND_ZERO" -> Some Ic_br_and_and_zero
  | "BR_AND_AND_NOT_ZERO" -> Some Ic_br_and_and_not_zero
  | "BR_OR_OR_ZERO" -> Some Ic_br_or_or_zero
  | "BR_OR_OR_NOT_ZERO" -> Some Ic_br_or_or_not_zero
  | "BR_BT" -> Some Ic_br_bt
  | "BR_BTS" -> Some Ic_br_bts
  | "BR_BTR" -> Some Ic_br_btr
  | "BR_BTC" -> Some Ic_br_btc
  | "BR_NOT_BT" -> Some Ic_br_not_bt
  | "BR_NOT_BTS" -> Some Ic_br_not_bts
  | "BR_NOT_BTR" -> Some Ic_br_not_btr
  | "BR_NOT_BTC" -> Some Ic_br_not_btc
  | "SWAP_U8" -> Some Ic_swap_u8
  | "SWAP_U16" -> Some Ic_swap_u16
  | "SWAP_U32" -> Some Ic_swap_u32
  | "SWAP_U64" -> Some Ic_swap_i64
  | "ABS_I64" -> Some Ic_abs_i64
  | "SIGN_I64" -> Some Ic_sign_i64
  | "I64_MIN" -> Some Ic_min_i64
  | "U64_MIN" -> Some Ic_min_u64
  | "I64_MAX" -> Some Ic_max_i64
  | "U64_MAX" -> Some Ic_max_u64
  | "MOD_U64" -> Some Ic_mod_u64
  | "SQRI64" -> Some Ic_sqr_i64
  | "SQRU64" -> Some Ic_sqr_u64
  | "SQR" -> Some Ic_sqr
  | "ABS" -> Some Ic_abs
  | "SQRT" -> Some Ic_sqrt
  | "SIN" -> Some Ic_sin
  | "COS" -> Some Ic_cos
  | "TAN" -> Some Ic_tan
  | "ATAN" -> Some Ic_atan
  | _ -> None

let information_array =
  [|
    { opcode = Ic_end; source_name = "IC_END"; display_name = "END"; code = 0x00; argument_count = Zero; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 20; metadata_line = 17 };
    { opcode = Ic_nop1; source_name = "IC_NOP1"; display_name = "NOP1"; code = 0x01; argument_count = Zero; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 21; metadata_line = 18 };
    { opcode = Ic_end_exp; source_name = "IC_END_EXP"; display_name = "END_EXP"; code = 0x02; argument_count = One; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 22; metadata_line = 19 };
    { opcode = Ic_nop2; source_name = "IC_NOP2"; display_name = "NOP2"; code = 0x03; argument_count = Zero; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 23; metadata_line = 20 };
    { opcode = Ic_label; source_name = "IC_LABEL"; display_name = "LABEL"; code = 0x04; argument_count = Zero; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 24; metadata_line = 21 };
    { opcode = Ic_call_start; source_name = "IC_CALL_START"; display_name = "CALL_START"; code = 0x05; argument_count = Zero; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 25; metadata_line = 22 };
    { opcode = Ic_call_end; source_name = "IC_CALL_END"; display_name = "CALL_END"; code = 0x06; argument_count = Zero; result_count = 1; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 26; metadata_line = 23 };
    { opcode = Ic_call_end2; source_name = "IC_CALL_END2"; display_name = "CALL_END2"; code = 0x07; argument_count = Zero; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 27; metadata_line = 24 };
    { opcode = Ic_return_val; source_name = "IC_RETURN_VAL"; display_name = "RETURN_VAL"; code = 0x08; argument_count = One; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 28; metadata_line = 25 };
    { opcode = Ic_return_val2; source_name = "IC_RETURN_VAL2"; display_name = "RETURN_VAL2"; code = 0x09; argument_count = Zero; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 29; metadata_line = 26 };
    { opcode = Ic_imm_i64; source_name = "IC_IMM_I64"; display_name = "IMM_I64"; code = 0x0A; argument_count = Zero; result_count = 1; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 30; metadata_line = 27 };
    { opcode = Ic_imm_f64; source_name = "IC_IMM_F64"; display_name = "IMM_F64"; code = 0x0B; argument_count = Zero; result_count = 1; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 31; metadata_line = 28 };
    { opcode = Ic_str_const; source_name = "IC_STR_CONST"; display_name = "STR_CONST"; code = 0x0C; argument_count = Zero; result_count = 1; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 32; metadata_line = 29 };
    { opcode = Ic_abs_addr; source_name = "IC_ABS_ADDR"; display_name = "ABS_ADDR"; code = 0x0D; argument_count = Zero; result_count = 1; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 33; metadata_line = 30 };
    { opcode = Ic_addr_import; source_name = "IC_ADDR_IMPORT"; display_name = "ADDR_IMPORT"; code = 0x0E; argument_count = Zero; result_count = 1; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 34; metadata_line = 31 };
    { opcode = Ic_heap_glbl; source_name = "IC_HEAP_GLBL"; display_name = "HEAP_GLBL"; code = 0x0F; argument_count = Zero; result_count = 1; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 35; metadata_line = 32 };
    { opcode = Ic_sizeof; source_name = "IC_SIZEOF"; display_name = "SIZEOF"; code = 0x10; argument_count = Zero; result_count = 1; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 36; metadata_line = 33 };
    { opcode = Ic_type; source_name = "IC_TYPE"; display_name = "TYPE"; code = 0x11; argument_count = Zero; result_count = 1; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 37; metadata_line = 34 };
    { opcode = Ic_get_label; source_name = "IC_GET_LABEL"; display_name = "GET_LABEL"; code = 0x12; argument_count = Zero; result_count = 1; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 38; metadata_line = 35 };
    { opcode = Ic_rbp; source_name = "IC_RBP"; display_name = "RBP"; code = 0x13; argument_count = Zero; result_count = 1; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 39; metadata_line = 36 };
    { opcode = Ic_reg; source_name = "IC_REG"; display_name = "REG"; code = 0x14; argument_count = Zero; result_count = 1; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 40; metadata_line = 37 };
    { opcode = Ic_fs; source_name = "IC_FS"; display_name = "FS"; code = 0x15; argument_count = Zero; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 41; metadata_line = 38 };
    { opcode = Ic_mov_fs; source_name = "IC_MOV_FS"; display_name = "MOV_FS"; code = 0x16; argument_count = Zero; result_count = 1; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 42; metadata_line = 39 };
    { opcode = Ic_gs; source_name = "IC_GS"; display_name = "GS"; code = 0x17; argument_count = Zero; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 43; metadata_line = 40 };
    { opcode = Ic_mov_gs; source_name = "IC_MOV_GS"; display_name = "MOV_GS"; code = 0x18; argument_count = Zero; result_count = 1; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 44; metadata_line = 41 };
    { opcode = Ic_lea; source_name = "IC_LEA"; display_name = "LEA"; code = 0x19; argument_count = Zero; result_count = 1; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 46; metadata_line = 42 };
    { opcode = Ic_mov; source_name = "IC_MOV"; display_name = "MOV"; code = 0x1A; argument_count = Zero; result_count = 1; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 47; metadata_line = 43 };
    { opcode = Ic_to_i64; source_name = "IC_TO_I64"; display_name = "TO_I64"; code = 0x1B; argument_count = One; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 49; metadata_line = 44 };
    { opcode = Ic_to_f64; source_name = "IC_TO_F64"; display_name = "TO_F64"; code = 0x1C; argument_count = One; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 50; metadata_line = 45 };
    { opcode = Ic_to_bool; source_name = "IC_TO_BOOL"; display_name = "TO_BOOL"; code = 0x1D; argument_count = One; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 51; metadata_line = 46 };
    { opcode = Ic_toupper; source_name = "IC_TOUPPER"; display_name = "TOUPPER"; code = 0x1E; argument_count = One; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 52; metadata_line = 47 };
    { opcode = Ic_holyc_typecast; source_name = "IC_HOLYC_TYPECAST"; display_name = "HOLYC_TYPECAST"; code = 0x1F; argument_count = One; result_count = 1; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 53; metadata_line = 48 };
    { opcode = Ic_addr; source_name = "IC_ADDR"; display_name = "ADDR"; code = 0x20; argument_count = One; result_count = 1; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 55; metadata_line = 49 };
    { opcode = Ic_com; source_name = "IC_COM"; display_name = "COM"; code = 0x21; argument_count = One; result_count = 1; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 56; metadata_line = 50 };
    { opcode = Ic_not; source_name = "IC_NOT"; display_name = "NOT"; code = 0x22; argument_count = One; result_count = 1; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 57; metadata_line = 51 };
    { opcode = Ic_unary_minus; source_name = "IC_UNARY_MINUS"; display_name = "UNARY_MINUS"; code = 0x23; argument_count = One; result_count = 1; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 58; metadata_line = 52 };
    { opcode = Ic_deref; source_name = "IC_DEREF"; display_name = "DEREF"; code = 0x24; argument_count = One; result_count = 1; structural_type = Dereference; pops_float = false; prevents_constant_folding = true; definition_line = 60; metadata_line = 53 };
    { opcode = Ic_deref_pp; source_name = "IC_DEREF_PP"; display_name = "DEREF_PP"; code = 0x25; argument_count = One; result_count = 1; structural_type = Dereference; pops_float = false; prevents_constant_folding = true; definition_line = 61; metadata_line = 54 };
    { opcode = Ic_deref_mm; source_name = "IC_DEREF_MM"; display_name = "DEREF_MM"; code = 0x26; argument_count = One; result_count = 1; structural_type = Dereference; pops_float = false; prevents_constant_folding = true; definition_line = 62; metadata_line = 55 };
    { opcode = Ic__pp; source_name = "IC__PP"; display_name = "_PP"; code = 0x27; argument_count = One; result_count = 1; structural_type = Dereference; pops_float = true; prevents_constant_folding = true; definition_line = 63; metadata_line = 56 };
    { opcode = Ic__mm; source_name = "IC__MM"; display_name = "_MM"; code = 0x28; argument_count = One; result_count = 1; structural_type = Dereference; pops_float = true; prevents_constant_folding = true; definition_line = 64; metadata_line = 57 };
    { opcode = Ic_pp_; source_name = "IC_PP_"; display_name = "PP_"; code = 0x29; argument_count = One; result_count = 1; structural_type = Dereference; pops_float = true; prevents_constant_folding = true; definition_line = 65; metadata_line = 58 };
    { opcode = Ic_mm_; source_name = "IC_MM_"; display_name = "MM_"; code = 0x2A; argument_count = One; result_count = 1; structural_type = Dereference; pops_float = true; prevents_constant_folding = true; definition_line = 66; metadata_line = 59 };
    { opcode = Ic_shl; source_name = "IC_SHL"; display_name = "SHL"; code = 0x2B; argument_count = Two; result_count = 1; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 68; metadata_line = 60 };
    { opcode = Ic_shr; source_name = "IC_SHR"; display_name = "SHR"; code = 0x2C; argument_count = Two; result_count = 1; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 69; metadata_line = 61 };
    { opcode = Ic_shl_const; source_name = "IC_SHL_CONST"; display_name = "SHL_CONST"; code = 0x2D; argument_count = One; result_count = 1; structural_type = Assignment; pops_float = false; prevents_constant_folding = false; definition_line = 70; metadata_line = 62 };
    { opcode = Ic_shr_const; source_name = "IC_SHR_CONST"; display_name = "SHR_CONST"; code = 0x2E; argument_count = One; result_count = 1; structural_type = Assignment; pops_float = false; prevents_constant_folding = false; definition_line = 71; metadata_line = 63 };
    { opcode = Ic_power; source_name = "IC_POWER"; display_name = "POWER"; code = 0x2F; argument_count = Two; result_count = 1; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 72; metadata_line = 64 };
    { opcode = Ic_mul; source_name = "IC_MUL"; display_name = "MUL"; code = 0x30; argument_count = Two; result_count = 1; structural_type = Null; pops_float = true; prevents_constant_folding = false; definition_line = 74; metadata_line = 65 };
    { opcode = Ic_div; source_name = "IC_DIV"; display_name = "DIV"; code = 0x31; argument_count = Two; result_count = 1; structural_type = Null; pops_float = true; prevents_constant_folding = false; definition_line = 75; metadata_line = 66 };
    { opcode = Ic_mod; source_name = "IC_MOD"; display_name = "MOD"; code = 0x32; argument_count = Two; result_count = 1; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 76; metadata_line = 67 };
    { opcode = Ic_and; source_name = "IC_AND"; display_name = "AND"; code = 0x33; argument_count = Two; result_count = 1; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 78; metadata_line = 68 };
    { opcode = Ic_or; source_name = "IC_OR"; display_name = "OR"; code = 0x34; argument_count = Two; result_count = 1; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 80; metadata_line = 69 };
    { opcode = Ic_xor; source_name = "IC_XOR"; display_name = "XOR"; code = 0x35; argument_count = Two; result_count = 1; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 81; metadata_line = 70 };
    { opcode = Ic_add; source_name = "IC_ADD"; display_name = "ADD"; code = 0x36; argument_count = Two; result_count = 1; structural_type = Null; pops_float = true; prevents_constant_folding = false; definition_line = 83; metadata_line = 71 };
    { opcode = Ic_sub; source_name = "IC_SUB"; display_name = "SUB"; code = 0x37; argument_count = Two; result_count = 1; structural_type = Null; pops_float = true; prevents_constant_folding = false; definition_line = 84; metadata_line = 72 };
    { opcode = Ic_add_const; source_name = "IC_ADD_CONST"; display_name = "ADD_CONST"; code = 0x38; argument_count = One; result_count = 1; structural_type = Assignment; pops_float = false; prevents_constant_folding = false; definition_line = 85; metadata_line = 73 };
    { opcode = Ic_sub_const; source_name = "IC_SUB_CONST"; display_name = "SUB_CONST"; code = 0x39; argument_count = One; result_count = 1; structural_type = Assignment; pops_float = false; prevents_constant_folding = false; definition_line = 86; metadata_line = 74 };
    { opcode = Ic_equ_equ; source_name = "IC_EQU_EQU"; display_name = "EQU_EQU"; code = 0x3A; argument_count = Two; result_count = 1; structural_type = Comparison; pops_float = false; prevents_constant_folding = false; definition_line = 88; metadata_line = 75 };
    { opcode = Ic_not_equ; source_name = "IC_NOT_EQU"; display_name = "NOT_EQU"; code = 0x3B; argument_count = Two; result_count = 1; structural_type = Comparison; pops_float = false; prevents_constant_folding = false; definition_line = 89; metadata_line = 76 };
    { opcode = Ic_less; source_name = "IC_LESS"; display_name = "LESS"; code = 0x3C; argument_count = Two; result_count = 1; structural_type = Comparison; pops_float = false; prevents_constant_folding = false; definition_line = 90; metadata_line = 77 };
    { opcode = Ic_greater_equ; source_name = "IC_GREATER_EQU"; display_name = "GREATER_EQU"; code = 0x3D; argument_count = Two; result_count = 1; structural_type = Comparison; pops_float = false; prevents_constant_folding = false; definition_line = 91; metadata_line = 78 };
    { opcode = Ic_greater; source_name = "IC_GREATER"; display_name = "GREATER"; code = 0x3E; argument_count = Two; result_count = 1; structural_type = Comparison; pops_float = false; prevents_constant_folding = false; definition_line = 92; metadata_line = 79 };
    { opcode = Ic_less_equ; source_name = "IC_LESS_EQU"; display_name = "LESS_EQU"; code = 0x3F; argument_count = Two; result_count = 1; structural_type = Comparison; pops_float = false; prevents_constant_folding = false; definition_line = 93; metadata_line = 80 };
    { opcode = Ic_push_cmp; source_name = "IC_PUSH_CMP"; display_name = "PUSH_CMP"; code = 0x40; argument_count = Zero; result_count = 1; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 94; metadata_line = 81 };
    { opcode = Ic_and_and; source_name = "IC_AND_AND"; display_name = "AND_AND"; code = 0x41; argument_count = Two; result_count = 1; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 96; metadata_line = 82 };
    { opcode = Ic_or_or; source_name = "IC_OR_OR"; display_name = "OR_OR"; code = 0x42; argument_count = Two; result_count = 1; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 98; metadata_line = 83 };
    { opcode = Ic_xor_xor; source_name = "IC_XOR_XOR"; display_name = "XOR_XOR"; code = 0x43; argument_count = Two; result_count = 1; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 99; metadata_line = 84 };
    { opcode = Ic_assign; source_name = "IC_ASSIGN"; display_name = "ASSIGN"; code = 0x44; argument_count = Two; result_count = 1; structural_type = Assignment; pops_float = false; prevents_constant_folding = true; definition_line = 101; metadata_line = 85 };
    { opcode = Ic_assign_pp; source_name = "IC_ASSIGN_PP"; display_name = "ASSIGN_PP"; code = 0x45; argument_count = Two; result_count = 1; structural_type = Assignment; pops_float = false; prevents_constant_folding = true; definition_line = 102; metadata_line = 86 };
    { opcode = Ic_assign_mm; source_name = "IC_ASSIGN_MM"; display_name = "ASSIGN_MM"; code = 0x46; argument_count = Two; result_count = 1; structural_type = Assignment; pops_float = false; prevents_constant_folding = true; definition_line = 103; metadata_line = 87 };
    { opcode = Ic_shl_equ; source_name = "IC_SHL_EQU"; display_name = "SHL_EQU"; code = 0x47; argument_count = Two; result_count = 1; structural_type = Assignment; pops_float = false; prevents_constant_folding = true; definition_line = 105; metadata_line = 88 };
    { opcode = Ic_shr_equ; source_name = "IC_SHR_EQU"; display_name = "SHR_EQU"; code = 0x48; argument_count = Two; result_count = 1; structural_type = Assignment; pops_float = false; prevents_constant_folding = true; definition_line = 106; metadata_line = 89 };
    { opcode = Ic_mul_equ; source_name = "IC_MUL_EQU"; display_name = "MUL_EQU"; code = 0x49; argument_count = Two; result_count = 1; structural_type = Assignment; pops_float = false; prevents_constant_folding = true; definition_line = 107; metadata_line = 90 };
    { opcode = Ic_div_equ; source_name = "IC_DIV_EQU"; display_name = "DIV_EQU"; code = 0x4A; argument_count = Two; result_count = 1; structural_type = Assignment; pops_float = false; prevents_constant_folding = true; definition_line = 108; metadata_line = 91 };
    { opcode = Ic_mod_equ; source_name = "IC_MOD_EQU"; display_name = "MOD_EQU"; code = 0x4B; argument_count = Two; result_count = 1; structural_type = Assignment; pops_float = false; prevents_constant_folding = true; definition_line = 109; metadata_line = 92 };
    { opcode = Ic_and_equ; source_name = "IC_AND_EQU"; display_name = "AND_EQU"; code = 0x4C; argument_count = Two; result_count = 1; structural_type = Assignment; pops_float = false; prevents_constant_folding = true; definition_line = 110; metadata_line = 93 };
    { opcode = Ic_or_equ; source_name = "IC_OR_EQU"; display_name = "OR_EQU"; code = 0x4D; argument_count = Two; result_count = 1; structural_type = Assignment; pops_float = false; prevents_constant_folding = true; definition_line = 111; metadata_line = 94 };
    { opcode = Ic_xor_equ; source_name = "IC_XOR_EQU"; display_name = "XOR_EQU"; code = 0x4E; argument_count = Two; result_count = 1; structural_type = Assignment; pops_float = false; prevents_constant_folding = true; definition_line = 112; metadata_line = 95 };
    { opcode = Ic_add_equ; source_name = "IC_ADD_EQU"; display_name = "ADD_EQU"; code = 0x4F; argument_count = Two; result_count = 1; structural_type = Assignment; pops_float = false; prevents_constant_folding = true; definition_line = 113; metadata_line = 96 };
    { opcode = Ic_sub_equ; source_name = "IC_SUB_EQU"; display_name = "SUB_EQU"; code = 0x50; argument_count = Two; result_count = 1; structural_type = Assignment; pops_float = false; prevents_constant_folding = true; definition_line = 114; metadata_line = 97 };
    { opcode = Ic_jmp; source_name = "IC_JMP"; display_name = "JMP"; code = 0x51; argument_count = Zero; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 116; metadata_line = 98 };
    { opcode = Ic_sub_call; source_name = "IC_SUB_CALL"; display_name = "SUB_CALL"; code = 0x52; argument_count = Zero; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 117; metadata_line = 99 };
    { opcode = Ic_switch; source_name = "IC_SWITCH"; display_name = "SWITCH"; code = 0x53; argument_count = Two; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 118; metadata_line = 100 };
    { opcode = Ic_nobound_switch; source_name = "IC_NOBOUND_SWITCH"; display_name = "NOBOUND_SWITCH"; code = 0x54; argument_count = Two; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 119; metadata_line = 101 };
    { opcode = Ic_add_rsp; source_name = "IC_ADD_RSP"; display_name = "ADD_RSP"; code = 0x55; argument_count = Variable; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 121; metadata_line = 102 };
    { opcode = Ic_add_rsp1; source_name = "IC_ADD_RSP1"; display_name = "ADD_RSP1"; code = 0x56; argument_count = Variable; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 122; metadata_line = 103 };
    { opcode = Ic_enter; source_name = "IC_ENTER"; display_name = "ENTER"; code = 0x57; argument_count = Zero; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 123; metadata_line = 104 };
    { opcode = Ic_push_regs; source_name = "IC_PUSH_REGS"; display_name = "PUSH_REGS"; code = 0x58; argument_count = Zero; result_count = 0; structural_type = Assignment; pops_float = false; prevents_constant_folding = false; definition_line = 124; metadata_line = 105 };
    { opcode = Ic_pop_regs; source_name = "IC_POP_REGS"; display_name = "POP_REGS"; code = 0x59; argument_count = Zero; result_count = 0; structural_type = Assignment; pops_float = false; prevents_constant_folding = false; definition_line = 125; metadata_line = 106 };
    { opcode = Ic_leave; source_name = "IC_LEAVE"; display_name = "LEAVE"; code = 0x5A; argument_count = Zero; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 126; metadata_line = 107 };
    { opcode = Ic_ret; source_name = "IC_RET"; display_name = "RET"; code = 0x5B; argument_count = Zero; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 127; metadata_line = 108 };
    { opcode = Ic_call; source_name = "IC_CALL"; display_name = "CALL"; code = 0x5C; argument_count = Zero; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 129; metadata_line = 109 };
    { opcode = Ic_call_indirect; source_name = "IC_CALL_INDIRECT"; display_name = "CALL_INDIRECT"; code = 0x5D; argument_count = Zero; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 130; metadata_line = 110 };
    { opcode = Ic_call_indirect2; source_name = "IC_CALL_INDIRECT2"; display_name = "CALL_INDIRECT2"; code = 0x5E; argument_count = Zero; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 131; metadata_line = 111 };
    { opcode = Ic_call_import; source_name = "IC_CALL_IMPORT"; display_name = "CALL_IMPORT"; code = 0x5F; argument_count = Zero; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 132; metadata_line = 112 };
    { opcode = Ic_call_extern; source_name = "IC_CALL_EXTERN"; display_name = "CALL_EXTERN"; code = 0x60; argument_count = Zero; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 133; metadata_line = 113 };
    { opcode = Ic_asm; source_name = "IC_ASM"; display_name = "ASM"; code = 0x61; argument_count = Zero; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 135; metadata_line = 114 };
    { opcode = Ic_push; source_name = "IC_PUSH"; display_name = "PUSH"; code = 0x62; argument_count = One; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 136; metadata_line = 115 };
    { opcode = Ic_pop; source_name = "IC_POP"; display_name = "POP"; code = 0x63; argument_count = Zero; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 137; metadata_line = 116 };
    { opcode = Ic_clflush; source_name = "IC_CLFLUSH"; display_name = "CLFLUSH"; code = 0x64; argument_count = One; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 139; metadata_line = 117 };
    { opcode = Ic_invlpg; source_name = "IC_INVLPG"; display_name = "INVLPG"; code = 0x65; argument_count = One; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 140; metadata_line = 118 };
    { opcode = Ic_in_u8; source_name = "IC_IN_U8"; display_name = "IN_U8"; code = 0x66; argument_count = One; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 142; metadata_line = 119 };
    { opcode = Ic_in_u16; source_name = "IC_IN_U16"; display_name = "IN_U16"; code = 0x67; argument_count = One; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 143; metadata_line = 120 };
    { opcode = Ic_in_u32; source_name = "IC_IN_U32"; display_name = "IN_U32"; code = 0x68; argument_count = One; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 144; metadata_line = 121 };
    { opcode = Ic_out_u8; source_name = "IC_OUT_U8"; display_name = "OUT_U8"; code = 0x69; argument_count = Two; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 145; metadata_line = 122 };
    { opcode = Ic_out_u16; source_name = "IC_OUT_U16"; display_name = "OUT_U16"; code = 0x6A; argument_count = Two; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 146; metadata_line = 123 };
    { opcode = Ic_out_u32; source_name = "IC_OUT_U32"; display_name = "OUT_U32"; code = 0x6B; argument_count = Two; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 147; metadata_line = 124 };
    { opcode = Ic_get_rflags; source_name = "IC_GET_RFLAGS"; display_name = "GET_RFLAGS"; code = 0x6C; argument_count = Zero; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 149; metadata_line = 125 };
    { opcode = Ic_carry; source_name = "IC_CARRY"; display_name = "CARRY"; code = 0x6D; argument_count = Zero; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 150; metadata_line = 126 };
    { opcode = Ic_set_rflags; source_name = "IC_SET_RFLAGS"; display_name = "SET_RFLAGS"; code = 0x6E; argument_count = One; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 151; metadata_line = 127 };
    { opcode = Ic_get_rax; source_name = "IC_GET_RAX"; display_name = "GET_RAX"; code = 0x6F; argument_count = Zero; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 152; metadata_line = 128 };
    { opcode = Ic_set_rax; source_name = "IC_SET_RAX"; display_name = "SET_RAX"; code = 0x70; argument_count = One; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 153; metadata_line = 129 };
    { opcode = Ic_get_rbp; source_name = "IC_GET_RBP"; display_name = "GET_RBP"; code = 0x71; argument_count = Zero; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 154; metadata_line = 130 };
    { opcode = Ic_set_rbp; source_name = "IC_SET_RBP"; display_name = "SET_RBP"; code = 0x72; argument_count = One; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 155; metadata_line = 131 };
    { opcode = Ic_get_rsp; source_name = "IC_GET_RSP"; display_name = "GET_RSP"; code = 0x73; argument_count = Zero; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 156; metadata_line = 132 };
    { opcode = Ic_set_rsp; source_name = "IC_SET_RSP"; display_name = "SET_RSP"; code = 0x74; argument_count = One; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 157; metadata_line = 133 };
    { opcode = Ic_rip; source_name = "IC_RIP"; display_name = "RIP"; code = 0x75; argument_count = Zero; result_count = 1; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 158; metadata_line = 134 };
    { opcode = Ic_rdtsc; source_name = "IC_RDTSC"; display_name = "RDTSC"; code = 0x76; argument_count = Zero; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 160; metadata_line = 135 };
    { opcode = Ic_bt; source_name = "IC_BT"; display_name = "BT"; code = 0x77; argument_count = Two; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 162; metadata_line = 136 };
    { opcode = Ic_bts; source_name = "IC_BTS"; display_name = "BTS"; code = 0x78; argument_count = Two; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 163; metadata_line = 137 };
    { opcode = Ic_btr; source_name = "IC_BTR"; display_name = "BTR"; code = 0x79; argument_count = Two; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 164; metadata_line = 138 };
    { opcode = Ic_btc; source_name = "IC_BTC"; display_name = "BTC"; code = 0x7A; argument_count = Two; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 165; metadata_line = 139 };
    { opcode = Ic_lbts; source_name = "IC_LBTS"; display_name = "LBTS"; code = 0x7B; argument_count = Two; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 166; metadata_line = 140 };
    { opcode = Ic_lbtr; source_name = "IC_LBTR"; display_name = "LBTR"; code = 0x7C; argument_count = Two; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 167; metadata_line = 141 };
    { opcode = Ic_lbtc; source_name = "IC_LBTC"; display_name = "LBTC"; code = 0x7D; argument_count = Two; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 168; metadata_line = 142 };
    { opcode = Ic_bsf; source_name = "IC_BSF"; display_name = "BSF"; code = 0x7E; argument_count = One; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 169; metadata_line = 143 };
    { opcode = Ic_bsr; source_name = "IC_BSR"; display_name = "BSR"; code = 0x7F; argument_count = One; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 170; metadata_line = 144 };
    { opcode = Ic_que_init; source_name = "IC_QUE_INIT"; display_name = "QUE_INIT"; code = 0x80; argument_count = One; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 172; metadata_line = 145 };
    { opcode = Ic_que_ins; source_name = "IC_QUE_INS"; display_name = "QUE_INS"; code = 0x81; argument_count = Two; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 173; metadata_line = 146 };
    { opcode = Ic_que_ins_rev; source_name = "IC_QUE_INS_REV"; display_name = "QUE_INS_REV"; code = 0x82; argument_count = Two; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 174; metadata_line = 147 };
    { opcode = Ic_que_rem; source_name = "IC_QUE_REM"; display_name = "QUE_REM"; code = 0x83; argument_count = One; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 175; metadata_line = 148 };
    { opcode = Ic_strlen; source_name = "IC_STRLEN"; display_name = "STRLEN"; code = 0x84; argument_count = One; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 177; metadata_line = 149 };
    { opcode = Ic_br_zero; source_name = "IC_BR_ZERO"; display_name = "BR_ZERO"; code = 0x85; argument_count = One; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 179; metadata_line = 150 };
    { opcode = Ic_br_not_zero; source_name = "IC_BR_NOT_ZERO"; display_name = "BR_NOT_ZERO"; code = 0x86; argument_count = One; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 180; metadata_line = 151 };
    { opcode = Ic_br_carry; source_name = "IC_BR_CARRY"; display_name = "BR_CARRY"; code = 0x87; argument_count = Zero; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 181; metadata_line = 152 };
    { opcode = Ic_br_not_carry; source_name = "IC_BR_NOT_CARRY"; display_name = "BR_NOT_CARRY"; code = 0x88; argument_count = Zero; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 182; metadata_line = 153 };
    { opcode = Ic_br_equ_equ; source_name = "IC_BR_EQU_EQU"; display_name = "BR_EQU_EQU"; code = 0x89; argument_count = Two; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 184; metadata_line = 154 };
    { opcode = Ic_br_not_equ; source_name = "IC_BR_NOT_EQU"; display_name = "BR_NOT_EQU"; code = 0x8A; argument_count = Two; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 185; metadata_line = 155 };
    { opcode = Ic_br_less; source_name = "IC_BR_LESS"; display_name = "BR_LESS"; code = 0x8B; argument_count = Two; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 186; metadata_line = 156 };
    { opcode = Ic_br_greater_equ; source_name = "IC_BR_GREATER_EQU"; display_name = "BR_GREATER_EQU"; code = 0x8C; argument_count = Two; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 187; metadata_line = 157 };
    { opcode = Ic_br_greater; source_name = "IC_BR_GREATER"; display_name = "BR_GREATER"; code = 0x8D; argument_count = Two; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 188; metadata_line = 158 };
    { opcode = Ic_br_less_equ; source_name = "IC_BR_LESS_EQU"; display_name = "BR_LESS_EQU"; code = 0x8E; argument_count = Two; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 189; metadata_line = 159 };
    { opcode = Ic_br_equ_equ2; source_name = "IC_BR_EQU_EQU2"; display_name = "BR_2EQU_EQU"; code = 0x8F; argument_count = Two; result_count = 1; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 191; metadata_line = 160 };
    { opcode = Ic_br_not_equ2; source_name = "IC_BR_NOT_EQU2"; display_name = "BR_2NOT_EQU"; code = 0x90; argument_count = Two; result_count = 1; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 192; metadata_line = 161 };
    { opcode = Ic_br_less2; source_name = "IC_BR_LESS2"; display_name = "BR_2LESS"; code = 0x91; argument_count = Two; result_count = 1; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 193; metadata_line = 162 };
    { opcode = Ic_br_greater_equ2; source_name = "IC_BR_GREATER_EQU2"; display_name = "BR_2GREATER_EQU"; code = 0x92; argument_count = Two; result_count = 1; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 194; metadata_line = 163 };
    { opcode = Ic_br_greater2; source_name = "IC_BR_GREATER2"; display_name = "BR_2GREATER"; code = 0x93; argument_count = Two; result_count = 1; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 195; metadata_line = 164 };
    { opcode = Ic_br_less_equ2; source_name = "IC_BR_LESS_EQU2"; display_name = "BR_2LESS_EQU"; code = 0x94; argument_count = Two; result_count = 1; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 196; metadata_line = 165 };
    { opcode = Ic_br_and_zero; source_name = "IC_BR_AND_ZERO"; display_name = "BR_AND_ZERO"; code = 0x95; argument_count = Two; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 198; metadata_line = 166 };
    { opcode = Ic_br_and_not_zero; source_name = "IC_BR_AND_NOT_ZERO"; display_name = "BR_AND_NOT_ZERO"; code = 0x96; argument_count = Two; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 199; metadata_line = 167 };
    { opcode = Ic_br_mm_zero; source_name = "IC_BR_MM_ZERO"; display_name = "BR_MM_ZERO"; code = 0x97; argument_count = One; result_count = 0; structural_type = Dereference; pops_float = false; prevents_constant_folding = true; definition_line = 200; metadata_line = 168 };
    { opcode = Ic_br_mm_not_zero; source_name = "IC_BR_MM_NOT_ZERO"; display_name = "BR_MM_NOT_ZERO"; code = 0x98; argument_count = One; result_count = 0; structural_type = Dereference; pops_float = false; prevents_constant_folding = true; definition_line = 201; metadata_line = 169 };
    { opcode = Ic_br_and_and_zero; source_name = "IC_BR_AND_AND_ZERO"; display_name = "BR_AND_AND_ZERO"; code = 0x99; argument_count = Two; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 202; metadata_line = 170 };
    { opcode = Ic_br_and_and_not_zero; source_name = "IC_BR_AND_AND_NOT_ZERO"; display_name = "BR_AND_AND_NOT_ZERO"; code = 0x9A; argument_count = Two; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 203; metadata_line = 171 };
    { opcode = Ic_br_or_or_zero; source_name = "IC_BR_OR_OR_ZERO"; display_name = "BR_OR_OR_ZERO"; code = 0x9B; argument_count = Two; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 204; metadata_line = 172 };
    { opcode = Ic_br_or_or_not_zero; source_name = "IC_BR_OR_OR_NOT_ZERO"; display_name = "BR_OR_OR_NOT_ZERO"; code = 0x9C; argument_count = Two; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 205; metadata_line = 173 };
    { opcode = Ic_br_bt; source_name = "IC_BR_BT"; display_name = "BR_BT"; code = 0x9D; argument_count = Two; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 207; metadata_line = 174 };
    { opcode = Ic_br_bts; source_name = "IC_BR_BTS"; display_name = "BR_BTS"; code = 0x9E; argument_count = Two; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 208; metadata_line = 175 };
    { opcode = Ic_br_btr; source_name = "IC_BR_BTR"; display_name = "BR_BTR"; code = 0x9F; argument_count = Two; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 209; metadata_line = 176 };
    { opcode = Ic_br_btc; source_name = "IC_BR_BTC"; display_name = "BR_BTC"; code = 0xA0; argument_count = Two; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 210; metadata_line = 177 };
    { opcode = Ic_br_not_bt; source_name = "IC_BR_NOT_BT"; display_name = "BR_NOT_BT"; code = 0xA1; argument_count = Two; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 211; metadata_line = 178 };
    { opcode = Ic_br_not_bts; source_name = "IC_BR_NOT_BTS"; display_name = "BR_NOT_BTS"; code = 0xA2; argument_count = Two; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 212; metadata_line = 179 };
    { opcode = Ic_br_not_btr; source_name = "IC_BR_NOT_BTR"; display_name = "BR_NOT_BTR"; code = 0xA3; argument_count = Two; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 213; metadata_line = 180 };
    { opcode = Ic_br_not_btc; source_name = "IC_BR_NOT_BTC"; display_name = "BR_NOT_BTC"; code = 0xA4; argument_count = Two; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 214; metadata_line = 181 };
    { opcode = Ic_swap_u8; source_name = "IC_SWAP_U8"; display_name = "SWAP_U8"; code = 0xA5; argument_count = Two; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 216; metadata_line = 182 };
    { opcode = Ic_swap_u16; source_name = "IC_SWAP_U16"; display_name = "SWAP_U16"; code = 0xA6; argument_count = Two; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 217; metadata_line = 183 };
    { opcode = Ic_swap_u32; source_name = "IC_SWAP_U32"; display_name = "SWAP_U32"; code = 0xA7; argument_count = Two; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 218; metadata_line = 184 };
    { opcode = Ic_swap_i64; source_name = "IC_SWAP_I64"; display_name = "SWAP_U64"; code = 0xA8; argument_count = Two; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = true; definition_line = 219; metadata_line = 185 };
    { opcode = Ic_abs_i64; source_name = "IC_ABS_I64"; display_name = "ABS_I64"; code = 0xA9; argument_count = One; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 221; metadata_line = 186 };
    { opcode = Ic_sign_i64; source_name = "IC_SIGN_I64"; display_name = "SIGN_I64"; code = 0xAA; argument_count = One; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 222; metadata_line = 187 };
    { opcode = Ic_min_i64; source_name = "IC_MIN_I64"; display_name = "I64_MIN"; code = 0xAB; argument_count = Two; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 223; metadata_line = 188 };
    { opcode = Ic_min_u64; source_name = "IC_MIN_U64"; display_name = "U64_MIN"; code = 0xAC; argument_count = Two; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 224; metadata_line = 189 };
    { opcode = Ic_max_i64; source_name = "IC_MAX_I64"; display_name = "I64_MAX"; code = 0xAD; argument_count = Two; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 225; metadata_line = 190 };
    { opcode = Ic_max_u64; source_name = "IC_MAX_U64"; display_name = "U64_MAX"; code = 0xAE; argument_count = Two; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 226; metadata_line = 191 };
    { opcode = Ic_mod_u64; source_name = "IC_MOD_U64"; display_name = "MOD_U64"; code = 0xAF; argument_count = Two; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 227; metadata_line = 192 };
    { opcode = Ic_sqr_i64; source_name = "IC_SQR_I64"; display_name = "SQRI64"; code = 0xB0; argument_count = One; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 228; metadata_line = 193 };
    { opcode = Ic_sqr_u64; source_name = "IC_SQR_U64"; display_name = "SQRU64"; code = 0xB1; argument_count = One; result_count = 0; structural_type = Null; pops_float = false; prevents_constant_folding = false; definition_line = 229; metadata_line = 194 };
    { opcode = Ic_sqr; source_name = "IC_SQR"; display_name = "SQR"; code = 0xB2; argument_count = One; result_count = 1; structural_type = Null; pops_float = true; prevents_constant_folding = false; definition_line = 230; metadata_line = 195 };
    { opcode = Ic_abs; source_name = "IC_ABS"; display_name = "ABS"; code = 0xB3; argument_count = One; result_count = 1; structural_type = Null; pops_float = true; prevents_constant_folding = false; definition_line = 231; metadata_line = 196 };
    { opcode = Ic_sqrt; source_name = "IC_SQRT"; display_name = "SQRT"; code = 0xB4; argument_count = One; result_count = 1; structural_type = Null; pops_float = true; prevents_constant_folding = false; definition_line = 232; metadata_line = 197 };
    { opcode = Ic_sin; source_name = "IC_SIN"; display_name = "SIN"; code = 0xB5; argument_count = One; result_count = 1; structural_type = Null; pops_float = true; prevents_constant_folding = false; definition_line = 233; metadata_line = 198 };
    { opcode = Ic_cos; source_name = "IC_COS"; display_name = "COS"; code = 0xB6; argument_count = One; result_count = 1; structural_type = Null; pops_float = true; prevents_constant_folding = false; definition_line = 234; metadata_line = 199 };
    { opcode = Ic_tan; source_name = "IC_TAN"; display_name = "TAN"; code = 0xB7; argument_count = One; result_count = 1; structural_type = Null; pops_float = true; prevents_constant_folding = false; definition_line = 235; metadata_line = 200 };
    { opcode = Ic_atan; source_name = "IC_ATAN"; display_name = "ATAN"; code = 0xB8; argument_count = One; result_count = 1; structural_type = Null; pops_float = true; prevents_constant_folding = false; definition_line = 236; metadata_line = 201 };
  |]

let information = Array.to_list information_array

let info opcode = Array.get information_array (to_code opcode)

let to_source_name opcode = (info opcode).source_name
let to_display_name opcode = (info opcode).display_name

let compare left right = Int.compare (to_code left) (to_code right)
let equal left right = compare left right = 0
