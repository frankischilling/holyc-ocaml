(* Generated interface for the pinned TempleOS intermediate-code specification. *)

[@@@ocamlformat "disable"]

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

type info = private {
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

type source = { path : string; sha256 : string }

val reference_commit : string
val sources : source list
val count : int
val all : t list
val compare : t -> t -> int
val equal : t -> t -> bool
val to_code : t -> int
val of_code : int -> t option
val to_source_name : t -> string
val of_source_name : string -> t option
val to_display_name : t -> string
val of_display_name : string -> t option
val info : t -> info
val information : info list
