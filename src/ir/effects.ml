type access = None | Read | Write | Read_write | Opaque

type call =
  | No_call
  | Local_call
  | Direct_call
  | Indirect_call
  | Import_call
  | Extern_call

type t = {
  memory : access;
  stack : access;
  machine : access;
  port : access;
  cache_tlb : access;
  clock : access;
  call : call;
  inline_assembly : bool;
  prevents_constant_folding : bool;
}

let make ?(memory = None) ?(stack = None) ?(machine = None) ?(port = None)
    ?(cache_tlb = None) ?(clock = None) ?(call = No_call)
    ?(inline_assembly = false) opcode =
  {
    memory;
    stack;
    machine;
    port;
    cache_tlb;
    clock;
    call;
    inline_assembly;
    prevents_constant_folding = (Opcode.info opcode).prevents_constant_folding;
  }

let classify opcode =
  match opcode with
  | Opcode.Ic_sub_call ->
      make ~memory:Opaque ~stack:Read_write ~machine:Opaque ~call:Local_call
        opcode
  | Opcode.Ic_call ->
      make ~memory:Opaque ~stack:Read_write ~machine:Opaque ~call:Direct_call
        opcode
  | Opcode.Ic_call_indirect | Opcode.Ic_call_indirect2 ->
      make ~memory:Opaque ~stack:Read_write ~machine:Opaque ~call:Indirect_call
        opcode
  | Opcode.Ic_call_import ->
      make ~memory:Opaque ~stack:Read_write ~machine:Opaque ~call:Import_call
        opcode
  | Opcode.Ic_call_extern ->
      make ~memory:Opaque ~stack:Read_write ~machine:Opaque ~call:Extern_call
        opcode
  | Opcode.Ic_asm ->
      make ~memory:Opaque ~stack:Opaque ~machine:Opaque ~port:Opaque
        ~cache_tlb:Opaque ~clock:Opaque ~inline_assembly:true opcode
  | Opcode.Ic_deref -> make ~memory:Read opcode
  | Opcode.Ic_deref_pp
  | Opcode.Ic_deref_mm
  | Opcode.Ic__pp
  | Opcode.Ic__mm
  | Opcode.Ic_pp_
  | Opcode.Ic_mm_
  | Opcode.Ic_shl_const
  | Opcode.Ic_shr_const
  | Opcode.Ic_add_const
  | Opcode.Ic_sub_const
  | Opcode.Ic_assign_pp
  | Opcode.Ic_assign_mm
  | Opcode.Ic_shl_equ
  | Opcode.Ic_shr_equ
  | Opcode.Ic_mul_equ
  | Opcode.Ic_div_equ
  | Opcode.Ic_mod_equ
  | Opcode.Ic_and_equ
  | Opcode.Ic_or_equ
  | Opcode.Ic_xor_equ
  | Opcode.Ic_add_equ
  | Opcode.Ic_sub_equ
  | Opcode.Ic_bts
  | Opcode.Ic_btr
  | Opcode.Ic_btc
  | Opcode.Ic_lbts
  | Opcode.Ic_lbtr
  | Opcode.Ic_lbtc
  | Opcode.Ic_br_bts
  | Opcode.Ic_br_btr
  | Opcode.Ic_br_btc
  | Opcode.Ic_br_not_bts
  | Opcode.Ic_br_not_btr
  | Opcode.Ic_br_not_btc
  | Opcode.Ic_br_mm_zero
  | Opcode.Ic_br_mm_not_zero
  | Opcode.Ic_swap_u8
  | Opcode.Ic_swap_u16
  | Opcode.Ic_swap_u32
  | Opcode.Ic_swap_i64
  | Opcode.Ic_que_ins
  | Opcode.Ic_que_ins_rev
  | Opcode.Ic_que_rem
  | Opcode.Ic_mov -> make ~memory:Read_write opcode
  | Opcode.Ic_assign | Opcode.Ic_que_init -> make ~memory:Write opcode
  | Opcode.Ic_bt | Opcode.Ic_br_bt | Opcode.Ic_br_not_bt | Opcode.Ic_strlen ->
      make ~memory:Read opcode
  | Opcode.Ic_call_start | Opcode.Ic_push_regs ->
      make ~stack:Write ~machine:Read opcode
  | Opcode.Ic_call_end | Opcode.Ic_pop_regs ->
      make ~stack:Read ~machine:Write opcode
  | Opcode.Ic_add_rsp | Opcode.Ic_add_rsp1 ->
      make ~stack:Write ~machine:Write opcode
  | Opcode.Ic_enter -> make ~stack:Read_write ~machine:Read_write opcode
  | Opcode.Ic_leave | Opcode.Ic_ret ->
      make ~stack:Read ~machine:Read_write opcode
  | Opcode.Ic_push -> make ~memory:Read ~stack:Write opcode
  | Opcode.Ic_pop -> make ~stack:Read opcode
  | Opcode.Ic_clflush -> make ~memory:Read ~cache_tlb:Write opcode
  | Opcode.Ic_invlpg -> make ~cache_tlb:Write opcode
  | Opcode.Ic_in_u8 | Opcode.Ic_in_u16 | Opcode.Ic_in_u32 ->
      make ~port:Read opcode
  | Opcode.Ic_out_u8 | Opcode.Ic_out_u16 | Opcode.Ic_out_u32 ->
      make ~port:Write opcode
  | Opcode.Ic_fs | Opcode.Ic_gs | Opcode.Ic_mov_fs | Opcode.Ic_mov_gs ->
      make ~memory:Read ~machine:Read opcode
  | Opcode.Ic_rbp
  | Opcode.Ic_reg
  | Opcode.Ic_get_rflags
  | Opcode.Ic_carry
  | Opcode.Ic_get_rax
  | Opcode.Ic_get_rbp
  | Opcode.Ic_get_rsp
  | Opcode.Ic_rip -> make ~machine:Read opcode
  | Opcode.Ic_set_rflags
  | Opcode.Ic_set_rax
  | Opcode.Ic_set_rbp
  | Opcode.Ic_set_rsp -> make ~machine:Write opcode
  | Opcode.Ic_rdtsc -> make ~clock:Read opcode
  | Opcode.Ic_end
  | Opcode.Ic_nop1
  | Opcode.Ic_end_exp
  | Opcode.Ic_nop2
  | Opcode.Ic_label
  | Opcode.Ic_call_end2
  | Opcode.Ic_return_val
  | Opcode.Ic_return_val2
  | Opcode.Ic_imm_i64
  | Opcode.Ic_imm_f64
  | Opcode.Ic_str_const
  | Opcode.Ic_abs_addr
  | Opcode.Ic_addr_import
  | Opcode.Ic_heap_glbl
  | Opcode.Ic_sizeof
  | Opcode.Ic_type
  | Opcode.Ic_get_label
  | Opcode.Ic_lea
  | Opcode.Ic_to_i64
  | Opcode.Ic_to_f64
  | Opcode.Ic_to_bool
  | Opcode.Ic_toupper
  | Opcode.Ic_holyc_typecast
  | Opcode.Ic_addr
  | Opcode.Ic_com
  | Opcode.Ic_not
  | Opcode.Ic_unary_minus
  | Opcode.Ic_shl
  | Opcode.Ic_shr
  | Opcode.Ic_power
  | Opcode.Ic_mul
  | Opcode.Ic_div
  | Opcode.Ic_mod
  | Opcode.Ic_and
  | Opcode.Ic_or
  | Opcode.Ic_xor
  | Opcode.Ic_add
  | Opcode.Ic_sub
  | Opcode.Ic_equ_equ
  | Opcode.Ic_not_equ
  | Opcode.Ic_less
  | Opcode.Ic_greater_equ
  | Opcode.Ic_greater
  | Opcode.Ic_less_equ
  | Opcode.Ic_push_cmp
  | Opcode.Ic_and_and
  | Opcode.Ic_or_or
  | Opcode.Ic_xor_xor
  | Opcode.Ic_jmp
  | Opcode.Ic_switch
  | Opcode.Ic_nobound_switch
  | Opcode.Ic_br_zero
  | Opcode.Ic_br_not_zero
  | Opcode.Ic_br_carry
  | Opcode.Ic_br_not_carry
  | Opcode.Ic_br_equ_equ
  | Opcode.Ic_br_not_equ
  | Opcode.Ic_br_less
  | Opcode.Ic_br_greater_equ
  | Opcode.Ic_br_greater
  | Opcode.Ic_br_less_equ
  | Opcode.Ic_br_equ_equ2
  | Opcode.Ic_br_not_equ2
  | Opcode.Ic_br_less2
  | Opcode.Ic_br_greater_equ2
  | Opcode.Ic_br_greater2
  | Opcode.Ic_br_less_equ2
  | Opcode.Ic_br_and_zero
  | Opcode.Ic_br_and_not_zero
  | Opcode.Ic_br_and_and_zero
  | Opcode.Ic_br_and_and_not_zero
  | Opcode.Ic_br_or_or_zero
  | Opcode.Ic_br_or_or_not_zero
  | Opcode.Ic_bsf
  | Opcode.Ic_bsr
  | Opcode.Ic_abs_i64
  | Opcode.Ic_sign_i64
  | Opcode.Ic_min_i64
  | Opcode.Ic_min_u64
  | Opcode.Ic_max_i64
  | Opcode.Ic_max_u64
  | Opcode.Ic_mod_u64
  | Opcode.Ic_sqr_i64
  | Opcode.Ic_sqr_u64
  | Opcode.Ic_sqr
  | Opcode.Ic_abs
  | Opcode.Ic_sqrt
  | Opcode.Ic_sin
  | Opcode.Ic_cos
  | Opcode.Ic_tan
  | Opcode.Ic_atan -> make opcode

let may_read = function
  | Read | Read_write | Opaque -> true
  | None | Write -> false

let may_write = function
  | Write | Read_write | Opaque -> true
  | None | Read -> false

let accesses_anything classification =
  List.exists
    (fun access -> access <> None)
    [
      classification.memory;
      classification.stack;
      classification.machine;
      classification.port;
      classification.cache_tlb;
      classification.clock;
    ]

let has_observable_effect classification =
  classification.call <> No_call
  || classification.inline_assembly
  || may_write classification.memory
  || classification.stack <> None
  || classification.machine <> None
  || classification.port <> None
  || classification.cache_tlb <> None
  || classification.clock <> None

let is_reorder_barrier classification =
  classification.call <> No_call
  || classification.inline_assembly
  || accesses_anything classification

let access_name = function
  | None -> "none"
  | Read -> "read"
  | Write -> "write"
  | Read_write -> "read-write"
  | Opaque -> "opaque"

let call_name = function
  | No_call -> "none"
  | Local_call -> "local"
  | Direct_call -> "direct"
  | Indirect_call -> "indirect"
  | Import_call -> "import"
  | Extern_call -> "extern"

let human classification =
  Printf.sprintf
    "memory=%s stack=%s machine=%s port=%s cache-tlb=%s clock=%s call=%s \
     asm=%b fold-barrier=%b"
    (access_name classification.memory)
    (access_name classification.stack)
    (access_name classification.machine)
    (access_name classification.port)
    (access_name classification.cache_tlb)
    (access_name classification.clock)
    (call_name classification.call)
    classification.inline_assembly classification.prevents_constant_folding
