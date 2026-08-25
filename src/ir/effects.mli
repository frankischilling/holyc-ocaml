type access = None | Read | Write | Read_write | Opaque

type call =
  | No_call
  | Local_call
  | Direct_call
  | Indirect_call
  | Import_call
  | Extern_call

type t = private {
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

val classify : Opcode.t -> t
val may_read : access -> bool
val may_write : access -> bool
val has_observable_effect : t -> bool
val is_reorder_barrier : t -> bool
val access_name : access -> string
val call_name : call -> string
val human : t -> string
