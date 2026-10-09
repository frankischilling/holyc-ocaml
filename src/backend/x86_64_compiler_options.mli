type t = {
  index_stage : int;
  value_stage : int option;
  result_stage : int;
  scratch_stage : int;
}

val scratch_slots : int

val emit : 'label X86_64_print_format.emitter -> t -> unit
(** Invoke the original entered compiler operation bridge with staged I64/U8
    arguments. The caller reserves host argument space and bounds all four
    scratch slots. The bridge receives the actual machine instruction prefix;
    R9/R10/R11 and the previous-bit result survive collection and resumption. *)
