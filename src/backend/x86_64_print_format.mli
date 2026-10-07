type argument_kind =
  | Word
  | Unsigned_byte_pointer
  | Signed_byte_pointer
  | Other_pointer

type target = Task_output | Generation | Formatted_source

type t = {
  target : target;
  format_stage : int;
  arguments_stage : int;
  argument_kinds : argument_kind array;
  scratch_stage : int;
  activation_bytes : int;
}

type branch = Always | Equal | Not_equal | Below | Less | Overflow

type provider_input = {
  target : target;
  format_stage : int;
  count_stage : int;
  arguments_stage : int;
  kinds_stage : int;
  scratch_stage : int;
}

val argument_kind_tag : argument_kind -> int64
val provider_scratch_slots : int

type 'label emitter = {
  status_abi : X86_64_encoder.status_abi;
  instruction : X86_64_encoder.instruction -> unit;
  fresh : unit -> 'label;
  mark : 'label -> unit;
  branch : branch -> 'label -> unit;
  fault : int -> 'label;
  slot : int -> X86_64_encoder.stack_slot;
}

val scratch_slots : int -> int
(** Fixed parser/layout counters, an 80-byte reverse numeric buffer and bounded
    quoted-field transducer state, auxiliary-format state and an original packed
    word for repeated [%c]/[%C], followed by one kind tag for each variadic
    argument. The caller bounds the complete frame before entry. *)

val emit : 'label emitter -> t -> unit
(** Emit the bounded dynamic Print formatter from authenticated, staged call
    values, including quoted [%Q]/[%q] strings, auxiliary [%h] parsing for the
    supported nonfloating directives, repeated packed [%c]/[%C] fields and
    lowercase list selection [%z]. Only RAX, RCX, RDX and R8 are clobbered.
    R9/R10/R11 retain their arena, instruction-budget and context roles. Draft
    bytes are committed only after the entire format succeeds; charged work
    survives a fault. Formatted source checks the active block after formatting
    and may invoke its original C callback. The callback preserves the private
    registers and returns the actual word into the staged source-call result. *)

val emit_provider : 'label emitter -> provider_input -> unit
(** Emit the same formatter from original caller-supplied count and
    argument/kind tables. The private entry has already reserved semantic
    depth/frame and physical stack; it owns the fixed bounded scratch frame.
    Argument indexing checks the count before reading either caller table. This
    emits no host formatter call and grants no call or executable authority. *)
