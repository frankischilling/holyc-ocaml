type argument_kind =
  | Word
  | Unsigned_byte_pointer
  | Signed_byte_pointer
  | Other_pointer

type t = {
  format_stage : int;
  arguments_stage : int;
  argument_kinds : argument_kind array;
  scratch_stage : int;
  activation_bytes : int;
}

type branch = Always | Equal | Not_equal | Below | Less | Overflow

type 'label emitter = {
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
    survives a fault. *)
