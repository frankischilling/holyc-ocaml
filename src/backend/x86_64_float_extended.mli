val emit_modulo :
  emit:(X86_64_encoder.instruction -> unit) ->
  mark_loop:(unit -> unit) ->
  repeat_on_carry:(unit -> unit) ->
  scratch:X86_64_encoder.stack_slot ->
  left:X86_64_encoder.register ->
  right:X86_64_encoder.register ->
  result:X86_64_encoder.register ->
  truncate_result:bool ->
  unit
(** Emit the original floating remainder template for two materialized binary64
    words. The caller supplies its checked private scratch slot, a fresh loop
    label through [mark_loop], and a branch to that label on carry through
    [repeat_on_carry]. All general registers except [result] are preserved.

    The x87 stack must be empty on entry and is empty on return, with a peak of
    two values. The binary64 result is stored before an optional F64-to-I64
    truncation. Source admission, numeric ownership, instruction charging and
    conversion authority remain with the caller; this emitter grants none. *)
