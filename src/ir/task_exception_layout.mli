val code_offset : int
val caught_offset : int
val byte_count : int
val handler_byte_count : int

val pointer_type : Sema.Type.t -> bool
(** The checked IC_FS result shape. Its original internal declaration and
    instruction ownership are validated by the runtime call context. *)
