type t

val create :
  allocation:Integer_static_allocation.t ->
  task_view:Integer_globals.task_view ->
  cursor:Integer_initializer_layout.stream ->
  Sema.Function_call_expression_result.top_level_t ->
  (t, string) result

val allocation : t -> Integer_static_allocation.t
val fragment : t -> Sema.Static_initializer_fragment.t
val globals : t -> Integer_globals.t
val typed : t -> Sema.Function_call_expression_result.top_level_t
val root : t -> Sema.Function_call_expression_result.top_level_root_result
val cell_offset : t -> int
val byte_offset : t -> int
val next : t -> Integer_initializer_layout.stream
val copy_byte_count : t -> int option

val operation : t -> Integer_initializer_layout.operation
(** The original checked stream operation. Copy payloads are detached from the
    retained source, include only the checked byte count, and may include its
    terminating zero. *)

val span : t -> Common.Span.t
val storage : t -> Integer_globals.storage_slot
