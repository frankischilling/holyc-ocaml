type error = { code : string; message : string; span : Common.Span.t option }
type region
type t

val hard_max_literal_bytes : int
val validate_limit : max_literal_bytes:int -> (unit, error list) result

val create :
  max_literal_bytes:int ->
  max_arena_bytes:int ->
  arena_prefix_bytes:int ->
  runtime_calls:Ir.Runtime_call_context.t ->
  initialization:Ir.Global_initialization.t ->
  entry:Ir.X87_stack.t ->
  functions:Ir.Integer_interpreter.function_definition list ->
  (t, error list) result
(** Allocate a separate mutable byte region for each original IC_STR_CONST
    producer in the sealed entry and function graphs. Each region includes its
    trailing NUL and a bounded canonical reference table. Layout and all quotas
    are checked before the initial byte image is allocated. *)

val find :
  t ->
  owner:Ir.Runtime_call_context.owner ->
  graph:Ir.Block_graph.t ->
  Ir.Instruction_sequence.Instruction_id.t ->
  region option

val data_offset : region -> int
val table_offset : region -> int
val byte_count : region -> int
val is_empty : t -> bool
val literal_bytes : t -> int
val metadata_bytes : t -> int

val image : t -> string
(** A fresh copy containing only the literal suffix. All exposed offsets include
    the exact preceding global-storage image supplied to [create]. *)
