type t
type slot

val create :
  ?initializers:Sema.Function_call_expression_result.top_level_t ->
  span:Common.Span.t ->
  Sema.Global_record_classification.t ->
  (t, Common.Diagnostic.t list) result
(** Check every declaration for ordinary, non-aliased public I64/U64 code-heap
    storage. Initialized declarations require their exact checked scalar roots.
    Unsupported declarations fail even when unused. The context contains
    immutable metadata, not mutable execution storage. *)

val slots : t -> slot list
val byte_size : t -> int

val find : t -> Sema.Symbol.t -> slot option
(** Lookup requires the exact symbol object, not just its table-local ID. *)

val slot_index : slot -> int
val slot_symbol : slot -> Sema.Symbol.t
val slot_type : slot -> Sema.Type.t
val slot_record : slot -> Sema.Global_record_classification.classified_record
val slot_opcode : slot -> Opcode.t
val slot_initial_bits : slot -> int64 option

val slot_initializer :
  slot -> Sema.Function_call_expression_result.top_level_root_result option

val slot_initializer_materialized : slot -> bool
val slot_initializer_preparation_steps : slot -> int
val requires_initializer_execution : t -> bool

val with_initial_values :
  span:Common.Span.t ->
  t ->
  (Sema.Symbol.t * int64 * int) list ->
  (t, Common.Diagnostic.t list) result
(** Internal driver publication after checked constant preparation. This raw
    image updater is deliberately absent from the public library signature. *)

val human : t -> string
