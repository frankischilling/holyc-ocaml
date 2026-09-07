type t
type slot

val create :
  span:Common.Span.t ->
  Sema.Global_record_classification.t ->
  (t, Common.Diagnostic.t list) result
(** Check every declaration for ordinary, non-aliased, uninitialized public
    I64/U64 code-heap storage. Unsupported declarations fail even when unused.
    The context contains immutable metadata, not mutable execution storage. *)

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
val human : t -> string
