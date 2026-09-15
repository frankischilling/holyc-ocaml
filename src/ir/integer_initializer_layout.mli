type operation = Scalar_store | Copy_bytes of string
type entry
type t
type live

val begin_live : Sema.Compiler_record.declared_global -> (live, string) result

val observe_live_delimiter :
  live ->
  Frontend.Parser.completed_initializer_delimiter ->
  (live, string) result

val prepare_live :
  live -> Sema.Initializer_source.leaf -> (live * entry, string) result

val complete_live : live -> Sema.Initializer_source.t -> (t, string) result
(** Pure incremental layout from original leaf/delimiter receipts and checked
    declared dimensions. Does not allocate, execute, or grant runtime authority.
    Completion must retain every exact leaf and destination. *)

val create :
  shape:Integer_storage_shape.t ->
  Sema.Initializer_source.t ->
  (t, string) result
(** Consume the retained initializer delimiters with the pinned fixed-array
    recursion. No missing-element fill or inferred dimensions are synthesized.
    Copied strings retain only source-owned bytes, including the terminator. *)

val source : t -> Sema.Initializer_source.t
val entries : t -> entry list
val leaf : entry -> Sema.Initializer_source.leaf
val cell_offset : entry -> int
val byte_offset : entry -> int
val operation : entry -> operation
val declared_owner : entry -> Sema.Compiler_record.declared_global option
val find : t -> Sema.Initializer_source.leaf -> entry option
