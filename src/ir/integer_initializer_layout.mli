type operation = Scalar_store | Copy_bytes of string
type entry
type t
type live
type stream

val begin_stream : Integer_storage_shape.t -> stream

val prepare_stream :
  stream ->
  delimiters:Frontend.Parser.initializer_delimiter list ->
  value:Frontend.Ast.initial_value ->
  (stream * (int * int * operation), string) result
(** Advance the same fixed-array recursion over one scalar leaf and the original
    delimiters preceding it. The result describes its cell offset, byte offset
    and operation. It grants no source or runtime authority and cannot construct
    an [entry] or [t]; consumers must retain the original parser receipt and
    match the completed source-owned layout before publishing an image. *)

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
