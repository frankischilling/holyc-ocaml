type operation = Scalar_store | Copy_bytes of string
type entry
type t
type live
type stream
type stream_preparation
type static_snapshot

val begin_static_snapshot :
  table:Sema.Symbol_table.t ->
  parent:Sema.Symbol_table.scope ->
  allocation:Sema.Compiler_record.automatic_aggregate_allocation ->
  dimensions:int64 list ->
  (static_snapshot, string) result

val static_snapshot_receipt :
  static_snapshot -> Frontend.Parser.function_local_allocation

val observe_static_snapshot_delimiter :
  static_snapshot ->
  Frontend.Parser.completed_static_initializer_delimiter ->
  (unit, string) result

val prepare_static_snapshot_expression :
  static_snapshot ->
  Frontend.Parser.static_leaf_preparation ->
  (unit, string) result

val prepare_static_snapshot_leaf :
  static_snapshot ->
  Frontend.Parser.static_initializer_preparation ->
  (unit, string) result

val complete_static_snapshot :
  static_snapshot ->
  Frontend.Parser.completed_static_initializer ->
  (unit, string) result

val static_snapshot_leaf_destination :
  static_snapshot ->
  Frontend.Parser.static_initializer_preparation ->
  (int * int * operation * Sema.Initializer_type.scalar option, string) result
(** Read the exact reached destination during its original leaf callback. This
    metadata read grants no value, successful copy or execution authority. *)

val static_snapshot_matches :
  static_snapshot ->
  frame:Sema.Function_frame_layout.function_layout ->
  location:Sema.Function_frame_layout.location ->
  bool

val static_snapshot_layout :
  static_snapshot ->
  frame:Sema.Function_frame_layout.function_layout ->
  location:Sema.Function_frame_layout.location ->
  shape:Integer_storage_shape.t ->
  source:Sema.Initializer_source.t ->
  (t, string) result
(** Original static layout metadata only, without evaluated values or successful
    execution receipts. The completed consumer must retain its original frame,
    local, source leaves, shape and delimiter sequence. *)

val begin_stream : Integer_storage_shape.t -> stream

val observe_stream_delimiter :
  stream -> Frontend.Parser.initializer_delimiter -> (stream, string) result
(** Select reached grammar before advancing past the delimiter. Metadata only;
    original parser receipts and allocation ownership remain the consumer's
    responsibility. *)

val prepare_stream_expression :
  stream ->
  first:Frontend.Token.t ->
  delimiters:Frontend.Parser.initializer_delimiter list ->
  (stream_preparation, string) result

val prepare_stream_leaf :
  stream_preparation ->
  value:Frontend.Ast.initial_value ->
  ( stream * (int * int * operation * Sema.Initializer_type.scalar option),
    string )
  result

val stream_preparation_result :
  stream_preparation ->
  ( stream * (int * int * operation * Sema.Initializer_type.scalar option),
    string )
  result

val stream_preparation_requires_settlement : stream_preparation -> bool

val settle_stream_preparation : stream_preparation -> (unit, string) result
(** Retain the selected scalar branch before expression parsing and settle the
    reached nominal width once afterward. The original root bounds and fixed
    counts remain in force. These metadata reads grant no successful-copy or
    runtime execution authority. *)

val prepare_stream_typed :
  stream ->
  delimiters:Frontend.Parser.initializer_delimiter list ->
  value:Frontend.Ast.initial_value ->
  ( stream * (int * int * operation * Sema.Initializer_type.scalar option),
    string )
  result

(** Traverse the original fixed arrays and direct class members. Typed class
    leaves retain their own scalar destination; byte coordinates use the source
    initializer cursor rather than member-access offsets. *)

val prepare_stream :
  stream ->
  delimiters:Frontend.Parser.initializer_delimiter list ->
  value:Frontend.Ast.initial_value ->
  (stream * (int * int * operation), string) result
(** Advance the same fixed-array recursion over one scalar leaf and the original
    delimiters preceding it. The result describes its cell offset, byte offset
    and operation. Typed class leaves reject here; their consumers must use
    [prepare_stream_typed]. This grants no source or runtime authority and
    cannot construct an [entry] or [t]; consumers must retain the original
    parser receipt and match the completed source-owned layout before publishing
    an image. *)

val begin_live : Sema.Compiler_record.declared_global -> (live, string) result

val prepare_live_expression :
  live -> Frontend.Parser.initializer_leaf_preparation -> (live, string) result

val requires_settlement : entry -> bool

val settle_live_entry : live -> entry -> (live * entry, string) result
(** Freeze the original scalar branch before expression parsing, then settle its
    reached width after evaluation. Settlement preserves the original root
    extent and source leaf. These metadata transitions grant no execution or
    successful-store authority. *)

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

val create_static_retained :
  allocation:Sema.Compiler_record.static_allocation ->
  shape:Integer_storage_shape.t ->
  cursor:stream ->
  delimiters:Frontend.Parser.initializer_delimiter list ->
  Sema.Initializer_source.t ->
  (Frontend.Parser.static_initializer_preparation
  * int
  * int
  * operation
  * Sema.Initializer_type.scalar option)
  list ->
  (t, string) result

val entries : t -> entry list

val belongs_to_declared_shape :
  t -> Sema.Compiler_record.declared_global -> Integer_storage_shape.t -> bool

val leaf : entry -> Sema.Initializer_source.leaf
val cell_offset : entry -> int
val byte_offset : entry -> int
val operation : entry -> operation
val scalar : entry -> Sema.Initializer_type.scalar option
val declared_owner : entry -> Sema.Compiler_record.declared_global option
val find : t -> Sema.Initializer_source.leaf -> entry option

val normalized_word :
  Sema.Initializer_type.scalar -> int64 -> (int64, string) result

val word_bytes :
  Sema.Initializer_type.scalar -> int64 -> (string, string) result
