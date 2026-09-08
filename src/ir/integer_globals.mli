type t
type slot
type static_slot
type storage_slot

val with_statics :
  span:Common.Span.t ->
  frames:Sema.Function_frame_layout.t ->
  functions:Sema.Function_call_expression_result.t ->
  records:Sema.Function_record_classification.t ->
  t ->
  (t, Common.Diagnostic.t list) result
(** Internal source-driver join, absent from the public library signature. *)

val statics : t -> static_slot list
val static_frame : static_slot -> Sema.Function_frame_layout.function_layout
val static_location : static_slot -> Sema.Function_frame_layout.location

val static_initializer :
  static_slot -> Sema.Function_call_expression_result.initializer_result option

val static_compiler_options : static_slot -> int64
val static_storage : static_slot -> storage_slot
val global_storage : slot -> storage_slot
val storage_slots : t -> storage_slot list
val storage_index : storage_slot -> int
val storage_symbol : storage_slot -> Sema.Symbol.t
val storage_type : storage_slot -> Sema.Type.t
val storage_opcode : storage_slot -> Opcode.t
val storage_initial_bits : storage_slot -> int64 option
val storage_preparation_steps : storage_slot -> int

val storage_frame :
  storage_slot -> Sema.Function_frame_layout.function_layout option

val find_static : t -> Sema.Symbol.t -> static_slot option
val find_storage : t -> Sema.Symbol.t -> storage_slot option
val has_initializers : t -> bool
val has_unprepared_statics : t -> bool

val create :
  ?initializers:Sema.Function_call_expression_result.top_level_t ->
  span:Common.Span.t ->
  Sema.Global_record_classification.t ->
  (t, Common.Diagnostic.t list) result
(** Check every declaration for ordinary, non-aliased public I64/U64/U8
    code-heap storage. Initialized declarations require their exact checked
    scalar roots. Unsupported declarations fail even when unused. The context
    contains immutable metadata, not mutable execution storage. *)

val slots : t -> slot list
(** Ordinary global declarations only; [statics] retains separate owners. *)

val byte_size : t -> int
(** Combined global declared widths and eight-byte-padded static allocations,
    including unused declarations. Padding is not accessible object extent. This
    hosted quota excludes inter-object AOT alignment gaps and host overhead. *)

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
