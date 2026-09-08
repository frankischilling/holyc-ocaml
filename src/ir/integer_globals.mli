type t
type slot
type static_slot
type storage_slot
type task_catalog
type task_view

type task_publication = private
  | Global_publication of Retained_global.t * slot
  | Function_publication of Retained_function.t

val create_task_catalog : table:Sema.Symbol_table.t -> task_catalog
val task_catalog_owns_table : task_catalog -> Sema.Symbol_table.t -> bool
val snapshot_task : task_catalog -> (task_view, string) result
val task_environment : task_view -> Sema.Outer_environment.t
val task_catalog_owns_view : task_catalog -> task_view -> bool

val task_global_binding :
  task_view -> Retained_global.t -> Sema.Outer_environment.binding option

val task_function_binding :
  task_view -> Retained_function.t -> Sema.Outer_environment.binding option

val with_task_view : task_view -> t -> t

val with_function_publications :
  records:Sema.Function_record_classification.t -> t -> (t, string) result

val function_publications : t -> Retained_function.t list

val retained_function_binding :
  t -> Sema.Outer_environment.binding -> Retained_function.t option

val retained_function_symbol : t -> Sema.Symbol.t -> Retained_function.t option
(** Source inspection for initializer guards; runtime authority still requires
    the exact selected binding and sealed call site. *)

val retained_binding :
  t -> Sema.Outer_environment.binding -> (Retained_global.t * slot) option

val retained_slot : t -> Retained_global.t -> storage_slot option
val is_task_command : t -> bool
val check_task_command : task_catalog -> t -> (unit, string) result

val publish_task : task_catalog -> t -> task_publication list
(** Internal task admission API; absent from the public storage signature. *)

val same_storage : storage_slot -> storage_slot -> bool

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

val static_initializers :
  static_slot -> Sema.Function_call_expression_result.initializer_result list

val static_array_initializers :
  static_slot ->
  Sema.Function_call_expression_result.initializer_result
  Integer_array_initializers.t
  option

val static_storage : static_slot -> storage_slot
val global_storage : slot -> storage_slot
val storage_slots : t -> storage_slot list
val storage_index : storage_slot -> int
val storage_element_count : storage_slot -> int
val storage_dimensions : storage_slot -> int64 list
val storage_strides : storage_slot -> int64 list
val cell_count : t -> int
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
(** Check every declaration for ordinary, non-aliased public nonzero integer
    code-heap storage. Initialized declarations require their exact checked
    scalar roots. Unsupported declarations fail even when unused. The context
    contains immutable metadata, not mutable execution storage. *)

val create_with_layout :
  layout:Sema.Global_array_layout.t ->
  ?initializers:Sema.Function_call_expression_result.top_level_t ->
  span:Common.Span.t ->
  Sema.Global_record_classification.t ->
  (t, Common.Diagnostic.t list) result

val slot_shape : slot -> Integer_storage_shape.t

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

val slot_root_materialized :
  slot -> Sema.Function_call_expression_result.top_level_root_result -> bool

val static_root_materialized :
  static_slot -> Sema.Function_call_expression_result.initializer_result -> bool

val slot_initializers :
  slot -> Sema.Function_call_expression_result.top_level_root_result list

val slot_array_initializers :
  slot ->
  Sema.Function_call_expression_result.top_level_root_result
  Integer_array_initializers.t
  option

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

val with_array_initial_values :
  span:Common.Span.t ->
  t ->
  global_values:
    (Sema.Function_call_expression_result.top_level_root_result
    * Integer_array_initializers.payload
    * int)
    list ->
  static_values:
    (Sema.Function_call_expression_result.initializer_result
    * Integer_array_initializers.payload
    * int)
    list ->
  (t, Common.Diagnostic.t list) result

val storage_array_image :
  storage_slot -> (int * Integer_array_initializers.payload) list
