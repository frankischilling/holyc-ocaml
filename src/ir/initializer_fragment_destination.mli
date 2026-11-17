type t

val create :
  task_view:Integer_globals.task_view ->
  reference:Retained_global.t ->
  slot:Integer_globals.declared_slot ->
  layout:Integer_initializer_layout.entry ->
  Sema.Function_call_expression_result.top_level_t ->
  (t, string) result
(** Pure checked destination. The original fragment, typed root, declared
    object, layout entry and retained snapshot must agree. No execution is
    authorized. *)

val globals : t -> Integer_globals.t
val fragment : t -> Sema.Initializer_fragment.t
val root : t -> Sema.Function_call_expression_result.top_level_root_result
val typed : t -> Sema.Function_call_expression_result.top_level_t
val layout : t -> Integer_initializer_layout.entry
val storage : t -> Integer_globals.storage_slot
val reference : t -> Retained_global.t
val span : t -> Common.Span.t
val discards_value : t -> bool
val requires_evaluation : t -> bool
val stores_pointer : t -> bool
val stores_same_f64 : t -> bool
val expression_is_f64 : t -> bool

val numeric_scalar : t -> Sema.Initializer_type.scalar option
(** Select the original numeric class, including primitive integer cell storage
    whose raw layout entry has no byte-leaf scalar. This leaves layout geometry
    and its receipt unchanged. *)

val stores_native_numeric : t -> bool

val convert_native_word : t -> int64 -> int64
(** Convert the completed expression word before copying its scalar bytes. The
    original native capture remains necessary for publication. *)
