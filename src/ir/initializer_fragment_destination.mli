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
