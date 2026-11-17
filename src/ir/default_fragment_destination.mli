type t

val create_source :
  Sema.Function_call_expression_result.top_level_t -> (t, string) result

val create_native_source :
  Sema.Function_call_expression_result.top_level_t -> (t, string) result
(** Preserve a closed original default's mode with isolated empty storage. *)

val create :
  task_view:Integer_globals.task_view ->
  Sema.Function_call_expression_result.top_level_t ->
  (t, string) result

val create_native :
  task_view:Integer_globals.task_view ->
  Sema.Function_call_expression_result.top_level_t ->
  (t, string) result
(** The original live native default also admits scalar F64 parameters and
    expression results. Isolated constant preparation retains its own domain. *)

val convert_native_word : t -> int64 -> int64
(** Apply PrsVar's post-Call numeric conversion to the materialized expression
    word. Integer-to-F64 uses the signed saved word; F64-to-integer truncates
    before any parameter storage narrowing. This function grants no execution
    authority: publication still requires the original native capture. *)

val fragment : t -> Sema.Default_fragment.t
val typed : t -> Sema.Function_call_expression_result.top_level_t
val root : t -> Sema.Function_call_expression_result.top_level_root_result
val globals : t -> Integer_globals.t
val type_ : t -> Sema.Type.t
val aggregate_value_type : t -> Sema.Type.t option

(** The qualified raw class of an original class parameter. Its saved default
    still uses a full word, independently of this class's scalar width. *)

val is_callback : t -> bool
val symbol : t -> Sema.Symbol.t
val span : t -> Common.Span.t
val symbol_opt : t -> Sema.Symbol.t option
