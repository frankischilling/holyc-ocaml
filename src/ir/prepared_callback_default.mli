type t

val create :
  namespace:Sema.Declaration_collection.namespace ->
  header:Frontend.Parser.completed_callback_signature ->
  receipt:Frontend.Parser.completed_callback_default ->
  bits:int64 ->
  (t, string) result

val bits : t -> int64
(** Read a numeric saved word. Raises [Invalid_argument] for an owned callback;
    use [word_bits] when the original parameter can retain an owner. *)

val value : t -> Saved_parameter_value.t
val word_bits : t -> int64 option

val callback_source :
  t ->
  (Retained_function.t * Sema.Function_call_expression_result.expression_result)
  option

val undefined_callback_source :
  t -> Sema.Function_call_expression_result.expression_result option

val create_value :
  namespace:Sema.Declaration_collection.namespace ->
  header:Frontend.Parser.completed_callback_signature ->
  receipt:Frontend.Parser.completed_callback_default ->
  value:Saved_parameter_value.t ->
  (t, string) result

val type_ : t -> Sema.Type.t
(** The original parameter's physical class. Callback parameters retain RT_PTR
    independently of their return metadata. Saved owners keep their original
    checked expression and function identity without a native address or entry
    permission. *)

val receipt : t -> Frontend.Parser.completed_callback_default
val namespace : t -> Sema.Declaration_collection.namespace
val header : t -> Frontend.Parser.completed_callback_signature

val matches :
  t ->
  pointer:Sema.Function_type_resolution.function_pointer ->
  parameter:Sema.Function_type_resolution.parameter ->
  bool
