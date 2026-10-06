type t

val create :
  publication:Sema.Declaration_collection.publication ->
  header:Frontend.Parser.completed_function_header ->
  receipt:Frontend.Parser.completed_parameter_default ->
  bits:int64 ->
  (t, string) result

val bits : t -> int64
(** Read a numeric saved word. Raises [Invalid_argument] for an owned callback;
    use [word_bits] when the original parameter can retain an owner. *)

val word_bits : t -> int64 option
val value : t -> Saved_parameter_value.t

val create_value :
  publication:Sema.Declaration_collection.publication ->
  header:Frontend.Parser.completed_function_header ->
  receipt:Frontend.Parser.completed_parameter_default ->
  value:Saved_parameter_value.t ->
  (t, string) result

val callback_source :
  t ->
  (Retained_function.t * Sema.Function_call_expression_result.expression_result)
  option

val undefined_callback_source :
  t -> Sema.Function_call_expression_result.expression_result option

val type_ : t -> Sema.Type.t
(** The original parameter's storage class. For a one-star callback this is
    internal RT_PTR with pointer depth one, independently of its return class.
    Numeric defaults preserve all sixty-four bits. An owned callback retains its
    checked original expression and function identity, without an executable
    address or native entry permission. *)

val receipt : t -> Frontend.Parser.completed_parameter_default
val publication : t -> Sema.Declaration_collection.publication
val header : t -> Frontend.Parser.completed_function_header

val matches :
  t ->
  header:Sema.Function_type_resolution.resolved_function ->
  parameter:Sema.Function_type_resolution.parameter ->
  bool
