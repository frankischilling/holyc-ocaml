type t

val create :
  publication:Sema.Declaration_collection.publication ->
  header:Frontend.Parser.completed_function_header ->
  receipt:Frontend.Parser.completed_parameter_default ->
  bits:int64 ->
  (t, string) result

val bits : t -> int64

val type_ : t -> Sema.Type.t
(** The original parameter's storage class. For a one-star callback this is
    internal RT_PTR with pointer depth one, independently of its return class.
    The saved bits are a word; they do not confer executable authority. *)

val receipt : t -> Frontend.Parser.completed_parameter_default
val publication : t -> Sema.Declaration_collection.publication
val header : t -> Frontend.Parser.completed_function_header

val matches :
  t ->
  header:Sema.Function_type_resolution.resolved_function ->
  parameter:Sema.Function_type_resolution.parameter ->
  bool
