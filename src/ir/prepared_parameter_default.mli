type t

val create :
  publication:Sema.Declaration_collection.publication ->
  header:Frontend.Parser.completed_function_header ->
  receipt:Frontend.Parser.completed_parameter_default ->
  bits:int64 ->
  (t, string) result

val bits : t -> int64
val type_ : t -> Sema.Type.t
val receipt : t -> Frontend.Parser.completed_parameter_default
val publication : t -> Sema.Declaration_collection.publication
val header : t -> Frontend.Parser.completed_function_header

val matches :
  t ->
  header:Sema.Function_type_resolution.resolved_function ->
  parameter:Sema.Function_type_resolution.parameter ->
  bool
