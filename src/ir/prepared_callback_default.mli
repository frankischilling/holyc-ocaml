type t

val create :
  namespace:Sema.Declaration_collection.namespace ->
  header:Frontend.Parser.completed_callback_signature ->
  receipt:Frontend.Parser.completed_callback_default ->
  bits:int64 ->
  (t, string) result

val bits : t -> int64
val type_ : t -> Sema.Type.t
val receipt : t -> Frontend.Parser.completed_callback_default
val namespace : t -> Sema.Declaration_collection.namespace
val header : t -> Frontend.Parser.completed_callback_signature

val matches :
  t ->
  pointer:Sema.Function_type_resolution.function_pointer ->
  parameter:Sema.Function_type_resolution.parameter ->
  bool
