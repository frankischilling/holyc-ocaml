type t

val create :
  table:Symbol_table.t ->
  namespace:Declaration_collection.namespace ->
  receipt:Frontend.Parser.internal_binding_preparation ->
  bits:int64 ->
  work:int ->
  (t, string) result
(** Source evidence only. Runtime admission separately requires the exact value
    registered by the owning VM after successful original expression execution.
*)

val receipt : t -> Frontend.Parser.internal_binding_preparation
val bits : t -> int64
val work : t -> int
val owns_table : t -> Symbol_table.t -> bool
val namespace : t -> Declaration_collection.namespace
val matches_header : t -> Frontend.Parser.declaration_header -> bool
