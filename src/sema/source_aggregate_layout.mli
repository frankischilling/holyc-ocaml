val place_member :
  origin:Symbol.origin ->
  kind:Frontend.Ast.aggregate_kind ->
  union_base:int64 ->
  current_size:int64 ->
  member_size:int64 ->
  (int64, string) result

val member_extent :
  origin:Symbol.origin ->
  element_size:int64 ->
  counts:int64 list ->
  (int64, string) result

val layout :
  dimensions:
    (Frontend.Ast.aggregate_member_declarator -> (int64 list, string) result) ->
  table:Symbol_table.t ->
  namespace:Declaration_collection.namespace ->
  symbol:Symbol.t ->
  Frontend.Ast.aggregate_definition ->
  (int64, string) result
(** Adapt original primitive member syntax to the shared aggregate layout
    checker. This computes metadata; it grants no source or runtime authority.
*)
