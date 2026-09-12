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
