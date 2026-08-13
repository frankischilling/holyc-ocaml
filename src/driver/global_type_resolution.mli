val resolve :
  table:Sema.Symbol_table.t ->
  declarations:Sema.Declaration_collection.t ->
  aggregates:Sema.Aggregate_resolution.t ->
  Frontend.Ast.module_ ->
  (Sema.Global_type_resolution.t, string) result
(** Resolve source-ordered global types without changing the symbol table. *)
