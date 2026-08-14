val resolve :
  table:Sema.Symbol_table.t ->
  declarations:Sema.Declaration_collection.t ->
  aggregates:Sema.Aggregate_resolution.t ->
  functions:Sema.Function_resolution.t ->
  globals:Sema.Global_resolution.t ->
  expressions:Sema.Function_expression_binding.t ->
  (Sema.Module_expression_binding.t, string) result
(** Reconcile checked declaration sites with their aggregate, function, and
    global records, then bind function expression candidates through the
    source-visible compilation-unit publication stream. *)
