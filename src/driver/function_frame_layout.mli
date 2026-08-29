val layout :
  table:Sema.Symbol_table.t ->
  declarations:Sema.Declaration_collection.t ->
  bindings:Sema.Function_binding_index.t ->
  function_types:Sema.Function_type_resolution.t ->
  local_types:Sema.Local_type_resolution.t ->
  aggregate_layouts:Sema.Aggregate_layout.t ->
  Frontend.Ast.module_ ->
  (Sema.Function_frame_layout.t, string) result
(** Reconcile the semantic passes with each function definition in the AST,
    evaluate its local extent expressions, and calculate the completed frame
    layouts. *)
