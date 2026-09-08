val layout :
  table:Sema.Symbol_table.t ->
  bindings:Sema.Global_dimension_binding.t ->
  Frontend.Ast.module_ ->
  (Sema.Global_array_layout.t, string) result
(** Join every real global declaration with its exact dimension binding batch
    and evaluate supported fixed extents before allocating persistent storage.
    Dimension names retain the original before-owner publication environment. *)
