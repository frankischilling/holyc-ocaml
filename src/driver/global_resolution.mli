val resolve :
  table:Sema.Symbol_table.t ->
  declarations:Sema.Declaration_collection.t ->
  globals:Sema.Global_type_resolution.t ->
  compilation_mode:Frontend.Preprocessor.compilation_mode ->
  Frontend.Ast.module_ ->
  (Sema.Global_resolution.t, string) result
(** Classify source global bindings and reconcile their records without changing
    the symbol table. The driver uses the current code-heap default;
    compiler-option execution will supply per-declaration storage later. *)
