val resolve :
  ?compiler_option_mask:int64 ->
  table:Sema.Symbol_table.t ->
  declarations:Sema.Declaration_collection.t ->
  globals:Sema.Global_type_resolution.t ->
  compilation_mode:Frontend.Preprocessor.compilation_mode ->
  Frontend.Ast.module_ ->
  (Sema.Global_resolution.t, string) result
(** Classify source global bindings and reconcile their records without changing
    the symbol table. The driver uses the current code-heap default and one
    optional compiler-option snapshot; source-positioned option execution will
    supply distinct declaration states later. *)
