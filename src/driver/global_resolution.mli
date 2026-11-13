val resolve :
  ?compiler_options:(Frontend.Ast.identifier -> (int64, string) result) ->
  ?compiler_option_mask:int64 ->
  table:Sema.Symbol_table.t ->
  declarations:Sema.Declaration_collection.t ->
  globals:Sema.Global_type_resolution.t ->
  compilation_mode:Frontend.Preprocessor.compilation_mode ->
  Frontend.Ast.module_ ->
  (Sema.Global_resolution.t, string) result
(** Classify source global bindings and reconcile their records without changing
    the symbol table. An original declaration resolver supplies each
    occurrence's option snapshot. Without one, the optional batch snapshot
    applies. *)
