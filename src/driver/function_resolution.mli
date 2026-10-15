val resolve :
  ?namespace:Sema.Declaration_collection.namespace ->
  ?compiler_options:(Frontend.Ast.identifier -> (int64, string) result) ->
  ?previous:Sema.Function_resolution.resolved_declaration list ->
  ?record_heads:Sema.Function_resolution.resolved_declaration list ->
  ?compiler_option_mask:int64 ->
  table:Sema.Symbol_table.t ->
  declarations:Sema.Declaration_collection.t ->
  functions:Sema.Function_type_resolution.t ->
  compilation_mode:Frontend.Preprocessor.compilation_mode ->
  Frontend.Ast.module_ ->
  (Sema.Function_resolution.t, string) result
(** Classify source function declarations and reconcile their semantic
    identities without changing the symbol table. Original occurrence snapshots
    take precedence over the optional batch snapshot. *)
