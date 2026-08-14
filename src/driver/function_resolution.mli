val resolve :
  ?compiler_option_mask:int64 ->
  table:Sema.Symbol_table.t ->
  declarations:Sema.Declaration_collection.t ->
  functions:Sema.Function_type_resolution.t ->
  compilation_mode:Frontend.Preprocessor.compilation_mode ->
  Frontend.Ast.module_ ->
  (Sema.Function_resolution.t, string) result
(** Classify source function declarations and reconcile their semantic
    identities without changing the symbol table. The optional batch snapshot
    supplies the current compiler-option state at each declaration. *)
