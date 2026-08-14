val create_environment :
  table:Sema.Symbol_table.t ->
  compilation_mode:Frontend.Preprocessor.compilation_mode ->
  Sema.Outer_environment.table list ->
  (Sema.Outer_environment.t, string) result
(** Validate a source-ordered JIT or AOT table chain against one semantic symbol
    table. *)

val resolve :
  table:Sema.Symbol_table.t ->
  environment:Sema.Outer_environment.t ->
  expressions:Sema.Module_expression_binding.t ->
  (Sema.Outer_expression_binding.t, string) result
(** Resolve candidates left by compilation-unit binding through the supplied
    outer table chain. *)
