val collect :
  table:Sema.Symbol_table.t ->
  declarations:Sema.Declaration_collection.t ->
  Frontend.Ast.module_ ->
  (Sema.Function_collection.t, string) result
(** Collect parameters and function-wide locals from the same AST used to
    produce [declarations]. Types, storage, and duplicate checks are not
    applied. *)

val collect_completed_header :
  table:Sema.Symbol_table.t ->
  namespace:Sema.Declaration_collection.namespace ->
  Sema.Compiler_record.declared_function ->
  (Sema.Function_collection.collected_function, string) result
(** Collect the original completed header's named and variadic parameters in one
    new function scope owned by its exact declaration namespace. The local
    header view uses item index zero and contains no body or local variables. *)
