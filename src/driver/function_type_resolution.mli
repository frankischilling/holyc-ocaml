val resolve :
  ?retained_headers:Sema.Function_type_resolution.resolved_function list ->
  table:Sema.Symbol_table.t ->
  declarations:Sema.Declaration_collection.t ->
  aggregates:Sema.Aggregate_resolution.t ->
  functions:Sema.Function_collection.t ->
  Frontend.Ast.module_ ->
  (Sema.Function_type_resolution.t, string) result

val resolve_completed_header :
  table:Sema.Symbol_table.t ->
  namespace:Sema.Declaration_collection.namespace ->
  Sema.Compiler_record.declared_function ->
  (Sema.Function_type_resolution.resolved_function, string) result
(** Resolve the original completed header under its declaration's exact table
    and namespace ownership, retaining its parameter AST children. This creates
    a parameter-only function scope with item index zero, without constructing a
    module, body, command seal, executable frame or runtime admission. Named
    aggregate types reject until selected source-type evidence exists. *)

val resolve_completed_header_with_collection :
  table:Sema.Symbol_table.t ->
  namespace:Sema.Declaration_collection.namespace ->
  Sema.Compiler_record.declared_function ->
  ( Sema.Function_collection.collected_function
    * Sema.Function_type_resolution.resolved_function,
    string )
  result
(** Resolve one completed header while retaining the exact parameter collection
    needed by eventual body completion. *)
