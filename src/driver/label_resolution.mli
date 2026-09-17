val resolve :
  table:Sema.Symbol_table.t ->
  functions:Sema.Function_collection.t ->
  Frontend.Ast.module_ ->
  (Sema.Label_resolution.t, string) result
(** Resolve language [goto] occurrences and language or assembly-block label
    definitions against the function scopes created from the same AST. *)

type indexed
type error

val resolve_indexed :
  table:Sema.Symbol_table.t ->
  functions:Sema.Function_collection.t ->
  Frontend.Ast.module_ ->
  (indexed, error) result
(** Resolve the same semantic labels while retaining an exact mapping from each
    original language goto/label statement object to its checked occurrence. The
    mapping is function-owner scoped and is intended for source execution
    composition whose runtime traversal order can differ from source order. *)

val error_message : error -> string
val error_span : error -> Common.Span.t option
val semantic : indexed -> Sema.Label_resolution.t

val function_for_symbol :
  indexed -> Sema.Symbol.t -> Sema.Label_resolution.resolved_function option
(** Find only a function carried by the exact source resolution owner. *)

val occurrence_for_statement :
  indexed ->
  function_symbol:Sema.Symbol.t ->
  Frontend.Ast.statement ->
  (Sema.Label_resolution.resolved_occurrence, string) result
(** Recover the occurrence bound to this exact source statement object and exact
    function owner. Reconstructed or foreign statements are rejected. *)
