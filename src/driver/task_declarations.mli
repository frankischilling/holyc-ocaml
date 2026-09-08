type t
type command

val create : Session.t -> (t, string) result

val observe :
  t ->
  Frontend.Parser.declaration_event ->
  (unit, Common.Diagnostic.t list) result
(** Assign semantic identity at parser publication and retain exact completion
    witnesses. Reject foreign source/environment owners, replay and phase
    errors. This does not type an initializer or admit runtime
    storage/executables. *)

val symbol_for : t -> Frontend.Symbol_visibility.entry -> Sema.Symbol.t option
(** Read-only association for this exact parser entry snapshot. *)

val seal :
  t -> Frontend.Ast.module_ -> (command, Common.Diagnostic.t list) result
(** Associate a completed integer-task AST with its original publications.
    Reusing the exact module returns its existing seal. Overlapping
    publications, incomplete declarations and substituted source children are
    rejected. *)

val collection :
  table:Sema.Symbol_table.t ->
  ast:Frontend.Ast.module_ ->
  command ->
  (Sema.Declaration_collection.t, Common.Diagnostic.t list) result
(** The collection is available only for its owning table and exact AST. *)
