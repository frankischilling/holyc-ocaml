type t
type command

val create :
  ?runtime:Ir.Integer_interpreter.task_state -> Session.t -> (t, string) result

val observe_admission :
  t -> Ir.Integer_interpreter.task_admission -> (unit, string) result
(** Publish new frontend entries linked to exact legacy runtime publications.
    Requires the current admission receipt from the runtime supplied at
    creation. Failed preflight, foreign tasks and replay cannot publish entries.
    Source origin is descriptive; association uses retained publication
    identity, never matching names or locations against discarded parser state.
*)

val retained_for :
  t ->
  Frontend.Symbol_visibility.entry ->
  Ir.Integer_interpreter.admitted_publication option

val observe_command :
  t -> Frontend.Parser.command_event -> (unit, Common.Diagnostic.t list) result
(** Consume the parser's context/command lifecycle before declaration events.
    Reject foreign ownership, suspended-parent mismatch, replay and phase
    errors. Completed commands retain whole source views even if later parsing
    aborts; they grant no runtime execution or predecessor admission. *)

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
(** Associate an exact parser-owned command or successful sequence AST with its
    original publications. Reconstructed modules and command subsets are
    rejected. A whole sequence can be sealed only after its completion callback
    succeeds. Reusing the exact module returns its existing seal. Overlapping
    commands (including statements), incomplete declarations and substituted
    source children are rejected. *)

val collection :
  table:Sema.Symbol_table.t ->
  ast:Frontend.Ast.module_ ->
  command ->
  (Sema.Declaration_collection.t, Common.Diagnostic.t list) result
(** The collection is available only for its owning table and exact AST. *)
