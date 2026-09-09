type t
type command
type source_command
type query

val create_source :
  ?max_dimension_work:int ->
  Session.t ->
  source:Common.Source_file.t ->
  (t, string) result
(** Create an ordinary source ledger with required query metadata. It grants no
    task-runtime authority and cannot import runtime admissions. The exact
    registered input owns the source namespace and its display-path name. *)

val seal_source :
  t -> Frontend.Ast.module_ -> (source_command, Common.Diagnostic.t list) result
(** Seal original source callbacks from a source-compilation ledger. Analysis
    and runtime ledgers cannot be converted into this authority. *)

val source_collection :
  table:Sema.Symbol_table.t ->
  ast:Frontend.Ast.module_ ->
  source_command ->
  (Sema.Declaration_collection.t, Common.Diagnostic.t list) result

val source_query_for :
  table:Sema.Symbol_table.t ->
  ast:Frontend.Ast.module_ ->
  source_command ->
  Frontend.Ast.expression ->
  (query, Common.Diagnostic.t list) result

val owns_runtime : Ir.Integer_interpreter.task_state -> command -> bool
(** A runtime-bound ledger's command retains that exact runtime owner. A
    semantic-only ledger grants no runtime compilation authority. *)

type reference_stage = private
  | Global_selection of
      Frontend.Parser.global_publication * Frontend.Ast.global_declarator option
  | Provisional_function_selection of Frontend.Parser.function_publication
  | Function_selection of
      Frontend.Parser.completed_function_header
      * Frontend.Ast.function_definition option

val dimension_work : t -> int
val command_dimension_work : command -> int
val source_dimension_work : source_command -> int

type reference_target = private
  | Selected_absent
  | Selected_unbound of Frontend.Symbol_visibility.entry
  | Selected_local
  | Selected_source of {
      publication : Sema.Declaration_collection.publication;
      stage : reference_stage;
      admitted : Ir.Integer_interpreter.admitted_publication option;
    }
  | Selected_runtime of Ir.Integer_interpreter.admitted_publication

val observe_query :
  t -> Frontend.Parser.query_event -> (unit, Common.Diagnostic.t list) result
(** Observe the original root, each ordered member and completed expression
    while its parser command is active. Reject foreign ownership, replay and
    missing phases. Freeze root presence and source stage independently of
    runtime admission; this receipt alone does not prepare size/member metadata.
*)

val query_for :
  table:Sema.Symbol_table.t ->
  ast:Frontend.Ast.module_ ->
  command ->
  Frontend.Ast.expression ->
  (query, Common.Diagnostic.t list) result
(** Read an exact completed query in its original sealed command view.
    Reconstructed ASTs or expressions and missing completions are rejected. *)

val query_receipt : query -> Frontend.Parser.completed_query
val query_selection : query -> Sema.Query_selection.t
val query_target : query -> reference_target
val query_presence : query -> bool

val dimension_for :
  table:Sema.Symbol_table.t ->
  ast:Frontend.Ast.module_ ->
  command ->
  Frontend.Ast.array_dimension ->
  (Frontend.Parser.completed_array_dimension, Common.Diagnostic.t list) result

val checked_dimension_for :
  table:Sema.Symbol_table.t ->
  ast:Frontend.Ast.module_ ->
  command ->
  Frontend.Ast.array_dimension ->
  (Sema.Compiler_record.declared_dimension, Common.Diagnostic.t list) result

val source_checked_dimension_for :
  table:Sema.Symbol_table.t ->
  ast:Frontend.Ast.module_ ->
  source_command ->
  Frontend.Ast.array_dimension ->
  (Sema.Compiler_record.declared_dimension, Common.Diagnostic.t list) result

val source_dimension_for :
  table:Sema.Symbol_table.t ->
  ast:Frontend.Ast.module_ ->
  source_command ->
  Frontend.Ast.array_dimension ->
  (Frontend.Parser.completed_array_dimension, Common.Diagnostic.t list) result
(** Read original ordered preparation and completion witnesses in the exact
    sealed command. These receipts establish source ownership and callback
    order, independently of evaluated extent values or runtime admission. *)

val observe_reference :
  t ->
  Frontend.Parser.reference_selection ->
  (unit, Common.Diagnostic.t list) result
(** Freeze the exact selected entry, source stage and already admitted runtime
    publication while its original parser command is active. Later completion or
    admission cannot upgrade the saved selection. Observation rejects replay. *)

val reference_for :
  table:Sema.Symbol_table.t ->
  ast:Frontend.Ast.module_ ->
  command ->
  Frontend.Ast.identifier ->
  (reference_target, Common.Diagnostic.t list) result
(** Read the selection for this exact AST occurrence in its sealed command view.
    Repeated reads preserve the same target; foreign owners and missing receipts
    are errors, including equal rebuilt identifiers. *)

val reference_resolver :
  table:Sema.Symbol_table.t ->
  ast:Frontend.Ast.module_ ->
  task_view:Ir.Integer_globals.task_view ->
  command ->
  ( Frontend.Ast.identifier -> (Sema.Reference_selection.t, string) result,
    Common.Diagnostic.t list )
  result
(** Normalize frozen source/retained selections against this exact compilation
    view of the owning runtime's catalog. Different snapshots of that catalog
    remain valid. Repeated identifier walks reuse the same outer binding. *)

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
