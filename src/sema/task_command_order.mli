type t
type command

val check_function_publication :
  t ->
  admitted:command list ->
  Frontend.Parser.function_publication ->
  (unit, string) result

val check_function_header :
  t ->
  admitted:command list ->
  Frontend.Parser.completed_function_header ->
  (unit, string) result

val check_dimension :
  ?require_admitted:bool ->
  t ->
  admitted:command list ->
  Frontend.Parser.array_dimension_preparation ->
  (unit, string) result

val command_receipts : command -> Frontend.Parser.completed_command list

val check_completion :
  ?require_accepted:bool ->
  t ->
  admitted:command list ->
  Frontend.Parser.completed_sequence ->
  (unit, string) result

val check_declaration :
  t ->
  admitted:command list ->
  publication:Frontend.Parser.global_publication ->
  predecessor:Frontend.Parser.completed_command option ->
  (unit, string) result

val contains_global :
  command ->
  publication:Frontend.Parser.global_publication ->
  completed:Frontend.Ast.global_declarator ->
  item_index:int ->
  declarator_index:int option ->
  bool

val create : table:Symbol_table.t -> t

val observe : t -> Frontend.Parser.command_event -> (unit, string) result
(** Internal projection of successfully validated parser lifecycle events.
    Resume events order commands within their exact parser-root family. *)

val import_source_events :
  t -> Frontend.Parser.command_event list -> (unit, string) result
(** Atomically project a source ledger's previously validated lifecycle into an
    empty order. The original receipts retain their readiness and predecessors;
    this operation does not run parser callbacks or admit any command. *)

val seal_command :
  t -> Frontend.Parser.completed_command -> (command, string) result

val seal_sequence :
  t -> Frontend.Parser.completed_sequence -> (command, string) result

val owns : t -> ast:Frontend.Ast.module_ -> command -> bool

val has_source_syntax : t -> Frontend.Ast.module_ -> bool
(** Detect exact observed ASTs and borrowed original items. This is a denial
    check for unmarked legacy compilation, not a way to construct authority.
    Fresh callback-free syntax is independent even for the same input file. *)

val check : t -> admitted:command list -> command -> (unit, string) result
(** Readiness, original predecessor admission and replay checks are read-only.
    The owning VM commits a proof only after all preflight checks succeed. *)

val check_suspended_completion :
  t ->
  admitted:command list ->
  suspension:Frontend.Parser.suspension ->
  Frontend.Parser.completed_sequence ->
  (unit, string) result

val check_offset :
  ?require_admitted:bool ->
  t ->
  admitted:command list ->
  Frontend.Parser.aggregate_phase ->
  (unit, string) result
