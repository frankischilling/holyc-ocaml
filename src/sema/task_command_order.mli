type t
type command

val create : table:Symbol_table.t -> t

val observe : t -> Frontend.Parser.command_event -> (unit, string) result
(** Internal projection of successfully validated parser lifecycle events.
    Resume events order commands within their exact parser-root family. *)

val seal_command :
  t -> Frontend.Parser.completed_command -> (command, string) result

val seal_sequence :
  t -> Frontend.Parser.completed_sequence -> (command, string) result

val owns : t -> ast:Frontend.Ast.module_ -> command -> bool

val check : t -> admitted:command list -> command -> (unit, string) result
(** Readiness, original predecessor admission and replay checks are read-only.
    The owning VM commits a proof only after all preflight checks succeed. *)
