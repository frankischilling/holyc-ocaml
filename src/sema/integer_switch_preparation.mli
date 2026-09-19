type budget
type tracker
type t
type case

val hard_max_table_entries : int
val create_budget : max_work:int -> (budget, string) result
val budget_limit : budget -> int
val budget_work : budget -> int
val budget_table_entries : budget -> int
val create_tracker : budget:budget -> tracker

val observe :
  tracker ->
  Frontend.Parser.declaration_event ->
  (unit, Common.Diagnostic.t list) result
(** Evaluate only the exact current original endpoint callback. Case completion
    retains those successful results; switch completion validates its original
    ordered cases after brace lookahead. Failure keeps reached work and revokes
    that owner's preparation. Other declaration events are ignored. *)

val find : tracker -> Frontend.Ast.switch_statement -> t option

val preparations : tracker -> t list
(** Exact original AST lookup and completed preparations in completion order.
    These accessors never evaluate source. The driver separately binds its
    original command, semantic table and complete AST view. *)

val command : t -> Frontend.Parser.command_start
val receipt : t -> Frontend.Parser.completed_switch
val source : t -> Frontend.Ast.switch_statement
val lower_bound : t -> int64
val range : t -> int
val cases : t -> case list
val case_source : case -> Frontend.Ast.switch_case_label
val case_lower_bound : case -> int64
val case_upper_bound : case -> int64
val default : t -> Frontend.Ast.switch_default_label option

val work : t -> int
(** Numeric node visits for this switch's own endpoints. Nested switches retain
    separate descriptors; the shared budget counts all of them once. *)
