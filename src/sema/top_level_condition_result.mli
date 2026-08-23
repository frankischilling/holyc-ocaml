type branch_test = Branch_on_zero | Branch_on_nonzero
type condition
type t
type error_kind = Invalid_input of string
type error

val collect :
  table:Symbol_table.t ->
  Function_call_expression_result.top_level_t ->
  (t, error) result
(** Build a source-ordered view of executable top-level conditions from an
    already typed expression batch. The view records the branch test selected by
    [PrsStmt.HC] without inserting a Boolean conversion or an IR edge. *)

val owns_table : t -> Symbol_table.t -> bool
val source : t -> Function_call_expression_result.top_level_t
val conditions : t -> condition list

val condition_statement :
  condition -> Function_call_expression_result.top_level_statement_result

val condition_root :
  condition -> Function_call_expression_result.top_level_root_result

val condition_index : condition -> int
val condition_role : condition -> Function_call_resolution.condition_role
val condition_keyword_origin : condition -> Symbol.origin
val condition_branch_test : condition -> branch_test

val condition_value :
  condition -> Function_call_expression_result.expression_result

val branch_test_name : branch_test -> string
val error_code : error -> string
val error_kind : error -> error_kind
val error_origin : error -> Symbol.origin option
val error_message : error -> string
val error_to_string : error -> string
