type selector
type t
type error_kind = Invalid_input of string
type error

val collect :
  table:Symbol_table.t ->
  Function_call_expression_result.top_level_t ->
  (t, error) result
(** Build a source-ordered view of executable top-level switch selectors from an
    already typed expression batch. The view retains bounded or no-bound mode
    without evaluating a selector, applying range arithmetic, or creating IR. *)

val owns_table : t -> Symbol_table.t -> bool
val source : t -> Function_call_expression_result.top_level_t
val selectors : t -> selector list

val selector_statement :
  selector -> Function_call_expression_result.top_level_statement_result

val selector_root :
  selector -> Function_call_expression_result.top_level_root_result

val selector_index : selector -> int
val selector_mode : selector -> Function_call_resolution.selector_mode
val selector_keyword_origin : selector -> Symbol.origin

val selector_value :
  selector -> Function_call_expression_result.expression_result

val selector_mode_name : Function_call_resolution.selector_mode -> string
val error_code : error -> string
val error_kind : error -> error_kind
val error_origin : error -> Symbol.origin option
val error_message : error -> string
val error_to_string : error -> string
