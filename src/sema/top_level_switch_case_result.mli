type case_value

type case_pattern =
  | Implicit_case_result
  | Single_case_result of case_value
  | Ranged_case_result of {
      ellipsis_origin : Symbol.origin;
      start_value : case_value;
      end_value : case_value;
    }

type case_result
type t
type error_kind = Invalid_input of string
type error

val collect :
  table:Symbol_table.t ->
  Function_call_expression_result.top_level_t ->
  (t, error) result
(** Join source-ordered executable top-level switch case metadata to its already
    typed value roots. Implicit cases stay expression-free. Explicit [F64]
    values record integer-conversion intent without evaluation or IR. *)

val owns_table : t -> Symbol_table.t -> bool
val source : t -> Function_call_expression_result.top_level_t
val cases : t -> case_result list

val case_statement :
  case_result -> Function_call_expression_result.top_level_statement_result

val case_source : case_result -> Top_level_expression_tree.switch_case
val case_index : case_result -> int
val case_keyword_origin : case_result -> Symbol.origin
val case_origin : case_result -> Symbol.origin
val case_pattern : case_result -> case_pattern

val case_value_root :
  case_value -> Function_call_expression_result.top_level_root_result

val case_value_result :
  case_value -> Function_call_expression_result.expression_result

val case_value_conversion :
  case_value -> Function_call_expression_result.intrinsic_conversion

val case_pattern_name : case_pattern -> string
val error_code : error -> string
val error_kind : error -> error_kind
val error_origin : error -> Symbol.origin option
val error_message : error -> string
val error_to_string : error -> string
