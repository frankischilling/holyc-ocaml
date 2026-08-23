type switch_case_position = Single_case | Range_start | Range_end

type switch_case_pattern =
  | Implicit_case
  | Single_case_pattern
  | Ranged_case_pattern of { ellipsis_origin : Symbol.origin }

type root_role =
  | Expression_statement of { statement_index : int }
  | Implicit_output_fixed of {
      output_index : int;
      target : Function_call_resolution.implicit_output_target;
      source : Function_call_resolution.implicit_output_fixed_source;
      marker_origin : Symbol.origin;
    }
  | Implicit_output_argument of { output_index : int; argument_index : int }
  | Condition of {
      condition_index : int;
      role : Function_call_resolution.condition_role;
      keyword_origin : Symbol.origin;
    }
  | Switch_selector of {
      selector_index : int;
      mode : Function_call_resolution.selector_mode;
      keyword_origin : Symbol.origin;
    }
  | Switch_case_value of { case_index : int; position : switch_case_position }
  | Local_array_dimension of {
      declaration_index : int;
      declarator_index : int;
      dimension_index : int;
    }
  | Local_initializer of {
      declaration_index : int;
      declarator_index : int;
      element_path : int list;
    }
  | Return_value of { return_index : int }

type root
type switch_case
type call
type statement
type expression_node
type t
type error_kind = Invalid_input of string
type error

val make_root :
  index:int ->
  role:root_role ->
  expression:Function_call_resolution.argument_expression ->
  origin:Symbol.origin ->
  (root, error) result

val make_switch_case :
  index:int ->
  keyword_origin:Symbol.origin ->
  pattern:switch_case_pattern ->
  origin:Symbol.origin ->
  (switch_case, error) result

val make_call :
  source:Function_call_resolution.call ->
  callee:Top_level_outer_expression_binding.occurrence ->
  callee_expression:Function_call_resolution.argument_expression ->
  result_expression:Function_call_resolution.argument_expression ->
  (call, error) result

val make_statement :
  source:Top_level_outer_expression_binding.statement ->
  roots:root list ->
  calls:call list ->
  switch_cases:switch_case list ->
  (statement, error) result

val create :
  table:Symbol_table.t ->
  source:Top_level_outer_expression_binding.t ->
  statement list ->
  (t, error) result
(** Validate and freeze the semantic expression trees for executable top-level
    statements. Inputs stay grouped by their original statement while root,
    call, and expression-node identities remain deterministic across the whole
    module. *)

val owns_table : t -> Symbol_table.t -> bool
val source : t -> Top_level_outer_expression_binding.t
val statements : t -> statement list
val all_roots : t -> root list
val all_calls : t -> call list
val all_switch_cases : t -> switch_case list
val all_expression_nodes : t -> expression_node list
val statement_source : statement -> Top_level_outer_expression_binding.statement
val statement_roots : statement -> root list
val statement_calls : statement -> call list
val statement_switch_cases : statement -> switch_case list
val root_index : root -> int
val root_role : root -> root_role
val root_expression : root -> Function_call_resolution.argument_expression
val root_origin : root -> Symbol.origin
val switch_case_index : switch_case -> int
val switch_case_keyword_origin : switch_case -> Symbol.origin
val switch_case_pattern : switch_case -> switch_case_pattern
val switch_case_origin : switch_case -> Symbol.origin
val call_source : call -> Function_call_resolution.call
val call_callee : call -> Top_level_outer_expression_binding.occurrence

val call_callee_expression :
  call -> Function_call_resolution.argument_expression

val call_result_expression :
  call -> Function_call_resolution.argument_expression

val expression_node_index : expression_node -> int

val expression_node_source :
  expression_node -> Function_call_resolution.argument_expression

val switch_case_position_name : switch_case_position -> string
val switch_case_pattern_name : switch_case_pattern -> string
val root_role_name : root_role -> string
val error_code : error -> string
val error_kind : error -> error_kind
val error_origin : error -> Symbol.origin option
val error_message : error -> string
val error_to_string : error -> string
