type location = private {
  span : Common.Span.t;
  source_segments : Common.Span.t list;
  generated_from : Common.Span.t option;
  defined_at : Common.Span.t option;
}

type declaration_modifier_kind =
  | Public
  | Static
  | Interrupt
  | Has_error_code
  | Argument_pop
  | No_argument_pop

type declaration_modifier = private {
  kind : declaration_modifier_kind;
  spelling : string;
  location : location;
}

type identifier = private { spelling : string; location : location }

type primitive_type = private {
  primitive : Sema.Primitive_type.t;
  spelling : string;
  location : location;
}

type pointer_layer = private {
  depth : int;
  spelling : string;
  location : location;
}

type declaration_delimiter_kind = Comma | Semicolon

type declaration_delimiter = private {
  kind : declaration_delimiter_kind;
  spelling : string;
  location : location;
}

type register_qualifier_kind = Reg | Noreg
type register_qualifier_position = Before_type | After_type

type register_qualifier = private {
  kind : register_qualifier_kind;
  position : register_qualifier_position;
  spelling : string;
  explicit_register : identifier option;
  location : location;
}

type variadic_marker = private {
  register_qualifiers : register_qualifier list;
  spelling : string;
  location : location;
}

type literal_value =
  | Integer_value of int64
  | Float_value of float
  | Bytes_value of string

type unary_operator_kind =
  | Unary_plus
  | Unary_minus
  | Logical_not
  | Bitwise_not
  | Dereference
  | Address_of
  | Pre_increment
  | Pre_decrement

type postfix_operator_kind = Post_increment | Post_decrement
type member_access_kind = Direct_member | Pointer_member
type defined_operand_kind = Defined_name | Defined_non_name

type expression =
  | Integer_literal of expression_literal
  | Float_literal of expression_literal
  | Character_literal of expression_literal
  | String_literal of expression_literal
  | Identifier_expression of identifier
  | Current_position_expression of expression_operator
  | Sizeof_expression of sizeof_expression
  | Offset_expression of offset_expression
  | Defined_expression of defined_expression
  | Parenthesized_expression of parenthesized_expression
  | Prefix_expression of prefix_expression
  | Postfix_expression of postfix_expression
  | Postfix_cast_expression of postfix_cast_expression
  | Binary_expression of binary_expression
  | Call_expression of call_expression
  | Index_expression of index_expression
  | Member_expression of member_expression

and expression_literal = private {
  literal_spelling : string;
  literal_value : literal_value;
  literal_location : location;
}

and expression_operator = private {
  operator_spelling : string;
  operator_location : location;
}

and sizeof_member = private {
  sizeof_member_dot : location;
  sizeof_member_name : identifier;
  sizeof_member_location : location;
}

and sizeof_expression = private {
  sizeof_keyword_spelling : string;
  sizeof_keyword_location : location;
  sizeof_opening_parentheses : location list;
  sizeof_target : identifier;
  sizeof_members : sizeof_member list;
  sizeof_pointer_layers : pointer_layer list;
  sizeof_closing_parentheses : location list;
  sizeof_location : location;
}

and offset_member = private {
  offset_member_dot : location;
  offset_member_name : identifier;
  offset_member_location : location;
}

and offset_expression = private {
  offset_keyword_spelling : string;
  offset_keyword_location : location;
  offset_opening_parentheses : location list;
  offset_target : identifier;
  offset_members : offset_member list;
  offset_closing_parentheses : location list;
  offset_location : location;
}

and defined_operand = private {
  defined_operand_kind : defined_operand_kind;
  defined_operand_spelling : string;
  defined_operand_location : location;
}

and defined_expression = private {
  defined_keyword_spelling : string;
  defined_keyword_location : location;
  defined_opening_parentheses : location list;
  defined_operand : defined_operand;
  defined_closing_parentheses : location list;
  defined_location : location;
}

and parenthesized_expression = private {
  opening_parenthesis : location;
  grouped_expression : expression;
  closing_parenthesis : location;
  parenthesized_location : location;
}

and prefix_expression = private {
  prefix_operator_kind : unary_operator_kind;
  prefix_operator : expression_operator;
  prefix_operand : expression;
  prefix_location : location;
}

and postfix_expression = private {
  postfix_operand : expression;
  postfix_operator_kind : postfix_operator_kind;
  postfix_operator : expression_operator;
  postfix_location : location;
}

and postfix_cast_expression = private {
  cast_operand : expression;
  cast_opening_parenthesis : location;
  cast_type : primitive_type;
  cast_pointer_layers : pointer_layer list;
  cast_closing_parenthesis : location;
  cast_location : location;
}

and binary_expression = private {
  binary_left : expression;
  binary_operator : expression_operator;
  binary_operator_spec : Operator.binary_operator;
  binary_right : expression;
  binary_location : location;
}

and call_argument_value =
  | Omitted_call_argument
  | Provided_call_argument of expression

and call_argument = private {
  call_argument_value : call_argument_value;
  following_comma : location option;
  call_argument_location : location;
}

and call_syntax =
  | Parenthesized_call of {
      opening_parenthesis : location;
      closing_parenthesis : location;
    }
  | Parenthesis_free_call

and call_expression = private {
  call_callee : expression;
  call_syntax : call_syntax;
  call_arguments : call_argument list;
  call_location : location;
}

and index_expression = private {
  index_base : expression;
  index_opening_bracket : location;
  index_value : expression;
  index_closing_bracket : location;
  index_location : location;
}

and member_expression = private {
  member_base : expression;
  member_access_kind : member_access_kind;
  member_operator : expression_operator;
  member_name : identifier;
  member_location : location;
}

type declaration_binding_kind = Extern | Import | Intern

type declaration_binding_target =
  | No_binding_target
  | Symbol_binding_target of identifier
  | Expression_binding_target of expression

type declaration_binding = private {
  kind : declaration_binding_kind;
  spelling : string;
  location : location;
  target : declaration_binding_target;
}

type aggregate_kind = Class_aggregate | Union_aggregate

type aggregate_forward_declaration = private {
  modifiers : declaration_modifier list;
  binding : declaration_binding;
  aggregate_kind : aggregate_kind;
  aggregate_keyword_spelling : string;
  aggregate_keyword_location : location;
  name : identifier;
  semicolon : location;
  location : location;
}

type array_dimension = private {
  opening_bracket : location;
  dimension_expression : expression option;
  closing_bracket : location;
  location : location;
}

type global_declarator = private {
  pointer_layers : pointer_layer list;
  name : identifier;
  array_dimensions : array_dimension list;
  delimiter : declaration_delimiter;
  location : location;
}

type global_variable = private {
  modifiers : declaration_modifier list;
  binding : declaration_binding option;
  type_specifier : primitive_type;
  pointer_layers : pointer_layer list;
  name : identifier;
  array_dimensions : array_dimension list;
  semicolon : Common.Span.t;
  location : location;
}

type global_declaration = private {
  modifiers : declaration_modifier list;
  binding : declaration_binding option;
  type_specifier : primitive_type;
  declarators : global_declarator list;
  location : location;
}

type lastclass_default = private {
  lastclass_spelling : string;
  lastclass_location : location;
}

type parameter_default_value =
  | Expression_default of expression
  | Lastclass_default of lastclass_default

type parameter_default = private {
  equals : location;
  value : parameter_default_value;
  location : location;
}

type function_parameter = private {
  register_qualifiers : register_qualifier list;
  type_specifier : primitive_type;
  pointer_layers : pointer_layer list;
  name : identifier option;
  function_pointer : function_pointer_declarator option;
  default : parameter_default option;
  delimiter : declaration_delimiter option;
  location : location;
}

and function_pointer_declarator = private {
  declarator_opening_parenthesis : location;
  indirection_layers : pointer_layer list;
  declarator_closing_parenthesis : location;
  signature_opening_parenthesis : location;
  signature_parameters : function_parameter list;
  signature_variadic : variadic_marker option;
  signature_closing_parenthesis : location;
  function_pointer_location : location;
}

type function_prototype = private {
  modifiers : declaration_modifier list;
  binding : declaration_binding;
  return_type : primitive_type;
  return_pointer_layers : pointer_layer list;
  name : identifier;
  opening_parenthesis : location;
  parameters : function_parameter list;
  variadic : variadic_marker option;
  closing_parenthesis : location;
  semicolon : location;
  location : location;
}

type implicit_output_target = Print_target | Put_chars_target

type implicit_output_fixed_argument =
  | Marker_fixed_argument of expression
  | Expression_fixed_argument of expression

type implicit_output_argument = private {
  leading_comma : location;
  value : expression;
  location : location;
}

type implicit_output_statement = private {
  target : implicit_output_target;
  marker : expression_literal;
  fixed_argument : implicit_output_fixed_argument;
  arguments : implicit_output_argument list;
  semicolon : location option;
  location : location;
}

type empty_statement = private {
  empty_statement_semicolon : location;
  empty_statement_location : location;
}

type expression_statement = private {
  expression_statement_expression : expression;
  expression_statement_semicolon : location option;
  expression_statement_location : location;
}

type local_storage = Automatic_local | Static_local

type local_initializer = private {
  local_initializer_equals : location;
  local_initializer_value : expression;
  local_initializer_location : location;
}

type local_declarator = private {
  local_register_qualifiers : register_qualifier list;
  local_pointer_layers : pointer_layer list;
  local_name : identifier;
  local_array_dimensions : array_dimension list;
  local_initializer : local_initializer option;
  local_delimiter : declaration_delimiter;
  local_declarator_location : location;
}

type local_declaration = private {
  local_storage : local_storage;
  local_modifiers : declaration_modifier list;
  local_type_specifier : primitive_type;
  local_declarators : local_declarator list;
  local_declaration_location : location;
}

type switch_mode = Bounded_switch | No_bound_switch

type switch_case_range = private {
  case_range_start : expression;
  case_range_ellipsis : location;
  case_range_end : expression;
  case_range_location : location;
}

type switch_case_pattern =
  | Implicit_case
  | Single_case of expression
  | Ranged_case of switch_case_range

type statement =
  | Block_statement of block_statement
  | Break_statement of break_statement
  | Do_while_statement of do_while_statement
  | Empty_statement of empty_statement
  | Expression_statement of expression_statement
  | For_statement of for_statement
  | Goto_statement of goto_statement
  | If_statement of if_statement
  | Implicit_output_statement of implicit_output_statement
  | Label_statement of label_statement
  | Local_declaration_statement of local_declaration
  | Lock_statement of lock_statement
  | Return_statement of return_statement
  | Sequence_statement of statement_sequence
  | Switch_statement of switch_statement
  | Try_catch_statement of try_catch_statement
  | While_statement of while_statement

and block_statement = private {
  block_opening_brace : location;
  block_statements : statement list;
  block_closing_brace : location;
  block_location : location;
}

and break_statement = private {
  break_keyword : location;
  break_semicolon : location option;
  break_location : location;
}

and do_while_statement = private {
  do_keyword : location;
  do_body : statement;
  do_while_keyword : location;
  do_while_opening_parenthesis : location;
  do_while_condition : expression;
  do_while_closing_parenthesis : location;
  do_while_semicolon : location;
  do_while_location : location;
}

and else_clause = private {
  else_keyword : location;
  else_branch : statement;
  else_location : location;
}

and for_statement = private {
  for_keyword : location;
  for_opening_parenthesis : location;
  for_initializer : statement;
  for_condition : expression;
  for_condition_semicolon : location;
  for_update : statement option;
  for_closing_parenthesis : location;
  for_body : statement;
  for_location : location;
}

and goto_statement = private {
  goto_keyword : location;
  goto_target : identifier;
  goto_semicolon : location option;
  goto_location : location;
}

and if_statement = private {
  if_keyword : location;
  if_opening_parenthesis : location;
  if_condition : expression;
  if_closing_parenthesis : location;
  if_then_branch : statement;
  if_else_clause : else_clause option;
  if_location : location;
}

and label_statement = private {
  label_name : identifier;
  label_colon : location;
  label_location : location;
}

and lock_statement = private {
  lock_keyword : location;
  lock_body : statement;
  lock_location : location;
}

and switch_statement = private {
  switch_keyword : location;
  switch_mode : switch_mode;
  switch_opening_delimiter : location;
  switch_expression : expression;
  switch_closing_delimiter : location;
  switch_opening_brace : location;
  switch_elements : switch_element list;
  switch_closing_brace : location;
  switch_location : location;
}

and switch_element =
  | Switch_case_element of switch_case_label
  | Switch_default_element of switch_default_label
  | Switch_subswitch_element of switch_subswitch
  | Switch_statement_element of statement

and switch_case_label = private {
  switch_case_keyword : location;
  switch_case_pattern : switch_case_pattern;
  switch_case_colon : location;
  switch_case_location : location;
}

and switch_default_label = private {
  switch_default_keyword : location;
  switch_default_colon : location;
  switch_default_location : location;
}

and switch_subswitch = private {
  subswitch_start_keyword : location;
  subswitch_start_colon : location;
  subswitch_elements : switch_element list;
  subswitch_end_keyword : location;
  subswitch_end_colon : location;
  subswitch_location : location;
}

and try_catch_statement = private {
  try_keyword : location;
  try_body : statement;
  catch_keyword : location;
  catch_body : statement;
  try_catch_location : location;
}

and return_statement = private {
  return_keyword : location;
  return_value : expression option;
  return_semicolon : location option;
  return_location : location;
}

and while_statement = private {
  while_keyword : location;
  while_opening_parenthesis : location;
  while_condition : expression;
  while_closing_parenthesis : location;
  while_body : statement;
  while_location : location;
}

and statement_sequence_element = private {
  sequence_statement : statement;
  sequence_following_commas : location list;
  sequence_element_location : location;
}

and statement_sequence = private {
  sequence_leading_commas : location list;
  sequence_elements : statement_sequence_element list;
  sequence_location : location;
}

type function_definition = private {
  modifiers : declaration_modifier list;
  return_type : primitive_type;
  return_pointer_layers : pointer_layer list;
  name : identifier;
  opening_parenthesis : location;
  parameters : function_parameter list;
  variadic : variadic_marker option;
  closing_parenthesis : location;
  body : statement option;
  location : location;
}

type item =
  | Aggregate_forward_declaration of aggregate_forward_declaration
  | Global_variable of global_variable
  | Global_declaration of global_declaration
  | Function_prototype of function_prototype
  | Function_definition of function_definition
  | Top_level_statement of statement

type module_ = private {
  source : Common.Source_id.t;
  span : Common.Span.t;
  items : item list;
}

val make_location :
  ?generated_from:Common.Span.t ->
  ?defined_at:Common.Span.t ->
  span:Common.Span.t ->
  source_segments:Common.Span.t list ->
  unit ->
  location

val make_declaration_modifier :
  kind:declaration_modifier_kind ->
  spelling:string ->
  location:location ->
  declaration_modifier

val make_declaration_binding :
  kind:declaration_binding_kind ->
  spelling:string ->
  location:location ->
  target:declaration_binding_target ->
  declaration_binding

val make_aggregate_forward_declaration :
  modifiers:declaration_modifier list ->
  binding:declaration_binding ->
  aggregate_kind:aggregate_kind ->
  aggregate_keyword_spelling:string ->
  aggregate_keyword_location:location ->
  name:identifier ->
  semicolon:location ->
  location:location ->
  aggregate_forward_declaration

val make_primitive_type :
  primitive:Sema.Primitive_type.t ->
  spelling:string ->
  location:location ->
  primitive_type

val make_identifier : spelling:string -> location:location -> identifier

val make_pointer_layer :
  depth:int -> spelling:string -> location:location -> pointer_layer

val make_declaration_delimiter :
  kind:declaration_delimiter_kind ->
  spelling:string ->
  location:location ->
  declaration_delimiter

val make_array_dimension :
  opening_bracket:location ->
  dimension_expression:expression option ->
  closing_bracket:location ->
  location:location ->
  array_dimension

val make_global_declarator :
  pointer_layers:pointer_layer list ->
  name:identifier ->
  array_dimensions:array_dimension list ->
  delimiter:declaration_delimiter ->
  location:location ->
  global_declarator

val make_global_variable :
  modifiers:declaration_modifier list ->
  binding:declaration_binding option ->
  type_specifier:primitive_type ->
  pointer_layers:pointer_layer list ->
  name:identifier ->
  array_dimensions:array_dimension list ->
  semicolon:Common.Span.t ->
  location:location ->
  global_variable

val make_global_declaration :
  modifiers:declaration_modifier list ->
  binding:declaration_binding option ->
  type_specifier:primitive_type ->
  declarators:global_declarator list ->
  location:location ->
  global_declaration

val make_register_qualifier :
  kind:register_qualifier_kind ->
  position:register_qualifier_position ->
  spelling:string ->
  explicit_register:identifier option ->
  location:location ->
  register_qualifier

val make_expression_literal :
  spelling:string ->
  value:literal_value ->
  location:location ->
  expression_literal

val make_expression_operator :
  spelling:string -> location:location -> expression_operator

val make_sizeof_member :
  dot:location -> name:identifier -> location:location -> sizeof_member

val make_sizeof_expression :
  keyword_spelling:string ->
  keyword_location:location ->
  opening_parentheses:location list ->
  target:identifier ->
  members:sizeof_member list ->
  pointer_layers:pointer_layer list ->
  closing_parentheses:location list ->
  location:location ->
  sizeof_expression

val make_offset_member :
  dot:location -> name:identifier -> location:location -> offset_member

val make_offset_expression :
  keyword_spelling:string ->
  keyword_location:location ->
  opening_parentheses:location list ->
  target:identifier ->
  members:offset_member list ->
  closing_parentheses:location list ->
  location:location ->
  offset_expression

val make_defined_operand :
  kind:defined_operand_kind ->
  spelling:string ->
  location:location ->
  defined_operand

val make_defined_expression :
  keyword_spelling:string ->
  keyword_location:location ->
  opening_parentheses:location list ->
  operand:defined_operand ->
  closing_parentheses:location list ->
  location:location ->
  defined_expression

val make_parenthesized_expression :
  opening_parenthesis:location ->
  expression:expression ->
  closing_parenthesis:location ->
  location:location ->
  parenthesized_expression

val make_prefix_expression :
  operator_kind:unary_operator_kind ->
  operator:expression_operator ->
  operand:expression ->
  location:location ->
  prefix_expression

val make_postfix_expression :
  operand:expression ->
  operator_kind:postfix_operator_kind ->
  operator:expression_operator ->
  location:location ->
  postfix_expression

val make_postfix_cast_expression :
  operand:expression ->
  opening_parenthesis:location ->
  type_specifier:primitive_type ->
  pointer_layers:pointer_layer list ->
  closing_parenthesis:location ->
  location:location ->
  postfix_cast_expression

val make_binary_expression :
  left:expression ->
  operator:expression_operator ->
  operator_spec:Operator.binary_operator ->
  right:expression ->
  location:location ->
  binary_expression

val make_call_argument :
  value:call_argument_value ->
  following_comma:location option ->
  location:location ->
  call_argument

val make_call_expression :
  callee:expression ->
  syntax:call_syntax ->
  arguments:call_argument list ->
  location:location ->
  call_expression

val make_index_expression :
  base:expression ->
  opening_bracket:location ->
  index:expression ->
  closing_bracket:location ->
  location:location ->
  index_expression

val make_member_expression :
  base:expression ->
  access_kind:member_access_kind ->
  operator:expression_operator ->
  member:identifier ->
  location:location ->
  member_expression

val expression_location : expression -> location

val make_parameter_default :
  equals:location ->
  value:parameter_default_value ->
  location:location ->
  parameter_default

val make_lastclass_default :
  spelling:string -> location:location -> lastclass_default

val make_function_parameter :
  register_qualifiers:register_qualifier list ->
  type_specifier:primitive_type ->
  pointer_layers:pointer_layer list ->
  name:identifier option ->
  function_pointer:function_pointer_declarator option ->
  default:parameter_default option ->
  delimiter:declaration_delimiter option ->
  location:location ->
  function_parameter

val make_function_pointer_declarator :
  declarator_opening_parenthesis:location ->
  indirection_layers:pointer_layer list ->
  declarator_closing_parenthesis:location ->
  signature_opening_parenthesis:location ->
  signature_parameters:function_parameter list ->
  signature_variadic:variadic_marker option ->
  signature_closing_parenthesis:location ->
  function_pointer_location:location ->
  function_pointer_declarator

val make_variadic_marker :
  register_qualifiers:register_qualifier list ->
  spelling:string ->
  location:location ->
  variadic_marker

val make_function_prototype :
  modifiers:declaration_modifier list ->
  binding:declaration_binding ->
  return_type:primitive_type ->
  return_pointer_layers:pointer_layer list ->
  name:identifier ->
  opening_parenthesis:location ->
  parameters:function_parameter list ->
  variadic:variadic_marker option ->
  closing_parenthesis:location ->
  semicolon:location ->
  location:location ->
  function_prototype

val make_implicit_output_argument :
  leading_comma:location ->
  value:expression ->
  location:location ->
  implicit_output_argument

val make_implicit_output_statement :
  target:implicit_output_target ->
  marker:expression_literal ->
  fixed_argument:implicit_output_fixed_argument ->
  arguments:implicit_output_argument list ->
  semicolon:location option ->
  location:location ->
  implicit_output_statement

val make_empty_statement :
  semicolon:location -> location:location -> empty_statement

val make_expression_statement :
  expression:expression ->
  semicolon:location option ->
  location:location ->
  expression_statement

val make_local_initializer :
  equals:location -> value:expression -> location:location -> local_initializer

val make_local_declarator :
  register_qualifiers:register_qualifier list ->
  pointer_layers:pointer_layer list ->
  name:identifier ->
  array_dimensions:array_dimension list ->
  initial_value:local_initializer option ->
  delimiter:declaration_delimiter ->
  location:location ->
  local_declarator

val make_local_declaration :
  storage:local_storage ->
  modifiers:declaration_modifier list ->
  type_specifier:primitive_type ->
  declarators:local_declarator list ->
  location:location ->
  local_declaration

val make_block_statement :
  opening_brace:location ->
  statements:statement list ->
  closing_brace:location ->
  location:location ->
  block_statement

val make_break_statement :
  keyword:location ->
  semicolon:location option ->
  location:location ->
  break_statement

val make_do_while_statement :
  do_keyword:location ->
  body:statement ->
  while_keyword:location ->
  opening_parenthesis:location ->
  condition:expression ->
  closing_parenthesis:location ->
  semicolon:location ->
  location:location ->
  do_while_statement

val make_else_clause :
  keyword:location -> branch:statement -> location:location -> else_clause

val make_for_statement :
  keyword:location ->
  opening_parenthesis:location ->
  initialization:statement ->
  condition:expression ->
  condition_semicolon:location ->
  update:statement option ->
  closing_parenthesis:location ->
  body:statement ->
  location:location ->
  for_statement

val make_goto_statement :
  keyword:location ->
  target:identifier ->
  semicolon:location option ->
  location:location ->
  goto_statement

val make_if_statement :
  keyword:location ->
  opening_parenthesis:location ->
  condition:expression ->
  closing_parenthesis:location ->
  then_branch:statement ->
  else_clause:else_clause option ->
  location:location ->
  if_statement

val make_label_statement :
  name:identifier -> colon:location -> location:location -> label_statement

val make_lock_statement :
  keyword:location -> body:statement -> location:location -> lock_statement

val make_switch_case_range :
  start:expression ->
  ellipsis:location ->
  end_:expression ->
  location:location ->
  switch_case_range

val make_switch_case_label :
  keyword:location ->
  pattern:switch_case_pattern ->
  colon:location ->
  location:location ->
  switch_case_label

val make_switch_default_label :
  keyword:location ->
  colon:location ->
  location:location ->
  switch_default_label

val make_switch_subswitch :
  start_keyword:location ->
  start_colon:location ->
  elements:switch_element list ->
  end_keyword:location ->
  end_colon:location ->
  location:location ->
  switch_subswitch

val make_switch_statement :
  keyword:location ->
  mode:switch_mode ->
  opening_delimiter:location ->
  expression:expression ->
  closing_delimiter:location ->
  opening_brace:location ->
  elements:switch_element list ->
  closing_brace:location ->
  location:location ->
  switch_statement

val make_try_catch_statement :
  try_keyword:location ->
  try_body:statement ->
  catch_keyword:location ->
  catch_body:statement ->
  location:location ->
  try_catch_statement

val make_return_statement :
  keyword:location ->
  value:expression option ->
  semicolon:location option ->
  location:location ->
  return_statement

val make_while_statement :
  keyword:location ->
  opening_parenthesis:location ->
  condition:expression ->
  closing_parenthesis:location ->
  body:statement ->
  location:location ->
  while_statement

val make_statement_sequence_element :
  statement:statement ->
  following_commas:location list ->
  location:location ->
  statement_sequence_element

val make_statement_sequence :
  leading_commas:location list ->
  elements:statement_sequence_element list ->
  location:location ->
  statement_sequence

val make_function_definition :
  modifiers:declaration_modifier list ->
  return_type:primitive_type ->
  return_pointer_layers:pointer_layer list ->
  name:identifier ->
  opening_parenthesis:location ->
  parameters:function_parameter list ->
  variadic:variadic_marker option ->
  closing_parenthesis:location ->
  body:statement option ->
  location:location ->
  function_definition

val statement_location : statement -> location

val make_module :
  source:Common.Source_id.t -> span:Common.Span.t -> items:item list -> module_
