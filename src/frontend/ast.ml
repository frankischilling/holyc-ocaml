type location = {
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

type declaration_modifier = {
  kind : declaration_modifier_kind;
  spelling : string;
  location : location;
}

type identifier = { spelling : string; location : location }

type primitive_type = {
  primitive : Sema.Primitive_type.t;
  spelling : string;
  location : location;
}

type type_specifier =
  | Primitive_type_specifier of primitive_type
  | Named_type_specifier of identifier

type pointer_layer = { depth : int; spelling : string; location : location }
type declaration_delimiter_kind = Comma | Semicolon

type declaration_delimiter = {
  kind : declaration_delimiter_kind;
  spelling : string;
  location : location;
}

type register_qualifier_kind = Reg | Noreg
type register_qualifier_position = Before_type | After_type

type register_qualifier = {
  kind : register_qualifier_kind;
  position : register_qualifier_position;
  spelling : string;
  explicit_register : identifier option;
  location : location;
}

type variadic_marker = {
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

and expression_literal = {
  literal_spelling : string;
  literal_value : literal_value;
  literal_location : location;
}

and expression_operator = {
  operator_spelling : string;
  operator_location : location;
}

and sizeof_member = {
  sizeof_member_dot : location;
  sizeof_member_name : identifier;
  sizeof_member_location : location;
}

and sizeof_expression = {
  sizeof_keyword_spelling : string;
  sizeof_keyword_location : location;
  sizeof_opening_parentheses : location list;
  sizeof_target : identifier;
  sizeof_members : sizeof_member list;
  sizeof_pointer_layers : pointer_layer list;
  sizeof_closing_parentheses : location list;
  sizeof_location : location;
}

and offset_member = {
  offset_member_dot : location;
  offset_member_name : identifier;
  offset_member_location : location;
}

and offset_expression = {
  offset_keyword_spelling : string;
  offset_keyword_location : location;
  offset_opening_parentheses : location list;
  offset_target : identifier;
  offset_members : offset_member list;
  offset_closing_parentheses : location list;
  offset_location : location;
}

and defined_operand = {
  defined_operand_kind : defined_operand_kind;
  defined_operand_spelling : string;
  defined_operand_location : location;
}

and defined_expression = {
  defined_keyword_spelling : string;
  defined_keyword_location : location;
  defined_opening_parentheses : location list;
  defined_operand : defined_operand;
  defined_closing_parentheses : location list;
  defined_location : location;
}

and parenthesized_expression = {
  opening_parenthesis : location;
  grouped_expression : expression;
  closing_parenthesis : location;
  parenthesized_location : location;
}

and prefix_expression = {
  prefix_operator_kind : unary_operator_kind;
  prefix_operator : expression_operator;
  prefix_operand : expression;
  prefix_location : location;
}

and postfix_expression = {
  postfix_operand : expression;
  postfix_operator_kind : postfix_operator_kind;
  postfix_operator : expression_operator;
  postfix_location : location;
}

and postfix_cast_expression = {
  cast_operand : expression;
  cast_opening_parenthesis : location;
  cast_type : type_specifier;
  cast_pointer_layers : pointer_layer list;
  cast_closing_parenthesis : location;
  cast_location : location;
}

and binary_expression = {
  binary_left : expression;
  binary_operator : expression_operator;
  binary_operator_spec : Operator.binary_operator;
  binary_right : expression;
  binary_location : location;
}

and call_argument_value =
  | Omitted_call_argument
  | Provided_call_argument of expression

and call_argument = {
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

and call_expression = {
  call_callee : expression;
  call_syntax : call_syntax;
  call_arguments : call_argument list;
  call_location : location;
}

and index_expression = {
  index_base : expression;
  index_opening_bracket : location;
  index_value : expression;
  index_closing_bracket : location;
  index_location : location;
}

and member_expression = {
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

type declaration_binding = {
  kind : declaration_binding_kind;
  spelling : string;
  location : location;
  target : declaration_binding_target;
}

type aggregate_kind = Class_aggregate | Union_aggregate

type aggregate_forward_declaration = {
  modifiers : declaration_modifier list;
  binding : declaration_binding;
  aggregate_kind : aggregate_kind;
  aggregate_keyword_spelling : string;
  aggregate_keyword_location : location;
  name : identifier;
  semicolon : location;
  location : location;
}

type array_dimension = {
  opening_bracket : location;
  dimension_expression : expression option;
  closing_bracket : location;
  location : location;
}

type aggregate_member =
  | Aggregate_member_declaration of aggregate_member_declaration
  | Anonymous_union_member of anonymous_union_member
  | Empty_aggregate_member of location

and aggregate_member_declarator = {
  member_pointer_layers : pointer_layer list;
  member_name : identifier;
  member_array_dimensions : array_dimension list;
  member_delimiter : declaration_delimiter;
  member_declarator_location : location;
}

and aggregate_member_declaration = {
  member_type_specifier : type_specifier;
  member_declarators : aggregate_member_declarator list;
  member_declaration_location : location;
}

and anonymous_union_member = {
  anonymous_union_keyword_spelling : string;
  anonymous_union_keyword_location : location;
  anonymous_union_opening_brace : location;
  anonymous_union_members : aggregate_member list;
  anonymous_union_closing_brace : location;
  anonymous_union_semicolon : location option;
  anonymous_union_location : location;
}

type aggregate_definition = {
  modifiers : declaration_modifier list;
  aggregate_kind : aggregate_kind;
  aggregate_keyword_spelling : string;
  aggregate_keyword_location : location;
  name : identifier;
  opening_brace : location;
  members : aggregate_member list;
  closing_brace : location;
  semicolon : location;
  location : location;
}

type global_declarator = {
  pointer_layers : pointer_layer list;
  name : identifier;
  array_dimensions : array_dimension list;
  delimiter : declaration_delimiter;
  location : location;
}

type global_variable = {
  modifiers : declaration_modifier list;
  binding : declaration_binding option;
  type_specifier : type_specifier;
  pointer_layers : pointer_layer list;
  name : identifier;
  array_dimensions : array_dimension list;
  semicolon : Common.Span.t;
  location : location;
}

type global_declaration = {
  modifiers : declaration_modifier list;
  binding : declaration_binding option;
  type_specifier : type_specifier;
  declarators : global_declarator list;
  location : location;
}

type lastclass_default = {
  lastclass_spelling : string;
  lastclass_location : location;
}

type parameter_default_value =
  | Expression_default of expression
  | Lastclass_default of lastclass_default

type parameter_default = {
  equals : location;
  value : parameter_default_value;
  location : location;
}

type function_parameter = {
  register_qualifiers : register_qualifier list;
  type_specifier : type_specifier;
  pointer_layers : pointer_layer list;
  name : identifier option;
  function_pointer : function_pointer_declarator option;
  default : parameter_default option;
  delimiter : declaration_delimiter option;
  location : location;
}

and function_pointer_declarator = {
  declarator_opening_parenthesis : location;
  indirection_layers : pointer_layer list;
  declarator_closing_parenthesis : location;
  signature_opening_parenthesis : location;
  signature_parameters : function_parameter list;
  signature_variadic : variadic_marker option;
  signature_closing_parenthesis : location;
  function_pointer_location : location;
}

type function_prototype = {
  modifiers : declaration_modifier list;
  binding : declaration_binding;
  return_type : type_specifier;
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

type implicit_output_argument = {
  leading_comma : location;
  value : expression;
  location : location;
}

type implicit_output_statement = {
  target : implicit_output_target;
  marker : expression_literal;
  fixed_argument : implicit_output_fixed_argument;
  arguments : implicit_output_argument list;
  semicolon : location option;
  location : location;
}

type empty_statement = {
  empty_statement_semicolon : location;
  empty_statement_location : location;
}

type expression_statement = {
  expression_statement_expression : expression;
  expression_statement_semicolon : location option;
  expression_statement_location : location;
}

type local_storage = Automatic_local | Static_local

type local_initializer = {
  local_initializer_equals : location;
  local_initializer_value : expression;
  local_initializer_location : location;
}

type local_declarator = {
  local_register_qualifiers : register_qualifier list;
  local_pointer_layers : pointer_layer list;
  local_name : identifier;
  local_array_dimensions : array_dimension list;
  local_initializer : local_initializer option;
  local_delimiter : declaration_delimiter;
  local_declarator_location : location;
}

type local_declaration = {
  local_storage : local_storage;
  local_modifiers : declaration_modifier list;
  local_type_specifier : type_specifier;
  local_declarators : local_declarator list;
  local_declaration_location : location;
}

type switch_mode = Bounded_switch | No_bound_switch

type switch_case_range = {
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

and block_statement = {
  block_opening_brace : location;
  block_statements : statement list;
  block_closing_brace : location;
  block_location : location;
}

and break_statement = {
  break_keyword : location;
  break_semicolon : location option;
  break_location : location;
}

and do_while_statement = {
  do_keyword : location;
  do_body : statement;
  do_while_keyword : location;
  do_while_opening_parenthesis : location;
  do_while_condition : expression;
  do_while_closing_parenthesis : location;
  do_while_semicolon : location;
  do_while_location : location;
}

and else_clause = {
  else_keyword : location;
  else_branch : statement;
  else_location : location;
}

and for_statement = {
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

and goto_statement = {
  goto_keyword : location;
  goto_target : identifier;
  goto_semicolon : location option;
  goto_location : location;
}

and if_statement = {
  if_keyword : location;
  if_opening_parenthesis : location;
  if_condition : expression;
  if_closing_parenthesis : location;
  if_then_branch : statement;
  if_else_clause : else_clause option;
  if_location : location;
}

and label_statement = {
  label_name : identifier;
  label_colon : location;
  label_location : location;
}

and lock_statement = {
  lock_keyword : location;
  lock_body : statement;
  lock_location : location;
}

and switch_statement = {
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

and switch_case_label = {
  switch_case_keyword : location;
  switch_case_pattern : switch_case_pattern;
  switch_case_colon : location;
  switch_case_location : location;
}

and switch_default_label = {
  switch_default_keyword : location;
  switch_default_colon : location;
  switch_default_location : location;
}

and switch_subswitch = {
  subswitch_start_keyword : location;
  subswitch_start_colon : location;
  subswitch_elements : switch_element list;
  subswitch_end_keyword : location;
  subswitch_end_colon : location;
  subswitch_location : location;
}

and try_catch_statement = {
  try_keyword : location;
  try_body : statement;
  catch_keyword : location;
  catch_body : statement;
  try_catch_location : location;
}

and return_statement = {
  return_keyword : location;
  return_value : expression option;
  return_semicolon : location option;
  return_location : location;
}

and while_statement = {
  while_keyword : location;
  while_opening_parenthesis : location;
  while_condition : expression;
  while_closing_parenthesis : location;
  while_body : statement;
  while_location : location;
}

and statement_sequence_element = {
  sequence_statement : statement;
  sequence_following_commas : location list;
  sequence_element_location : location;
}

and statement_sequence = {
  sequence_leading_commas : location list;
  sequence_elements : statement_sequence_element list;
  sequence_location : location;
}

type function_definition = {
  modifiers : declaration_modifier list;
  return_type : type_specifier;
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
  | Aggregate_definition of aggregate_definition
  | Global_variable of global_variable
  | Global_declaration of global_declaration
  | Function_prototype of function_prototype
  | Function_definition of function_definition
  | Top_level_statement of statement

type module_ = {
  source : Common.Source_id.t;
  span : Common.Span.t;
  items : item list;
}

let make_location ?generated_from ?defined_at ~span ~source_segments () =
  let source_segments =
    match source_segments with
    | [] -> [ span ]
    | segments -> segments
  in
  { span; source_segments; generated_from; defined_at }

let make_declaration_modifier ~(kind : declaration_modifier_kind) ~spelling
    ~location : declaration_modifier =
  { kind; spelling; location }

let make_declaration_binding ~(kind : declaration_binding_kind) ~spelling
    ~location ~(target : declaration_binding_target) : declaration_binding =
  { kind; spelling; location; target }

let make_aggregate_forward_declaration ~modifiers ~binding ~aggregate_kind
    ~aggregate_keyword_spelling ~aggregate_keyword_location ~name ~semicolon
    ~location =
  {
    modifiers;
    binding;
    aggregate_kind;
    aggregate_keyword_spelling;
    aggregate_keyword_location;
    name;
    semicolon;
    location;
  }

let make_primitive_type ~primitive ~spelling ~location : primitive_type =
  { primitive; spelling; location }

let type_specifier_spelling = function
  | Primitive_type_specifier primitive -> primitive.spelling
  | Named_type_specifier name -> name.spelling

let type_specifier_location = function
  | Primitive_type_specifier primitive -> primitive.location
  | Named_type_specifier name -> name.location

let make_identifier ~spelling ~location : identifier = { spelling; location }

let make_pointer_layer ~depth ~spelling ~location =
  { depth; spelling; location }

let make_declaration_delimiter ~kind ~spelling ~location =
  { kind; spelling; location }

let make_array_dimension ~opening_bracket ~dimension_expression ~closing_bracket
    ~location =
  { opening_bracket; dimension_expression; closing_bracket; location }

let make_aggregate_member_declarator ~pointer_layers ~name ~array_dimensions
    ~delimiter ~location =
  {
    member_pointer_layers = pointer_layers;
    member_name = name;
    member_array_dimensions = array_dimensions;
    member_delimiter = delimiter;
    member_declarator_location = location;
  }

let make_aggregate_member_declaration ~type_specifier ~declarators ~location =
  {
    member_type_specifier = type_specifier;
    member_declarators = declarators;
    member_declaration_location = location;
  }

let make_anonymous_union_member ~keyword_spelling ~keyword_location
    ~opening_brace ~members ~closing_brace ~semicolon ~location =
  {
    anonymous_union_keyword_spelling = keyword_spelling;
    anonymous_union_keyword_location = keyword_location;
    anonymous_union_opening_brace = opening_brace;
    anonymous_union_members = members;
    anonymous_union_closing_brace = closing_brace;
    anonymous_union_semicolon = semicolon;
    anonymous_union_location = location;
  }

let make_aggregate_definition ~modifiers ~aggregate_kind
    ~aggregate_keyword_spelling ~aggregate_keyword_location ~name ~opening_brace
    ~members ~closing_brace ~semicolon ~location =
  {
    modifiers;
    aggregate_kind;
    aggregate_keyword_spelling;
    aggregate_keyword_location;
    name;
    opening_brace;
    members;
    closing_brace;
    semicolon;
    location;
  }

let make_global_declarator ~pointer_layers ~name ~array_dimensions ~delimiter
    ~location =
  { pointer_layers; name; array_dimensions; delimiter; location }

let make_global_variable ~modifiers ~binding ~type_specifier ~pointer_layers
    ~name ~array_dimensions ~semicolon ~location =
  {
    modifiers;
    binding;
    type_specifier;
    pointer_layers;
    name;
    array_dimensions;
    semicolon;
    location;
  }

let make_global_declaration ~modifiers ~binding ~type_specifier ~declarators
    ~location =
  { modifiers; binding; type_specifier; declarators; location }

let make_register_qualifier ~kind ~position ~spelling ~explicit_register
    ~location =
  { kind; position; spelling; explicit_register; location }

let make_expression_literal ~spelling ~value ~location =
  {
    literal_spelling = spelling;
    literal_value = value;
    literal_location = location;
  }

let make_expression_operator ~spelling ~location =
  { operator_spelling = spelling; operator_location = location }

let make_sizeof_member ~dot ~name ~location =
  {
    sizeof_member_dot = dot;
    sizeof_member_name = name;
    sizeof_member_location = location;
  }

let make_sizeof_expression ~keyword_spelling ~keyword_location
    ~opening_parentheses ~target ~members ~pointer_layers ~closing_parentheses
    ~location =
  {
    sizeof_keyword_spelling = keyword_spelling;
    sizeof_keyword_location = keyword_location;
    sizeof_opening_parentheses = opening_parentheses;
    sizeof_target = target;
    sizeof_members = members;
    sizeof_pointer_layers = pointer_layers;
    sizeof_closing_parentheses = closing_parentheses;
    sizeof_location = location;
  }

let make_offset_member ~dot ~name ~location =
  {
    offset_member_dot = dot;
    offset_member_name = name;
    offset_member_location = location;
  }

let make_offset_expression ~keyword_spelling ~keyword_location
    ~opening_parentheses ~target ~members ~closing_parentheses ~location =
  {
    offset_keyword_spelling = keyword_spelling;
    offset_keyword_location = keyword_location;
    offset_opening_parentheses = opening_parentheses;
    offset_target = target;
    offset_members = members;
    offset_closing_parentheses = closing_parentheses;
    offset_location = location;
  }

let make_defined_operand ~kind ~spelling ~location =
  {
    defined_operand_kind = kind;
    defined_operand_spelling = spelling;
    defined_operand_location = location;
  }

let make_defined_expression ~keyword_spelling ~keyword_location
    ~opening_parentheses ~operand ~closing_parentheses ~location =
  {
    defined_keyword_spelling = keyword_spelling;
    defined_keyword_location = keyword_location;
    defined_opening_parentheses = opening_parentheses;
    defined_operand = operand;
    defined_closing_parentheses = closing_parentheses;
    defined_location = location;
  }

let make_parenthesized_expression ~opening_parenthesis ~expression
    ~closing_parenthesis ~location =
  {
    opening_parenthesis;
    grouped_expression = expression;
    closing_parenthesis;
    parenthesized_location = location;
  }

let make_prefix_expression ~operator_kind ~operator ~operand ~location =
  {
    prefix_operator_kind = operator_kind;
    prefix_operator = operator;
    prefix_operand = operand;
    prefix_location = location;
  }

let make_postfix_expression ~operand ~operator_kind ~operator ~location =
  {
    postfix_operand = operand;
    postfix_operator_kind = operator_kind;
    postfix_operator = operator;
    postfix_location = location;
  }

let make_postfix_cast_expression ~operand ~opening_parenthesis ~type_specifier
    ~pointer_layers ~closing_parenthesis ~location =
  {
    cast_operand = operand;
    cast_opening_parenthesis = opening_parenthesis;
    cast_type = type_specifier;
    cast_pointer_layers = pointer_layers;
    cast_closing_parenthesis = closing_parenthesis;
    cast_location = location;
  }

let make_binary_expression ~left ~operator ~operator_spec ~right ~location =
  {
    binary_left = left;
    binary_operator = operator;
    binary_operator_spec = operator_spec;
    binary_right = right;
    binary_location = location;
  }

let make_call_argument ~value ~following_comma ~location =
  {
    call_argument_value = value;
    following_comma;
    call_argument_location = location;
  }

let make_call_expression ~callee ~syntax ~arguments ~location =
  {
    call_callee = callee;
    call_syntax = syntax;
    call_arguments = arguments;
    call_location = location;
  }

let make_index_expression ~base ~opening_bracket ~index ~closing_bracket
    ~location =
  {
    index_base = base;
    index_opening_bracket = opening_bracket;
    index_value = index;
    index_closing_bracket = closing_bracket;
    index_location = location;
  }

let make_member_expression ~base ~access_kind ~operator ~member ~location =
  {
    member_base = base;
    member_access_kind = access_kind;
    member_operator = operator;
    member_name = member;
    member_location = location;
  }

let expression_location = function
  | Integer_literal literal
  | Float_literal literal
  | Character_literal literal
  | String_literal literal -> literal.literal_location
  | Identifier_expression identifier -> identifier.location
  | Current_position_expression operator -> operator.operator_location
  | Sizeof_expression expression -> expression.sizeof_location
  | Offset_expression expression -> expression.offset_location
  | Defined_expression expression -> expression.defined_location
  | Parenthesized_expression expression -> expression.parenthesized_location
  | Prefix_expression expression -> expression.prefix_location
  | Postfix_expression expression -> expression.postfix_location
  | Postfix_cast_expression expression -> expression.cast_location
  | Binary_expression expression -> expression.binary_location
  | Call_expression expression -> expression.call_location
  | Index_expression expression -> expression.index_location
  | Member_expression expression -> expression.member_location

let make_parameter_default ~equals ~value ~location =
  { equals; value; location }

let make_lastclass_default ~spelling ~location =
  { lastclass_spelling = spelling; lastclass_location = location }

let make_function_parameter ~register_qualifiers ~type_specifier ~pointer_layers
    ~name ~function_pointer ~default ~delimiter ~location =
  {
    register_qualifiers;
    type_specifier;
    pointer_layers;
    name;
    function_pointer;
    default;
    delimiter;
    location;
  }

let make_function_pointer_declarator ~declarator_opening_parenthesis
    ~indirection_layers ~declarator_closing_parenthesis
    ~signature_opening_parenthesis ~signature_parameters ~signature_variadic
    ~signature_closing_parenthesis ~function_pointer_location =
  {
    declarator_opening_parenthesis;
    indirection_layers;
    declarator_closing_parenthesis;
    signature_opening_parenthesis;
    signature_parameters;
    signature_variadic;
    signature_closing_parenthesis;
    function_pointer_location;
  }

let make_variadic_marker ~register_qualifiers ~spelling ~location :
    variadic_marker =
  { register_qualifiers; spelling; location }

let make_function_prototype ~modifiers ~binding ~return_type
    ~return_pointer_layers ~name ~opening_parenthesis ~parameters ~variadic
    ~closing_parenthesis ~semicolon ~location =
  {
    modifiers;
    binding;
    return_type;
    return_pointer_layers;
    name;
    opening_parenthesis;
    parameters;
    variadic;
    closing_parenthesis;
    semicolon;
    location;
  }

let make_implicit_output_argument ~leading_comma ~value ~location =
  { leading_comma; value; location }

let make_implicit_output_statement ~target ~marker ~fixed_argument ~arguments
    ~semicolon ~location =
  { target; marker; fixed_argument; arguments; semicolon; location }

let make_empty_statement ~semicolon ~location =
  { empty_statement_semicolon = semicolon; empty_statement_location = location }

let make_expression_statement ~expression ~semicolon ~location =
  {
    expression_statement_expression = expression;
    expression_statement_semicolon = semicolon;
    expression_statement_location = location;
  }

let make_local_initializer ~equals ~value ~location =
  {
    local_initializer_equals = equals;
    local_initializer_value = value;
    local_initializer_location = location;
  }

let make_local_declarator ~register_qualifiers ~pointer_layers ~name
    ~array_dimensions ~initial_value ~delimiter ~location =
  {
    local_register_qualifiers = register_qualifiers;
    local_pointer_layers = pointer_layers;
    local_name = name;
    local_array_dimensions = array_dimensions;
    local_initializer = initial_value;
    local_delimiter = delimiter;
    local_declarator_location = location;
  }

let make_local_declaration ~storage ~modifiers ~type_specifier ~declarators
    ~location =
  {
    local_storage = storage;
    local_modifiers = modifiers;
    local_type_specifier = type_specifier;
    local_declarators = declarators;
    local_declaration_location = location;
  }

let make_block_statement ~opening_brace ~statements ~closing_brace ~location =
  {
    block_opening_brace = opening_brace;
    block_statements = statements;
    block_closing_brace = closing_brace;
    block_location = location;
  }

let make_break_statement ~keyword ~semicolon ~location =
  {
    break_keyword = keyword;
    break_semicolon = semicolon;
    break_location = location;
  }

let make_do_while_statement ~do_keyword ~body ~while_keyword
    ~opening_parenthesis ~condition ~closing_parenthesis ~semicolon ~location =
  {
    do_keyword;
    do_body = body;
    do_while_keyword = while_keyword;
    do_while_opening_parenthesis = opening_parenthesis;
    do_while_condition = condition;
    do_while_closing_parenthesis = closing_parenthesis;
    do_while_semicolon = semicolon;
    do_while_location = location;
  }

let make_else_clause ~keyword ~branch ~location =
  { else_keyword = keyword; else_branch = branch; else_location = location }

let make_for_statement ~keyword ~opening_parenthesis ~initialization ~condition
    ~condition_semicolon ~update ~closing_parenthesis ~body ~location =
  {
    for_keyword = keyword;
    for_opening_parenthesis = opening_parenthesis;
    for_initializer = initialization;
    for_condition = condition;
    for_condition_semicolon = condition_semicolon;
    for_update = update;
    for_closing_parenthesis = closing_parenthesis;
    for_body = body;
    for_location = location;
  }

let make_goto_statement ~keyword ~target ~semicolon ~location =
  {
    goto_keyword = keyword;
    goto_target = target;
    goto_semicolon = semicolon;
    goto_location = location;
  }

let make_if_statement ~keyword ~opening_parenthesis ~condition
    ~closing_parenthesis ~then_branch ~else_clause ~location =
  {
    if_keyword = keyword;
    if_opening_parenthesis = opening_parenthesis;
    if_condition = condition;
    if_closing_parenthesis = closing_parenthesis;
    if_then_branch = then_branch;
    if_else_clause = else_clause;
    if_location = location;
  }

let make_label_statement ~name ~colon ~location =
  { label_name = name; label_colon = colon; label_location = location }

let make_lock_statement ~keyword ~body ~location =
  { lock_keyword = keyword; lock_body = body; lock_location = location }

let make_switch_case_range ~start ~ellipsis ~end_ ~location =
  {
    case_range_start = start;
    case_range_ellipsis = ellipsis;
    case_range_end = end_;
    case_range_location = location;
  }

let make_switch_case_label ~keyword ~pattern ~colon ~location =
  {
    switch_case_keyword = keyword;
    switch_case_pattern = pattern;
    switch_case_colon = colon;
    switch_case_location = location;
  }

let make_switch_default_label ~keyword ~colon ~location =
  {
    switch_default_keyword = keyword;
    switch_default_colon = colon;
    switch_default_location = location;
  }

let make_switch_subswitch ~start_keyword ~start_colon ~elements ~end_keyword
    ~end_colon ~location =
  {
    subswitch_start_keyword = start_keyword;
    subswitch_start_colon = start_colon;
    subswitch_elements = elements;
    subswitch_end_keyword = end_keyword;
    subswitch_end_colon = end_colon;
    subswitch_location = location;
  }

let make_switch_statement ~keyword ~mode ~opening_delimiter ~expression
    ~closing_delimiter ~opening_brace ~elements ~closing_brace ~location =
  {
    switch_keyword = keyword;
    switch_mode = mode;
    switch_opening_delimiter = opening_delimiter;
    switch_expression = expression;
    switch_closing_delimiter = closing_delimiter;
    switch_opening_brace = opening_brace;
    switch_elements = elements;
    switch_closing_brace = closing_brace;
    switch_location = location;
  }

let make_try_catch_statement ~try_keyword ~try_body ~catch_keyword ~catch_body
    ~location =
  {
    try_keyword;
    try_body;
    catch_keyword;
    catch_body;
    try_catch_location = location;
  }

let make_return_statement ~keyword ~value ~semicolon ~location =
  {
    return_keyword = keyword;
    return_value = value;
    return_semicolon = semicolon;
    return_location = location;
  }

let make_while_statement ~keyword ~opening_parenthesis ~condition
    ~closing_parenthesis ~body ~location =
  {
    while_keyword = keyword;
    while_opening_parenthesis = opening_parenthesis;
    while_condition = condition;
    while_closing_parenthesis = closing_parenthesis;
    while_body = body;
    while_location = location;
  }

let make_statement_sequence_element ~statement ~following_commas ~location =
  {
    sequence_statement = statement;
    sequence_following_commas = following_commas;
    sequence_element_location = location;
  }

let make_statement_sequence ~leading_commas ~elements ~location =
  {
    sequence_leading_commas = leading_commas;
    sequence_elements = elements;
    sequence_location = location;
  }

let make_function_definition ~modifiers ~return_type ~return_pointer_layers
    ~name ~opening_parenthesis ~parameters ~variadic ~closing_parenthesis ~body
    ~location =
  {
    modifiers;
    return_type;
    return_pointer_layers;
    name;
    opening_parenthesis;
    parameters;
    variadic;
    closing_parenthesis;
    body;
    location;
  }

let statement_location = function
  | Block_statement statement -> statement.block_location
  | Break_statement statement -> statement.break_location
  | Do_while_statement statement -> statement.do_while_location
  | Empty_statement statement -> statement.empty_statement_location
  | Expression_statement statement -> statement.expression_statement_location
  | For_statement statement -> statement.for_location
  | Goto_statement statement -> statement.goto_location
  | If_statement statement -> statement.if_location
  | Implicit_output_statement statement -> statement.location
  | Label_statement statement -> statement.label_location
  | Local_declaration_statement declaration ->
      declaration.local_declaration_location
  | Lock_statement statement -> statement.lock_location
  | Return_statement statement -> statement.return_location
  | Sequence_statement sequence -> sequence.sequence_location
  | Switch_statement statement -> statement.switch_location
  | Try_catch_statement statement -> statement.try_catch_location
  | While_statement statement -> statement.while_location

let make_module ~source ~span ~items = { source; span; items }
