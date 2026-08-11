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

type expression =
  | Integer_literal of expression_literal
  | Float_literal of expression_literal
  | Character_literal of expression_literal
  | String_literal of expression_literal
  | Identifier_expression of identifier
  | Current_position_expression of expression_operator
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
  cast_type : primitive_type;
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

and call_expression = {
  call_callee : expression;
  call_opening_parenthesis : location;
  call_arguments : call_argument list;
  call_closing_parenthesis : location;
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

type array_dimension = {
  opening_bracket : location;
  dimension_expression : expression option;
  closing_bracket : location;
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
  type_specifier : primitive_type;
  pointer_layers : pointer_layer list;
  name : identifier;
  array_dimensions : array_dimension list;
  semicolon : Common.Span.t;
  location : location;
}

type global_declaration = {
  modifiers : declaration_modifier list;
  binding : declaration_binding option;
  type_specifier : primitive_type;
  declarators : global_declarator list;
  location : location;
}

type parameter_default = {
  equals : location;
  value : expression;
  location : location;
}

type function_parameter = {
  register_qualifiers : register_qualifier list;
  type_specifier : primitive_type;
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

type item =
  | Global_variable of global_variable
  | Global_declaration of global_declaration
  | Function_prototype of function_prototype

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

let make_primitive_type ~primitive ~spelling ~location : primitive_type =
  { primitive; spelling; location }

let make_identifier ~spelling ~location : identifier = { spelling; location }

let make_pointer_layer ~depth ~spelling ~location =
  { depth; spelling; location }

let make_declaration_delimiter ~kind ~spelling ~location =
  { kind; spelling; location }

let make_array_dimension ~opening_bracket ~dimension_expression ~closing_bracket
    ~location =
  { opening_bracket; dimension_expression; closing_bracket; location }

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

let make_call_expression ~callee ~opening_parenthesis ~arguments
    ~closing_parenthesis ~location =
  {
    call_callee = callee;
    call_opening_parenthesis = opening_parenthesis;
    call_arguments = arguments;
    call_closing_parenthesis = closing_parenthesis;
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

let make_module ~source ~span ~items = { source; span; items }
