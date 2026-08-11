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
type declaration_binding_kind = Extern | Import

type declaration_binding = private {
  kind : declaration_binding_kind;
  spelling : string;
  location : location;
  target : identifier option;
}

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

type expression =
  | Integer_literal of expression_literal
  | Float_literal of expression_literal
  | Character_literal of expression_literal
  | String_literal of expression_literal
  | Identifier_expression of identifier
  | Current_position_expression of expression_operator
  | Parenthesized_expression of parenthesized_expression
  | Prefix_expression of prefix_expression
  | Binary_expression of binary_expression

and expression_literal = private {
  literal_spelling : string;
  literal_value : literal_value;
  literal_location : location;
}

and expression_operator = private {
  operator_spelling : string;
  operator_location : location;
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

and binary_expression = private {
  binary_left : expression;
  binary_operator : expression_operator;
  binary_operator_spec : Operator.binary_operator;
  binary_right : expression;
  binary_location : location;
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

type parameter_default = private {
  equals : location;
  value : expression;
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

type item =
  | Global_variable of global_variable
  | Global_declaration of global_declaration
  | Function_prototype of function_prototype

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
  target:identifier option ->
  declaration_binding

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

val make_binary_expression :
  left:expression ->
  operator:expression_operator ->
  operator_spec:Operator.binary_operator ->
  right:expression ->
  location:location ->
  binary_expression

val expression_location : expression -> location

val make_parameter_default :
  equals:location -> value:expression -> location:location -> parameter_default

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

val make_module :
  source:Common.Source_id.t -> span:Common.Span.t -> items:item list -> module_
