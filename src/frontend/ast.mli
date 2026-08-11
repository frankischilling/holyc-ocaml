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

type global_declarator = private {
  pointer_layers : pointer_layer list;
  name : identifier;
  delimiter : declaration_delimiter;
  location : location;
}

type global_variable = private {
  modifiers : declaration_modifier list;
  binding : declaration_binding option;
  type_specifier : primitive_type;
  pointer_layers : pointer_layer list;
  name : identifier;
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

type function_parameter = private {
  register_qualifiers : register_qualifier list;
  type_specifier : primitive_type;
  pointer_layers : pointer_layer list;
  name : identifier option;
  function_pointer : function_pointer_declarator option;
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

val make_global_declarator :
  pointer_layers:pointer_layer list ->
  name:identifier ->
  delimiter:declaration_delimiter ->
  location:location ->
  global_declarator

val make_global_variable :
  modifiers:declaration_modifier list ->
  binding:declaration_binding option ->
  type_specifier:primitive_type ->
  pointer_layers:pointer_layer list ->
  name:identifier ->
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

val make_function_parameter :
  register_qualifiers:register_qualifier list ->
  type_specifier:primitive_type ->
  pointer_layers:pointer_layer list ->
  name:identifier option ->
  function_pointer:function_pointer_declarator option ->
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
