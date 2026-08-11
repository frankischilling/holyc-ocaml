type location = {
  span : Common.Span.t;
  source_segments : Common.Span.t list;
  generated_from : Common.Span.t option;
  defined_at : Common.Span.t option;
}

type declaration_modifier_kind = Public | Static

type declaration_modifier = {
  kind : declaration_modifier_kind;
  spelling : string;
  location : location;
}

type identifier = { spelling : string; location : location }
type declaration_binding_kind = Extern | Import

type declaration_binding = {
  kind : declaration_binding_kind;
  spelling : string;
  location : location;
  target : identifier option;
}

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

type global_declarator = {
  pointer_layers : pointer_layer list;
  name : identifier;
  delimiter : declaration_delimiter;
  location : location;
}

type global_variable = {
  modifiers : declaration_modifier list;
  binding : declaration_binding option;
  type_specifier : primitive_type;
  pointer_layers : pointer_layer list;
  name : identifier;
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

type function_parameter = {
  type_specifier : primitive_type;
  pointer_layers : pointer_layer list;
  name : identifier option;
  delimiter : declaration_delimiter option;
  location : location;
}

type variadic_marker = { spelling : string; location : location }

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
    ~location ~target : declaration_binding =
  { kind; spelling; location; target }

let make_primitive_type ~primitive ~spelling ~location : primitive_type =
  { primitive; spelling; location }

let make_identifier ~spelling ~location : identifier = { spelling; location }

let make_pointer_layer ~depth ~spelling ~location =
  { depth; spelling; location }

let make_declaration_delimiter ~kind ~spelling ~location =
  { kind; spelling; location }

let make_global_declarator ~pointer_layers ~name ~delimiter ~location =
  { pointer_layers; name; delimiter; location }

let make_global_variable ~modifiers ~binding ~type_specifier ~pointer_layers
    ~name ~semicolon ~location =
  {
    modifiers;
    binding;
    type_specifier;
    pointer_layers;
    name;
    semicolon;
    location;
  }

let make_global_declaration ~modifiers ~binding ~type_specifier ~declarators
    ~location =
  { modifiers; binding; type_specifier; declarators; location }

let make_function_parameter ~type_specifier ~pointer_layers ~name ~delimiter
    ~location =
  { type_specifier; pointer_layers; name; delimiter; location }

let make_variadic_marker ~spelling ~location : variadic_marker =
  { spelling; location }

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
