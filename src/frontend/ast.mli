type location = private {
  span : Common.Span.t;
  source_segments : Common.Span.t list;
  generated_from : Common.Span.t option;
  defined_at : Common.Span.t option;
}

type declaration_modifier_kind = Public | Static

type declaration_modifier = private {
  kind : declaration_modifier_kind;
  spelling : string;
  location : location;
}

type declaration_binding_kind = Extern | Import

type declaration_binding = private {
  kind : declaration_binding_kind;
  spelling : string;
  location : location;
}

type primitive_type = private {
  primitive : Sema.Primitive_type.t;
  spelling : string;
  location : location;
}

type identifier = private { spelling : string; location : location }

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

type item =
  | Global_variable of global_variable
  | Global_declaration of global_declaration

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

val make_module :
  source:Common.Source_id.t -> span:Common.Span.t -> items:item list -> module_
