type location = private {
  span : Common.Span.t;
  source_segments : Common.Span.t list;
  generated_from : Common.Span.t option;
  defined_at : Common.Span.t option;
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

type global_variable = private {
  type_specifier : primitive_type;
  pointer_layers : pointer_layer list;
  name : identifier;
  semicolon : Common.Span.t;
  location : location;
}

type item = Global_variable of global_variable

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

val make_primitive_type :
  primitive:Sema.Primitive_type.t ->
  spelling:string ->
  location:location ->
  primitive_type

val make_identifier : spelling:string -> location:location -> identifier

val make_pointer_layer :
  depth:int -> spelling:string -> location:location -> pointer_layer

val make_global_variable :
  type_specifier:primitive_type ->
  pointer_layers:pointer_layer list ->
  name:identifier ->
  semicolon:Common.Span.t ->
  location:location ->
  global_variable

val make_module :
  source:Common.Source_id.t -> span:Common.Span.t -> items:item list -> module_
