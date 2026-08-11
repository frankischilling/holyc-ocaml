type location = private {
  span : Common.Span.t;
  source_segments : Common.Span.t list;
}

type primitive_type = private {
  primitive : Sema.Primitive_type.t;
  spelling : string;
  location : location;
}

type identifier = private { spelling : string; location : location }

type global_variable = private {
  type_specifier : primitive_type;
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
  span:Common.Span.t -> source_segments:Common.Span.t list -> location

val make_primitive_type :
  primitive:Sema.Primitive_type.t ->
  spelling:string ->
  location:location ->
  primitive_type

val make_identifier : spelling:string -> location:location -> identifier

val make_global_variable :
  type_specifier:primitive_type ->
  name:identifier ->
  semicolon:Common.Span.t ->
  location:location ->
  global_variable

val make_module :
  source:Common.Source_id.t -> span:Common.Span.t -> items:item list -> module_
