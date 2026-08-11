type location = {
  span : Common.Span.t;
  source_segments : Common.Span.t list;
  generated_from : Common.Span.t option;
  defined_at : Common.Span.t option;
}

type primitive_type = {
  primitive : Sema.Primitive_type.t;
  spelling : string;
  location : location;
}

type identifier = { spelling : string; location : location }
type pointer_layer = { depth : int; spelling : string; location : location }

type global_variable = {
  type_specifier : primitive_type;
  pointer_layers : pointer_layer list;
  name : identifier;
  semicolon : Common.Span.t;
  location : location;
}

type item = Global_variable of global_variable

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

let make_primitive_type ~primitive ~spelling ~location =
  { primitive; spelling; location }

let make_identifier ~spelling ~location = { spelling; location }

let make_pointer_layer ~depth ~spelling ~location =
  { depth; spelling; location }

let make_global_variable ~type_specifier ~pointer_layers ~name ~semicolon
    ~location =
  { type_specifier; pointer_layers; name; semicolon; location }

let make_module ~source ~span ~items = { source; span; items }
