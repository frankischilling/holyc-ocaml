let origin (location : Frontend.Ast.location) =
  Symbol.Source_location
    {
      span = location.span;
      source_segments = location.source_segments;
      generated_from = location.generated_from;
      defined_at = location.defined_at;
    }

let builtin type_specifier pointer_layers =
  let ( let* ) = Result.bind in
  let rec pointer_depth expected = function
    | [] -> Ok (expected - 1)
    | (layer : Frontend.Ast.pointer_layer) :: rest ->
        if layer.depth <> expected || layer.spelling <> "*" then
          Error "semantic source type has inconsistent pointer children"
        else pointer_depth (expected + 1) rest
  in
  let* pointer_depth = pointer_depth 1 pointer_layers in
  let* resolved_type =
    match type_specifier with
    | Frontend.Ast.Primitive_type_specifier primitive ->
        Type.make_primitive ~form:Type.Public_spelling
          ~primitive:primitive.primitive ~pointer_depth
    | Frontend.Ast.Internal_type_specifier internal ->
        Type.make_primitive ~form:Type.Internal_storage
          ~primitive:internal.primitive ~pointer_depth
    | Frontend.Ast.Named_type_specifier _ ->
        Error "source query type requires its selected aggregate metadata"
  in
  Type_reference.make
    ~spelling:(Frontend.Ast.type_specifier_spelling type_specifier)
    ~spelling_origin:
      (origin (Frontend.Ast.type_specifier_location type_specifier))
    ~pointer_origins:
      (List.map
         (fun (layer : Frontend.Ast.pointer_layer) -> origin layer.location)
         pointer_layers)
    ~resolved_type
