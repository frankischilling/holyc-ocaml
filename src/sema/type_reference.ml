type t = {
  spelling : string;
  spelling_origin : Symbol.origin;
  pointer_origins : Symbol.origin list;
  resolved_type : Type.t;
}

let spelling reference = reference.spelling
let spelling_origin reference = reference.spelling_origin
let pointer_origins reference = reference.pointer_origins
let resolved_type reference = reference.resolved_type

let expected_spelling resolved_type =
  match Type.base resolved_type with
  | Type.Primitive (Type.Public_spelling, primitive) ->
      Primitive_type.to_string primitive
  | Type.Primitive (Type.Internal_storage, primitive) ->
      (Primitive_type.info primitive).storage_spelling
  | Type.Aggregate symbol -> Symbol.name symbol

let make ~spelling ~spelling_origin ~pointer_origins ~resolved_type =
  if String.equal spelling "" then
    Error "semantic type-reference spelling cannot be empty"
  else if List.length pointer_origins <> Type.pointer_depth resolved_type then
    Error "semantic type-reference pointer provenance does not match its type"
  else
    let expected = expected_spelling resolved_type in
    if not (String.equal spelling expected) then
      Error
        (Printf.sprintf "semantic type-reference spelling %S does not match %S"
           spelling expected)
    else Ok { spelling; spelling_origin; pointer_origins; resolved_type }
