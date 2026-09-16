let origin (location : Frontend.Ast.location) =
  Symbol.Source_location
    {
      span = location.span;
      source_segments = location.source_segments;
      generated_from = location.generated_from;
      defined_at = location.defined_at;
    }

type selected_aggregate = {
  selected_type_specifier : Frontend.Ast.type_specifier;
  selected_identifier : Frontend.Ast.identifier;
  selected_pointer_layers : Frontend.Ast.pointer_layer list;
  selected_symbol : Symbol.t;
}

type selected_source =
  | Function_return of Frontend.Parser.function_publication
  | Function_parameter of Frontend.Parser.function_parameter_publication

let same_physical_list left right =
  List.length left = List.length right && List.for_all2 ( == ) left right

let pointer_depth pointer_layers =
  let rec collect expected = function
    | [] when expected - 1 <= Type.max_pointer_depth -> Ok (expected - 1)
    | [] -> Error "semantic source type exceeds the native pointer depth"
    | (layer : Frontend.Ast.pointer_layer) :: rest ->
        if layer.depth <> expected || layer.spelling <> "*" then
          Error "semantic source type has inconsistent pointer children"
        else collect (expected + 1) rest
  in
  collect 1 pointer_layers

let builtin type_specifier pointer_layers =
  let ( let* ) = Result.bind in
  let* pointer_depth = pointer_depth pointer_layers in
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

let select_aggregate ~table ~namespace ~source publication =
  let module Visibility = Frontend.Symbol_visibility in
  let module Collection = Declaration_collection in
  let ( let* ) = Result.bind in
  let* selection, owner_type_specifier, pointer_layers, environment =
    match source with
    | Function_return function_ -> (
        if not (Frontend.Parser.function_publication_is_current function_) then
          Error
            "selected aggregate return type is outside its original callback"
        else
          match function_.function_return_selection with
          | Some selection ->
              Ok
                ( selection,
                  function_.function_header.type_specifier,
                  function_.function_pointer_layers,
                  function_.function_environment )
          | None -> Error "named function return lacks its parser selection")
    | Function_parameter parameter -> (
        if not (Frontend.Parser.function_parameter_is_current parameter) then
          Error
            "selected aggregate parameter type is outside its original callback"
        else
          match parameter.parameter_type_selection with
          | Some selection ->
              Ok
                ( selection,
                  parameter.parameter_type_specifier,
                  parameter.parameter_pointer_layers,
                  parameter.parameter_function.function_environment )
          | None -> Error "named function parameter lacks its parser selection")
  in
  let type_specifier = selection.type_specifier in
  let identifier = selection.identifier in
  if
    (not (Collection.namespace_owns_table namespace table))
    || not (Collection.namespace_owns_publication namespace publication)
  then Error "selected aggregate type belongs to another namespace or table"
  else if Visibility.kind selection.entry <> Visibility.Class then
    Error "selected aggregate type did not select a class entry"
  else if selection.environment != environment then
    Error "selected aggregate type substituted its owning parser environment"
  else if
    selection.environment
    !=
    match Collection.publication_source_aggregate publication with
    | Some source -> source.aggregate_environment
    | None -> selection.environment
  then Error "selected aggregate type belongs to another parser environment"
  else if selection.type_specifier != owner_type_specifier then
    Error "selected aggregate type substituted its owning type occurrence"
  else
    let* _ = pointer_depth pointer_layers in
    match
      (type_specifier, Collection.publication_source_aggregate publication)
    with
    | Frontend.Ast.Named_type_specifier original, Some source
      when original == identifier
           && source.aggregate_entry == selection.entry
           && source.aggregate_name.spelling = identifier.spelling
           && Visibility.name selection.entry = identifier.spelling ->
        let* symbol =
          match Collection.publication_aggregate_identity publication with
          | Some symbol -> Ok symbol
          | None ->
              Error "selected aggregate publication has no canonical identity"
        in
        if not (Symbol_table.owns_symbol table symbol) then
          Error "selected aggregate identity belongs to another symbol table"
        else if
          not (Symbol.equal_kind (Symbol.kind symbol) Symbol.Aggregate_type)
        then Error "selected aggregate identity is not an aggregate type"
        else if Symbol.name symbol <> identifier.spelling then
          Error
            "selected aggregate identity disagrees with the selected spelling"
        else if
          not
            (Symbol.Scope_id.equal (Symbol.scope_id symbol)
               (Symbol_table.scope_id
                  (Declaration_collection.namespace_scope namespace)))
        then Error "selected aggregate identity is outside the owning namespace"
        else
          Ok
            {
              selected_type_specifier = type_specifier;
              selected_identifier = identifier;
              selected_pointer_layers = pointer_layers;
              selected_symbol = symbol;
            }
    | Frontend.Ast.Named_type_specifier _, Some _ ->
        Error "selected aggregate receipt substituted its original named type"
    | _, Some _ -> Error "selected aggregate receipt does not name an aggregate"
    | _, None -> Error "selected aggregate identity has no source publication"

let validate_selected_aggregate ~table ~namespace proof =
  if not (Declaration_collection.namespace_owns_table namespace table) then
    Error "selected aggregate consumer belongs to another semantic table"
  else if not (Symbol_table.owns_symbol table proof.selected_symbol) then
    Error "selected aggregate proof belongs to another semantic table"
  else if
    not
      (Symbol.Scope_id.equal
         (Symbol.scope_id proof.selected_symbol)
         (Symbol_table.scope_id
            (Declaration_collection.namespace_scope namespace)))
  then Error "selected aggregate proof belongs to another semantic namespace"
  else if
    not
      (Symbol.equal_kind
         (Symbol.kind proof.selected_symbol)
         Symbol.Aggregate_type)
  then Error "selected aggregate proof no longer identifies an aggregate type"
  else Ok ()

let selected proof type_specifier pointer_layers =
  let ( let* ) = Result.bind in
  if type_specifier != proof.selected_type_specifier then
    Error "selected aggregate type substituted its original type occurrence"
  else if not (same_physical_list pointer_layers proof.selected_pointer_layers)
  then Error "selected aggregate type substituted its original pointer children"
  else
    match type_specifier with
    | Frontend.Ast.Named_type_specifier identifier
      when identifier == proof.selected_identifier ->
        let* depth = pointer_depth pointer_layers in
        if depth = 0 then
          Error "selected aggregate values require separate layout admission"
        else
          let* resolved_type =
            Type.make_aggregate ~symbol:proof.selected_symbol
              ~pointer_depth:depth
          in
          Type_reference.make ~spelling:identifier.spelling
            ~spelling_origin:(origin identifier.location)
            ~pointer_origins:
              (List.map
                 (fun (layer : Frontend.Ast.pointer_layer) ->
                   origin layer.location)
                 pointer_layers)
            ~resolved_type
    | Frontend.Ast.Named_type_specifier _ ->
        Error "selected aggregate type substituted its original named child"
    | _ -> Error "selected aggregate proof cannot authorize a nonaggregate type"
