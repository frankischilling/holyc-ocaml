module String_map = Map.Make (String)

let origin (location : Frontend.Ast.location) =
  Sema.Symbol.Source_location
    {
      span = location.span;
      source_segments = location.source_segments;
      generated_from = location.generated_from;
      defined_at = location.defined_at;
    }

type ast_declaration = {
  identifier : Frontend.Ast.identifier;
  declaration_kind : Sema.Declaration_collection.declaration_kind;
  aggregate_kind : Sema.Aggregate_resolution.aggregate_kind;
  item_index : int;
  definition : Frontend.Ast.aggregate_definition option;
}

type event = { ast : ast_declaration; identity_symbol : Sema.Symbol.t }

let aggregate_kind = function
  | Frontend.Ast.Class_aggregate -> Sema.Aggregate_resolution.Class
  | Frontend.Ast.Union_aggregate -> Sema.Aggregate_resolution.Union

let ast_declarations (module_ : Frontend.Ast.module_) =
  module_.items
  |> List.mapi (fun item_index item -> (item_index, item))
  |> List.filter_map (function
    | item_index, Frontend.Ast.Aggregate_forward_declaration forward ->
        Some
          {
            identifier = forward.name;
            declaration_kind = Sema.Declaration_collection.Aggregate_forward;
            aggregate_kind = aggregate_kind forward.aggregate_kind;
            item_index;
            definition = None;
          }
    | item_index, Frontend.Ast.Aggregate_definition definition ->
        Some
          {
            identifier = definition.name;
            declaration_kind = Sema.Declaration_collection.Aggregate_definition;
            aggregate_kind = aggregate_kind definition.aggregate_kind;
            item_index;
            definition = Some definition;
          }
    | _ -> None)

let aggregate_entries declarations =
  Sema.Declaration_collection.entries declarations
  |> List.filter (fun entry ->
      match Sema.Declaration_collection.entry_kind entry with
      | Sema.Declaration_collection.Aggregate_forward
      | Sema.Declaration_collection.Aggregate_definition -> true
      | Sema.Declaration_collection.Aggregate_attached_global
      | Sema.Declaration_collection.Global_variable
      | Sema.Declaration_collection.Function_prototype
      | Sema.Declaration_collection.Function_definition -> false)

let expected_resolution_kind = function
  | Sema.Declaration_collection.Aggregate_forward ->
      Ok Sema.Aggregate_resolution.Forward
  | Sema.Declaration_collection.Aggregate_definition ->
      Ok Sema.Aggregate_resolution.Definition
  | Sema.Declaration_collection.Aggregate_attached_global
  | Sema.Declaration_collection.Global_variable
  | Sema.Declaration_collection.Function_prototype
  | Sema.Declaration_collection.Function_definition ->
      Error "semantic aggregate headers received a nonaggregate declaration"

let same_symbol left right =
  Sema.Symbol.Id.equal (Sema.Symbol.id left) (Sema.Symbol.id right)

let validate_event ~table ~scope entry resolved ast =
  let entry_kind = Sema.Declaration_collection.entry_kind entry in
  let entry_symbol = Sema.Declaration_collection.entry_symbol entry in
  let site = Sema.Aggregate_resolution.resolved_declaration_site resolved in
  let site_symbol = Sema.Aggregate_resolution.declaration_site_symbol site in
  let identity_symbol =
    Sema.Aggregate_resolution.resolved_declaration_identity_symbol resolved
  in
  if entry_kind <> ast.declaration_kind then
    Error "semantic aggregate header declaration does not match the AST kind"
  else if Sema.Declaration_collection.entry_item_index entry <> ast.item_index
  then
    Error
      "semantic aggregate header declaration does not match the AST item order"
  else if Sema.Declaration_collection.entry_declarator_index entry <> None then
    Error "semantic aggregate header declaration cannot have a declarator index"
  else if
    not (String.equal (Sema.Symbol.name entry_symbol) ast.identifier.spelling)
  then Error "semantic aggregate header declaration does not match the AST name"
  else if Sema.Symbol.origin entry_symbol <> origin ast.identifier.location then
    Error "semantic aggregate header declaration does not match the AST origin"
  else if not (same_symbol entry_symbol site_symbol) then
    Error
      "semantic aggregate reconciliation does not match the declaration \
       collection"
  else if
    Sema.Aggregate_resolution.declaration_site_item_index site <> ast.item_index
  then
    Error "semantic aggregate reconciliation does not match the AST item order"
  else if
    Sema.Aggregate_resolution.declaration_site_aggregate_kind site
    <> ast.aggregate_kind
  then Error "semantic aggregate reconciliation does not match the AST kind"
  else if not (Sema.Symbol_table.owns_symbol table identity_symbol) then
    Error "semantic aggregate identity belongs to a different symbol table"
  else if
    not
      (Sema.Symbol.Scope_id.equal
         (Sema.Symbol.scope_id identity_symbol)
         (Sema.Symbol_table.scope_id scope))
  then Error "semantic aggregate identity does not belong to the module scope"
  else
    match expected_resolution_kind entry_kind with
    | Error _ as error -> error
    | Ok expected_kind ->
        if Sema.Aggregate_resolution.declaration_site_kind site <> expected_kind
        then
          Error
            "semantic aggregate reconciliation has the wrong declaration role"
        else Ok { ast; identity_symbol }

let events ~table ~declarations ~aggregates module_ =
  let scope = Sema.Declaration_collection.scope declarations in
  let entries = aggregate_entries declarations in
  let resolved = Sema.Aggregate_resolution.declarations aggregates in
  let ast = ast_declarations module_ in
  let rec pair events_rev entries resolved ast =
    match (entries, resolved, ast) with
    | [], [], [] -> Ok (List.rev events_rev)
    | entry :: entry_rest, resolved :: resolved_rest, ast :: ast_rest -> (
        match validate_event ~table ~scope entry resolved ast with
        | Error _ as error -> error
        | Ok event ->
            pair (event :: events_rev) entry_rest resolved_rest ast_rest)
    | [], _, _ | _, [], _ | _, _, [] ->
        Error "semantic aggregate headers do not match the AST declarations"
  in
  pair [] entries resolved ast

let pointer_depth pointer_layers =
  let rec validate expected = function
    | [] -> Ok (expected - 1)
    | (layer : Frontend.Ast.pointer_layer) :: rest ->
        if layer.depth <> expected then
          Error "semantic aggregate backing has inconsistent pointer depths"
        else if not (String.equal layer.spelling "*") then
          Error "semantic aggregate backing has an invalid pointer spelling"
        else validate (expected + 1) rest
  in
  validate 1 pointer_layers

let primitive_type ~form primitive pointer_layers =
  match pointer_depth pointer_layers with
  | Error _ as error -> error
  | Ok pointer_depth -> Sema.Type.make_primitive ~form ~primitive ~pointer_depth

let aggregate_type symbol pointer_layers =
  match pointer_depth pointer_layers with
  | Error _ as error -> error
  | Ok pointer_depth -> Sema.Type.make_aggregate ~symbol ~pointer_depth

let resolve_backing visible (backing : Frontend.Ast.aggregate_backing) =
  let spelling =
    Frontend.Ast.type_specifier_spelling backing.backing_type_specifier
  in
  let resolved_type =
    match backing.backing_type_specifier with
    | Frontend.Ast.Primitive_type_specifier primitive ->
        primitive_type ~form:Sema.Type.Public_spelling primitive.primitive
          backing.backing_pointer_layers
    | Frontend.Ast.Internal_type_specifier internal ->
        primitive_type ~form:Sema.Type.Internal_storage internal.primitive
          backing.backing_pointer_layers
    | Frontend.Ast.Named_type_specifier identifier -> (
        match String_map.find_opt identifier.spelling visible with
        | None ->
            Error
              (Printf.sprintf
                 "aggregate backing %S is not visible before this definition"
                 identifier.spelling)
        | Some symbol -> aggregate_type symbol backing.backing_pointer_layers)
  in
  match resolved_type with
  | Error _ as error -> error
  | Ok resolved_type ->
      Sema.Aggregate_header_resolution.make_backing_site ~spelling
        ~origin:(origin backing.backing_location)
        ~spelling_origin:
          (origin
             (Frontend.Ast.type_specifier_location
                backing.backing_type_specifier))
        ~pointer_origins:
          (List.map
             (fun (layer : Frontend.Ast.pointer_layer) -> origin layer.location)
             backing.backing_pointer_layers)
        ~resolved_type

let resolve_base visible (base : Frontend.Ast.aggregate_base) =
  match String_map.find_opt base.base_name.spelling visible with
  | None ->
      Error
        (Printf.sprintf "aggregate base %S is not visible at this definition"
           base.base_name.spelling)
  | Some symbol ->
      Sema.Aggregate_header_resolution.make_base_site
        ~spelling:base.base_name.spelling
        ~origin:(origin base.base_location)
        ~colon_origin:(origin base.base_colon_location)
        ~name_origin:(origin base.base_name.location)
        ~symbol

let resolve_events ~table ~scope events =
  let rec resolve visible headers_rev = function
    | [] ->
        Sema.Aggregate_header_resolution.resolve ~table ~parent:scope
          (List.rev headers_rev)
    | event :: rest -> (
        let name = event.ast.identifier.spelling in
        match event.ast.definition with
        | None ->
            resolve
              (String_map.add name event.identity_symbol visible)
              headers_rev rest
        | Some definition -> (
            let backing =
              match definition.backing with
              | None -> Ok None
              | Some backing ->
                  Result.map Option.some (resolve_backing visible backing)
            in
            match backing with
            | Error _ as error -> error
            | Ok backing -> (
                let visible =
                  String_map.add name event.identity_symbol visible
                in
                let base =
                  match definition.base with
                  | None -> Ok None
                  | Some base ->
                      Result.map Option.some (resolve_base visible base)
                in
                match base with
                | Error _ as error -> error
                | Ok base -> (
                    match
                      Sema.Aggregate_header_resolution.make_header
                        ~symbol:event.identity_symbol
                        ~aggregate_kind:event.ast.aggregate_kind
                        ~item_index:event.ast.item_index
                        ~origin:(origin definition.location)
                        ~keyword_origin:
                          (origin definition.aggregate_keyword_location)
                        ~backing ~base
                    with
                    | Error _ as error -> error
                    | Ok header -> resolve visible (header :: headers_rev) rest)
                )))
  in
  resolve String_map.empty [] events

let resolve ~table ~declarations ~aggregates module_ =
  let scope = Sema.Declaration_collection.scope declarations in
  if not (Sema.Symbol_table.owns_scope table scope) then
    Error "semantic aggregate declarations belong to a different symbol table"
  else if Sema.Symbol_table.scope_kind scope <> Sema.Symbol_table.Module then
    Error "semantic aggregate declarations must belong to a module scope"
  else
    match events ~table ~declarations ~aggregates module_ with
    | Error _ as error -> error
    | Ok events -> resolve_events ~table ~scope events
