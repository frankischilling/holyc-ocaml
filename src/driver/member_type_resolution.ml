module String_map = Map.Make (String)

let origin (location : Frontend.Ast.location) =
  Sema.Symbol.Source_location
    {
      span = location.span;
      source_segments = location.source_segments;
      generated_from = location.generated_from;
      defined_at = location.defined_at;
    }

let same_symbol left right =
  Sema.Symbol.Id.equal (Sema.Symbol.id left) (Sema.Symbol.id right)

let same_scope left right =
  Sema.Symbol.Scope_id.equal
    (Sema.Symbol_table.scope_id left)
    (Sema.Symbol_table.scope_id right)

let pointer_depth pointer_layers =
  let rec validate expected = function
    | [] -> Ok (expected - 1)
    | (layer : Frontend.Ast.pointer_layer) :: rest ->
        if layer.depth <> expected then
          Error "semantic member type has inconsistent pointer depths"
        else if not (String.equal layer.spelling "*") then
          Error "semantic member type has an invalid pointer spelling"
        else validate (expected + 1) rest
  in
  validate 1 pointer_layers

let pointer_origins pointer_layers =
  List.map
    (fun (layer : Frontend.Ast.pointer_layer) -> origin layer.location)
    pointer_layers

let type_equal left right =
  Sema.Type.pointer_depth left = Sema.Type.pointer_depth right
  &&
  match (Sema.Type.base left, Sema.Type.base right) with
  | ( Sema.Type.Primitive (left_form, left_primitive),
      Sema.Type.Primitive (right_form, right_primitive) ) ->
      left_form = right_form
      && Sema.Primitive_type.equal left_primitive right_primitive
  | Sema.Type.Aggregate left, Sema.Type.Aggregate right ->
      same_symbol left right
  | Sema.Type.Primitive _, Sema.Type.Aggregate _
  | Sema.Type.Aggregate _, Sema.Type.Primitive _ -> false

let resolve_type visible type_specifier pointer_layers =
  match pointer_depth pointer_layers with
  | Error _ as error -> error
  | Ok pointer_depth -> (
      match type_specifier with
      | Frontend.Ast.Primitive_type_specifier primitive ->
          Sema.Type.make_primitive ~form:Sema.Type.Public_spelling
            ~primitive:primitive.primitive ~pointer_depth
      | Frontend.Ast.Internal_type_specifier internal ->
          Sema.Type.make_primitive ~form:Sema.Type.Internal_storage
            ~primitive:internal.primitive ~pointer_depth
      | Frontend.Ast.Named_type_specifier identifier -> (
          match String_map.find_opt identifier.spelling visible with
          | None ->
              Error
                (Printf.sprintf
                   "member type %S is not visible at this aggregate definition"
                   identifier.spelling)
          | Some symbol -> Sema.Type.make_aggregate ~symbol ~pointer_depth))

let make_type_reference visible type_specifier pointer_layers =
  match resolve_type visible type_specifier pointer_layers with
  | Error _ as error -> error
  | Ok resolved_type ->
      Sema.Member_type_resolution.make_type_reference
        ~spelling:(Frontend.Ast.type_specifier_spelling type_specifier)
        ~spelling_origin:
          (origin (Frontend.Ast.type_specifier_location type_specifier))
        ~pointer_origins:(pointer_origins pointer_layers)
        ~resolved_type

type ast_declaration = {
  identifier : Frontend.Ast.identifier;
  declaration_kind : Sema.Declaration_collection.declaration_kind;
  aggregate_kind : Sema.Aggregate_resolution.aggregate_kind;
  item_index : int;
  definition : Frontend.Ast.aggregate_definition option;
}

type event = {
  ast : ast_declaration;
  declaration_symbol : Sema.Symbol.t;
  identity_symbol : Sema.Symbol.t;
}

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
      Error "semantic member types received a nonaggregate declaration"

let validate_event ~table ~scope entry resolved ast =
  let entry_kind = Sema.Declaration_collection.entry_kind entry in
  let entry_symbol = Sema.Declaration_collection.entry_symbol entry in
  let site = Sema.Aggregate_resolution.resolved_declaration_site resolved in
  let site_symbol = Sema.Aggregate_resolution.declaration_site_symbol site in
  let identity_symbol =
    Sema.Aggregate_resolution.resolved_declaration_identity_symbol resolved
  in
  if entry_kind <> ast.declaration_kind then
    Error "semantic member type declaration does not match the AST kind"
  else if Sema.Declaration_collection.entry_item_index entry <> ast.item_index
  then Error "semantic member type declaration does not match the AST order"
  else if Sema.Declaration_collection.entry_declarator_index entry <> None then
    Error "semantic member type aggregate cannot have a declarator index"
  else if
    not (String.equal (Sema.Symbol.name entry_symbol) ast.identifier.spelling)
  then Error "semantic member type declaration does not match the AST name"
  else if Sema.Symbol.origin entry_symbol <> origin ast.identifier.location then
    Error "semantic member type declaration does not match the AST origin"
  else if not (same_symbol entry_symbol site_symbol) then
    Error
      "semantic member type reconciliation does not match the declaration \
       collection"
  else if
    Sema.Aggregate_resolution.declaration_site_item_index site <> ast.item_index
  then Error "semantic member type reconciliation has the wrong item order"
  else if
    Sema.Aggregate_resolution.declaration_site_aggregate_kind site
    <> ast.aggregate_kind
  then Error "semantic member type reconciliation has the wrong aggregate kind"
  else if not (Sema.Symbol_table.owns_symbol table identity_symbol) then
    Error "semantic member type identity belongs to a different symbol table"
  else if
    not
      (Sema.Symbol.Scope_id.equal
         (Sema.Symbol.scope_id identity_symbol)
         (Sema.Symbol_table.scope_id scope))
  then Error "semantic member type identity does not belong to the module"
  else
    match expected_resolution_kind entry_kind with
    | Error _ as error -> error
    | Ok expected_kind ->
        if Sema.Aggregate_resolution.declaration_site_kind site <> expected_kind
        then Error "semantic member type reconciliation has the wrong role"
        else Ok { ast; declaration_symbol = entry_symbol; identity_symbol }

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
        Error "semantic member types do not match the aggregate declarations"
  in
  pair [] entries resolved ast

type ast_member = {
  declaration : Frontend.Ast.aggregate_member_declaration;
  declarator : Frontend.Ast.aggregate_member_declarator;
  member_path : int list;
  declarator_index : int;
}

let declarator_members declaration member_path =
  List.mapi
    (fun declarator_index declarator ->
      { declaration; declarator; member_path; declarator_index })
    declaration.Frontend.Ast.member_declarators

let rec ast_members ~path_prefix members =
  members
  |> List.mapi (fun member_index member ->
      let member_path = path_prefix @ [ member_index ] in
      match member with
      | Frontend.Ast.Aggregate_member_declaration declaration ->
          declarator_members declaration member_path
      | Frontend.Ast.Anonymous_union_member anonymous_union ->
          ast_members ~path_prefix:member_path
            anonymous_union.anonymous_union_members
      | Frontend.Ast.Aggregate_offset_directive _
      | Frontend.Ast.Empty_aggregate_member _ -> [])
  |> List.concat

let validate_header_source ~table event header
    (definition : Frontend.Ast.aggregate_definition) =
  if
    not
      (Sema.Symbol_table.owns_symbol table
         (Sema.Aggregate_header_resolution.header_symbol header))
  then Error "semantic member type header belongs to a different symbol table"
  else if
    not
      (same_symbol
         (Sema.Aggregate_header_resolution.header_symbol header)
         event.identity_symbol)
  then Error "semantic member type header has the wrong aggregate identity"
  else if
    Sema.Aggregate_header_resolution.header_aggregate_kind header
    <> event.ast.aggregate_kind
  then Error "semantic member type header has the wrong aggregate kind"
  else if
    Sema.Aggregate_header_resolution.header_item_index header
    <> event.ast.item_index
  then Error "semantic member type header has the wrong item order"
  else if
    Sema.Aggregate_header_resolution.header_origin header
    <> origin definition.Frontend.Ast.location
  then Error "semantic member type header has the wrong definition origin"
  else if
    Sema.Aggregate_header_resolution.header_keyword_origin header
    <> origin definition.aggregate_keyword_location
  then Error "semantic member type header has the wrong keyword origin"
  else Ok ()

let type_belongs_to table type_ =
  match Sema.Type.base type_ with
  | Sema.Type.Primitive _ -> true
  | Sema.Type.Aggregate symbol -> Sema.Symbol_table.owns_symbol table symbol

let validate_backing ~table visible header
    (definition : Frontend.Ast.aggregate_definition) =
  match
    ( definition.Frontend.Ast.backing,
      Sema.Aggregate_header_resolution.header_backing header )
  with
  | None, None -> Ok ()
  | Some backing, Some resolved -> (
      let actual_type =
        Sema.Aggregate_header_resolution.backing_type resolved
      in
      if not (type_belongs_to table actual_type) then
        Error "semantic member type backing belongs to a different symbol table"
      else
        match
          resolve_type visible backing.backing_type_specifier
            backing.backing_pointer_layers
        with
        | Error _ as error -> error
        | Ok expected_type ->
            if
              not
                (String.equal
                   (Frontend.Ast.type_specifier_spelling
                      backing.backing_type_specifier)
                   (Sema.Aggregate_header_resolution.backing_spelling resolved))
            then Error "semantic member type header has the wrong backing name"
            else if
              Sema.Aggregate_header_resolution.backing_origin resolved
              <> origin backing.backing_location
            then
              Error "semantic member type header has the wrong backing origin"
            else if
              Sema.Aggregate_header_resolution.backing_spelling_origin resolved
              <> origin
                   (Frontend.Ast.type_specifier_location
                      backing.backing_type_specifier)
            then
              Error
                "semantic member type header has the wrong backing spelling \
                 origin"
            else if
              Sema.Aggregate_header_resolution.backing_pointer_origins resolved
              <> pointer_origins backing.backing_pointer_layers
            then
              Error
                "semantic member type header has the wrong backing pointer \
                 origins"
            else if not (type_equal expected_type actual_type) then
              Error "semantic member type header has the wrong backing type"
            else Ok ())
  | None, Some _ | Some _, None ->
      Error "semantic member type header has the wrong backing shape"

let validate_base ~table visible header
    (definition : Frontend.Ast.aggregate_definition) =
  match
    ( definition.Frontend.Ast.base,
      Sema.Aggregate_header_resolution.header_base header )
  with
  | None, None -> Ok ()
  | Some base, Some resolved -> (
      let actual_symbol =
        Sema.Aggregate_header_resolution.base_symbol resolved
      in
      if not (Sema.Symbol_table.owns_symbol table actual_symbol) then
        Error "semantic member type base belongs to a different symbol table"
      else
        match String_map.find_opt base.base_name.spelling visible with
        | None ->
            Error
              (Printf.sprintf
                 "aggregate base %S is not visible while resolving member types"
                 base.base_name.spelling)
        | Some expected_symbol ->
            if
              not
                (String.equal base.base_name.spelling
                   (Sema.Aggregate_header_resolution.base_spelling resolved))
            then Error "semantic member type header has the wrong base name"
            else if
              Sema.Aggregate_header_resolution.base_origin resolved
              <> origin base.base_location
            then Error "semantic member type header has the wrong base origin"
            else if
              Sema.Aggregate_header_resolution.base_colon_origin resolved
              <> origin base.base_colon_location
            then Error "semantic member type header has the wrong colon origin"
            else if
              Sema.Aggregate_header_resolution.base_name_origin resolved
              <> origin base.base_name.location
            then
              Error "semantic member type header has the wrong base-name origin"
            else if not (same_symbol expected_symbol actual_symbol) then
              Error "semantic member type header has the wrong base identity"
            else Ok ())
  | None, Some _ | Some _, None ->
      Error "semantic member type header has the wrong base shape"

let validate_collected_aggregate ~table ~scope event collected
    (definition : Frontend.Ast.aggregate_definition) =
  let collected_symbol = Sema.Member_collection.aggregate_symbol collected in
  let collected_scope = Sema.Member_collection.aggregate_scope collected in
  if not (same_symbol collected_symbol event.declaration_symbol) then
    Error "semantic member collection has the wrong aggregate declaration"
  else if not (same_symbol collected_symbol event.identity_symbol) then
    Error "semantic member collection has the wrong canonical identity"
  else if
    Sema.Member_collection.aggregate_item_index collected
    <> event.ast.item_index
  then Error "semantic member collection has the wrong item order"
  else if not (Sema.Symbol_table.owns_scope table collected_scope) then
    Error "semantic member collection belongs to a different symbol table"
  else if
    Sema.Symbol_table.scope_kind collected_scope <> Sema.Symbol_table.Aggregate
  then Error "semantic member collection does not use an aggregate scope"
  else if
    match Sema.Symbol_table.parent collected_scope with
    | Some parent -> not (same_scope parent scope)
    | None -> true
  then Error "semantic member collection does not belong to the module"
  else
    let expected =
      ast_members ~path_prefix:[] definition.Frontend.Ast.members
    in
    let entries = Sema.Member_collection.aggregate_entries collected in
    let rec pair pairs_rev entries expected =
      match (entries, expected) with
      | [], [] -> Ok (List.rev pairs_rev)
      | entry :: entry_rest, ast_member :: expected_rest ->
          let symbol = Sema.Member_collection.entry_symbol entry in
          if
            Sema.Member_collection.entry_member_path entry
            <> ast_member.member_path
          then Error "semantic member collection has the wrong member path"
          else if
            Sema.Member_collection.entry_declarator_index entry
            <> ast_member.declarator_index
          then Error "semantic member collection has the wrong declarator order"
          else if
            not
              (String.equal (Sema.Symbol.name symbol)
                 ast_member.declarator.member_name.spelling)
          then Error "semantic member collection has the wrong member name"
          else if
            Sema.Symbol.origin symbol
            <> origin ast_member.declarator.member_name.location
          then Error "semantic member collection has the wrong member origin"
          else pair ((entry, ast_member) :: pairs_rev) entry_rest expected_rest
      | [], _ :: _ | _ :: _, [] ->
          Error "semantic member collection does not match the AST members"
    in
    pair [] entries expected

let member_fact visible entry ast_member =
  let declarator = ast_member.declarator in
  match
    make_type_reference visible ast_member.declaration.member_type_specifier
      declarator.member_pointer_layers
  with
  | Error _ as error -> error
  | Ok type_reference ->
      let declarator_kind =
        match declarator.member_function_pointer with
        | None -> Ok Sema.Member_type_resolution.Object
        | Some function_pointer -> (
            match pointer_depth function_pointer.indirection_layers with
            | Error _ as error -> error
            | Ok _ -> (
                match
                  Sema.Member_type_resolution.make_function_pointer
                    ~origin:(origin function_pointer.function_pointer_location)
                    ~indirection_origins:
                      (pointer_origins function_pointer.indirection_layers)
                with
                | Error _ as error -> error
                | Ok function_pointer ->
                    Ok
                      (Sema.Member_type_resolution.Function_pointer
                         function_pointer)))
      in
      Result.bind declarator_kind (fun declarator_kind ->
          Sema.Member_type_resolution.make_member
            ~symbol:(Sema.Member_collection.entry_symbol entry)
            ~member_path:ast_member.member_path
            ~declarator_index:ast_member.declarator_index
            ~declarator_origin:(origin declarator.member_declarator_location)
            ~type_reference ~declarator_kind
            ~array_dimension_origins:
              (List.map
                 (fun (dimension : Frontend.Ast.array_dimension) ->
                   origin dimension.location)
                 declarator.member_array_dimensions))

let member_facts visible pairs =
  let rec resolve facts_rev = function
    | [] -> Ok (List.rev facts_rev)
    | (entry, ast_member) :: rest -> (
        match member_fact visible entry ast_member with
        | Error _ as error -> error
        | Ok fact -> resolve (fact :: facts_rev) rest)
  in
  resolve [] pairs

let resolve_definition ~table ~scope visible event header collected
    (definition : Frontend.Ast.aggregate_definition) =
  match validate_header_source ~table event header definition with
  | Error _ as error -> error
  | Ok () -> (
      match validate_backing ~table visible header definition with
      | Error _ as error -> error
      | Ok () -> (
          let visible =
            String_map.add event.ast.identifier.spelling event.identity_symbol
              visible
          in
          match validate_base ~table visible header definition with
          | Error _ as error -> error
          | Ok () -> (
              match
                validate_collected_aggregate ~table ~scope event collected
                  definition
              with
              | Error _ as error -> error
              | Ok pairs -> (
                  match member_facts visible pairs with
                  | Error _ as error -> error
                  | Ok members ->
                      Result.map
                        (fun aggregate -> (visible, aggregate))
                        (Sema.Member_type_resolution.make_aggregate
                           ~symbol:event.identity_symbol
                           ~scope:
                             (Sema.Member_collection.aggregate_scope collected)
                           ~item_index:event.ast.item_index members)))))

let resolve_events ~table ~scope events headers collected =
  let rec resolve visible facts_rev events headers collected =
    match events with
    | [] ->
        if headers <> [] || collected <> [] then
          Error
            "semantic member type definitions do not match their semantic \
             inputs"
        else
          Sema.Member_type_resolution.resolve ~table ~parent:scope
            (List.rev facts_rev)
    | event :: rest -> (
        match event.ast.definition with
        | None ->
            resolve
              (String_map.add event.ast.identifier.spelling
                 event.identity_symbol visible)
              facts_rev rest headers collected
        | Some definition -> (
            match (headers, collected) with
            | header :: header_rest, aggregate :: aggregate_rest -> (
                match
                  resolve_definition ~table ~scope visible event header
                    aggregate definition
                with
                | Error _ as error -> error
                | Ok (visible, fact) ->
                    resolve visible (fact :: facts_rev) rest header_rest
                      aggregate_rest)
            | [], _ | _, [] ->
                Error
                  "semantic member type definitions do not match their \
                   semantic inputs"))
  in
  resolve String_map.empty [] events headers collected

let resolve ~table ~declarations ~aggregates ~headers ~members module_ =
  let scope = Sema.Declaration_collection.scope declarations in
  if not (Sema.Symbol_table.owns_scope table scope) then
    Error "semantic member declarations belong to a different symbol table"
  else if Sema.Symbol_table.scope_kind scope <> Sema.Symbol_table.Module then
    Error "semantic member declarations must belong to a module scope"
  else
    match events ~table ~declarations ~aggregates module_ with
    | Error _ as error -> error
    | Ok events ->
        resolve_events ~table ~scope events
          (Sema.Aggregate_header_resolution.headers headers)
          (Sema.Member_collection.aggregates members)
