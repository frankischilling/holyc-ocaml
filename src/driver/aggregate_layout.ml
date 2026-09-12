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
      Error "aggregate layout received a nonaggregate declaration"

let validate_event ~table ~scope entry resolved ast =
  let entry_kind = Sema.Declaration_collection.entry_kind entry in
  let entry_symbol = Sema.Declaration_collection.entry_symbol entry in
  let site = Sema.Aggregate_resolution.resolved_declaration_site resolved in
  let site_symbol = Sema.Aggregate_resolution.declaration_site_symbol site in
  let identity_symbol =
    Sema.Aggregate_resolution.resolved_declaration_identity_symbol resolved
  in
  if entry_kind <> ast.declaration_kind then
    Error "aggregate layout declaration does not match the AST kind"
  else if Sema.Declaration_collection.entry_item_index entry <> ast.item_index
  then Error "aggregate layout declaration does not match the AST order"
  else if Sema.Declaration_collection.entry_declarator_index entry <> None then
    Error "aggregate layout declaration unexpectedly has a declarator index"
  else if
    not (String.equal (Sema.Symbol.name entry_symbol) ast.identifier.spelling)
  then Error "aggregate layout declaration does not match the AST name"
  else if Sema.Symbol.origin entry_symbol <> origin ast.identifier.location then
    Error "aggregate layout declaration does not match the AST origin"
  else if not (same_symbol entry_symbol site_symbol) then
    Error "aggregate layout reconciliation does not match the declaration"
  else if
    Sema.Aggregate_resolution.declaration_site_item_index site <> ast.item_index
  then Error "aggregate layout reconciliation has the wrong source order"
  else if
    Sema.Aggregate_resolution.declaration_site_aggregate_kind site
    <> ast.aggregate_kind
  then Error "aggregate layout reconciliation has the wrong aggregate kind"
  else if not (Sema.Symbol_table.owns_symbol table identity_symbol) then
    Error "aggregate layout identity belongs to a different symbol table"
  else if
    not
      (Sema.Symbol.Scope_id.equal
         (Sema.Symbol.scope_id identity_symbol)
         (Sema.Symbol_table.scope_id scope))
  then Error "aggregate layout identity does not belong to the module"
  else
    match expected_resolution_kind entry_kind with
    | Error _ as error -> error
    | Ok expected_kind ->
        if Sema.Aggregate_resolution.declaration_site_kind site <> expected_kind
        then Error "aggregate layout reconciliation has the wrong role"
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
        Error "aggregate layout inputs do not match the aggregate declarations"
  in
  pair [] entries resolved ast

let expression ast =
  Sema.Closed_layout_expression.of_ast ~allow_floating:false ast

let dimension ~table ?prepared (dimension : Frontend.Ast.array_dimension) =
  let expression =
    match prepared with
    | None -> Ok (Option.map expression dimension.dimension_expression)
    | Some prepared ->
        Result.bind (prepared dimension) (fun checked ->
            Result.map
              (fun () ->
                Some
                  (Sema.Aggregate_layout.Integer_expression
                     {
                       value = Sema.Compiler_record.dimension_count checked;
                       origin = origin dimension.location;
                     }))
              (Sema.Compiler_record.validate_dimension ~table ~dimension checked))
  in
  Result.map
    (fun expression ->
      {
        Sema.Aggregate_layout.dimension_expression = expression;
        dimension_origin = origin dimension.location;
      })
    expression

let dimensions dimension values =
  let rec loop reversed = function
    | [] -> Ok (List.rev reversed)
    | value :: rest ->
        Result.bind (dimension value) (fun value ->
            loop (value :: reversed) rest)
  in
  loop [] values

let validate_member fact path declarator_index
    (declarator : Frontend.Ast.aggregate_member_declarator) =
  let symbol = Sema.Member_type_resolution.member_symbol fact in
  if Sema.Member_type_resolution.member_path fact <> path then
    Error "aggregate layout member has the wrong anonymous-union path"
  else if
    Sema.Member_type_resolution.member_declarator_index fact <> declarator_index
  then Error "aggregate layout member has the wrong declarator order"
  else if
    not (String.equal (Sema.Symbol.name symbol) declarator.member_name.spelling)
  then Error "aggregate layout member does not match the AST name"
  else if Sema.Symbol.origin symbol <> origin declarator.member_name.location
  then Error "aggregate layout member does not match the AST origin"
  else if
    Sema.Member_type_resolution.member_declarator_origin fact
    <> origin declarator.member_declarator_location
  then Error "aggregate layout member does not match the declarator origin"
  else
    let expected_dimension_origins =
      List.map
        (fun (dimension : Frontend.Ast.array_dimension) ->
          origin dimension.location)
        declarator.member_array_dimensions
    in
    if
      Sema.Member_type_resolution.member_array_dimension_origins fact
      <> expected_dimension_origins
    then Error "aggregate layout member does not match its array dimensions"
    else Ok ()

let field ~dimension fact path declarator_index
    (declarator : Frontend.Ast.aggregate_member_declarator) =
  let ( let* ) = Result.bind in
  let* () = validate_member fact path declarator_index declarator in
  let* member_dimensions =
    dimensions dimension declarator.member_array_dimensions
  in
  let type_reference = Sema.Member_type_resolution.member_type_reference fact in
  let member_is_function_pointer =
    match Sema.Member_type_resolution.member_declarator_kind fact with
    | Sema.Member_type_resolution.Object -> false
    | Sema.Member_type_resolution.Function_pointer _ -> true
  in
  Ok
    (Sema.Aggregate_layout.Field
       {
         member_symbol = Sema.Member_type_resolution.member_symbol fact;
         member_path = path;
         member_declarator_index = declarator_index;
         member_origin = origin declarator.member_declarator_location;
         member_type =
           Sema.Member_type_resolution.type_reference_type type_reference;
         member_is_function_pointer;
         member_dimensions;
       })

let declaration_items ~dimension path
    (declaration : Frontend.Ast.aggregate_member_declaration) facts =
  let rec loop index items_rev facts = function
    | [] -> Ok (List.rev items_rev, facts)
    | declarator :: rest -> (
        match facts with
        | [] -> Error "aggregate layout is missing a resolved member"
        | fact :: fact_rest ->
            Result.bind (field ~dimension fact path index declarator)
              (fun item -> loop (index + 1) (item :: items_rev) fact_rest rest))
  in
  loop 0 [] facts declaration.Frontend.Ast.member_declarators

let rec member_items ~offset ~dimension path_prefix members facts =
  let rec loop member_index items_rev facts = function
    | [] -> Ok (List.rev items_rev, facts)
    | member :: rest ->
        let path = path_prefix @ [ member_index ] in
        let built =
          match member with
          | Frontend.Ast.Aggregate_member_declaration declaration ->
              declaration_items ~dimension path declaration facts
          | Frontend.Ast.Aggregate_offset_directive directive ->
              Result.map
                (fun prepared ->
                  ([ Sema.Aggregate_layout.Offset_directive prepared ], facts))
                (offset directive.aggregate_offset_expression)
          | Frontend.Ast.Anonymous_union_member anonymous_union ->
              Result.map
                (fun (union_items, facts) ->
                  ( [
                      Sema.Aggregate_layout.Anonymous_union
                        {
                          union_origin =
                            origin anonymous_union.anonymous_union_location;
                          union_items;
                        };
                    ],
                    facts ))
                (member_items ~offset ~dimension path
                   anonymous_union.anonymous_union_members facts)
          | Frontend.Ast.Empty_aggregate_member location ->
              Ok
                ([ Sema.Aggregate_layout.Empty_member (origin location) ], facts)
        in
        Result.bind built (fun (items, facts) ->
            loop (member_index + 1) (List.rev_append items items_rev) facts rest)
  in
  loop 0 [] facts members

let layout_kind = function
  | Sema.Aggregate_resolution.Class -> Sema.Aggregate_layout.Class
  | Sema.Aggregate_resolution.Union -> Sema.Aggregate_layout.Union

let validate_definition ~table ~scope event header aggregate
    (definition : Frontend.Ast.aggregate_definition) =
  let header_symbol = Sema.Aggregate_header_resolution.header_symbol header in
  let aggregate_symbol =
    Sema.Member_type_resolution.aggregate_symbol aggregate
  in
  let aggregate_scope = Sema.Member_type_resolution.aggregate_scope aggregate in
  if not (same_symbol header_symbol event.identity_symbol) then
    Error "aggregate layout header has the wrong aggregate identity"
  else if not (same_symbol aggregate_symbol event.declaration_symbol) then
    Error "aggregate layout members have the wrong declaration identity"
  else if not (same_symbol aggregate_symbol event.identity_symbol) then
    Error "aggregate layout members have the wrong aggregate identity"
  else if
    Sema.Aggregate_header_resolution.header_item_index header
    <> event.ast.item_index
    || Sema.Member_type_resolution.aggregate_item_index aggregate
       <> event.ast.item_index
  then Error "aggregate layout definition inputs have the wrong source order"
  else if
    Sema.Aggregate_header_resolution.header_aggregate_kind header
    <> event.ast.aggregate_kind
  then Error "aggregate layout header has the wrong aggregate kind"
  else if
    Sema.Aggregate_header_resolution.header_origin header
    <> origin definition.location
  then Error "aggregate layout header does not match the AST definition"
  else if not (Sema.Symbol_table.owns_scope table aggregate_scope) then
    Error "aggregate layout member scope belongs to a different symbol table"
  else if
    Sema.Symbol_table.scope_kind aggregate_scope <> Sema.Symbol_table.Aggregate
  then Error "aggregate layout members do not use an aggregate scope"
  else if
    match Sema.Symbol_table.parent aggregate_scope with
    | Some parent -> not (same_scope parent scope)
    | None -> true
  then Error "aggregate layout member scope does not belong to the module"
  else Ok ()

let aggregate_input ~offset ~dimension ~table ~scope event header aggregate
    definition =
  Result.bind
    (validate_definition ~table ~scope event header aggregate definition)
    (fun () ->
      let facts = Sema.Member_type_resolution.aggregate_members aggregate in
      Result.bind
        (member_items ~offset ~dimension [] definition.Frontend.Ast.members
           facts) (fun (items, remaining) ->
          if remaining <> [] then
            Error "aggregate layout has extra resolved members"
          else
            let aggregate_base =
              Option.map
                (fun base ->
                  {
                    Sema.Aggregate_layout.base_symbol =
                      Sema.Aggregate_header_resolution.base_symbol base;
                    base_origin =
                      Sema.Aggregate_header_resolution.base_origin base;
                  })
                (Sema.Aggregate_header_resolution.header_base header)
            in
            Ok
              {
                Sema.Aggregate_layout.aggregate_symbol = event.identity_symbol;
                aggregate_scope =
                  Sema.Member_type_resolution.aggregate_scope aggregate;
                aggregate_kind = layout_kind event.ast.aggregate_kind;
                aggregate_item_index = event.ast.item_index;
                aggregate_origin = origin definition.location;
                aggregate_base;
                aggregate_items = items;
              }))

let inputs ~offset ~dimension ~table ~scope events headers aggregates =
  let rec loop inputs_rev events headers aggregates =
    match events with
    | [] ->
        if headers = [] && aggregates = [] then Ok (List.rev inputs_rev)
        else Error "aggregate layout definitions do not match their inputs"
    | event :: rest -> (
        match event.ast.definition with
        | None -> loop inputs_rev rest headers aggregates
        | Some definition -> (
            match (headers, aggregates) with
            | header :: header_rest, aggregate :: aggregate_rest ->
                Result.bind
                  (aggregate_input ~offset ~dimension ~table ~scope event header
                     aggregate definition) (fun input ->
                    loop (input :: inputs_rev) rest header_rest aggregate_rest)
            | [], _ | _, [] ->
                Error "aggregate layout is missing a definition input"))
  in
  loop [] events headers aggregates

let layout ?offsets ?prepared ~table ~declarations ~aggregates ~headers ~members
    module_ =
  let dimension = dimension ~table ?prepared in
  let offset ast =
    match offsets with
    | None -> Ok (expression ast)
    | Some resolve ->
        Result.bind (resolve ast) (fun checked ->
            if
              Sema.Compiler_record.aggregate_offset_expression checked != ast
              || Sema.Compiler_record.aggregate_offset_table checked != table
            then Error "aggregate layout offset has another original expression"
            else
              Ok
                (Sema.Aggregate_layout.Integer_expression
                   {
                     value = Sema.Compiler_record.aggregate_offset_value checked;
                     origin = origin (Frontend.Ast.expression_location ast);
                   }))
  in
  let scope = Sema.Declaration_collection.scope declarations in
  let result =
    if not (Sema.Symbol_table.owns_scope table scope) then
      Error "aggregate layout declarations belong to a different symbol table"
    else if Sema.Symbol_table.scope_kind scope <> Sema.Symbol_table.Module then
      Error "aggregate layout declarations must belong to a module scope"
    else
      Result.bind (events ~table ~declarations ~aggregates module_)
        (fun events ->
          Result.bind
            (inputs ~offset ~dimension ~table ~scope events
               (Sema.Aggregate_header_resolution.headers headers)
               (Sema.Member_type_resolution.aggregates members))
            (fun inputs ->
              Sema.Aggregate_layout.layout ~table ~parent:scope inputs
              |> Result.map_error Sema.Aggregate_layout.error_to_string))
  in
  Result.map_error
    (fun message ->
      if String.starts_with ~prefix:"HCSEMA" message then message
      else "HCSEMA0001: " ^ message)
    result
