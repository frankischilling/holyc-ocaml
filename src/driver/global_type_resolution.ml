module String_map = Map.Make (String)

let origin (location : Frontend.Ast.location) =
  Sema.Symbol.Source_location
    {
      span = location.span;
      source_segments = location.source_segments;
      generated_from = location.generated_from;
      defined_at = location.defined_at;
    }

let origin_at_span (location : Frontend.Ast.location) span =
  Sema.Symbol.Source_location
    {
      span;
      source_segments = location.source_segments;
      generated_from = location.generated_from;
      defined_at = location.defined_at;
    }

let same_symbol left right =
  Sema.Symbol.Id.equal (Sema.Symbol.id left) (Sema.Symbol.id right)

let pointer_depth pointer_layers =
  let rec validate expected = function
    | [] -> Ok (expected - 1)
    | (layer : Frontend.Ast.pointer_layer) :: rest ->
        if layer.depth <> expected then
          Error "semantic global type has inconsistent pointer depths"
        else if not (String.equal layer.spelling "*") then
          Error "semantic global type has an invalid pointer spelling"
        else validate (expected + 1) rest
  in
  validate 1 pointer_layers

let pointer_origins pointer_layers =
  List.map
    (fun (layer : Frontend.Ast.pointer_layer) -> origin layer.location)
    pointer_layers

type type_source =
  | Explicit_type of Frontend.Ast.type_specifier
  | Attached_aggregate of Frontend.Ast.identifier

let type_source_spelling = function
  | Explicit_type type_specifier ->
      Frontend.Ast.type_specifier_spelling type_specifier
  | Attached_aggregate identifier -> identifier.spelling

let type_source_origin = function
  | Explicit_type type_specifier ->
      origin (Frontend.Ast.type_specifier_location type_specifier)
  | Attached_aggregate identifier -> origin identifier.location

let resolve_type visible type_source pointer_layers =
  match pointer_depth pointer_layers with
  | Error _ as error -> error
  | Ok pointer_depth -> (
      match type_source with
      | Explicit_type (Frontend.Ast.Primitive_type_specifier primitive) ->
          Sema.Type.make_primitive ~form:Sema.Type.Public_spelling
            ~primitive:primitive.primitive ~pointer_depth
      | Explicit_type (Frontend.Ast.Internal_type_specifier internal) ->
          Sema.Type.make_primitive ~form:Sema.Type.Internal_storage
            ~primitive:internal.primitive ~pointer_depth
      | Explicit_type (Frontend.Ast.Named_type_specifier identifier)
      | Attached_aggregate identifier -> (
          match String_map.find_opt identifier.spelling visible with
          | None ->
              Error
                (Printf.sprintf
                   "global type %S is not visible at this declaration"
                   identifier.spelling)
          | Some symbol -> Sema.Type.make_aggregate ~symbol ~pointer_depth))

let make_type_reference visible type_source pointer_layers =
  match resolve_type visible type_source pointer_layers with
  | Error _ as error -> error
  | Ok resolved_type ->
      Sema.Type_reference.make
        ~spelling:(type_source_spelling type_source)
        ~spelling_origin:(type_source_origin type_source)
        ~pointer_origins:(pointer_origins pointer_layers)
        ~resolved_type

type aggregate_ast = {
  identifier : Frontend.Ast.identifier;
  declaration_kind : Sema.Declaration_collection.declaration_kind;
  aggregate_kind : Sema.Aggregate_resolution.aggregate_kind;
  item_index : int;
}

type aggregate_event = {
  item_index : int;
  name : string;
  identity : Sema.Symbol.t;
  declaration_kind : Sema.Declaration_collection.declaration_kind;
}

let aggregate_kind = function
  | Frontend.Ast.Class_aggregate -> Sema.Aggregate_resolution.Class
  | Frontend.Ast.Union_aggregate -> Sema.Aggregate_resolution.Union

let aggregate_ast (module_ : Frontend.Ast.module_) =
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
          }
    | item_index, Frontend.Ast.Aggregate_definition definition ->
        Some
          {
            identifier = definition.name;
            declaration_kind = Sema.Declaration_collection.Aggregate_definition;
            aggregate_kind = aggregate_kind definition.aggregate_kind;
            item_index;
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

let expected_aggregate_role = function
  | Sema.Declaration_collection.Aggregate_forward ->
      Ok Sema.Aggregate_resolution.Forward
  | Sema.Declaration_collection.Aggregate_definition ->
      Ok Sema.Aggregate_resolution.Definition
  | Sema.Declaration_collection.Aggregate_attached_global
  | Sema.Declaration_collection.Global_variable
  | Sema.Declaration_collection.Function_prototype
  | Sema.Declaration_collection.Function_definition ->
      Error "semantic global types received a nonaggregate declaration"

let aggregate_event ~table ~scope entry resolved (ast : aggregate_ast) =
  let entry_kind = Sema.Declaration_collection.entry_kind entry in
  let entry_symbol = Sema.Declaration_collection.entry_symbol entry in
  let site = Sema.Aggregate_resolution.resolved_declaration_site resolved in
  let site_symbol = Sema.Aggregate_resolution.declaration_site_symbol site in
  let identity =
    Sema.Aggregate_resolution.resolved_declaration_identity_symbol resolved
  in
  if not (Sema.Symbol_table.owns_symbol table entry_symbol) then
    Error "semantic global type aggregate belongs to a different symbol table"
  else if not (Sema.Symbol_table.owns_symbol table site_symbol) then
    Error
      "semantic global type aggregate reconciliation belongs to a different \
       symbol table"
  else if not (Sema.Symbol_table.owns_symbol table identity) then
    Error
      "semantic global type aggregate identity belongs to a different symbol \
       table"
  else if entry_kind <> ast.declaration_kind then
    Error "semantic global type aggregate does not match the AST kind"
  else if Sema.Declaration_collection.entry_item_index entry <> ast.item_index
  then Error "semantic global type aggregate does not match the AST order"
  else if Sema.Declaration_collection.entry_declarator_index entry <> None then
    Error "semantic global type aggregate cannot have a declarator index"
  else if
    not (String.equal (Sema.Symbol.name entry_symbol) ast.identifier.spelling)
  then Error "semantic global type aggregate does not match the AST name"
  else if Sema.Symbol.origin entry_symbol <> origin ast.identifier.location then
    Error "semantic global type aggregate does not match the AST origin"
  else if not (same_symbol entry_symbol site_symbol) then
    Error
      "semantic global type aggregate reconciliation does not match the \
       declaration"
  else if
    Sema.Aggregate_resolution.declaration_site_item_index site <> ast.item_index
  then Error "semantic global type aggregate reconciliation has the wrong order"
  else if
    Sema.Aggregate_resolution.declaration_site_aggregate_kind site
    <> ast.aggregate_kind
  then Error "semantic global type aggregate reconciliation has the wrong kind"
  else if
    not
      (Sema.Symbol.Scope_id.equal
         (Sema.Symbol.scope_id identity)
         (Sema.Symbol_table.scope_id scope))
  then Error "semantic global type aggregate identity is outside the module"
  else
    match expected_aggregate_role entry_kind with
    | Error _ as error -> error
    | Ok expected_role ->
        if Sema.Aggregate_resolution.declaration_site_kind site <> expected_role
        then
          Error
            "semantic global type aggregate reconciliation has the wrong role"
        else
          Ok
            {
              item_index = ast.item_index;
              name = ast.identifier.spelling;
              identity;
              declaration_kind = ast.declaration_kind;
            }

let aggregate_events ~table ~declarations ~aggregates module_ =
  let scope = Sema.Declaration_collection.scope declarations in
  let entries = aggregate_entries declarations in
  let resolved = Sema.Aggregate_resolution.declarations aggregates in
  let ast = aggregate_ast module_ in
  let rec pair events_rev entries resolved ast =
    match (entries, resolved, ast) with
    | [], [], [] -> Ok (List.rev events_rev)
    | entry :: entry_rest, resolved :: resolved_rest, ast :: ast_rest -> (
        match aggregate_event ~table ~scope entry resolved ast with
        | Error _ as error -> error
        | Ok event ->
            pair (event :: events_rev) entry_rest resolved_rest ast_rest)
    | [], _, _ | _, [], _ | _, _, [] ->
        Error "semantic global types do not match the aggregate declarations"
  in
  pair [] entries resolved ast

type ast_global = {
  declaration_kind : Sema.Declaration_collection.declaration_kind;
  item_index : int;
  declarator_index : int option;
  type_source : type_source;
  pointer_layers : Frontend.Ast.pointer_layer list;
  name : Frontend.Ast.identifier;
  function_pointer : Frontend.Ast.function_pointer_declarator option;
  array_dimensions : Frontend.Ast.array_dimension list;
  initial_value : Frontend.Ast.global_initializer option;
  delimiter_kind : Frontend.Ast.declaration_delimiter_kind;
  delimiter_origin : Sema.Symbol.origin;
  declarator_origin : Sema.Symbol.origin;
}

type global_event = { ast : ast_global; symbol : Sema.Symbol.t }

let declarator_ast ~declaration_kind ~item_index ~type_source
    ~trailing_semicolon ~last_declarator_index declarator_index
    (declarator : Frontend.Ast.global_declarator) =
  let delimiter_kind, delimiter_origin =
    match trailing_semicolon with
    | Some semicolon when declarator_index = last_declarator_index ->
        (Frontend.Ast.Semicolon, origin semicolon)
    | None | Some _ ->
        (declarator.delimiter.kind, origin declarator.delimiter.location)
  in
  {
    declaration_kind;
    item_index;
    declarator_index = Some declarator_index;
    type_source;
    pointer_layers = declarator.pointer_layers;
    name = declarator.name;
    function_pointer = declarator.function_pointer;
    array_dimensions = declarator.array_dimensions;
    initial_value = declarator.global_initial_value;
    delimiter_kind;
    delimiter_origin;
    declarator_origin = origin declarator.location;
  }

let global_ast (module_ : Frontend.Ast.module_) =
  module_.items
  |> List.mapi (fun item_index item ->
      match item with
      | Frontend.Ast.Global_variable variable ->
          [
            {
              declaration_kind = Sema.Declaration_collection.Global_variable;
              item_index;
              declarator_index = None;
              type_source = Explicit_type variable.type_specifier;
              pointer_layers = variable.pointer_layers;
              name = variable.name;
              function_pointer = None;
              array_dimensions = variable.array_dimensions;
              initial_value = None;
              delimiter_kind = Frontend.Ast.Semicolon;
              delimiter_origin =
                origin_at_span variable.location variable.semicolon;
              declarator_origin = origin variable.location;
            };
          ]
      | Frontend.Ast.Global_declaration declaration ->
          let last_declarator_index = List.length declaration.declarators - 1 in
          declaration.declarators
          |> List.mapi
               (declarator_ast
                  ~declaration_kind:Sema.Declaration_collection.Global_variable
                  ~item_index
                  ~type_source:(Explicit_type declaration.type_specifier)
                  ~trailing_semicolon:declaration.trailing_semicolon
                  ~last_declarator_index)
      | Frontend.Ast.Aggregate_definition definition ->
          let last_declarator_index =
            List.length definition.attached_declarators - 1
          in
          definition.attached_declarators
          |> List.mapi
               (declarator_ast
                  ~declaration_kind:
                    Sema.Declaration_collection.Aggregate_attached_global
                  ~item_index ~type_source:(Attached_aggregate definition.name)
                  ~trailing_semicolon:(Some definition.semicolon)
                  ~last_declarator_index)
      | Frontend.Ast.Aggregate_forward_declaration _
      | Frontend.Ast.Function_prototype _
      | Frontend.Ast.Function_definition _
      | Frontend.Ast.Top_level_statement _ -> [])
  |> List.concat

let global_entries declarations =
  Sema.Declaration_collection.entries declarations
  |> List.filter (fun entry ->
      match Sema.Declaration_collection.entry_kind entry with
      | Sema.Declaration_collection.Aggregate_attached_global
      | Sema.Declaration_collection.Global_variable -> true
      | Sema.Declaration_collection.Aggregate_forward
      | Sema.Declaration_collection.Aggregate_definition
      | Sema.Declaration_collection.Function_prototype
      | Sema.Declaration_collection.Function_definition -> false)

let validate_global_event ~table entry ast =
  let symbol = Sema.Declaration_collection.entry_symbol entry in
  if not (Sema.Symbol_table.owns_symbol table symbol) then
    Error "semantic global declaration belongs to a different symbol table"
  else if Sema.Declaration_collection.entry_kind entry <> ast.declaration_kind
  then Error "semantic global type declaration does not match the AST kind"
  else if Sema.Declaration_collection.entry_item_index entry <> ast.item_index
  then Error "semantic global type declaration does not match the AST order"
  else if
    Sema.Declaration_collection.entry_declarator_index entry
    <> ast.declarator_index
  then Error "semantic global type declaration has the wrong declarator index"
  else if not (String.equal (Sema.Symbol.name symbol) ast.name.spelling) then
    Error "semantic global type declaration does not match the AST name"
  else if Sema.Symbol.origin symbol <> origin ast.name.location then
    Error "semantic global type declaration does not match the AST origin"
  else Ok { ast; symbol }

let global_events ~table ~declarations module_ =
  let entries = global_entries declarations in
  let ast = global_ast module_ in
  let rec pair events_rev entries ast =
    match (entries, ast) with
    | [], [] -> Ok (List.rev events_rev)
    | entry :: entry_rest, ast :: ast_rest -> (
        match validate_global_event ~table entry ast with
        | Error _ as error -> error
        | Ok event -> pair (event :: events_rev) entry_rest ast_rest)
    | [], _ | _, [] ->
        Error "semantic global types do not match the global declarations"
  in
  pair [] entries ast

let default_fact (default : Frontend.Ast.parameter_default) =
  let default_origin = origin default.location in
  let equals_origin = origin default.equals in
  match default.value with
  | Frontend.Ast.Expression_default expression ->
      Sema.Function_type_resolution.Expression_default
        {
          origin = default_origin;
          equals_origin;
          expression_origin =
            origin (Frontend.Ast.expression_location expression);
          contains_string_literal =
            Expression_facts.contains_string_literal expression;
        }
  | Frontend.Ast.Lastclass_default lastclass ->
      Sema.Function_type_resolution.Lastclass_default
        {
          origin = default_origin;
          equals_origin;
          keyword_origin = origin lastclass.lastclass_location;
        }

let rec signature_fact visible ~opening parameters variadic ~closing =
  let rec parameter_facts index facts_rev = function
    | [] -> Ok (List.rev facts_rev)
    | (parameter : Frontend.Ast.function_parameter) :: rest -> (
        match parameter_fact visible index parameter with
        | Error _ as error -> error
        | Ok fact -> parameter_facts (index + 1) (fact :: facts_rev) rest)
  in
  Result.bind (parameter_facts 0 [] parameters) (fun parameters ->
      Result.bind
        (match variadic with
        | None -> Ok []
        | Some (marker : Frontend.Ast.variadic_marker) ->
            Register_request.of_list marker.register_qualifiers)
        (fun variadic_register_requests ->
          Sema.Function_type_resolution.make_signature
            ~opening_origin:(origin opening) ~parameters
            ?variadic_origin:
              (Option.map
                 (fun (marker : Frontend.Ast.variadic_marker) ->
                   origin marker.location)
                 variadic)
            ~variadic_register_requests ~closing_origin:(origin closing) ()))

and parameter_fact visible index (parameter : Frontend.Ast.function_parameter) =
  match
    make_type_reference visible (Explicit_type parameter.type_specifier)
      parameter.pointer_layers
  with
  | Error _ as error -> error
  | Ok type_reference -> (
      match declarator_kind_fact visible parameter.function_pointer with
      | Error _ as error -> error
      | Ok declarator_kind ->
          Result.bind (Register_request.of_list parameter.register_qualifiers)
            (fun register_requests ->
              Sema.Function_type_resolution.make_parameter ~index
                ~origin:(origin parameter.location)
                ~register_requests
                ?name:
                  (Option.map
                     (fun (name : Frontend.Ast.identifier) -> name.spelling)
                     parameter.name)
                ?name_origin:
                  (Option.map
                     (fun (name : Frontend.Ast.identifier) ->
                       origin name.location)
                     parameter.name)
                ~type_reference ~declarator_kind
                ~default:(Option.map default_fact parameter.default)
                ?delimiter_origin:
                  (Option.map
                     (fun (delimiter : Frontend.Ast.declaration_delimiter) ->
                       origin delimiter.location)
                     parameter.delimiter)
                ()))

and function_pointer_fact visible
    (pointer : Frontend.Ast.function_pointer_declarator) =
  match pointer_depth pointer.indirection_layers with
  | Error _ as error -> error
  | Ok _ -> (
      match
        signature_fact visible ~opening:pointer.signature_opening_parenthesis
          pointer.signature_parameters pointer.signature_variadic
          ~closing:pointer.signature_closing_parenthesis
      with
      | Error _ as error -> error
      | Ok signature ->
          Sema.Function_type_resolution.make_function_pointer
            ~origin:(origin pointer.function_pointer_location)
            ~opening_origin:(origin pointer.declarator_opening_parenthesis)
            ~indirection_origins:(pointer_origins pointer.indirection_layers)
            ~closing_origin:(origin pointer.declarator_closing_parenthesis)
            ~signature)

and declarator_kind_fact visible = function
  | None -> Ok Sema.Function_type_resolution.Object
  | Some pointer ->
      Result.map
        (fun pointer -> Sema.Function_type_resolution.Function_pointer pointer)
        (function_pointer_fact visible pointer)

let global_declarator_kind visible = function
  | None -> Ok Sema.Global_type_resolution.Object
  | Some pointer ->
      Result.map
        (fun pointer -> Sema.Global_type_resolution.Function_pointer pointer)
        (function_pointer_fact visible pointer)

let array_dimension_fact index (dimension : Frontend.Ast.array_dimension) =
  Sema.Global_type_resolution.make_array_dimension ~index
    ~origin:(origin dimension.location)
    ~opening_origin:(origin dimension.opening_bracket)
    ?expression_origin:
      (Option.map
         (fun expression ->
           origin (Frontend.Ast.expression_location expression))
         dimension.dimension_expression)
    ~closing_origin:(origin dimension.closing_bracket)
    ()

let array_dimension_facts dimensions =
  let rec collect index facts_rev = function
    | [] -> Ok (List.rev facts_rev)
    | dimension :: rest -> (
        match array_dimension_fact index dimension with
        | Error _ as error -> error
        | Ok fact -> collect (index + 1) (fact :: facts_rev) rest)
  in
  collect 0 [] dimensions

let initializer_fact (initial_value : Frontend.Ast.global_initializer) =
  let kind, value_origin =
    match initial_value.global_initializer_value with
    | Frontend.Ast.Scalar_initializer expression ->
        ( Sema.Global_type_resolution.Scalar_initializer,
          origin (Frontend.Ast.expression_location expression) )
    | Frontend.Ast.Braced_initializer braced ->
        ( Sema.Global_type_resolution.Braced_initializer,
          origin braced.initializer_location )
    | Frontend.Ast.Unbraced_array_initializer unbraced ->
        ( Sema.Global_type_resolution.Braced_initializer,
          origin unbraced.unbraced_initializer_location )
  in
  Sema.Global_type_resolution.make_initializer ~kind
    ~origin:(origin initial_value.global_initializer_location)
    ~equals_origin:(origin initial_value.global_initializer_equals)
    ~value_origin

let delimiter_fact kind origin =
  let kind =
    match kind with
    | Frontend.Ast.Comma -> Sema.Global_type_resolution.Comma
    | Frontend.Ast.Semicolon -> Sema.Global_type_resolution.Semicolon
  in
  Sema.Global_type_resolution.make_delimiter ~kind ~origin

let global_fact visible (event : global_event) =
  let ast = event.ast in
  match make_type_reference visible ast.type_source ast.pointer_layers with
  | Error _ as error -> error
  | Ok type_reference -> (
      match global_declarator_kind visible ast.function_pointer with
      | Error _ as error -> error
      | Ok declarator_kind -> (
          match array_dimension_facts ast.array_dimensions with
          | Error _ as error -> error
          | Ok array_dimensions ->
              Sema.Global_type_resolution.make_global ~symbol:event.symbol
                ~item_index:ast.item_index
                ?declarator_index:ast.declarator_index
                ~declarator_origin:ast.declarator_origin ~type_reference
                ~declarator_kind ~array_dimensions
                ~initial_value:(Option.map initializer_fact ast.initial_value)
                ~delimiter:
                  (delimiter_fact ast.delimiter_kind ast.delimiter_origin)
                ()))

let publish visible (aggregate : aggregate_event) =
  String_map.add aggregate.name aggregate.identity visible

let resolve_events ~table ~scope aggregates globals =
  let rec resolve visible facts_rev aggregates globals =
    match (aggregates, globals) with
    | [], [] ->
        Sema.Global_type_resolution.resolve ~table ~parent:scope
          (List.rev facts_rev)
    | aggregate :: aggregate_rest, [] ->
        resolve (publish visible aggregate) facts_rev aggregate_rest []
    | [], global :: global_rest -> (
        match global_fact visible global with
        | Error _ as error -> error
        | Ok fact -> resolve visible (fact :: facts_rev) [] global_rest)
    | aggregate :: aggregate_rest, global :: global_rest -> (
        if aggregate.item_index < global.ast.item_index then
          resolve (publish visible aggregate) facts_rev aggregate_rest globals
        else if aggregate.item_index = global.ast.item_index then
          if
            aggregate.declaration_kind
            <> Sema.Declaration_collection.Aggregate_definition
            || global.ast.declaration_kind
               <> Sema.Declaration_collection.Aggregate_attached_global
          then
            Error "only aggregate-attached globals can share an aggregate item"
          else
            let visible = publish visible aggregate in
            match global_fact visible global with
            | Error _ as error -> error
            | Ok fact ->
                resolve visible (fact :: facts_rev) aggregate_rest global_rest
        else
          match global_fact visible global with
          | Error _ as error -> error
          | Ok fact ->
              resolve visible (fact :: facts_rev) aggregates global_rest)
  in
  resolve String_map.empty [] aggregates globals

let resolve ~table ~declarations ~aggregates module_ =
  let scope = Sema.Declaration_collection.scope declarations in
  if not (Sema.Symbol_table.owns_scope table scope) then
    Error "semantic global type module belongs to a different symbol table"
  else if Sema.Symbol_table.scope_kind scope <> Sema.Symbol_table.Module then
    Error "semantic global types require a module declaration collection"
  else
    match aggregate_events ~table ~declarations ~aggregates module_ with
    | Error _ as error -> error
    | Ok aggregates -> (
        match global_events ~table ~declarations module_ with
        | Error _ as error -> error
        | Ok globals -> resolve_events ~table ~scope aggregates globals)
