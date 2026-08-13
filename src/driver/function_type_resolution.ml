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
          Error "semantic function type has inconsistent pointer depths"
        else if not (String.equal layer.spelling "*") then
          Error "semantic function type has an invalid pointer spelling"
        else validate (expected + 1) rest
  in
  validate 1 pointer_layers

let pointer_origins pointer_layers =
  List.map
    (fun (layer : Frontend.Ast.pointer_layer) -> origin layer.location)
    pointer_layers

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
                   "function type %S is not visible at this declaration"
                   identifier.spelling)
          | Some symbol -> Sema.Type.make_aggregate ~symbol ~pointer_depth))

let make_type_reference visible type_specifier pointer_layers =
  match resolve_type visible type_specifier pointer_layers with
  | Error _ as error -> error
  | Ok resolved_type ->
      Sema.Type_reference.make
        ~spelling:(Frontend.Ast.type_specifier_spelling type_specifier)
        ~spelling_origin:
          (origin (Frontend.Ast.type_specifier_location type_specifier))
        ~pointer_origins:(pointer_origins pointer_layers)
        ~resolved_type

type aggregate_ast = {
  aggregate_identifier : Frontend.Ast.identifier;
  aggregate_declaration_kind : Sema.Declaration_collection.declaration_kind;
  aggregate_kind : Sema.Aggregate_resolution.aggregate_kind;
  aggregate_item_index : int;
}

type aggregate_event = {
  aggregate_item_index : int;
  aggregate_name : string;
  aggregate_identity : Sema.Symbol.t;
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
            aggregate_identifier = forward.name;
            aggregate_declaration_kind =
              Sema.Declaration_collection.Aggregate_forward;
            aggregate_kind = aggregate_kind forward.aggregate_kind;
            aggregate_item_index = item_index;
          }
    | item_index, Frontend.Ast.Aggregate_definition definition ->
        Some
          {
            aggregate_identifier = definition.name;
            aggregate_declaration_kind =
              Sema.Declaration_collection.Aggregate_definition;
            aggregate_kind = aggregate_kind definition.aggregate_kind;
            aggregate_item_index = item_index;
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
      Error "semantic function types received a nonaggregate declaration"

let aggregate_event ~table ~scope entry resolved ast =
  let entry_kind = Sema.Declaration_collection.entry_kind entry in
  let entry_symbol = Sema.Declaration_collection.entry_symbol entry in
  let site = Sema.Aggregate_resolution.resolved_declaration_site resolved in
  let site_symbol = Sema.Aggregate_resolution.declaration_site_symbol site in
  let identity =
    Sema.Aggregate_resolution.resolved_declaration_identity_symbol resolved
  in
  if not (Sema.Symbol_table.owns_symbol table entry_symbol) then
    Error "semantic function type aggregate belongs to a different symbol table"
  else if not (Sema.Symbol_table.owns_symbol table site_symbol) then
    Error
      "semantic function type aggregate reconciliation belongs to a different \
       symbol table"
  else if not (Sema.Symbol_table.owns_symbol table identity) then
    Error
      "semantic function type aggregate identity belongs to a different symbol \
       table"
  else if entry_kind <> ast.aggregate_declaration_kind then
    Error "semantic function type aggregate does not match the AST kind"
  else if
    Sema.Declaration_collection.entry_item_index entry
    <> ast.aggregate_item_index
  then Error "semantic function type aggregate does not match the AST order"
  else if Sema.Declaration_collection.entry_declarator_index entry <> None then
    Error "semantic function type aggregate cannot have a declarator index"
  else if
    not
      (String.equal
         (Sema.Symbol.name entry_symbol)
         ast.aggregate_identifier.spelling)
  then Error "semantic function type aggregate does not match the AST name"
  else if
    Sema.Symbol.origin entry_symbol <> origin ast.aggregate_identifier.location
  then Error "semantic function type aggregate does not match the AST origin"
  else if not (same_symbol entry_symbol site_symbol) then
    Error
      "semantic function type aggregate reconciliation does not match the \
       declaration"
  else if
    Sema.Aggregate_resolution.declaration_site_item_index site
    <> ast.aggregate_item_index
  then
    Error "semantic function type aggregate reconciliation has the wrong order"
  else if
    Sema.Aggregate_resolution.declaration_site_aggregate_kind site
    <> ast.aggregate_kind
  then
    Error "semantic function type aggregate reconciliation has the wrong kind"
  else if
    not
      (Sema.Symbol.Scope_id.equal
         (Sema.Symbol.scope_id identity)
         (Sema.Symbol_table.scope_id scope))
  then Error "semantic function type aggregate identity is outside the module"
  else
    match expected_aggregate_role entry_kind with
    | Error _ as error -> error
    | Ok expected_role ->
        if Sema.Aggregate_resolution.declaration_site_kind site <> expected_role
        then
          Error
            "semantic function type aggregate reconciliation has the wrong role"
        else
          Ok
            {
              aggregate_item_index = ast.aggregate_item_index;
              aggregate_name = ast.aggregate_identifier.spelling;
              aggregate_identity = identity;
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
        Error "semantic function types do not match the aggregate declarations"
  in
  pair [] entries resolved ast

type function_ast = {
  function_declaration_kind : Sema.Declaration_collection.declaration_kind;
  function_item_index : int;
  function_name : Frontend.Ast.identifier;
  function_return_type : Frontend.Ast.type_specifier;
  function_return_pointers : Frontend.Ast.pointer_layer list;
  function_opening : Frontend.Ast.location;
  function_parameters : Frontend.Ast.function_parameter list;
  function_variadic : Frontend.Ast.variadic_marker option;
  function_closing : Frontend.Ast.location;
}

type function_event = {
  function_ast : function_ast;
  function_symbol : Sema.Symbol.t;
  function_scope : Sema.Symbol_table.scope;
  function_entries : Sema.Function_collection.entry list;
}

let function_ast (module_ : Frontend.Ast.module_) =
  module_.items
  |> List.mapi (fun item_index item -> (item_index, item))
  |> List.filter_map (function
    | item_index, Frontend.Ast.Function_prototype prototype ->
        Some
          {
            function_declaration_kind =
              Sema.Declaration_collection.Function_prototype;
            function_item_index = item_index;
            function_name = prototype.name;
            function_return_type = prototype.return_type;
            function_return_pointers = prototype.return_pointer_layers;
            function_opening = prototype.opening_parenthesis;
            function_parameters = prototype.parameters;
            function_variadic = prototype.variadic;
            function_closing = prototype.closing_parenthesis;
          }
    | item_index, Frontend.Ast.Function_definition definition ->
        Some
          {
            function_declaration_kind =
              Sema.Declaration_collection.Function_definition;
            function_item_index = item_index;
            function_name = definition.name;
            function_return_type = definition.return_type;
            function_return_pointers = definition.return_pointer_layers;
            function_opening = definition.opening_parenthesis;
            function_parameters = definition.parameters;
            function_variadic = definition.variadic;
            function_closing = definition.closing_parenthesis;
          }
    | _ -> None)

let function_entries declarations =
  Sema.Declaration_collection.entries declarations
  |> List.filter (fun entry ->
      match Sema.Declaration_collection.entry_kind entry with
      | Sema.Declaration_collection.Function_prototype
      | Sema.Declaration_collection.Function_definition -> true
      | Sema.Declaration_collection.Aggregate_forward
      | Sema.Declaration_collection.Aggregate_definition
      | Sema.Declaration_collection.Aggregate_attached_global
      | Sema.Declaration_collection.Global_variable -> false)

let validate_function_event ~table ~scope declaration collected ast =
  let symbol = Sema.Declaration_collection.entry_symbol declaration in
  let collected_symbol = Sema.Function_collection.function_symbol collected in
  let collected_scope = Sema.Function_collection.function_scope collected in
  if not (Sema.Symbol_table.owns_symbol table symbol) then
    Error "semantic function declaration belongs to a different symbol table"
  else if not (Sema.Symbol_table.owns_symbol table collected_symbol) then
    Error "semantic function collection belongs to a different symbol table"
  else if not (Sema.Symbol_table.owns_scope table collected_scope) then
    Error "semantic function scope belongs to a different symbol table"
  else if
    Sema.Declaration_collection.entry_kind declaration
    <> ast.function_declaration_kind
  then Error "semantic function type declaration does not match the AST kind"
  else if
    Sema.Declaration_collection.entry_item_index declaration
    <> ast.function_item_index
  then Error "semantic function type declaration does not match the AST order"
  else if Sema.Declaration_collection.entry_declarator_index declaration <> None
  then Error "semantic function type declaration cannot have a declarator index"
  else if
    not (String.equal (Sema.Symbol.name symbol) ast.function_name.spelling)
  then Error "semantic function type declaration does not match the AST name"
  else if Sema.Symbol.origin symbol <> origin ast.function_name.location then
    Error "semantic function type declaration does not match the AST origin"
  else if not (same_symbol symbol collected_symbol) then
    Error "semantic function collection has the wrong declaration symbol"
  else if
    Sema.Function_collection.function_item_index collected
    <> ast.function_item_index
  then Error "semantic function collection has the wrong item order"
  else if
    Sema.Symbol_table.scope_kind collected_scope <> Sema.Symbol_table.Function
  then Error "semantic function collection does not use a function scope"
  else if
    match Sema.Symbol_table.parent collected_scope with
    | Some parent -> not (same_scope parent scope)
    | None -> true
  then Error "semantic function collection does not belong to the module"
  else
    Ok
      {
        function_ast = ast;
        function_symbol = symbol;
        function_scope = collected_scope;
        function_entries = Sema.Function_collection.function_entries collected;
      }

let function_events ~table ~declarations ~functions module_ =
  let scope = Sema.Declaration_collection.scope declarations in
  let declarations = function_entries declarations in
  let collected = Sema.Function_collection.functions functions in
  let ast = function_ast module_ in
  let rec pair events_rev declarations collected ast =
    match (declarations, collected, ast) with
    | [], [], [] -> Ok (List.rev events_rev)
    | ( declaration :: declaration_rest,
        collected :: collected_rest,
        ast :: ast_rest ) -> (
        match
          validate_function_event ~table ~scope declaration collected ast
        with
        | Error _ as error -> error
        | Ok event ->
            pair (event :: events_rev) declaration_rest collected_rest ast_rest)
    | [], _, _ | _, [], _ | _, _, [] ->
        Error "semantic function types do not match the function declarations"
  in
  pair [] declarations collected ast

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
  match parameter_facts 0 [] parameters with
  | Error _ as error -> error
  | Ok parameters ->
      Sema.Function_type_resolution.make_signature
        ~opening_origin:(origin opening) ~parameters
        ?variadic_origin:
          (Option.map
             (fun (marker : Frontend.Ast.variadic_marker) ->
               origin marker.location)
             variadic)
        ~closing_origin:(origin closing) ()

and parameter_fact visible index (parameter : Frontend.Ast.function_parameter) =
  match
    make_type_reference visible parameter.type_specifier
      parameter.pointer_layers
  with
  | Error _ as error -> error
  | Ok type_reference -> (
      let declarator_kind =
        match parameter.function_pointer with
        | None -> Ok Sema.Function_type_resolution.Object
        | Some pointer -> (
            match pointer_depth pointer.indirection_layers with
            | Error _ as error -> error
            | Ok _ -> (
                match
                  signature_fact visible
                    ~opening:pointer.signature_opening_parenthesis
                    pointer.signature_parameters pointer.signature_variadic
                    ~closing:pointer.signature_closing_parenthesis
                with
                | Error _ as error -> error
                | Ok signature ->
                    Result.map
                      (fun pointer ->
                        Sema.Function_type_resolution.Function_pointer pointer)
                      (Sema.Function_type_resolution.make_function_pointer
                         ~origin:(origin pointer.function_pointer_location)
                         ~opening_origin:
                           (origin pointer.declarator_opening_parenthesis)
                         ~indirection_origins:
                           (pointer_origins pointer.indirection_layers)
                         ~closing_origin:
                           (origin pointer.declarator_closing_parenthesis)
                         ~signature)))
      in
      match declarator_kind with
      | Error _ as error -> error
      | Ok declarator_kind ->
          Sema.Function_type_resolution.make_parameter ~index
            ~origin:(origin parameter.location)
            ?name:
              (Option.map
                 (fun (name : Frontend.Ast.identifier) -> name.spelling)
                 parameter.name)
            ?name_origin:
              (Option.map
                 (fun (name : Frontend.Ast.identifier) -> origin name.location)
                 parameter.name)
            ~type_reference ~declarator_kind
            ~default:(Option.map default_fact parameter.default)
            ?delimiter_origin:
              (Option.map
                 (fun (delimiter : Frontend.Ast.declaration_delimiter) ->
                   origin delimiter.location)
                 parameter.delimiter)
            ())

let parameter_entries entries =
  List.filter
    (fun entry ->
      match Sema.Function_collection.entry_kind entry with
      | Sema.Function_collection.Named_parameter
      | Sema.Function_collection.Variadic_argc
      | Sema.Function_collection.Variadic_argv -> true
      | Sema.Function_collection.Automatic_local
      | Sema.Function_collection.Static_local -> false)
    entries

let parameter_binding entry =
  match Sema.Function_collection.entry_parameter_index entry with
  | None -> Error "semantic function parameter has no signature slot"
  | Some parameter_index ->
      Sema.Function_type_resolution.make_parameter_binding ~parameter_index
        ~symbol:(Sema.Function_collection.entry_symbol entry)

let internal_i64 () =
  Sema.Type.make_primitive ~form:Sema.Type.Internal_storage
    ~primitive:Sema.Primitive_type.I64 ~pointer_depth:0

let variadic_binding kind entry shape =
  match Sema.Function_collection.entry_parameter_index entry with
  | None -> Error "semantic variadic binding has no signature slot"
  | Some parameter_index -> (
      match internal_i64 () with
      | Error _ as error -> error
      | Ok resolved_type ->
          Sema.Function_type_resolution.make_synthetic_binding kind
            ~symbol:(Sema.Function_collection.entry_symbol entry)
            ~parameter_index ~resolved_type ~shape)

let collected_bindings event =
  let entries = parameter_entries event.function_entries in
  let rec collect named_rev argc argv = function
    | [] -> Ok (List.rev named_rev, argc, argv)
    | entry :: rest -> (
        match Sema.Function_collection.entry_kind entry with
        | Sema.Function_collection.Named_parameter -> (
            match parameter_binding entry with
            | Error _ as error -> error
            | Ok binding -> collect (binding :: named_rev) argc argv rest)
        | Sema.Function_collection.Variadic_argc ->
            if Option.is_some argc then
              Error "semantic function collection repeats variadic argc"
            else collect named_rev (Some entry) argv rest
        | Sema.Function_collection.Variadic_argv ->
            if Option.is_some argv then
              Error "semantic function collection repeats variadic argv"
            else collect named_rev argc (Some entry) rest
        | Sema.Function_collection.Automatic_local
        | Sema.Function_collection.Static_local -> assert false)
  in
  collect [] None None entries

let variadic_bindings ast argc argv =
  match (ast.function_variadic, argc, argv) with
  | None, None, None -> Ok None
  | Some marker, Some argc_entry, Some argv_entry -> (
      match
        variadic_binding Sema.Function_type_resolution.Argc argc_entry
          Sema.Function_type_resolution.Scalar
      with
      | Error _ as error -> error
      | Ok argc -> (
          match
            variadic_binding Sema.Function_type_resolution.Argv argv_entry
              (Sema.Function_type_resolution.Array
                 { source_extent = None; compiler_placeholder_extent = 127 })
          with
          | Error _ as error -> error
          | Ok argv ->
              Result.map
                (fun bindings -> Some bindings)
                (Sema.Function_type_resolution.make_variadic_bindings
                   ~marker_origin:(origin marker.location) ~argc ~argv)))
  | None, Some _, _ | None, _, Some _ | Some _, None, _ | Some _, _, None ->
      Error "semantic function collection does not match the variadic marker"

let function_fact visible event =
  let ast = event.function_ast in
  match
    make_type_reference visible ast.function_return_type
      ast.function_return_pointers
  with
  | Error _ as error -> error
  | Ok return_type -> (
      match
        signature_fact visible ~opening:ast.function_opening
          ast.function_parameters ast.function_variadic
          ~closing:ast.function_closing
      with
      | Error _ as error -> error
      | Ok signature -> (
          match collected_bindings event with
          | Error _ as error -> error
          | Ok (parameter_bindings, argc, argv) -> (
              match variadic_bindings ast argc argv with
              | Error _ as error -> error
              | Ok variadic_bindings ->
                  Sema.Function_type_resolution.make_function
                    ~symbol:event.function_symbol ~scope:event.function_scope
                    ~item_index:ast.function_item_index ~return_type ~signature
                    ~parameter_bindings ~variadic_bindings)))

let resolve_events ~table ~scope aggregates functions =
  let rec resolve visible facts_rev aggregates functions =
    match (aggregates, functions) with
    | [], [] ->
        Sema.Function_type_resolution.resolve ~table ~parent:scope
          (List.rev facts_rev)
    | aggregate :: aggregate_rest, [] ->
        let visible =
          String_map.add aggregate.aggregate_name aggregate.aggregate_identity
            visible
        in
        resolve visible facts_rev aggregate_rest []
    | [], function_ :: function_rest -> (
        match function_fact visible function_ with
        | Error _ as error -> error
        | Ok fact -> resolve visible (fact :: facts_rev) [] function_rest)
    | aggregate :: aggregate_rest, function_ :: function_rest -> (
        if
          aggregate.aggregate_item_index
          < function_.function_ast.function_item_index
        then
          let visible =
            String_map.add aggregate.aggregate_name aggregate.aggregate_identity
              visible
          in
          resolve visible facts_rev aggregate_rest functions
        else if
          aggregate.aggregate_item_index
          = function_.function_ast.function_item_index
        then Error "aggregate and function declarations share one module item"
        else
          match function_fact visible function_ with
          | Error _ as error -> error
          | Ok fact ->
              resolve visible (fact :: facts_rev) aggregates function_rest)
  in
  resolve String_map.empty [] aggregates functions

let resolve ~table ~declarations ~aggregates ~functions module_ =
  let scope = Sema.Declaration_collection.scope declarations in
  if not (Sema.Symbol_table.owns_scope table scope) then
    Error "semantic function type module belongs to a different symbol table"
  else if Sema.Symbol_table.scope_kind scope <> Sema.Symbol_table.Module then
    Error "semantic function types require a module declaration collection"
  else
    match aggregate_events ~table ~declarations ~aggregates module_ with
    | Error _ as error -> error
    | Ok aggregates -> (
        match function_events ~table ~declarations ~functions module_ with
        | Error _ as error -> error
        | Ok functions -> resolve_events ~table ~scope aggregates functions)
