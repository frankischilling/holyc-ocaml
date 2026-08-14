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
          Error "semantic local type has inconsistent pointer depths"
        else if not (String.equal layer.spelling "*") then
          Error "semantic local type has an invalid pointer spelling"
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
                   "local type %S is not visible at this function declaration"
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
      Error "semantic local types received a nonaggregate declaration"

let aggregate_event ~table ~scope entry resolved ast =
  let entry_kind = Sema.Declaration_collection.entry_kind entry in
  let entry_symbol = Sema.Declaration_collection.entry_symbol entry in
  let site = Sema.Aggregate_resolution.resolved_declaration_site resolved in
  let site_symbol = Sema.Aggregate_resolution.declaration_site_symbol site in
  let identity =
    Sema.Aggregate_resolution.resolved_declaration_identity_symbol resolved
  in
  if not (Sema.Symbol_table.owns_symbol table entry_symbol) then
    Error "semantic local type aggregate belongs to a different symbol table"
  else if not (Sema.Symbol_table.owns_symbol table site_symbol) then
    Error
      "semantic local type aggregate reconciliation belongs to a different \
       symbol table"
  else if not (Sema.Symbol_table.owns_symbol table identity) then
    Error
      "semantic local type aggregate identity belongs to a different symbol \
       table"
  else if entry_kind <> ast.aggregate_declaration_kind then
    Error "semantic local type aggregate does not match the AST kind"
  else if
    Sema.Declaration_collection.entry_item_index entry
    <> ast.aggregate_item_index
  then Error "semantic local type aggregate does not match the AST order"
  else if Sema.Declaration_collection.entry_declarator_index entry <> None then
    Error "semantic local type aggregate cannot have a declarator index"
  else if
    not
      (String.equal
         (Sema.Symbol.name entry_symbol)
         ast.aggregate_identifier.spelling)
  then Error "semantic local type aggregate does not match the AST name"
  else if
    Sema.Symbol.origin entry_symbol <> origin ast.aggregate_identifier.location
  then Error "semantic local type aggregate does not match the AST origin"
  else if not (same_symbol entry_symbol site_symbol) then
    Error
      "semantic local type aggregate reconciliation does not match the \
       declaration"
  else if
    Sema.Aggregate_resolution.declaration_site_item_index site
    <> ast.aggregate_item_index
  then Error "semantic local type aggregate reconciliation has the wrong order"
  else if
    Sema.Aggregate_resolution.declaration_site_aggregate_kind site
    <> ast.aggregate_kind
  then Error "semantic local type aggregate reconciliation has the wrong kind"
  else if
    not
      (Sema.Symbol.Scope_id.equal
         (Sema.Symbol.scope_id identity)
         (Sema.Symbol_table.scope_id scope))
  then Error "semantic local type aggregate identity is outside the module"
  else
    match expected_aggregate_role entry_kind with
    | Error _ as error -> error
    | Ok expected_role ->
        if Sema.Aggregate_resolution.declaration_site_kind site <> expected_role
        then
          Error
            "semantic local type aggregate reconciliation has the wrong role"
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
        Error "semantic local types do not match the aggregate declarations"
  in
  pair [] entries resolved ast

type function_ast = {
  function_declaration_kind : Sema.Declaration_collection.declaration_kind;
  function_item_index : int;
  function_name : Frontend.Ast.identifier;
  function_body : Frontend.Ast.statement option;
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
            function_body = None;
          }
    | item_index, Frontend.Ast.Function_definition definition ->
        Some
          {
            function_declaration_kind =
              Sema.Declaration_collection.Function_definition;
            function_item_index = item_index;
            function_name = definition.name;
            function_body = definition.body;
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
    Error "semantic local type function belongs to a different symbol table"
  else if not (Sema.Symbol_table.owns_symbol table collected_symbol) then
    Error "semantic local collection belongs to a different symbol table"
  else if not (Sema.Symbol_table.owns_scope table collected_scope) then
    Error "semantic local scope belongs to a different symbol table"
  else if
    Sema.Declaration_collection.entry_kind declaration
    <> ast.function_declaration_kind
  then Error "semantic local type declaration does not match the AST kind"
  else if
    Sema.Declaration_collection.entry_item_index declaration
    <> ast.function_item_index
  then Error "semantic local type declaration does not match the AST order"
  else if Sema.Declaration_collection.entry_declarator_index declaration <> None
  then Error "semantic local type function cannot have a declarator index"
  else if
    not (String.equal (Sema.Symbol.name symbol) ast.function_name.spelling)
  then Error "semantic local type declaration does not match the AST name"
  else if Sema.Symbol.origin symbol <> origin ast.function_name.location then
    Error "semantic local type declaration does not match the AST origin"
  else if not (same_symbol symbol collected_symbol) then
    Error "semantic local collection has the wrong function symbol"
  else if
    Sema.Function_collection.function_item_index collected
    <> ast.function_item_index
  then Error "semantic local collection has the wrong item order"
  else if
    Sema.Symbol_table.scope_kind collected_scope <> Sema.Symbol_table.Function
  then Error "semantic local collection does not use a function scope"
  else if
    match Sema.Symbol_table.parent collected_scope with
    | Some parent -> not (same_scope parent scope)
    | None -> true
  then Error "semantic local collection does not belong to the module"
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
        Error "semantic local types do not match the function declarations"
  in
  pair [] declarations collected ast

type local_ast = {
  declaration_index : int;
  declarator_index : int;
  declaration_origin : Sema.Symbol.origin;
  declarator_origin : Sema.Symbol.origin;
  storage : Sema.Local_type_resolution.storage;
  storage_origins : Sema.Symbol.origin list;
  type_specifier : Frontend.Ast.type_specifier;
  pointer_layers : Frontend.Ast.pointer_layer list;
  name : Frontend.Ast.identifier;
  register_qualifiers : Frontend.Ast.register_qualifier list;
  function_pointer : Frontend.Ast.function_pointer_declarator option;
  array_dimensions : Frontend.Ast.array_dimension list;
  initial_value : Frontend.Ast.local_initializer option;
  delimiter : Frontend.Ast.declaration_delimiter;
}

let storage_facts (declaration : Frontend.Ast.local_declaration) =
  match declaration.local_storage with
  | Frontend.Ast.Automatic_local ->
      if declaration.local_modifiers <> [] then
        Error "semantic automatic local has unexpected declaration modifiers"
      else Ok (Sema.Local_type_resolution.Automatic, [])
  | Frontend.Ast.Static_local ->
      let rec validate origins_rev = function
        | [] ->
            if origins_rev = [] then
              Error "semantic static local has no static source token"
            else Ok (Sema.Local_type_resolution.Static, List.rev origins_rev)
        | (modifier : Frontend.Ast.declaration_modifier) :: rest ->
            if modifier.kind <> Frontend.Ast.Static then
              Error "semantic static local has a nonstatic modifier"
            else if not (String.equal modifier.spelling "static") then
              Error "semantic static local has an invalid modifier spelling"
            else validate (origin modifier.location :: origins_rev) rest
      in
      validate [] declaration.local_modifiers

let local_declaration_facts declaration_index
    (declaration : Frontend.Ast.local_declaration) =
  match storage_facts declaration with
  | Error _ as error -> error
  | Ok (storage, storage_origins) ->
      Ok
        (declaration.local_declarators
        |> List.mapi
             (fun
               declarator_index (declarator : Frontend.Ast.local_declarator) ->
               {
                 declaration_index;
                 declarator_index;
                 declaration_origin =
                   origin declaration.local_declaration_location;
                 declarator_origin = origin declarator.local_declarator_location;
                 storage;
                 storage_origins;
                 type_specifier = declaration.local_type_specifier;
                 pointer_layers = declarator.local_pointer_layers;
                 name = declarator.local_name;
                 register_qualifiers = declarator.local_register_qualifiers;
                 function_pointer = declarator.local_function_pointer;
                 array_dimensions = declarator.local_array_dimensions;
                 initial_value = declarator.local_initializer;
                 delimiter = declarator.local_delimiter;
               }))

let append_results first second =
  match first with
  | Error _ as error -> error
  | Ok (first_locals, next_index) -> (
      match second next_index with
      | Error _ as error -> error
      | Ok (second_locals, next_index) ->
          Ok (first_locals @ second_locals, next_index))

let rec statement_facts declaration_index = function
  | Frontend.Ast.Local_declaration_statement declaration ->
      Result.map
        (fun locals -> (locals, declaration_index + 1))
        (local_declaration_facts declaration_index declaration)
  | Frontend.Ast.Block_statement block ->
      statements_facts declaration_index block.block_statements
  | Frontend.Ast.Do_while_statement do_while ->
      statement_facts declaration_index do_while.do_body
  | Frontend.Ast.For_statement for_ ->
      append_results (statement_facts declaration_index for_.for_initializer)
        (fun declaration_index ->
          append_results
            (match for_.for_update with
            | None -> Ok ([], declaration_index)
            | Some update -> statement_facts declaration_index update)
            (fun declaration_index ->
              statement_facts declaration_index for_.for_body))
  | Frontend.Ast.If_statement if_ ->
      append_results (statement_facts declaration_index if_.if_then_branch)
        (fun declaration_index ->
          match if_.if_else_clause with
          | None -> Ok ([], declaration_index)
          | Some else_ -> statement_facts declaration_index else_.else_branch)
  | Frontend.Ast.Lock_statement lock ->
      statement_facts declaration_index lock.lock_body
  | Frontend.Ast.Sequence_statement sequence ->
      sequence_facts declaration_index sequence.sequence_elements
  | Frontend.Ast.Switch_statement switch ->
      switch_facts declaration_index switch.switch_elements
  | Frontend.Ast.Try_catch_statement try_catch ->
      append_results (statement_facts declaration_index try_catch.try_body)
        (fun declaration_index ->
          statement_facts declaration_index try_catch.catch_body)
  | Frontend.Ast.While_statement while_ ->
      statement_facts declaration_index while_.while_body
  | Frontend.Ast.Assembly_block_statement _
  | Frontend.Ast.Inline_assembly_statement _
  | Frontend.Ast.Break_statement _
  | Frontend.Ast.Empty_statement _
  | Frontend.Ast.Expression_statement _
  | Frontend.Ast.Goto_statement _
  | Frontend.Ast.Implicit_output_statement _
  | Frontend.Ast.Label_statement _
  | Frontend.Ast.No_warn_statement _
  | Frontend.Ast.Return_statement _ -> Ok ([], declaration_index)

and statements_facts declaration_index statements =
  let rec collect locals_rev declaration_index = function
    | [] -> Ok (List.rev locals_rev |> List.concat, declaration_index)
    | statement :: rest -> (
        match statement_facts declaration_index statement with
        | Error _ as error -> error
        | Ok (locals, next_index) ->
            collect (locals :: locals_rev) next_index rest)
  in
  collect [] declaration_index statements

and sequence_facts declaration_index elements =
  elements
  |> List.map (fun (element : Frontend.Ast.statement_sequence_element) ->
      element.sequence_statement)
  |> statements_facts declaration_index

and switch_facts declaration_index elements =
  let rec collect locals_rev declaration_index = function
    | [] -> Ok (List.rev locals_rev |> List.concat, declaration_index)
    | element :: rest -> (
        let current =
          match element with
          | Frontend.Ast.Switch_statement_element statement ->
              statement_facts declaration_index statement
          | Frontend.Ast.Switch_subswitch_element subswitch ->
              switch_facts declaration_index subswitch.subswitch_elements
          | Frontend.Ast.Switch_case_element _
          | Frontend.Ast.Switch_default_element _ -> Ok ([], declaration_index)
        in
        match current with
        | Error _ as error -> error
        | Ok (locals, next_index) ->
            collect (locals :: locals_rev) next_index rest)
  in
  collect [] declaration_index elements

let local_asts event =
  match event.function_ast.function_body with
  | None -> Ok []
  | Some statement -> Result.map fst (statement_facts 0 statement)

let local_entries entries =
  List.filter
    (fun entry ->
      match Sema.Function_collection.entry_kind entry with
      | Sema.Function_collection.Automatic_local
      | Sema.Function_collection.Static_local -> true
      | Sema.Function_collection.Named_parameter
      | Sema.Function_collection.Variadic_argc
      | Sema.Function_collection.Variadic_argv -> false)
    entries

let validate_local_event entry ast =
  let symbol = Sema.Function_collection.entry_symbol entry in
  let expected_kind =
    match ast.storage with
    | Sema.Local_type_resolution.Automatic ->
        Sema.Function_collection.Automatic_local
    | Sema.Local_type_resolution.Static -> Sema.Function_collection.Static_local
  in
  if Sema.Function_collection.entry_kind entry <> expected_kind then
    Error "semantic local collection has the wrong storage"
  else if
    Sema.Function_collection.entry_local_declaration_index entry
    <> Some ast.declaration_index
  then Error "semantic local collection has the wrong declaration index"
  else if
    Sema.Function_collection.entry_declarator_index entry
    <> Some ast.declarator_index
  then Error "semantic local collection has the wrong declarator index"
  else if not (String.equal (Sema.Symbol.name symbol) ast.name.spelling) then
    Error "semantic local collection has the wrong name"
  else if Sema.Symbol.origin symbol <> origin ast.name.location then
    Error "semantic local collection has the wrong source origin"
  else Ok symbol

let local_events event =
  match local_asts event with
  | Error _ as error -> error
  | Ok asts ->
      let entries = local_entries event.function_entries in
      let rec pair events_rev entries asts =
        match (entries, asts) with
        | [], [] -> Ok (List.rev events_rev)
        | entry :: entry_rest, ast :: ast_rest -> (
            match validate_local_event entry ast with
            | Error _ as error -> error
            | Ok symbol ->
                pair ((symbol, ast) :: events_rev) entry_rest ast_rest)
        | [], _ :: _ | _ :: _, [] ->
            Error "semantic local collection does not match the local AST"
      in
      pair [] entries asts

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
          Result.bind
            (Register_request.of_list parameter.register_qualifiers)
            (fun register_requests ->
              Sema.Function_type_resolution.make_parameter ~index
                ~origin:(origin parameter.location) ~register_requests
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

let register_requests = Register_request.of_list

let array_dimension index (dimension : Frontend.Ast.array_dimension) =
  Sema.Local_type_resolution.make_array_dimension ~index
    ~origin:(origin dimension.location)
    ~opening_origin:(origin dimension.opening_bracket)
    ?expression_origin:
      (Option.map
         (fun expression ->
           origin (Frontend.Ast.expression_location expression))
         dimension.dimension_expression)
    ~closing_origin:(origin dimension.closing_bracket)
    ()

let array_dimensions dimensions =
  let rec collect index dimensions_rev = function
    | [] -> Ok (List.rev dimensions_rev)
    | dimension :: rest -> (
        match array_dimension index dimension with
        | Error _ as error -> error
        | Ok dimension -> collect (index + 1) (dimension :: dimensions_rev) rest
        )
  in
  collect 0 [] dimensions

let initializer_fact (initial_value : Frontend.Ast.local_initializer) =
  let kind =
    match initial_value.local_initializer_value with
    | Frontend.Ast.Scalar_initializer _ ->
        Sema.Local_type_resolution.Scalar_initializer
    | Frontend.Ast.Braced_initializer _ ->
        Sema.Local_type_resolution.Braced_initializer
  in
  Sema.Local_type_resolution.make_initializer ~kind
    ~origin:(origin initial_value.local_initializer_location)
    ~equals_origin:(origin initial_value.local_initializer_equals)
    ~value_origin:
      (origin
         (Frontend.Ast.initial_value_location
            initial_value.local_initializer_value))

let delimiter (delimiter : Frontend.Ast.declaration_delimiter) =
  let kind =
    match delimiter.kind with
    | Frontend.Ast.Comma -> Sema.Local_type_resolution.Comma
    | Frontend.Ast.Semicolon -> Sema.Local_type_resolution.Semicolon
  in
  Sema.Local_type_resolution.make_delimiter ~kind
    ~origin:(origin delimiter.location)

let declarator_kind visible = function
  | None -> Ok Sema.Local_type_resolution.Object
  | Some (pointer : Frontend.Ast.function_pointer_declarator) -> (
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
                  Sema.Local_type_resolution.Function_pointer pointer)
                (Sema.Function_type_resolution.make_function_pointer
                   ~origin:(origin pointer.function_pointer_location)
                   ~opening_origin:
                     (origin pointer.declarator_opening_parenthesis)
                   ~indirection_origins:
                     (pointer_origins pointer.indirection_layers)
                   ~closing_origin:
                     (origin pointer.declarator_closing_parenthesis)
                   ~signature)))

let local_fact visible (symbol, ast) =
  match make_type_reference visible ast.type_specifier ast.pointer_layers with
  | Error _ as error -> error
  | Ok type_reference -> (
      match register_requests ast.register_qualifiers with
      | Error _ as error -> error
      | Ok register_requests -> (
          match declarator_kind visible ast.function_pointer with
          | Error _ as error -> error
          | Ok declarator_kind -> (
              match array_dimensions ast.array_dimensions with
              | Error _ as error -> error
              | Ok array_dimensions ->
                  Sema.Local_type_resolution.make_local ~symbol
                    ~declaration_index:ast.declaration_index
                    ~declarator_index:ast.declarator_index
                    ~declaration_origin:ast.declaration_origin
                    ~declarator_origin:ast.declarator_origin
                    ~storage:ast.storage ~storage_origins:ast.storage_origins
                    ~type_reference ~register_requests ~declarator_kind
                    ~array_dimensions
                    ~initial_value:
                      (Option.map initializer_fact ast.initial_value)
                    ~delimiter:(delimiter ast.delimiter) ())))

let function_fact visible event =
  match local_events event with
  | Error _ as error -> error
  | Ok events ->
      let rec collect locals_rev = function
        | [] ->
            Sema.Local_type_resolution.make_function
              ~symbol:event.function_symbol ~scope:event.function_scope
              ~item_index:event.function_ast.function_item_index
              (List.rev locals_rev)
        | event :: rest -> (
            match local_fact visible event with
            | Error _ as error -> error
            | Ok local -> collect (local :: locals_rev) rest)
      in
      collect [] events

let resolve_events ~table ~scope aggregates functions =
  let rec resolve visible facts_rev aggregates functions =
    match (aggregates, functions) with
    | [], [] ->
        Sema.Local_type_resolution.resolve ~table ~parent:scope
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
    Error "semantic local type module belongs to a different symbol table"
  else if Sema.Symbol_table.scope_kind scope <> Sema.Symbol_table.Module then
    Error "semantic local types require a module declaration collection"
  else
    match aggregate_events ~table ~declarations ~aggregates module_ with
    | Error _ as error -> error
    | Ok aggregates -> (
        match function_events ~table ~declarations ~functions module_ with
        | Error _ as error -> error
        | Ok functions -> resolve_events ~table ~scope aggregates functions)
