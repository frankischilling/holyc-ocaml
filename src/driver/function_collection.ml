let origin_of_location (location : Frontend.Ast.location) =
  Sema.Symbol.Source_location
    {
      span = location.span;
      source_segments = location.source_segments;
      generated_from = location.generated_from;
      defined_at = location.defined_at;
    }

let origin (identifier : Frontend.Ast.identifier) =
  origin_of_location identifier.location

let named_parameter parameter_index
    (parameter : Frontend.Ast.function_parameter) =
  match parameter.name with
  | None -> Ok None
  | Some name ->
      Result.map
        (fun binding -> Some binding)
        (Sema.Function_collection.make_named_parameter ~name:name.spelling
           ~origin:(origin name) ~parameter_index)

let fixed_parameters parameters =
  let rec collect parameter_index bindings_rev = function
    | [] -> Ok (List.rev bindings_rev)
    | parameter :: rest -> (
        match named_parameter parameter_index parameter with
        | Error _ as error -> error
        | Ok None -> collect (parameter_index + 1) bindings_rev rest
        | Ok (Some binding) ->
            collect (parameter_index + 1) (binding :: bindings_rev) rest)
  in
  collect 0 [] parameters

let variadic_parameters fixed_count = function
  | None -> Ok []
  | Some (variadic : Frontend.Ast.variadic_marker) -> (
      let origin = origin_of_location variadic.location in
      match
        Sema.Function_collection.make_variadic_parameter
          Sema.Function_collection.Argc ~origin ~parameter_index:fixed_count
      with
      | Error _ as error -> error
      | Ok argc ->
          Result.map
            (fun argv -> [ argc; argv ])
            (Sema.Function_collection.make_variadic_parameter
               Sema.Function_collection.Argv ~origin
               ~parameter_index:(fixed_count + 1)))

let parameters fixed variadic =
  match fixed_parameters fixed with
  | Error _ as error -> error
  | Ok fixed_bindings ->
      Result.map
        (fun variadic_bindings -> fixed_bindings @ variadic_bindings)
        (variadic_parameters (List.length fixed) variadic)

let local_storage = function
  | Frontend.Ast.Automatic_local -> Sema.Function_collection.Automatic
  | Frontend.Ast.Static_local -> Sema.Function_collection.Static

let local_facts declaration_index (declaration : Frontend.Ast.local_declaration)
    =
  let storage = local_storage declaration.local_storage in
  let rec collect declarator_index bindings_rev = function
    | [] -> Ok (List.rev bindings_rev)
    | (declarator : Frontend.Ast.local_declarator) :: rest -> (
        match
          Sema.Function_collection.make_local
            ~name:declarator.local_name.spelling
            ~origin:(origin declarator.local_name)
            ~storage ~declaration_index ~declarator_index
        with
        | Error _ as error -> error
        | Ok binding ->
            collect (declarator_index + 1) (binding :: bindings_rev) rest)
  in
  collect 0 [] declaration.local_declarators

let append_results first second =
  match first with
  | Error _ as error -> error
  | Ok (first_bindings, next_index) -> (
      match second next_index with
      | Error _ as error -> error
      | Ok (second_bindings, next_index) ->
          Ok (first_bindings @ second_bindings, next_index))

let rec statement_facts declaration_index = function
  | Frontend.Ast.Local_declaration_statement declaration ->
      Result.map
        (fun bindings -> (bindings, declaration_index + 1))
        (local_facts declaration_index declaration)
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
  | Frontend.Ast.Return_statement _ -> Ok ([], declaration_index)

and statements_facts declaration_index statements =
  let rec collect bindings_rev declaration_index = function
    | [] -> Ok (List.rev bindings_rev |> List.concat, declaration_index)
    | statement :: rest -> (
        match statement_facts declaration_index statement with
        | Error _ as error -> error
        | Ok (bindings, next_index) ->
            collect (bindings :: bindings_rev) next_index rest)
  in
  collect [] declaration_index statements

and sequence_facts declaration_index elements =
  elements
  |> List.map (fun (element : Frontend.Ast.statement_sequence_element) ->
      element.sequence_statement)
  |> statements_facts declaration_index

and switch_facts declaration_index elements =
  let rec collect bindings_rev declaration_index = function
    | [] -> Ok (List.rev bindings_rev |> List.concat, declaration_index)
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
        | Ok (bindings, next_index) ->
            collect (bindings :: bindings_rev) next_index rest)
  in
  collect [] declaration_index elements

type function_ast =
  | Prototype of Frontend.Ast.function_prototype
  | Definition of Frontend.Ast.function_definition

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

let functions (module_ : Frontend.Ast.module_) =
  module_.items
  |> List.mapi (fun item_index item -> (item_index, item))
  |> List.filter_map (function
    | item_index, Frontend.Ast.Function_prototype prototype ->
        Some (item_index, Prototype prototype)
    | item_index, Frontend.Ast.Function_definition definition ->
        Some (item_index, Definition definition)
    | _ -> None)

let function_header = function
  | Prototype prototype ->
      (prototype.name, prototype.parameters, prototype.variadic, None)
  | Definition definition ->
      ( definition.name,
        definition.parameters,
        definition.variadic,
        definition.body )

let function_fact entry (item_index, function_ast) =
  let entry_item_index = Sema.Declaration_collection.entry_item_index entry in
  let symbol = Sema.Declaration_collection.entry_symbol entry in
  let name, fixed_parameters, variadic, body = function_header function_ast in
  if entry_item_index <> item_index then
    Error "semantic function declaration does not match the AST item order"
  else if not (String.equal (Sema.Symbol.name symbol) name.spelling) then
    Error "semantic function declaration does not match the AST name"
  else if Sema.Symbol.origin symbol <> origin name then
    Error "semantic function declaration does not match the AST origin"
  else
    match parameters fixed_parameters variadic with
    | Error _ as error -> error
    | Ok parameters -> (
        let locals =
          match body with
          | None -> Ok ([], 0)
          | Some statement -> statement_facts 0 statement
        in
        match locals with
        | Error _ as error -> error
        | Ok (locals, _) ->
            Sema.Function_collection.make_function ~symbol ~item_index
              (parameters @ locals))

let function_facts declarations module_ =
  let entries = function_entries declarations in
  let functions = functions module_ in
  let rec pair facts_rev entries functions =
    match (entries, functions) with
    | [], [] -> Ok (List.rev facts_rev)
    | entry :: entry_rest, function_ :: function_rest -> (
        match function_fact entry function_ with
        | Error _ as error -> error
        | Ok fact -> pair (fact :: facts_rev) entry_rest function_rest)
    | [], _ :: _ | _ :: _, [] ->
        Error "semantic function declarations do not match the AST"
  in
  pair [] entries functions

let collect ~table ~declarations module_ =
  match function_facts declarations module_ with
  | Error _ as error -> error
  | Ok facts ->
      Sema.Function_collection.collect ~table
        ~parent:(Sema.Declaration_collection.scope declarations)
        facts
