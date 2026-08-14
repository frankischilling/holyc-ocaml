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

type occurrence_state = {
  occurrences_rev : Sema.Label_resolution.occurrence list;
  next_index : int;
}

let empty_occurrences = { occurrences_rev = []; next_index = 0 }

let add_occurrence make state =
  if state.next_index = max_int then
    Error "semantic label occurrence identity space is exhausted"
  else
    match make state.next_index with
    | Error _ as error -> error
    | Ok occurrence ->
        Ok
          {
            occurrences_rev = occurrence :: state.occurrences_rev;
            next_index = state.next_index + 1;
          }

let add_goto state (statement : Frontend.Ast.goto_statement) =
  add_occurrence
    (fun occurrence_index ->
      Sema.Label_resolution.make_goto ~name:statement.goto_target.spelling
        ~origin:(origin statement.goto_target)
        ~occurrence_index)
    state

let add_language_label state (statement : Frontend.Ast.label_statement) =
  add_occurrence
    (fun occurrence_index ->
      Sema.Label_resolution.make_definition ~name:statement.label_name.spelling
        ~definition_kind:Sema.Label_resolution.Language_label
        ~origin:(origin statement.label_name)
        ~occurrence_index)
    state

let assembly_definition_kind = function
  | Frontend.Ast.Assembly_global_label ->
      Sema.Label_resolution.Assembly_global_label
  | Frontend.Ast.Assembly_exported_global_label ->
      Sema.Label_resolution.Assembly_exported_global_label
  | Frontend.Ast.Assembly_local_label ->
      Sema.Label_resolution.Assembly_local_label

let add_assembly_label state (label : Frontend.Ast.assembly_label) =
  add_occurrence
    (fun occurrence_index ->
      Sema.Label_resolution.make_definition
        ~name:label.assembly_label_name.spelling
        ~definition_kind:(assembly_definition_kind label.assembly_label_kind)
        ~origin:(origin label.assembly_label_name)
        ~occurrence_index)
    state

let add_assembly_line state (line : Frontend.Ast.assembly_line) =
  let rec add state = function
    | [] -> Ok state
    | label :: rest -> (
        match add_assembly_label state label with
        | Error _ as error -> error
        | Ok state -> add state rest)
  in
  add state line.assembly_line_labels

let add_assembly_block state (block : Frontend.Ast.assembly_block_statement) =
  let rec add state = function
    | [] -> Ok state
    | line :: rest -> (
        match add_assembly_line state line with
        | Error _ as error -> error
        | Ok state -> add state rest)
  in
  add state block.assembly_lines

let rec statement_occurrences state = function
  | Frontend.Ast.Assembly_block_statement block ->
      add_assembly_block state block
  | Frontend.Ast.Block_statement block ->
      statements_occurrences state block.block_statements
  | Frontend.Ast.Do_while_statement do_while ->
      statement_occurrences state do_while.do_body
  | Frontend.Ast.For_statement for_ -> (
      match statement_occurrences state for_.for_initializer with
      | Error _ as error -> error
      | Ok state -> (
          match for_.for_update with
          | Some update -> (
              match statement_occurrences state update with
              | Error _ as error -> error
              | Ok state -> statement_occurrences state for_.for_body)
          | None -> statement_occurrences state for_.for_body))
  | Frontend.Ast.Goto_statement statement -> add_goto state statement
  | Frontend.Ast.If_statement if_ -> (
      match statement_occurrences state if_.if_then_branch with
      | Error _ as error -> error
      | Ok state -> (
          match if_.if_else_clause with
          | None -> Ok state
          | Some else_ -> statement_occurrences state else_.else_branch))
  | Frontend.Ast.Label_statement statement -> add_language_label state statement
  | Frontend.Ast.Lock_statement lock ->
      statement_occurrences state lock.lock_body
  | Frontend.Ast.Sequence_statement sequence ->
      sequence_occurrences state sequence.sequence_elements
  | Frontend.Ast.Switch_statement switch ->
      switch_occurrences state switch.switch_elements
  | Frontend.Ast.Try_catch_statement try_catch -> (
      match statement_occurrences state try_catch.try_body with
      | Error _ as error -> error
      | Ok state -> statement_occurrences state try_catch.catch_body)
  | Frontend.Ast.While_statement while_ ->
      statement_occurrences state while_.while_body
  | Frontend.Ast.Inline_assembly_statement _
  | Frontend.Ast.Break_statement _
  | Frontend.Ast.Empty_statement _
  | Frontend.Ast.Expression_statement _
  | Frontend.Ast.Implicit_output_statement _
  | Frontend.Ast.Local_declaration_statement _
  | Frontend.Ast.No_warn_statement _
  | Frontend.Ast.Return_statement _ -> Ok state

and statements_occurrences state statements =
  let rec collect state = function
    | [] -> Ok state
    | statement :: rest -> (
        match statement_occurrences state statement with
        | Error _ as error -> error
        | Ok state -> collect state rest)
  in
  collect state statements

and sequence_occurrences state elements =
  let rec collect state = function
    | [] -> Ok state
    | (element : Frontend.Ast.statement_sequence_element) :: rest -> (
        match statement_occurrences state element.sequence_statement with
        | Error _ as error -> error
        | Ok state -> collect state rest)
  in
  collect state elements

and switch_occurrences state elements =
  let rec collect state = function
    | [] -> Ok state
    | element :: rest -> (
        let current =
          match element with
          | Frontend.Ast.Switch_statement_element statement ->
              statement_occurrences state statement
          | Frontend.Ast.Switch_subswitch_element subswitch ->
              switch_occurrences state subswitch.subswitch_elements
          | Frontend.Ast.Switch_case_element _
          | Frontend.Ast.Switch_default_element _ -> Ok state
        in
        match current with
        | Error _ as error -> error
        | Ok state -> collect state rest)
  in
  collect state elements

let occurrences = function
  | None -> Ok []
  | Some body ->
      Result.map
        (fun state -> List.rev state.occurrences_rev)
        (statement_occurrences empty_occurrences body)

type outside_kind = Outside_goto of string | Outside_label of string

let rec outside_statement = function
  | Frontend.Ast.Goto_statement statement ->
      Some (Outside_goto statement.goto_target.spelling)
  | Frontend.Ast.Label_statement statement ->
      Some (Outside_label statement.label_name.spelling)
  | Frontend.Ast.Block_statement block ->
      outside_statements block.block_statements
  | Frontend.Ast.Do_while_statement do_while ->
      outside_statement do_while.do_body
  | Frontend.Ast.For_statement for_ -> (
      match outside_statement for_.for_initializer with
      | Some _ as occurrence -> occurrence
      | None -> (
          match Option.bind for_.for_update outside_statement with
          | Some _ as occurrence -> occurrence
          | None -> outside_statement for_.for_body))
  | Frontend.Ast.If_statement if_ -> (
      match outside_statement if_.if_then_branch with
      | Some _ as occurrence -> occurrence
      | None ->
          Option.bind if_.if_else_clause (fun else_ ->
              outside_statement else_.else_branch))
  | Frontend.Ast.Lock_statement lock -> outside_statement lock.lock_body
  | Frontend.Ast.Sequence_statement sequence ->
      outside_sequence sequence.sequence_elements
  | Frontend.Ast.Switch_statement switch ->
      outside_switch switch.switch_elements
  | Frontend.Ast.Try_catch_statement try_catch -> (
      match outside_statement try_catch.try_body with
      | Some _ as occurrence -> occurrence
      | None -> outside_statement try_catch.catch_body)
  | Frontend.Ast.While_statement while_ -> outside_statement while_.while_body
  | Frontend.Ast.Assembly_block_statement _
  | Frontend.Ast.Inline_assembly_statement _
  | Frontend.Ast.Break_statement _
  | Frontend.Ast.Empty_statement _
  | Frontend.Ast.Expression_statement _
  | Frontend.Ast.Implicit_output_statement _
  | Frontend.Ast.Local_declaration_statement _
  | Frontend.Ast.No_warn_statement _
  | Frontend.Ast.Return_statement _ -> None

and outside_statements = function
  | [] -> None
  | statement :: rest -> (
      match outside_statement statement with
      | Some _ as occurrence -> occurrence
      | None -> outside_statements rest)

and outside_sequence = function
  | [] -> None
  | (element : Frontend.Ast.statement_sequence_element) :: rest -> (
      match outside_statement element.sequence_statement with
      | Some _ as occurrence -> occurrence
      | None -> outside_sequence rest)

and outside_switch = function
  | [] -> None
  | element :: rest -> (
      let current =
        match element with
        | Frontend.Ast.Switch_statement_element statement ->
            outside_statement statement
        | Frontend.Ast.Switch_subswitch_element subswitch ->
            outside_switch subswitch.subswitch_elements
        | Frontend.Ast.Switch_case_element _
        | Frontend.Ast.Switch_default_element _ -> None
      in
      match current with
      | Some _ as occurrence -> occurrence
      | None -> outside_switch rest)

let validate_top_level (module_ : Frontend.Ast.module_) =
  let rec validate = function
    | [] -> Ok ()
    | Frontend.Ast.Top_level_statement statement :: rest -> (
        match outside_statement statement with
        | None -> validate rest
        | Some (Outside_goto name) ->
            Error
              (Printf.sprintf "goto target %S appears outside a function body"
                 name)
        | Some (Outside_label name) ->
            Error
              (Printf.sprintf "label %S appears outside a function body" name))
    | _ :: rest -> validate rest
  in
  validate module_.items

type function_ast =
  | Prototype of Frontend.Ast.function_prototype
  | Definition of Frontend.Ast.function_definition

let ast_functions (module_ : Frontend.Ast.module_) =
  module_.items
  |> List.mapi (fun item_index item -> (item_index, item))
  |> List.filter_map (function
    | item_index, Frontend.Ast.Function_prototype prototype ->
        Some (item_index, Prototype prototype)
    | item_index, Frontend.Ast.Function_definition definition ->
        Some (item_index, Definition definition)
    | _ -> None)

let function_header = function
  | Prototype prototype -> (prototype.name, None)
  | Definition definition -> (definition.name, definition.body)

let function_fact collected (item_index, ast) =
  let expected_item_index =
    Sema.Function_collection.function_item_index collected
  in
  let symbol = Sema.Function_collection.function_symbol collected in
  let scope = Sema.Function_collection.function_scope collected in
  let name, body = function_header ast in
  if expected_item_index <> item_index then
    Error "semantic label functions do not match the AST item order"
  else if not (String.equal (Sema.Symbol.name symbol) name.spelling) then
    Error "semantic label function does not match the AST name"
  else if Sema.Symbol.origin symbol <> origin name then
    Error "semantic label function does not match the AST origin"
  else
    match occurrences body with
    | Error _ as error -> error
    | Ok occurrences ->
        Sema.Label_resolution.make_function ~symbol ~scope ~item_index
          occurrences

let function_facts functions module_ =
  let collected = Sema.Function_collection.functions functions in
  let ast = ast_functions module_ in
  let rec pair facts_rev collected ast =
    match (collected, ast) with
    | [], [] -> Ok (List.rev facts_rev)
    | function_ :: function_rest, ast_function :: ast_rest -> (
        match function_fact function_ ast_function with
        | Error _ as error -> error
        | Ok fact -> pair (fact :: facts_rev) function_rest ast_rest)
    | [], _ :: _ | _ :: _, [] ->
        Error "semantic label functions do not match the AST"
  in
  pair [] collected ast

let resolve ~table ~functions module_ =
  match validate_top_level module_ with
  | Error _ as error -> error
  | Ok () -> (
      match function_facts functions module_ with
      | Error _ as error -> error
      | Ok facts -> Sema.Label_resolution.resolve ~table facts)
