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

module Int_map = Map.Make (Int)
module Span_map = Map.Make (Common.Span)

type source_occurrence =
  | Language_statement of Frontend.Ast.statement
  | Assembly_label of Frontend.Ast.assembly_label

type collected_occurrence = {
  semantic : Sema.Label_resolution.occurrence;
  source : source_occurrence;
  name : string;
  kind : Sema.Label_resolution.occurrence_kind;
  origin : Sema.Symbol.origin;
  statement_origin : Sema.Symbol.origin;
  index : int;
}

type occurrence_state = {
  occurrences_rev : collected_occurrence list;
  next_index : int;
}

let empty_occurrences = { occurrences_rev = []; next_index = 0 }

let add_occurrence ~source ~name ~kind ~origin ~statement_origin make state =
  if state.next_index = max_int then
    Error "semantic label occurrence identity space is exhausted"
  else
    match make state.next_index with
    | Error _ as error -> error
    | Ok occurrence ->
        Ok
          {
            occurrences_rev =
              {
                semantic = occurrence;
                source;
                name;
                kind;
                origin;
                statement_origin;
                index = state.next_index;
              }
              :: state.occurrences_rev;
            next_index = state.next_index + 1;
          }

let add_goto state source (statement : Frontend.Ast.goto_statement) =
  let name = statement.goto_target.spelling in
  let origin = origin statement.goto_target in
  let statement_origin = origin_of_location statement.goto_location in
  add_occurrence ~source:(Language_statement source) ~name
    ~kind:Sema.Label_resolution.Goto_reference ~origin ~statement_origin
    (fun occurrence_index ->
      Sema.Label_resolution.make_goto_with_statement ~statement_origin ~name
        ~origin ~occurrence_index)
    state

let add_language_label state source (statement : Frontend.Ast.label_statement) =
  let name = statement.label_name.spelling in
  let origin = origin statement.label_name in
  let kind =
    Sema.Label_resolution.Definition Sema.Label_resolution.Language_label
  in
  let statement_origin = origin_of_location statement.label_location in
  add_occurrence ~source:(Language_statement source) ~name ~kind ~origin
    ~statement_origin
    (fun occurrence_index ->
      Sema.Label_resolution.make_definition_with_statement ~statement_origin
        ~name ~definition_kind:Sema.Label_resolution.Language_label ~origin
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
  let name = label.assembly_label_name.spelling in
  let origin = origin label.assembly_label_name in
  let statement_origin = origin_of_location label.assembly_label_location in
  let definition_kind = assembly_definition_kind label.assembly_label_kind in
  let kind = Sema.Label_resolution.Definition definition_kind in
  add_occurrence ~source:(Assembly_label label) ~name ~kind ~origin
    ~statement_origin
    (fun occurrence_index ->
      Sema.Label_resolution.make_definition_with_statement ~statement_origin
        ~name ~definition_kind ~origin ~occurrence_index)
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

let rec statement_occurrences state source =
  match source with
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
  | Frontend.Ast.Goto_statement statement -> add_goto state source statement
  | Frontend.Ast.If_statement if_ -> (
      match statement_occurrences state if_.if_then_branch with
      | Error _ as error -> error
      | Ok state -> (
          match if_.if_else_clause with
          | None -> Ok state
          | Some else_ -> statement_occurrences state else_.else_branch))
  | Frontend.Ast.Label_statement statement ->
      add_language_label state source statement
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

let collected_occurrences = function
  | None -> Ok []
  | Some body ->
      Result.map
        (fun state -> List.rev state.occurrences_rev)
        (statement_occurrences empty_occurrences body)

type outside_kind =
  | Outside_goto of string * Common.Span.t
  | Outside_label of string * Common.Span.t

let rec outside_statement = function
  | Frontend.Ast.Goto_statement statement ->
      Some
        (Outside_goto
           (statement.goto_target.spelling, statement.goto_location.span))
  | Frontend.Ast.Label_statement statement ->
      Some
        (Outside_label
           (statement.label_name.spelling, statement.label_location.span))
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
        | Some (Outside_goto (name, _)) ->
            Error
              (Printf.sprintf "goto target %S appears outside a function body"
                 name)
        | Some (Outside_label (name, _)) ->
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

type collected_function = {
  fact : Sema.Label_resolution.function_labels;
  symbol : Sema.Symbol.t;
  scope : Sema.Symbol_table.scope;
  item_index : int;
  occurrences : collected_occurrence list;
}

let function_fact_with_sources collected (item_index, ast) =
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
    match collected_occurrences body with
    | Error _ as error -> error
    | Ok occurrences -> (
        let semantic =
          List.map (fun occurrence -> occurrence.semantic) occurrences
        in
        match
          Sema.Label_resolution.make_function ~symbol ~scope ~item_index
            semantic
        with
        | Error _ as error -> error
        | Ok fact -> Ok { fact; symbol; scope; item_index; occurrences })

let function_fact collected ast =
  Result.map
    (fun collected -> collected.fact)
    (function_fact_with_sources collected ast)

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

let collected_function_facts functions module_ =
  let collected = Sema.Function_collection.functions functions in
  let ast = ast_functions module_ in
  let rec pair facts_rev collected ast =
    match (collected, ast) with
    | [], [] -> Ok (List.rev facts_rev)
    | function_ :: function_rest, ast_function :: ast_rest -> (
        match function_fact_with_sources function_ ast_function with
        | Error _ as error -> error
        | Ok fact -> pair (fact :: facts_rev) function_rest ast_rest)
    | [], _ :: _ | _ :: _, [] ->
        Error "semantic label functions do not match the AST"
  in
  pair [] collected ast

type error = { message : string; span : Common.Span.t option }

type indexed_function = {
  resolved : Sema.Label_resolution.resolved_function;
  owner : Sema.Symbol.t;
  statements :
    (Frontend.Ast.statement * Sema.Label_resolution.resolved_occurrence) list
    Span_map.t;
}

type indexed = {
  resolution : Sema.Label_resolution.t;
  functions_by_owner : indexed_function Int_map.t;
}

let error ?span message = { message; span }
let error_message error = error.message
let error_span error = error.span
let semantic indexed = indexed.resolution

let source_occurrence_span occurrence =
  match occurrence.source with
  | Language_statement statement ->
      Some (Frontend.Ast.statement_location statement).span
  | Assembly_label label -> Some label.assembly_label_location.span

let assembly_kind = function
  | Sema.Label_resolution.Language_label -> false
  | Sema.Label_resolution.Assembly_global_label
  | Sema.Label_resolution.Assembly_exported_global_label
  | Sema.Label_resolution.Assembly_local_label -> true

let invalid_source_span occurrences =
  let module String_map = Map.Make (String) in
  let rec definitions map = function
    | [] -> Ok map
    | occurrence :: rest -> (
        match occurrence.kind with
        | Sema.Label_resolution.Goto_reference -> definitions map rest
        | Sema.Label_resolution.Definition kind -> (
            match String_map.find_opt occurrence.name map with
            | None -> definitions (String_map.add occurrence.name kind map) rest
            | Some previous when assembly_kind previous && assembly_kind kind ->
                definitions map rest
            | Some _ -> Error (source_occurrence_span occurrence)))
  in
  match definitions String_map.empty occurrences with
  | Error span -> span
  | Ok definitions ->
      occurrences
      |> List.find_map (fun occurrence ->
          match occurrence.kind with
          | Sema.Label_resolution.Goto_reference
            when not (String_map.mem occurrence.name definitions) ->
              source_occurrence_span occurrence
          | Sema.Label_resolution.Goto_reference
          | Sema.Label_resolution.Definition _ -> None)

let semantic_failure_span functions =
  functions
  |> List.find_map (fun function_ -> invalid_source_span function_.occurrences)

let same_scope left right =
  Sema.Symbol.Scope_id.equal
    (Sema.Symbol_table.scope_id left)
    (Sema.Symbol_table.scope_id right)

let bind_occurrences source resolved =
  let rec bind statements sources resolved =
    match (sources, resolved) with
    | [], [] -> Ok statements
    | source :: source_rest, occurrence :: occurrence_rest -> (
        let symbol = Sema.Label_resolution.occurrence_symbol occurrence in
        if
          Sema.Label_resolution.occurrence_index occurrence <> source.index
          || Sema.Label_resolution.occurrence_kind occurrence <> source.kind
          || Sema.Label_resolution.occurrence_origin occurrence <> source.origin
          || Sema.Label_resolution.occurrence_statement_origin occurrence
             <> Some source.statement_origin
          || not (String.equal (Sema.Symbol.name symbol) source.name)
        then
          Error "resolved label occurrence does not match its source identity"
        else
          match source.source with
          | Assembly_label _ -> bind statements source_rest occurrence_rest
          | Language_statement statement ->
              let span = (Frontend.Ast.statement_location statement).span in
              let bucket =
                Option.value (Span_map.find_opt span statements) ~default:[]
              in
              if List.exists (fun (seen, _) -> seen == statement) bucket then
                Error "source label occurrence identity appears more than once"
              else
                bind
                  (Span_map.add span
                     ((statement, occurrence) :: bucket)
                     statements)
                  source_rest occurrence_rest)
    | [], _ :: _ | _ :: _, [] ->
        Error "resolved label occurrences do not cover their source identities"
  in
  bind Span_map.empty source.occurrences
    (Sema.Label_resolution.function_occurrences resolved)

let bind_functions collected resolution =
  let rec bind indexed_rev collected resolved =
    match (collected, resolved) with
    | [], [] -> Ok (List.rev indexed_rev)
    | source :: source_rest, function_ :: function_rest -> (
        let owner = Sema.Label_resolution.function_symbol function_ in
        let scope = Sema.Label_resolution.function_scope function_ in
        if
          (not (owner == source.symbol))
          || (not (same_scope scope source.scope))
          || Sema.Label_resolution.function_item_index function_
             <> source.item_index
        then
          Error "resolved label function does not match its exact source owner"
        else
          match bind_occurrences source function_ with
          | Error _ as error -> error
          | Ok statements ->
              bind
                ({ resolved = function_; owner; statements } :: indexed_rev)
                source_rest function_rest)
    | [], _ :: _ | _ :: _, [] ->
        Error "resolved label functions do not cover their source owners"
  in
  bind [] collected (Sema.Label_resolution.functions resolution)

let resolve_indexed ~table ~functions (module_ : Frontend.Ast.module_) =
  let top_level_error =
    module_.items
    |> List.find_map (function
      | Frontend.Ast.Top_level_statement statement -> (
          match outside_statement statement with
          | Some (Outside_goto (name, span)) ->
              Some
                (error ~span
                   (Printf.sprintf
                      "goto target %S appears outside a function body" name))
          | Some (Outside_label (name, span)) ->
              Some
                (error ~span
                   (Printf.sprintf "label %S appears outside a function body"
                      name))
          | None -> None)
      | _ -> None)
  in
  match top_level_error with
  | Some error -> Error error
  | None -> (
      match collected_function_facts functions module_ with
      | Error message -> Error (error ~span:module_.span message)
      | Ok collected -> (
          match
            Sema.Label_resolution.resolve ~table
              (List.map (fun function_ -> function_.fact) collected)
          with
          | Error message ->
              Error (error ?span:(semantic_failure_span collected) message)
          | Ok resolution -> (
              match bind_functions collected resolution with
              | Error message -> Error (error ~span:module_.span message)
              | Ok indexed_functions ->
                  let functions_by_owner =
                    List.fold_left
                      (fun map function_ ->
                        Int_map.add
                          (Sema.Symbol.Id.to_int
                             (Sema.Symbol.id function_.owner))
                          function_ map)
                      Int_map.empty indexed_functions
                  in
                  Ok { resolution; functions_by_owner })))

let function_for_symbol indexed symbol =
  let owner_id = Sema.Symbol.Id.to_int (Sema.Symbol.id symbol) in
  match Int_map.find_opt owner_id indexed.functions_by_owner with
  | Some function_ when function_.owner == symbol -> Some function_.resolved
  | Some _ | None -> None

let occurrence_for_statement indexed ~function_symbol statement =
  let owner_id = Sema.Symbol.Id.to_int (Sema.Symbol.id function_symbol) in
  match Int_map.find_opt owner_id indexed.functions_by_owner with
  | None -> Error "label occurrence owner is not part of this source resolution"
  | Some function_ when function_.owner != function_symbol ->
      Error "label occurrence owner is foreign to this source resolution"
  | Some function_ -> (
      let span = (Frontend.Ast.statement_location statement).span in
      let bucket =
        Option.value (Span_map.find_opt span function_.statements) ~default:[]
      in
      match List.filter (fun (source, _) -> source == statement) bucket with
      | [ (_, occurrence) ] -> Ok occurrence
      | [] -> Error "source goto or label has no exact resolved occurrence"
      | _ -> Error "source goto or label has duplicate resolved occurrences")

let resolve ~table ~functions module_ =
  match validate_top_level module_ with
  | Error _ as error -> error
  | Ok () -> (
      match function_facts functions module_ with
      | Error _ as error -> error
      | Ok facts -> Sema.Label_resolution.resolve ~table facts)
