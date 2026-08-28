module Ast = Frontend.Ast
module Resolution = Sema.Break_resolution

let origin_of_location (location : Ast.location) =
  Sema.Symbol.Source_location
    {
      span = location.span;
      source_segments = location.source_segments;
      generated_from = location.generated_from;
      defined_at = location.defined_at;
    }

type state = {
  regions_rev : Resolution.region_input list;
  breaks_rev : Resolution.break_input list;
  next_region : int;
  next_break : int;
}

let empty_state =
  { regions_rev = []; breaks_rev = []; next_region = 0; next_break = 0 }

let add_region state kind location =
  if state.next_region = Int.max_int then
    Error "semantic break region identity space is exhausted"
  else
    let region_index = state.next_region in
    let origin = origin_of_location location in
    match Resolution.make_region ~region_index ~kind ~origin with
    | Error _ as error -> error
    | Ok region ->
        Ok
          ( {
              state with
              regions_rev = region :: state.regions_rev;
              next_region = region_index + 1;
            },
            region_index )

let make_input occurrence_index origin target_region_index =
  Resolution.make_break ~occurrence_index ~origin ~target_region_index

let block_body (block : Ast.block_statement) = block.block_statements

let targetless_message =
  "break appears without an enclosing loop or switch region"

let add_break state target (statement : Ast.break_statement) =
  match target with
  | None -> Error targetless_message
  | Some target_region_index -> (
      if state.next_break = Int.max_int then
        Error "semantic break occurrence identity space is exhausted"
      else
        let occurrence_index = state.next_break in
        let origin = origin_of_location statement.break_location in
        match make_input occurrence_index origin target_region_index with
        | Error _ as error -> error
        | Ok occurrence ->
            Ok
              {
                state with
                breaks_rev = occurrence :: state.breaks_rev;
                next_break = occurrence_index + 1;
              })

let rec statement state target = function
  | Ast.Block_statement block ->
      let body = block_body block in
      statements state target body
  | Ast.Break_statement break -> add_break state target break
  | Ast.Do_while_statement do_while -> (
      let location = do_while.do_while_location in
      match add_region state Resolution.Do_while_region location with
      | Error _ as error -> error
      | Ok (state, region) ->
          let target = Some region in
          statement state target do_while.do_body)
  | Ast.For_statement for_ -> (
      let location = for_.for_location in
      match add_region state Resolution.For_body_region location with
      | Error _ as error -> error
      | Ok (state, region) -> (
          match statement state None for_.for_initializer with
          | Error _ as error -> error
          | Ok state -> (
              match for_.for_update with
              | Some update -> (
                  match statement state None update with
                  | Error _ as error -> error
                  | Ok state ->
                      let target = Some region in
                      statement state target for_.for_body)
              | None -> statement state (Some region) for_.for_body)))
  | Ast.If_statement if_ -> (
      match statement state target if_.if_then_branch with
      | Error _ as error -> error
      | Ok state -> (
          match if_.if_else_clause with
          | None -> Ok state
          | Some else_ -> statement state target else_.else_branch))
  | Ast.Lock_statement lock -> statement state target lock.lock_body
  | Ast.Sequence_statement sequence ->
      sequence_elements state target sequence.sequence_elements
  | Ast.Switch_statement switch -> (
      let location = switch.switch_location in
      match add_region state Resolution.Switch_region location with
      | Error _ as error -> error
      | Ok (state, region) ->
          switch_elements state (Some region) switch.switch_elements)
  | Ast.Try_catch_statement try_catch -> (
      match statement state target try_catch.try_body with
      | Error _ as error -> error
      | Ok state -> statement state target try_catch.catch_body)
  | Ast.While_statement while_ -> (
      let location = while_.while_location in
      match add_region state Resolution.While_region location with
      | Error _ as error -> error
      | Ok (state, region) ->
          let target = Some region in
          statement state target while_.while_body)
  | Ast.Assembly_block_statement _
  | Ast.Empty_statement _
  | Ast.Expression_statement _
  | Ast.Goto_statement _
  | Ast.Implicit_output_statement _
  | Ast.Inline_assembly_statement _
  | Ast.Label_statement _
  | Ast.Local_declaration_statement _
  | Ast.No_warn_statement _
  | Ast.Return_statement _ -> Ok state

and statements state target values =
  let rec collect state = function
    | [] -> Ok state
    | value :: rest -> (
        match statement state target value with
        | Error _ as error -> error
        | Ok state -> collect state rest)
  in
  collect state values

and sequence_elements state target values =
  let rec collect state = function
    | [] -> Ok state
    | (value : Ast.statement_sequence_element) :: rest -> (
        match statement state target value.sequence_statement with
        | Error _ as error -> error
        | Ok state -> collect state rest)
  in
  collect state values

and switch_elements state target values =
  let rec collect state = function
    | [] -> Ok state
    | value :: rest -> (
        let current =
          match value with
          | Ast.Switch_statement_element statement_ ->
              statement state target statement_
          | Ast.Switch_subswitch_element subswitch -> (
              match
                add_region state Resolution.Subswitch_region
                  subswitch.subswitch_location
              with
              | Error _ as error -> error
              | Ok (state, region) ->
                  switch_elements state (Some region)
                    subswitch.subswitch_elements)
          | Ast.Switch_case_element _ -> Ok state
          | Ast.Switch_default_element _ -> Ok state
        in
        match current with
        | Error _ as error -> error
        | Ok state -> collect state rest)
  in
  collect state values

let body_facts = function
  | None -> Ok ([], [])
  | Some body -> (
      match statement empty_state None body with
      | Error _ as error -> error
      | Ok state ->
          let regions = List.rev state.regions_rev in
          let breaks = List.rev state.breaks_rev in
          Ok (regions, breaks))

type function_ast =
  | Prototype of Ast.function_prototype
  | Definition of Ast.function_definition

let ast_functions (module_ : Ast.module_) =
  module_.items
  |> List.mapi (fun item_index item -> (item_index, item))
  |> List.filter_map (function
    | item_index, Ast.Function_prototype prototype ->
        Some (item_index, Prototype prototype)
    | item_index, Ast.Function_definition definition ->
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
    Error "semantic break functions do not match the AST item order"
  else if not (String.equal (Sema.Symbol.name symbol) name.spelling) then
    Error "semantic break function does not match the AST name"
  else
    let ast_origin = origin_of_location name.location in
    if Sema.Symbol.origin symbol <> ast_origin then
      Error "semantic break function does not match the AST origin"
    else
      match body_facts body with
      | Error _ as error -> error
      | Ok (regions, breaks) ->
          let make = Resolution.make_function in
          make ~symbol ~scope ~item_index ~regions ~breaks

let function_facts functions module_ =
  let collected = Sema.Function_collection.functions functions in
  let ast = ast_functions module_ in
  let rec pair reversed collected ast =
    match (collected, ast) with
    | [], [] -> Ok (List.rev reversed)
    | function_ :: function_rest, ast_function :: ast_rest -> (
        match function_fact function_ ast_function with
        | Error _ as error -> error
        | Ok fact -> pair (fact :: reversed) function_rest ast_rest)
    | [], _ :: _ | _ :: _, [] ->
        Error "semantic break functions do not match the AST"
  in
  pair [] collected ast

let validate_top_level (module_ : Ast.module_) =
  let rec validate = function
    | [] -> Ok ()
    | Ast.Top_level_statement statement_ :: rest -> (
        match statement empty_state None statement_ with
        | Error _ as error -> error
        | Ok _ -> validate rest)
    | _ :: rest -> validate rest
  in
  validate module_.items

let resolve ~table ~functions module_ =
  match validate_top_level module_ with
  | Error _ as error -> error
  | Ok () -> (
      match function_facts functions module_ with
      | Error _ as error -> error
      | Ok facts -> Resolution.resolve ~table facts)
