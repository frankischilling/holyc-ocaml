let origin (location : Frontend.Ast.location) =
  Sema.Symbol.Source_location
    {
      span = location.span;
      source_segments = location.source_segments;
      generated_from = location.generated_from;
      defined_at = location.defined_at;
    }

type state = {
  events_rev : Sema.Global_dimension_binding.event list;
  next_index : int;
}

let empty_state next_index = { events_rev = []; next_index }

let add_identifier dimension_index state (identifier : Frontend.Ast.identifier)
    =
  if state.next_index = max_int then
    Error "global array extent occurrence identity space is exhausted"
  else
    match
      Sema.Global_dimension_binding.make_identifier ~name:identifier.spelling
        ~origin:(origin identifier.location)
        ~occurrence_index:state.next_index ~dimension_index
    with
    | Error _ as error -> error
    | Ok event ->
        Ok
          {
            events_rev = event :: state.events_rev;
            next_index = state.next_index + 1;
          }

let rec fold_result apply state = function
  | [] -> Ok state
  | value :: rest -> (
      match apply state value with
      | Error _ as error -> error
      | Ok state -> fold_result apply state rest)

let rec expression dimension_index state = function
  | Frontend.Ast.Identifier_expression identifier ->
      add_identifier dimension_index state identifier
  | Frontend.Ast.Parenthesized_expression grouped ->
      expression dimension_index state grouped.grouped_expression
  | Frontend.Ast.Prefix_expression prefix ->
      expression dimension_index state prefix.prefix_operand
  | Frontend.Ast.Postfix_expression postfix ->
      expression dimension_index state postfix.postfix_operand
  | Frontend.Ast.Postfix_cast_expression cast ->
      expression dimension_index state cast.cast_operand
  | Frontend.Ast.Binary_expression binary -> (
      match expression dimension_index state binary.binary_left with
      | Error _ as error -> error
      | Ok state -> expression dimension_index state binary.binary_right)
  | Frontend.Ast.Call_expression call -> (
      match expression dimension_index state call.call_callee with
      | Error _ as error -> error
      | Ok state ->
          fold_result
            (fun state (argument : Frontend.Ast.call_argument) ->
              match argument.call_argument_value with
              | Frontend.Ast.Omitted_call_argument -> Ok state
              | Frontend.Ast.Provided_call_argument value ->
                  expression dimension_index state value)
            state call.call_arguments)
  | Frontend.Ast.Index_expression index -> (
      match expression dimension_index state index.index_base with
      | Error _ as error -> error
      | Ok state -> expression dimension_index state index.index_value)
  | Frontend.Ast.Member_expression member ->
      expression dimension_index state member.member_base
  | Frontend.Ast.Integer_literal _
  | Frontend.Ast.Float_literal _
  | Frontend.Ast.Character_literal _
  | Frontend.Ast.String_literal _
  | Frontend.Ast.Current_position_expression _
  | Frontend.Ast.Sizeof_expression _
  | Frontend.Ast.Offset_expression _
  | Frontend.Ast.Defined_expression _ -> Ok state

type ast_global = {
  item_index : int;
  declarator_index : int option;
  name : Frontend.Ast.identifier;
  array_dimensions : Frontend.Ast.array_dimension list;
}

let declarator_ast ~item_index declarator_index
    (declarator : Frontend.Ast.global_declarator) =
  {
    item_index;
    declarator_index = Some declarator_index;
    name = declarator.name;
    array_dimensions = declarator.array_dimensions;
  }

let ast_globals (module_ : Frontend.Ast.module_) =
  module_.items
  |> List.mapi (fun item_index item ->
      match item with
      | Frontend.Ast.Global_variable variable ->
          [
            {
              item_index;
              declarator_index = None;
              name = variable.name;
              array_dimensions = variable.array_dimensions;
            };
          ]
      | Frontend.Ast.Global_declaration declaration ->
          declaration.declarators |> List.mapi (declarator_ast ~item_index)
      | Frontend.Ast.Aggregate_definition definition ->
          definition.attached_declarators
          |> List.mapi (declarator_ast ~item_index)
      | Frontend.Ast.Aggregate_forward_declaration _
      | Frontend.Ast.Function_prototype _
      | Frontend.Ast.Function_definition _
      | Frontend.Ast.Top_level_statement _ -> [])
  |> List.concat

let same_symbol left right =
  Sema.Symbol.Id.equal (Sema.Symbol.id left) (Sema.Symbol.id right)

let validate_dimension index semantic (ast : Frontend.Ast.array_dimension) =
  let expression_origin =
    Option.map
      (fun expression -> origin (Frontend.Ast.expression_location expression))
      ast.dimension_expression
  in
  if Sema.Global_type_resolution.array_dimension_index semantic <> index then
    Error "semantic global array dimension has the wrong index"
  else if
    Sema.Global_type_resolution.array_dimension_origin semantic
    <> origin ast.location
  then Error "semantic global array dimension has the wrong source origin"
  else if
    Sema.Global_type_resolution.array_dimension_opening_origin semantic
    <> origin ast.opening_bracket
  then Error "semantic global array dimension has the wrong opening bracket"
  else if
    Sema.Global_type_resolution.array_dimension_expression_origin semantic
    <> expression_origin
  then Error "semantic global array dimension has the wrong expression origin"
  else if
    Sema.Global_type_resolution.array_dimension_closing_origin semantic
    <> origin ast.closing_bracket
  then Error "semantic global array dimension has the wrong closing bracket"
  else Ok ()

let dimension_input next_index index semantic
    (ast : Frontend.Ast.array_dimension) =
  match validate_dimension index semantic ast with
  | Error _ as error -> error
  | Ok () -> (
      let collected =
        match ast.dimension_expression with
        | None -> Ok (empty_state next_index)
        | Some value -> expression index (empty_state next_index) value
      in
      match collected with
      | Error _ as error -> error
      | Ok state -> (
          let events = List.rev state.events_rev in
          match
            Sema.Global_dimension_binding.make_dimension ~dimension:semantic
              events
          with
          | Error _ as error -> error
          | Ok dimension -> Ok (state.next_index, dimension)))

let dimension_inputs semantic ast =
  let rec pair next_index index inputs_rev semantic ast =
    match (semantic, ast) with
    | [], [] -> Ok (List.rev inputs_rev)
    | semantic :: semantic_rest, ast :: ast_rest -> (
        match dimension_input next_index index semantic ast with
        | Error _ as error -> error
        | Ok (next_index, input) ->
            if index = max_int then
              Error "global array dimension identity space is exhausted"
            else
              pair next_index (index + 1) (input :: inputs_rev) semantic_rest
                ast_rest)
    | [], _ :: _ | _ :: _, [] ->
        Error "semantic global array dimensions do not match the AST"
  in
  pair 0 0 [] semantic ast

let global_input table record (ast : ast_global) =
  let global = Sema.Global_resolution.global_record_global record in
  let symbol = Sema.Global_resolution.global_record_symbol record in
  let semantic_symbol = Sema.Global_type_resolution.global_symbol global in
  if
    not
      (Sema.Symbol_table.owns_symbol table symbol
      && Sema.Symbol_table.owns_symbol table semantic_symbol)
  then Error "global array extent record belongs to another symbol table"
  else if not (same_symbol symbol semantic_symbol) then
    Error "global array extent record has inconsistent semantic identities"
  else if Sema.Global_type_resolution.global_item_index global <> ast.item_index
  then Error "global array extent record does not match the AST item order"
  else if
    Sema.Global_type_resolution.global_declarator_index global
    <> ast.declarator_index
  then Error "global array extent record has the wrong AST declarator index"
  else if not (String.equal (Sema.Symbol.name symbol) ast.name.spelling) then
    Error "global array extent record does not match the AST name"
  else if Sema.Symbol.origin symbol <> origin ast.name.location then
    Error "global array extent record does not match the AST name origin"
  else
    match
      dimension_inputs
        (Sema.Global_type_resolution.global_array_dimensions global)
        ast.array_dimensions
    with
    | Error _ as error -> error
    | Ok dimensions ->
        Sema.Global_dimension_binding.make_global ~record dimensions

let inputs table globals module_ =
  let records = Sema.Global_resolution.records globals in
  let ast = ast_globals module_ in
  let rec pair inputs_rev records ast =
    match (records, ast) with
    | [], [] -> Ok (List.rev inputs_rev)
    | record :: record_rest, ast :: ast_rest -> (
        match global_input table record ast with
        | Error _ as error -> error
        | Ok input -> pair (input :: inputs_rev) record_rest ast_rest)
    | [], _ :: _ | _ :: _, [] ->
        Error "global array extent records do not match the global declarations"
  in
  pair [] records ast

let resolve ~table ~environment ~expressions ~globals module_ =
  let result =
    match inputs table globals module_ with
    | Error _ as error -> error
    | Ok inputs ->
        Sema.Global_dimension_binding.resolve ~table ~environment ~expressions
          ~globals inputs
        |> Result.map_error Sema.Global_dimension_binding.error_to_string
  in
  Result.map_error
    (fun message ->
      if String.starts_with ~prefix:"HCSEMA" message then message
      else "HCSEMA0027: " ^ message)
    result
