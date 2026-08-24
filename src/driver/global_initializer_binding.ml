let origin (location : Frontend.Ast.location) =
  Sema.Symbol.Source_location
    {
      span = location.span;
      source_segments = location.source_segments;
      generated_from = location.generated_from;
      defined_at = location.defined_at;
    }

type state = {
  events_rev : Sema.Global_initializer_binding.event list;
  next_index : int;
}

let empty_state = { events_rev = []; next_index = 0 }

let add_identifier initializer_path state (identifier : Frontend.Ast.identifier)
    =
  if state.next_index = max_int then
    Error "global initializer occurrence identity space is exhausted"
  else
    match
      Sema.Global_initializer_binding.make_identifier ~name:identifier.spelling
        ~origin:(origin identifier.location)
        ~occurrence_index:state.next_index ~initializer_path
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

let rec expression initializer_path state = function
  | Frontend.Ast.Identifier_expression identifier ->
      add_identifier initializer_path state identifier
  | Frontend.Ast.Parenthesized_expression grouped ->
      expression initializer_path state grouped.grouped_expression
  | Frontend.Ast.Prefix_expression prefix ->
      expression initializer_path state prefix.prefix_operand
  | Frontend.Ast.Postfix_expression postfix ->
      expression initializer_path state postfix.postfix_operand
  | Frontend.Ast.Postfix_cast_expression cast ->
      expression initializer_path state cast.cast_operand
  | Frontend.Ast.Binary_expression binary -> (
      match expression initializer_path state binary.binary_left with
      | Error _ as error -> error
      | Ok state -> expression initializer_path state binary.binary_right)
  | Frontend.Ast.Call_expression call -> (
      match expression initializer_path state call.call_callee with
      | Error _ as error -> error
      | Ok state ->
          fold_result
            (fun state (argument : Frontend.Ast.call_argument) ->
              match argument.call_argument_value with
              | Frontend.Ast.Omitted_call_argument -> Ok state
              | Frontend.Ast.Provided_call_argument value ->
                  expression initializer_path state value)
            state call.call_arguments)
  | Frontend.Ast.Index_expression index -> (
      match expression initializer_path state index.index_base with
      | Error _ as error -> error
      | Ok state -> expression initializer_path state index.index_value)
  | Frontend.Ast.Member_expression member ->
      expression initializer_path state member.member_base
  | Frontend.Ast.Integer_literal _
  | Frontend.Ast.Float_literal _
  | Frontend.Ast.Character_literal _
  | Frontend.Ast.String_literal _
  | Frontend.Ast.Current_position_expression _
  | Frontend.Ast.Sizeof_expression _
  | Frontend.Ast.Offset_expression _
  | Frontend.Ast.Defined_expression _ -> Ok state

let rec initial_value path state = function
  | Frontend.Ast.Scalar_initializer value -> expression path state value
  | Frontend.Ast.Braced_initializer braced ->
      let rec elements index state = function
        | [] -> Ok state
        | (element : Frontend.Ast.initializer_element) :: rest -> (
            match
              initial_value (path @ [ index ]) state
                element.initializer_element_value
            with
            | Error _ as error -> error
            | Ok state ->
                if index = max_int then
                  Error "global initializer path identity space is exhausted"
                else elements (index + 1) state rest)
      in
      elements 0 state braced.initializer_elements
  | Frontend.Ast.Unbraced_array_initializer unbraced ->
      let rec elements index state = function
        | [] -> Ok state
        | (element : Frontend.Ast.initializer_element) :: rest -> (
            match
              initial_value (path @ [ index ]) state
                element.initializer_element_value
            with
            | Error _ as error -> error
            | Ok state ->
                if index = max_int then
                  Error "global initializer path identity space is exhausted"
                else elements (index + 1) state rest)
      in
      elements 0 state unbraced.unbraced_initializer_elements

let events = function
  | None -> Ok []
  | Some (initial : Frontend.Ast.global_initializer) ->
      Result.map
        (fun state -> List.rev state.events_rev)
        (initial_value [] empty_state initial.global_initializer_value)

type ast_global = {
  item_index : int;
  declarator_index : int option;
  name : Frontend.Ast.identifier;
  initial_value : Frontend.Ast.global_initializer option;
}

let declarator_ast ~item_index declarator_index
    (declarator : Frontend.Ast.global_declarator) =
  {
    item_index;
    declarator_index = Some declarator_index;
    name = declarator.name;
    initial_value = declarator.global_initial_value;
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
              initial_value = None;
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

let initializer_shape = function
  | Frontend.Ast.Scalar_initializer expression ->
      ( Sema.Global_type_resolution.Scalar_initializer,
        origin (Frontend.Ast.expression_location expression) )
  | Frontend.Ast.Braced_initializer braced ->
      ( Sema.Global_type_resolution.Braced_initializer,
        origin braced.initializer_location )
  | Frontend.Ast.Unbraced_array_initializer unbraced ->
      ( Sema.Global_type_resolution.Braced_initializer,
        origin unbraced.unbraced_initializer_location )

let validate_initializer semantic ast =
  match (semantic, ast) with
  | None, None -> Ok ()
  | Some semantic, Some (ast : Frontend.Ast.global_initializer) ->
      let kind, value_origin = initializer_shape ast.global_initializer_value in
      if
        Sema.Global_type_resolution.initializer_origin semantic
        <> origin ast.global_initializer_location
      then Error "semantic global initializer has the wrong source origin"
      else if Sema.Global_type_resolution.initializer_kind semantic <> kind then
        Error "semantic global initializer has the wrong source shape"
      else if
        Sema.Global_type_resolution.initializer_value_origin semantic
        <> value_origin
      then Error "semantic global initializer has the wrong value origin"
      else Ok ()
  | None, Some _ | Some _, None ->
      Error "semantic global initializer does not match the AST"

let global_input table record (ast : ast_global) =
  let global = Sema.Global_resolution.global_record_global record in
  let symbol = Sema.Global_resolution.global_record_symbol record in
  let semantic_symbol = Sema.Global_type_resolution.global_symbol global in
  if
    not
      (Sema.Symbol_table.owns_symbol table symbol
      && Sema.Symbol_table.owns_symbol table semantic_symbol)
  then Error "global initializer record belongs to another symbol table"
  else if not (same_symbol symbol semantic_symbol) then
    Error "global initializer record has inconsistent semantic identities"
  else if Sema.Global_type_resolution.global_item_index global <> ast.item_index
  then Error "global initializer record does not match the AST item order"
  else if
    Sema.Global_type_resolution.global_declarator_index global
    <> ast.declarator_index
  then Error "global initializer record has the wrong AST declarator index"
  else if not (String.equal (Sema.Symbol.name symbol) ast.name.spelling) then
    Error "global initializer record does not match the AST name"
  else if Sema.Symbol.origin symbol <> origin ast.name.location then
    Error "global initializer record does not match the AST name origin"
  else
    match
      validate_initializer
        (Sema.Global_type_resolution.global_initializer global)
        ast.initial_value
    with
    | Error _ as error -> error
    | Ok () -> (
        match events ast.initial_value with
        | Error _ as error -> error
        | Ok events ->
            Sema.Global_initializer_binding.make_global ~record events)

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
        Error "global initializer records do not match the global declarations"
  in
  pair [] records ast

let resolve ~table ~environment ~expressions ~globals module_ =
  let result =
    match inputs table globals module_ with
    | Error _ as error -> error
    | Ok inputs ->
        Sema.Global_initializer_binding.resolve ~table ~environment ~expressions
          ~globals inputs
        |> Result.map_error Sema.Global_initializer_binding.error_to_string
  in
  Result.map_error
    (fun message ->
      if String.starts_with ~prefix:"HCSEMA" message then message
      else "HCSEMA0025: " ^ message)
    result
