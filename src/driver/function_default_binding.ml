let origin (location : Frontend.Ast.location) =
  Sema.Symbol.Source_location
    {
      span = location.span;
      source_segments = location.source_segments;
      generated_from = location.generated_from;
      defined_at = location.defined_at;
    }

type state = {
  events_rev : Sema.Function_default_binding.event list;
  next_index : int;
}

let empty_state = { events_rev = []; next_index = 0 }

let add_identifier parameter_index state
    (identifier : Frontend.Ast.identifier) =
  if state.next_index = max_int then
    Error "function default occurrence identity space is exhausted"
  else
    match
      Sema.Function_default_binding.make_identifier ~name:identifier.spelling
        ~origin:(origin identifier.location)
        ~occurrence_index:state.next_index ~parameter_index
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

let rec expression parameter_index state = function
  | Frontend.Ast.Identifier_expression identifier ->
      add_identifier parameter_index state identifier
  | Frontend.Ast.Parenthesized_expression grouped ->
      expression parameter_index state grouped.grouped_expression
  | Frontend.Ast.Prefix_expression prefix ->
      expression parameter_index state prefix.prefix_operand
  | Frontend.Ast.Postfix_expression postfix ->
      expression parameter_index state postfix.postfix_operand
  | Frontend.Ast.Postfix_cast_expression cast ->
      expression parameter_index state cast.cast_operand
  | Frontend.Ast.Binary_expression binary -> (
      match expression parameter_index state binary.binary_left with
      | Error _ as error -> error
      | Ok state -> expression parameter_index state binary.binary_right)
  | Frontend.Ast.Call_expression call -> (
      match expression parameter_index state call.call_callee with
      | Error _ as error -> error
      | Ok state ->
          fold_result
            (fun state (argument : Frontend.Ast.call_argument) ->
              match argument.call_argument_value with
              | Frontend.Ast.Omitted_call_argument -> Ok state
              | Frontend.Ast.Provided_call_argument value ->
                  expression parameter_index state value)
            state call.call_arguments)
  | Frontend.Ast.Index_expression index -> (
      match expression parameter_index state index.index_base with
      | Error _ as error -> error
      | Ok state -> expression parameter_index state index.index_value)
  | Frontend.Ast.Member_expression member ->
      expression parameter_index state member.member_base
  | Frontend.Ast.Integer_literal _
  | Frontend.Ast.Float_literal _
  | Frontend.Ast.Character_literal _
  | Frontend.Ast.String_literal _
  | Frontend.Ast.Current_position_expression _
  | Frontend.Ast.Sizeof_expression _
  | Frontend.Ast.Offset_expression _
  | Frontend.Ast.Defined_expression _ -> Ok state

type ast_function = {
  item_index : int;
  name : Frontend.Ast.identifier;
  parameters : Frontend.Ast.function_parameter list;
}

let ast_functions (module_ : Frontend.Ast.module_) =
  module_.items
  |> List.mapi (fun item_index item ->
      match item with
      | Frontend.Ast.Function_prototype prototype ->
          Some
            {
              item_index;
              name = prototype.name;
              parameters = prototype.parameters;
            }
      | Frontend.Ast.Function_definition definition ->
          Some
            {
              item_index;
              name = definition.name;
              parameters = definition.parameters;
            }
      | Frontend.Ast.Aggregate_forward_declaration _
      | Frontend.Ast.Aggregate_definition _
      | Frontend.Ast.Global_variable _
      | Frontend.Ast.Global_declaration _
      | Frontend.Ast.Top_level_statement _ -> None)
  |> List.filter_map Fun.id

let validate_default semantic ast =
  match (semantic, ast) with
  | None, None -> Ok ()
  | ( Some
        (Sema.Function_type_resolution.Expression_default
          {
            origin = semantic_origin;
            equals_origin;
            expression_origin;
          }),
      Some (ast : Frontend.Ast.parameter_default) ) -> (
      match ast.value with
      | Frontend.Ast.Expression_default expression
        when semantic_origin = origin ast.location
             && equals_origin = origin ast.equals
             && expression_origin
                = origin (Frontend.Ast.expression_location expression) ->
          Ok ()
      | Frontend.Ast.Expression_default _ ->
          Error "function default expression has the wrong source origin"
      | Frontend.Ast.Lastclass_default _ ->
          Error "function default expression has the wrong source shape")
  | ( Some
        (Sema.Function_type_resolution.Lastclass_default
          { origin = semantic_origin; equals_origin; keyword_origin }),
      Some (ast : Frontend.Ast.parameter_default) ) -> (
      match ast.value with
      | Frontend.Ast.Lastclass_default lastclass
        when semantic_origin = origin ast.location
             && equals_origin = origin ast.equals
             && keyword_origin = origin lastclass.lastclass_location ->
          Ok ()
      | Frontend.Ast.Lastclass_default _ ->
          Error "function lastclass default has the wrong source origin"
      | Frontend.Ast.Expression_default _ ->
          Error "function lastclass default has the wrong source shape")
  | None, Some _ | Some _, None ->
      Error "semantic function default does not match the AST"

let validate_parameter index semantic (ast : Frontend.Ast.function_parameter) =
  let ast_name =
    Option.map
      (fun (name : Frontend.Ast.identifier) -> name.spelling)
      ast.name
  in
  let ast_name_origin =
    Option.map
      (fun (name : Frontend.Ast.identifier) -> origin name.location)
      ast.name
  in
  if Sema.Function_type_resolution.parameter_index semantic <> index then
    Error "semantic function parameter has the wrong index"
  else if
    Sema.Function_type_resolution.parameter_origin semantic <> origin ast.location
  then Error "semantic function parameter has the wrong source origin"
  else if Sema.Function_type_resolution.parameter_name semantic <> ast_name then
    Error "semantic function parameter has the wrong name"
  else if
    Sema.Function_type_resolution.parameter_name_origin semantic
    <> ast_name_origin
  then Error "semantic function parameter has the wrong name origin"
  else
    validate_default
      (Sema.Function_type_resolution.parameter_default semantic)
      ast.default

let take count values =
  let rec loop count taken_rev = function
    | _ when count = 0 -> List.rev taken_rev
    | value :: rest -> loop (count - 1) (value :: taken_rev) rest
    | [] -> invalid_arg "function default event count exceeds collected events"
  in
  loop count [] values

let parameter_inputs semantic ast =
  let rec pair index state inputs_rev semantic ast =
    match (semantic, ast) with
    | [], [] -> Ok (List.rev inputs_rev)
    | semantic :: semantic_rest, ast :: ast_rest ->
        let previous_count = state.next_index in
        (match validate_parameter index semantic ast with
        | Error _ as error -> error
        | Ok () -> (
            let collected =
              match ast.default with
              | Some
                  {
                    value = Frontend.Ast.Expression_default value;
                    _;
                  } ->
                  expression index state value
              | None
              | Some
                  {
                    value = Frontend.Ast.Lastclass_default _;
                    _;
                  } ->
                  Ok state
            in
            match collected with
            | Error _ as error -> error
            | Ok state ->
                let added = state.next_index - previous_count in
                let events = state.events_rev |> take added |> List.rev in
                (match
                   Sema.Function_default_binding.make_parameter
                     ~parameter:semantic events
                 with
                | Error _ as error -> error
                | Ok input ->
                    if index = max_int then
                      Error "function default parameter identity space is exhausted"
                    else
                      pair (index + 1) state (input :: inputs_rev)
                        semantic_rest ast_rest)))
    | [], _ :: _ | _ :: _, [] ->
        Error "semantic function parameters do not match the AST"
  in
  pair 0 empty_state [] semantic ast

let function_input table declaration (ast : ast_function) =
  let site = Sema.Function_resolution.resolved_declaration_site declaration in
  let function_ = Sema.Function_resolution.declaration_site_function site in
  let symbol = Sema.Function_type_resolution.function_symbol function_ in
  if not (Sema.Symbol_table.owns_symbol table symbol) then
    Error "function default declaration belongs to another symbol table"
  else if Sema.Function_type_resolution.function_item_index function_ <> ast.item_index
  then Error "function default declaration does not match the AST item order"
  else if not (String.equal (Sema.Symbol.name symbol) ast.name.spelling) then
    Error "function default declaration does not match the AST name"
  else if Sema.Symbol.origin symbol <> origin ast.name.location then
    Error "function default declaration does not match the AST name origin"
  else
    let semantic_parameters =
      function_ |> Sema.Function_type_resolution.function_signature
      |> Sema.Function_type_resolution.signature_parameters
    in
    match parameter_inputs semantic_parameters ast.parameters with
    | Error _ as error -> error
    | Ok parameters ->
        Sema.Function_default_binding.make_function ~declaration parameters

let inputs table functions module_ =
  let declarations = Sema.Function_resolution.declarations functions in
  let ast = ast_functions module_ in
  let rec pair inputs_rev declarations ast =
    match (declarations, ast) with
    | [], [] -> Ok (List.rev inputs_rev)
    | declaration :: declaration_rest, ast :: ast_rest -> (
        match function_input table declaration ast with
        | Error _ as error -> error
        | Ok input -> pair (input :: inputs_rev) declaration_rest ast_rest)
    | [], _ :: _ | _ :: _, [] ->
        Error "function identity declarations do not match the AST"
  in
  pair [] declarations ast

let resolve ~table ~environment ~expressions ~functions module_ =
  let result =
    match inputs table functions module_ with
    | Error _ as error -> error
    | Ok inputs ->
        Sema.Function_default_binding.resolve ~table ~environment ~expressions
          ~functions inputs
        |> Result.map_error Sema.Function_default_binding.error_to_string
  in
  Result.map_error
    (fun message ->
      if String.starts_with ~prefix:"HCSEMA" message then message
      else "HCSEMA0029: " ^ message)
    result
