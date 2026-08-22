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

type state = { events_rev : Sema.Top_level_expression_binding.event list }

let empty_state = { events_rev = [] }

let add_event state = function
  | Error _ as error -> error
  | Ok event -> Ok { events_rev = event :: state.events_rev }

let add_identifier state (identifier : Frontend.Ast.identifier) =
  Sema.Top_level_expression_binding.make_identifier ~name:identifier.spelling
    ~origin:(origin identifier)
  |> add_event state

let rec fold_result apply state = function
  | [] -> Ok state
  | value :: rest -> (
      match apply state value with
      | Error _ as error -> error
      | Ok state -> fold_result apply state rest)

let rec expression state = function
  | Frontend.Ast.Identifier_expression identifier ->
      add_identifier state identifier
  | Frontend.Ast.Parenthesized_expression grouped ->
      expression state grouped.grouped_expression
  | Frontend.Ast.Prefix_expression prefix ->
      expression state prefix.prefix_operand
  | Frontend.Ast.Postfix_expression postfix ->
      expression state postfix.postfix_operand
  | Frontend.Ast.Postfix_cast_expression cast ->
      expression state cast.cast_operand
  | Frontend.Ast.Binary_expression binary -> (
      match expression state binary.binary_left with
      | Error _ as error -> error
      | Ok state -> expression state binary.binary_right)
  | Frontend.Ast.Call_expression call -> (
      match expression state call.call_callee with
      | Error _ as error -> error
      | Ok state -> fold_result call_argument state call.call_arguments)
  | Frontend.Ast.Index_expression index -> (
      match expression state index.index_base with
      | Error _ as error -> error
      | Ok state -> expression state index.index_value)
  | Frontend.Ast.Member_expression member -> expression state member.member_base
  | Frontend.Ast.Integer_literal _
  | Frontend.Ast.Float_literal _
  | Frontend.Ast.Character_literal _
  | Frontend.Ast.String_literal _
  | Frontend.Ast.Current_position_expression _
  | Frontend.Ast.Sizeof_expression _
  | Frontend.Ast.Offset_expression _
  | Frontend.Ast.Defined_expression _ -> Ok state

and call_argument state (argument : Frontend.Ast.call_argument) =
  match argument.call_argument_value with
  | Frontend.Ast.Omitted_call_argument -> Ok state
  | Frontend.Ast.Provided_call_argument value -> expression state value

let rec initial_value state = function
  | Frontend.Ast.Scalar_initializer value -> expression state value
  | Frontend.Ast.Braced_initializer braced ->
      fold_result initializer_element state braced.initializer_elements

and initializer_element state (element : Frontend.Ast.initializer_element) =
  initial_value state element.initializer_element_value

let array_dimension state (dimension : Frontend.Ast.array_dimension) =
  match dimension.dimension_expression with
  | None -> Ok state
  | Some value -> expression state value

let local_declarator state (declarator : Frontend.Ast.local_declarator) =
  match fold_result array_dimension state declarator.local_array_dimensions with
  | Error _ as error -> error
  | Ok state -> (
      match declarator.local_initializer with
      | None -> Ok state
      | Some initialized ->
          initial_value state initialized.local_initializer_value)

let local_declaration state (declaration : Frontend.Ast.local_declaration) =
  fold_result local_declarator state declaration.local_declarators

let implicit_output state (output : Frontend.Ast.implicit_output_statement) =
  let fixed =
    match output.fixed_argument with
    | Frontend.Ast.Marker_fixed_argument value
    | Frontend.Ast.Expression_fixed_argument value -> expression state value
  in
  match fixed with
  | Error _ as error -> error
  | Ok state ->
      fold_result
        (fun state (argument : Frontend.Ast.implicit_output_argument) ->
          expression state argument.value)
        state output.arguments

let case_pattern state = function
  | Frontend.Ast.Implicit_case -> Ok state
  | Frontend.Ast.Single_case value -> expression state value
  | Frontend.Ast.Ranged_case range -> (
      match expression state range.case_range_start with
      | Error _ as error -> error
      | Ok state -> expression state range.case_range_end)

let rec statement state = function
  | Frontend.Ast.Block_statement block ->
      statements state block.block_statements
  | Frontend.Ast.Do_while_statement do_while -> (
      match statement state do_while.do_body with
      | Error _ as error -> error
      | Ok state -> expression state do_while.do_while_condition)
  | Frontend.Ast.Expression_statement expression_statement ->
      expression state expression_statement.expression_statement_expression
  | Frontend.Ast.For_statement for_ -> (
      match statement state for_.for_initializer with
      | Error _ as error -> error
      | Ok state -> (
          match expression state for_.for_condition with
          | Error _ as error -> error
          | Ok state -> (
              match for_.for_update with
              | Some update -> (
                  match statement state update with
                  | Error _ as error -> error
                  | Ok state -> statement state for_.for_body)
              | None -> statement state for_.for_body)))
  | Frontend.Ast.If_statement if_ -> (
      match expression state if_.if_condition with
      | Error _ as error -> error
      | Ok state -> (
          match statement state if_.if_then_branch with
          | Error _ as error -> error
          | Ok state -> (
              match if_.if_else_clause with
              | None -> Ok state
              | Some else_ -> statement state else_.else_branch)))
  | Frontend.Ast.Implicit_output_statement output ->
      implicit_output state output
  | Frontend.Ast.Local_declaration_statement declaration ->
      local_declaration state declaration
  | Frontend.Ast.Lock_statement lock -> statement state lock.lock_body
  | Frontend.Ast.Return_statement return -> (
      match return.return_value with
      | None -> Ok state
      | Some value -> expression state value)
  | Frontend.Ast.Sequence_statement sequence ->
      sequence_elements state sequence.sequence_elements
  | Frontend.Ast.Switch_statement switch -> (
      match expression state switch.switch_expression with
      | Error _ as error -> error
      | Ok state -> switch_elements state switch.switch_elements)
  | Frontend.Ast.Try_catch_statement try_catch -> (
      match statement state try_catch.try_body with
      | Error _ as error -> error
      | Ok state -> statement state try_catch.catch_body)
  | Frontend.Ast.While_statement while_ -> (
      match expression state while_.while_condition with
      | Error _ as error -> error
      | Ok state -> statement state while_.while_body)
  | Frontend.Ast.Assembly_block_statement _
  | Frontend.Ast.Inline_assembly_statement _
  | Frontend.Ast.Break_statement _
  | Frontend.Ast.Empty_statement _
  | Frontend.Ast.Goto_statement _
  | Frontend.Ast.Label_statement _
  | Frontend.Ast.No_warn_statement _ -> Ok state

and statements state values = fold_result statement state values

and sequence_elements state elements =
  fold_result
    (fun state (element : Frontend.Ast.statement_sequence_element) ->
      statement state element.sequence_statement)
    state elements

and switch_elements state elements = fold_result switch_element state elements

and switch_element state = function
  | Frontend.Ast.Switch_case_element case ->
      case_pattern state case.switch_case_pattern
  | Frontend.Ast.Switch_default_element _ -> Ok state
  | Frontend.Ast.Switch_subswitch_element subswitch ->
      switch_elements state subswitch.subswitch_elements
  | Frontend.Ast.Switch_statement_element statement_ ->
      statement state statement_

let statement_input statement_index item_index statement_node =
  match statement empty_state statement_node with
  | Error _ as error -> error
  | Ok state ->
      Sema.Top_level_expression_binding.make_statement ~statement_index
        ~item_index
        ~origin:
          (statement_node |> Frontend.Ast.statement_location
         |> origin_of_location)
        (List.rev state.events_rev)

let statement_inputs (module_ : Frontend.Ast.module_) =
  let rec loop statement_index inputs_rev item_index = function
    | [] -> Ok (List.rev inputs_rev)
    | Frontend.Ast.Top_level_statement statement :: rest -> (
        match statement_input statement_index item_index statement with
        | Error _ as error -> error
        | Ok input ->
            if statement_index = max_int then
              Error "top-level statement identity space is exhausted"
            else
              loop (statement_index + 1) (input :: inputs_rev) (item_index + 1)
                rest)
    | _ :: rest -> loop statement_index inputs_rev (item_index + 1) rest
  in
  loop 0 [] 0 module_.items

let resolve ~table ~declarations ~module_expressions module_ =
  let parent = Sema.Declaration_collection.scope declarations in
  let result =
    if not (Sema.Symbol_table.owns_scope table parent) then
      Error "top-level expression declarations belong to another symbol table"
    else if Sema.Symbol_table.scope_kind parent <> Sema.Symbol_table.Module then
      Error "top-level expression binding requires a module declaration scope"
    else
      match statement_inputs module_ with
      | Error _ as error -> error
      | Ok inputs ->
          Sema.Top_level_expression_binding.resolve ~table ~parent
            ~module_expressions inputs
          |> Result.map_error Sema.Top_level_expression_binding.error_to_string
  in
  Result.map_error
    (fun message ->
      if String.starts_with ~prefix:"HCSEMA" message then message
      else "HCSEMA0052: " ^ message)
    result
