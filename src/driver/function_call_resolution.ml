let origin (location : Frontend.Ast.location) =
  Sema.Symbol.Source_location
    {
      span = location.span;
      source_segments = location.source_segments;
      generated_from = location.generated_from;
      defined_at = location.defined_at;
    }

type state = {
  next_occurrence : int;
  next_call : int;
  calls_rev : Sema.Function_call_resolution.call list;
}

let empty_state = { next_occurrence = 0; next_call = 0; calls_rev = [] }

let add_identifier state =
  if state.next_occurrence = max_int then
    Error "function call occurrence space is exhausted"
  else Ok { state with next_occurrence = state.next_occurrence + 1 }

let fold_result apply state values =
  let rec loop state = function
    | [] -> Ok state
    | value :: rest -> (
        match apply state value with
        | Error _ as error -> error
        | Ok state -> loop state rest)
  in
  loop state values

let call_syntax (call : Frontend.Ast.call_expression) =
  match call.call_syntax with
  | Frontend.Ast.Parenthesized_call _ ->
      Sema.Function_call_resolution.Parenthesized
  | Frontend.Ast.Parenthesis_free_call ->
      Sema.Function_call_resolution.Parenthesis_free

let cast_type_reference (cast : Frontend.Ast.postfix_cast_expression) =
  let pointer_origins =
    List.map
      (fun (layer : Frontend.Ast.pointer_layer) -> origin layer.location)
      cast.cast_pointer_layers
  in
  let pointer_depth = List.length pointer_origins in
  let make form primitive =
    match Sema.Type.make_primitive ~form ~primitive ~pointer_depth with
    | Error _ as error -> error
    | Ok resolved_type ->
        Sema.Type_reference.make
          ~spelling:(Frontend.Ast.type_specifier_spelling cast.cast_type)
          ~spelling_origin:
            (origin (Frontend.Ast.type_specifier_location cast.cast_type))
          ~pointer_origins ~resolved_type
  in
  match cast.cast_type with
  | Frontend.Ast.Primitive_type_specifier primitive ->
      make Sema.Type.Public_spelling primitive.primitive
      |> Result.map Option.some
  | Frontend.Ast.Internal_type_specifier internal ->
      make Sema.Type.Internal_storage internal.primitive
      |> Result.map Option.some
  | Frontend.Ast.Named_type_specifier _ -> Ok None

let rec argument_expression (expression : Frontend.Ast.expression) =
  let kind_result =
    match expression with
    | Frontend.Ast.Integer_literal _ ->
        Ok Sema.Function_call_resolution.Integer_literal
    | Frontend.Ast.Float_literal _ ->
        Ok Sema.Function_call_resolution.Float_literal
    | Frontend.Ast.Character_literal _ ->
        Ok Sema.Function_call_resolution.Character_literal
    | Frontend.Ast.String_literal _ ->
        Ok Sema.Function_call_resolution.String_literal
    | Frontend.Ast.Parenthesized_expression grouped -> (
        match argument_expression grouped.grouped_expression with
        | Error _ as error -> error
        | Ok grouped ->
            Ok (Sema.Function_call_resolution.Parenthesized_expression grouped))
    | Frontend.Ast.Identifier_expression _ ->
        Ok
          (Sema.Function_call_resolution.Unresolved_expression
             Sema.Function_call_resolution.Identifier_expression)
    | Frontend.Ast.Current_position_expression _ ->
        Ok
          (Sema.Function_call_resolution.Unresolved_expression
             Sema.Function_call_resolution.Current_position_expression)
    | Frontend.Ast.Sizeof_expression _ ->
        Ok
          (Sema.Function_call_resolution.Unresolved_expression
             Sema.Function_call_resolution.Sizeof_expression)
    | Frontend.Ast.Offset_expression _ ->
        Ok
          (Sema.Function_call_resolution.Unresolved_expression
             Sema.Function_call_resolution.Offset_expression)
    | Frontend.Ast.Defined_expression _ ->
        Ok
          (Sema.Function_call_resolution.Unresolved_expression
             Sema.Function_call_resolution.Defined_expression)
    | Frontend.Ast.Prefix_expression _ ->
        Ok
          (Sema.Function_call_resolution.Unresolved_expression
             Sema.Function_call_resolution.Prefix_expression)
    | Frontend.Ast.Postfix_expression _ ->
        Ok
          (Sema.Function_call_resolution.Unresolved_expression
             Sema.Function_call_resolution.Postfix_expression)
    | Frontend.Ast.Postfix_cast_expression cast -> (
        match cast_type_reference cast with
        | Error _ as error -> error
        | Ok None ->
            Ok
              (Sema.Function_call_resolution.Unresolved_expression
                 Sema.Function_call_resolution.Postfix_cast_expression)
        | Ok (Some target) -> (
            match argument_expression cast.cast_operand with
            | Error _ as error -> error
            | Ok operand ->
                Ok
                  (Sema.Function_call_resolution.Postfix_cast_expression
                     (operand, target))))
    | Frontend.Ast.Binary_expression _ ->
        Ok
          (Sema.Function_call_resolution.Unresolved_expression
             Sema.Function_call_resolution.Binary_expression)
    | Frontend.Ast.Call_expression _ ->
        Ok
          (Sema.Function_call_resolution.Unresolved_expression
             Sema.Function_call_resolution.Call_expression)
    | Frontend.Ast.Index_expression _ ->
        Ok
          (Sema.Function_call_resolution.Unresolved_expression
             Sema.Function_call_resolution.Index_expression)
    | Frontend.Ast.Member_expression _ ->
        Ok
          (Sema.Function_call_resolution.Unresolved_expression
             Sema.Function_call_resolution.Member_expression)
  in
  Result.map
    (fun kind ->
      Sema.Function_call_resolution.make_argument_expression ~kind
        ~origin:(origin (Frontend.Ast.expression_location expression)))
    kind_result

let argument index (argument : Frontend.Ast.call_argument) =
  let prepared =
    match argument.call_argument_value with
    | Frontend.Ast.Provided_call_argument expression ->
        Result.map
          (fun expression ->
            (Sema.Function_call_resolution.Provided, Some expression))
          (argument_expression expression)
    | Frontend.Ast.Omitted_call_argument ->
        Ok (Sema.Function_call_resolution.Omitted, None)
  in
  match prepared with
  | Error _ as error -> error
  | Ok (kind, expression) ->
      Sema.Function_call_resolution.make_argument ~index ~kind ~expression
        ~origin:(origin argument.call_argument_location)

let call_arguments call =
  let rec loop index rev = function
    | [] -> Ok (List.rev rev)
    | argument_ast :: rest -> (
        match argument index argument_ast with
        | Error _ as error -> error
        | Ok argument ->
            if index = max_int then
              Error "function call argument space is exhausted"
            else loop (index + 1) (argument :: rev) rest)
  in
  loop 0 [] call.Frontend.Ast.call_arguments

let collect_call state (call : Frontend.Ast.call_expression) =
  match call.call_callee with
  | Frontend.Ast.Identifier_expression callee -> (
      match call_arguments call with
      | Error _ as error -> error
      | Ok arguments -> (
          match
            Sema.Function_call_resolution.make_call ~index:state.next_call
              ~callee_occurrence_index:state.next_occurrence
              ~callee_name:callee.spelling
              ~callee_origin:(origin callee.location)
              ~origin:(origin call.call_location)
              ~syntax:(call_syntax call) arguments
          with
          | Error _ as error -> error
          | Ok call ->
              if state.next_call = max_int then
                Error "function call identity space is exhausted"
              else
                Ok
                  {
                    state with
                    next_call = state.next_call + 1;
                    calls_rev = call :: state.calls_rev;
                  }))
  | _ -> Ok state

let rec expression state = function
  | Frontend.Ast.Identifier_expression _ -> add_identifier state
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
      match collect_call state call with
      | Error _ as error -> error
      | Ok state -> (
          match expression state call.call_callee with
          | Error _ as error -> error
          | Ok state -> fold_result call_argument state call.call_arguments))
  | Frontend.Ast.Index_expression index -> (
      match expression state index.index_base with
      | Error _ as error -> error
      | Ok state -> expression state index.index_value)
  | Frontend.Ast.Member_expression member -> expression state member.member_base
  | Frontend.Ast.Sizeof_expression _
  | Frontend.Ast.Offset_expression _
  | Frontend.Ast.Defined_expression _
  | Frontend.Ast.Integer_literal _
  | Frontend.Ast.Float_literal _
  | Frontend.Ast.Character_literal _
  | Frontend.Ast.String_literal _
  | Frontend.Ast.Current_position_expression _ -> Ok state

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

let local_initializer state (initial : Frontend.Ast.local_initializer) =
  initial_value state initial.local_initializer_value

let local_declaration state (declaration : Frontend.Ast.local_declaration) =
  let declarator state (declarator : Frontend.Ast.local_declarator) =
    match
      fold_result array_dimension state declarator.local_array_dimensions
    with
    | Error _ as error -> error
    | Ok state -> (
        match declarator.local_initializer with
        | None -> Ok state
        | Some initial -> local_initializer state initial)
  in
  fold_result declarator state declaration.local_declarators

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
  | Frontend.Ast.Expression_statement statement ->
      expression state statement.expression_statement_expression
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

and statements state statements = fold_result statement state statements

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

let function_input table expected (item_index, ast) =
  let symbol = Sema.Module_expression_binding.function_symbol expected in
  let scope = Sema.Module_expression_binding.function_scope expected in
  let expected_item =
    Sema.Module_expression_binding.function_item_index expected
  in
  let name, body = function_header ast in
  if not (Sema.Symbol_table.owns_symbol table symbol) then
    Error "function call owner belongs to another symbol table"
  else if not (Sema.Symbol_table.owns_scope table scope) then
    Error "function call scope belongs to another symbol table"
  else if expected_item <> item_index then
    Error "function call owner does not match the AST item order"
  else if not (String.equal (Sema.Symbol.name symbol) name.spelling) then
    Error "function call owner does not match the AST name"
  else if Sema.Symbol.origin symbol <> origin name.location then
    Error "function call owner does not match the AST origin"
  else
    let collected =
      match body with
      | None -> Ok empty_state
      | Some body -> statement empty_state body
    in
    match collected with
    | Error _ as error -> error
    | Ok state ->
        let expected_occurrences =
          Sema.Module_expression_binding.function_occurrences expected
          |> List.length
        in
        if state.next_occurrence <> expected_occurrences then
          Error
            "function call traversal does not match ordinary expression binding"
        else
          Sema.Function_call_resolution.make_function ~symbol ~scope ~item_index
            (List.rev state.calls_rev)

let function_inputs table expressions module_ =
  let rec pair inputs_rev expected ast =
    match (expected, ast) with
    | [], [] -> Ok (List.rev inputs_rev)
    | expected :: expected_rest, ast :: ast_rest -> (
        match function_input table expected ast with
        | Error _ as error -> error
        | Ok input -> pair (input :: inputs_rev) expected_rest ast_rest)
    | [], _ :: _ | _ :: _, [] ->
        Error "function call inputs do not match the parsed function count"
  in
  pair []
    (Sema.Module_expression_binding.functions expressions)
    (ast_functions module_)

let resolve ~table ~declarations ~function_types ~functions ~expressions module_
    =
  let parent = Sema.Declaration_collection.scope declarations in
  let result =
    if not (Sema.Symbol_table.owns_scope table parent) then
      Error "function call declarations belong to another symbol table"
    else if Sema.Symbol_table.scope_kind parent <> Sema.Symbol_table.Module then
      Error "function call resolution requires a module declaration scope"
    else if not (Sema.Module_expression_binding.owns_table expressions table)
    then Error "function call expressions belong to another symbol table"
    else
      match function_inputs table expressions module_ with
      | Error _ as error -> error
      | Ok inputs ->
          Sema.Function_call_resolution.resolve ~table ~parent ~function_types
            ~functions ~expressions inputs
          |> Result.map_error Sema.Function_call_resolution.error_to_string
  in
  Result.map_error
    (fun message ->
      if String.starts_with ~prefix:"HCSEMA" message then message
      else "HCSEMA0039: " ^ message)
    result
