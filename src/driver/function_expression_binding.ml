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

type state = {
  events_rev : Sema.Function_expression_binding.event list;
  declaration_index : int;
}

let empty_state = { events_rev = []; declaration_index = 0 }

let add_event state = function
  | Error _ as error -> error
  | Ok event -> Ok { state with events_rev = event :: state.events_rev }

let add_identifier state (identifier : Frontend.Ast.identifier) =
  Sema.Function_expression_binding.make_identifier ~name:identifier.spelling
    ~origin:(origin identifier)
  |> add_event state

let add_publication state declaration_index declarator_index
    (identifier : Frontend.Ast.identifier) =
  Sema.Function_expression_binding.make_local_publication
    ~name:identifier.spelling ~origin:(origin identifier) ~declaration_index
    ~declarator_index
  |> add_event state

let add_no_warn_suppression state (target : Frontend.Ast.no_warn_target) =
  Sema.Function_expression_binding.make_no_warn_suppression
    ~name:target.no_warn_target_name.spelling
    ~origin:(origin target.no_warn_target_name)
  |> add_event state

let add_initializer_use_reset state declaration_index declarator_index
    (identifier : Frontend.Ast.identifier) =
  Sema.Function_expression_binding.make_initializer_use_reset
    ~name:identifier.spelling ~origin:(origin identifier) ~declaration_index
    ~declarator_index
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

let local_initializer state (initial : Frontend.Ast.local_initializer) =
  initial_value state initial.local_initializer_value

let local_declaration state (declaration : Frontend.Ast.local_declaration) =
  let declaration_index = state.declaration_index in
  let rec declarators state declarator_index = function
    | [] ->
        if declaration_index = max_int then
          Error "function local declaration identity space is exhausted"
        else Ok { state with declaration_index = declaration_index + 1 }
    | (declarator : Frontend.Ast.local_declarator) :: rest -> (
        match
          fold_result array_dimension state declarator.local_array_dimensions
        with
        | Error _ as error -> error
        | Ok state -> (
            match
              add_publication state declaration_index declarator_index
                declarator.local_name
            with
            | Error _ as error -> error
            | Ok state -> (
                match declarator.local_initializer with
                | Some initial -> (
                    match local_initializer state initial with
                    | Error _ as error -> error
                    | Ok state -> (
                        match
                          add_initializer_use_reset state declaration_index
                            declarator_index declarator.local_name
                        with
                        | Error _ as error -> error
                        | Ok state ->
                            if declarator_index = max_int then
                              Error
                                "function local declarator identity space is \
                                 exhausted"
                            else declarators state (declarator_index + 1) rest))
                | None ->
                    if declarator_index = max_int then
                      Error
                        "function local declarator identity space is exhausted"
                    else declarators state (declarator_index + 1) rest)))
  in
  declarators state 0 declaration.local_declarators

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
  | Frontend.Ast.Label_statement _ -> Ok state
  | Frontend.Ast.No_warn_statement no_warn ->
      fold_result add_no_warn_suppression state no_warn.no_warn_targets

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

let events = function
  | None -> Ok []
  | Some body ->
      Result.map
        (fun state -> List.rev state.events_rev)
        (statement empty_state body)

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

let same_symbol left right =
  Sema.Symbol.Id.equal (Sema.Symbol.id left) (Sema.Symbol.id right)

let same_scope left right =
  Sema.Symbol.Scope_id.equal
    (Sema.Symbol_table.scope_id left)
    (Sema.Symbol_table.scope_id right)

let binding_matches_entry entry (binding : Sema.Function_binding_index.binding)
    =
  let expected_kind =
    match Sema.Function_collection.entry_kind entry with
    | Sema.Function_collection.Named_parameter ->
        Sema.Function_binding_index.Named_parameter
    | Sema.Function_collection.Variadic_argc ->
        Sema.Function_binding_index.Variadic_argc
    | Sema.Function_collection.Variadic_argv ->
        Sema.Function_binding_index.Variadic_argv
    | Sema.Function_collection.Automatic_local ->
        Sema.Function_binding_index.Automatic_local
    | Sema.Function_collection.Static_local ->
        Sema.Function_binding_index.Static_local
  in
  same_symbol (Sema.Function_collection.entry_symbol entry) binding.symbol
  && Sema.Function_collection.entry_parameter_index entry
     = binding.parameter_index
  && Sema.Function_collection.entry_local_declaration_index entry
     = binding.local_declaration_index
  && Sema.Function_collection.entry_declarator_index entry
     = binding.local_declarator_index
  && expected_kind = binding.kind

let validate_entries collected indexed =
  let rec pair entries bindings =
    match (entries, bindings) with
    | [], [] -> Ok ()
    | entry :: entry_rest, binding :: binding_rest ->
        if binding_matches_entry entry binding then pair entry_rest binding_rest
        else
          Error
            "function expression binding entries do not match the function \
             collection"
    | [], _ :: _ | _ :: _, [] ->
        Error
          "function expression binding count does not match the function \
           collection"
  in
  pair
    (Sema.Function_collection.function_entries collected)
    (Sema.Function_binding_index.function_bindings indexed)

let local_binding_kind_matches local
    (binding : Sema.Function_binding_index.binding) =
  match (Sema.Local_type_resolution.local_storage local, binding.kind) with
  | ( Sema.Local_type_resolution.Automatic,
      Sema.Function_binding_index.Automatic_local )
  | Sema.Local_type_resolution.Static, Sema.Function_binding_index.Static_local
    -> true
  | ( (Sema.Local_type_resolution.Automatic | Sema.Local_type_resolution.Static),
      ( Sema.Function_binding_index.Named_parameter
      | Sema.Function_binding_index.Variadic_argc
      | Sema.Function_binding_index.Variadic_argv
      | Sema.Function_binding_index.Automatic_local
      | Sema.Function_binding_index.Static_local ) ) -> false

let validate_locals local_types indexed =
  let locals = Sema.Local_type_resolution.function_locals local_types in
  let bindings =
    Sema.Function_binding_index.function_bindings indexed
    |> List.filter (fun (binding : Sema.Function_binding_index.binding) ->
        match binding.kind with
        | Sema.Function_binding_index.Automatic_local
        | Sema.Function_binding_index.Static_local -> true
        | Sema.Function_binding_index.Named_parameter
        | Sema.Function_binding_index.Variadic_argc
        | Sema.Function_binding_index.Variadic_argv -> false)
  in
  let rec pair locals bindings =
    match (locals, bindings) with
    | [], [] -> Ok ()
    | local :: local_rest, binding :: binding_rest ->
        if
          local_binding_kind_matches local binding
          && same_symbol
               (Sema.Local_type_resolution.local_symbol local)
               binding.symbol
          && Some (Sema.Local_type_resolution.local_declaration_index local)
             = binding.local_declaration_index
          && Some (Sema.Local_type_resolution.local_declarator_index local)
             = binding.local_declarator_index
        then pair local_rest binding_rest
        else
          Error "function expression bindings do not match resolved local types"
    | [], _ :: _ | _ :: _, [] ->
        Error
          "function expression local count does not match resolved local types"
  in
  pair locals bindings

let function_input table collected local_types indexed (item_index, ast) =
  let collected_symbol = Sema.Function_collection.function_symbol collected in
  let collected_scope = Sema.Function_collection.function_scope collected in
  let collected_item = Sema.Function_collection.function_item_index collected in
  let local_symbol = Sema.Local_type_resolution.function_symbol local_types in
  let local_scope = Sema.Local_type_resolution.function_scope local_types in
  let local_item = Sema.Local_type_resolution.function_item_index local_types in
  let indexed_symbol = Sema.Function_binding_index.function_symbol indexed in
  let indexed_scope = Sema.Function_binding_index.function_scope indexed in
  let indexed_item = Sema.Function_binding_index.function_item_index indexed in
  let name, body = function_header ast in
  if
    not
      (Sema.Symbol_table.owns_symbol table collected_symbol
      && Sema.Symbol_table.owns_symbol table local_symbol
      && Sema.Symbol_table.owns_symbol table indexed_symbol)
  then Error "function expression inputs belong to another symbol table"
  else if
    not
      (Sema.Symbol_table.owns_scope table collected_scope
      && Sema.Symbol_table.owns_scope table local_scope
      && Sema.Symbol_table.owns_scope table indexed_scope)
  then Error "function expression scopes belong to another symbol table"
  else if
    not
      (same_symbol collected_symbol local_symbol
      && same_symbol collected_symbol indexed_symbol)
  then Error "function expression inputs have different function symbols"
  else if
    not
      (same_scope collected_scope local_scope
      && same_scope collected_scope indexed_scope)
  then Error "function expression inputs have different function scopes"
  else if
    collected_item <> item_index
    || local_item <> item_index || indexed_item <> item_index
  then Error "function expression inputs have different module positions"
  else if not (String.equal (Sema.Symbol.name collected_symbol) name.spelling)
  then Error "function expression input does not match the AST name"
  else if Sema.Symbol.origin collected_symbol <> origin name then
    Error "function expression input does not match the AST origin"
  else
    match validate_entries collected indexed with
    | Error _ as error -> error
    | Ok () -> (
        match validate_locals local_types indexed with
        | Error _ as error -> error
        | Ok () -> (
            match events body with
            | Error _ as error -> error
            | Ok events ->
                Sema.Function_expression_binding.make_function
                  ~symbol:collected_symbol ~scope:collected_scope ~item_index
                  events))

let function_inputs table functions local_types bindings module_ =
  let rec pair inputs_rev functions local_types bindings ast =
    match (functions, local_types, bindings, ast) with
    | [], [], [], [] -> Ok (List.rev inputs_rev)
    | ( collected :: function_rest,
        local :: local_rest,
        indexed :: binding_rest,
        ast_function :: ast_rest ) -> (
        match function_input table collected local indexed ast_function with
        | Error _ as error -> error
        | Ok input ->
            pair (input :: inputs_rev) function_rest local_rest binding_rest
              ast_rest)
    | [], _, _, _ | _, [], _, _ | _, _, [], _ | _, _, _, [] ->
        Error
          "function expression pass inputs contain different function counts"
  in
  pair []
    (Sema.Function_collection.functions functions)
    (Sema.Local_type_resolution.functions local_types)
    (Sema.Function_binding_index.functions bindings)
    (ast_functions module_)

let resolve ~table ~declarations ~functions ~local_types ~bindings module_ =
  let parent = Sema.Declaration_collection.scope declarations in
  let result =
    if not (Sema.Symbol_table.owns_scope table parent) then
      Error "function expression declarations belong to another symbol table"
    else if Sema.Symbol_table.scope_kind parent <> Sema.Symbol_table.Module then
      Error "function expression binding requires a module declaration scope"
    else
      match function_inputs table functions local_types bindings module_ with
      | Error _ as error -> error
      | Ok inputs ->
          Sema.Function_expression_binding.resolve ~table ~parent ~bindings
            inputs
          |> Result.map_error Sema.Function_expression_binding.error_to_string
  in
  Result.map_error
    (fun message ->
      if String.starts_with ~prefix:"HCSEMA" message then message
      else "HCSEMA0017: " ^ message)
    result
