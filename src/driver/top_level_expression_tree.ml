let origin (location : Frontend.Ast.location) =
  Sema.Symbol.Source_location
    {
      span = location.span;
      source_segments = location.source_segments;
      generated_from = location.generated_from;
      defined_at = location.defined_at;
    }

type state = {
  module_expressions : Sema.Module_expression_binding.t;
  item_index : int;
  occurrences : Sema.Top_level_outer_expression_binding.occurrence array;
  occurrence_cursor : int;
  next_occurrence : int;
  next_root : int;
  next_call : int;
  next_expression_statement : int;
  next_output : int;
  next_condition : int;
  next_selector : int;
  next_case : int;
  next_local_declaration : int;
  next_return : int;
  roots_rev : Sema.Top_level_expression_tree.root list;
  calls_rev : Sema.Top_level_expression_tree.call list;
}

let initial_state ~next_occurrence ~next_root ~next_call
    ~next_expression_statement ~next_output ~next_condition ~next_selector
    ~next_case ~next_local_declaration ~next_return ~module_expressions
    ~item_index occurrences =
  {
    module_expressions;
    item_index;
    occurrences = Array.of_list occurrences;
    occurrence_cursor = 0;
    next_occurrence;
    next_root;
    next_call;
    next_expression_statement;
    next_output;
    next_condition;
    next_selector;
    next_case;
    next_local_declaration;
    next_return;
    roots_rev = [];
    calls_rev = [];
  }

let increment label value =
  if value = max_int then Error (label ^ " identity space is exhausted")
  else Ok (value + 1)

let fold_result apply state values =
  let rec loop state = function
    | [] -> Ok state
    | value :: rest -> (
        match apply state value with
        | Error _ as error -> error
        | Ok state -> loop state rest)
  in
  loop state values

let prefix_operator = function
  | Frontend.Ast.Unary_plus -> Sema.Function_call_resolution.Unary_plus
  | Frontend.Ast.Unary_minus -> Sema.Function_call_resolution.Unary_minus
  | Frontend.Ast.Logical_not -> Sema.Function_call_resolution.Logical_not
  | Frontend.Ast.Bitwise_not -> Sema.Function_call_resolution.Bitwise_not
  | Frontend.Ast.Dereference -> Sema.Function_call_resolution.Dereference
  | Frontend.Ast.Address_of -> Sema.Function_call_resolution.Address_of
  | Frontend.Ast.Pre_increment -> Sema.Function_call_resolution.Pre_increment
  | Frontend.Ast.Pre_decrement -> Sema.Function_call_resolution.Pre_decrement

let postfix_operator = function
  | Frontend.Ast.Post_increment -> Sema.Function_call_resolution.Post_increment
  | Frontend.Ast.Post_decrement -> Sema.Function_call_resolution.Post_decrement

let call_syntax (call : Frontend.Ast.call_expression) =
  match call.call_syntax with
  | Frontend.Ast.Parenthesized_call _ ->
      Sema.Function_call_resolution.Parenthesized
  | Frontend.Ast.Parenthesis_free_call ->
      Sema.Function_call_resolution.Parenthesis_free

let rec identifier_callee dereference_depth = function
  | Frontend.Ast.Identifier_expression identifier ->
      let form =
        if dereference_depth = 0 then
          Sema.Function_call_resolution.Identifier_callee
        else
          Sema.Function_call_resolution.Dereferenced_identifier_callee
            dereference_depth
      in
      Some (identifier, form)
  | Frontend.Ast.Parenthesized_expression grouped ->
      identifier_callee dereference_depth grouped.grouped_expression
  | Frontend.Ast.Prefix_expression prefix
    when prefix.prefix_operator_kind = Frontend.Ast.Dereference ->
      identifier_callee (dereference_depth + 1) prefix.prefix_operand
  | _ -> None

let rec first_identifier = function
  | Frontend.Ast.Identifier_expression identifier -> Some identifier
  | Frontend.Ast.Parenthesized_expression grouped ->
      first_identifier grouped.grouped_expression
  | Frontend.Ast.Prefix_expression prefix ->
      first_identifier prefix.prefix_operand
  | Frontend.Ast.Postfix_expression postfix ->
      first_identifier postfix.postfix_operand
  | Frontend.Ast.Postfix_cast_expression cast ->
      first_identifier cast.cast_operand
  | Frontend.Ast.Index_expression index -> first_identifier index.index_base
  | Frontend.Ast.Member_expression member -> first_identifier member.member_base
  | Frontend.Ast.Call_expression _
  | Frontend.Ast.Binary_expression _
  | Frontend.Ast.Sizeof_expression _
  | Frontend.Ast.Offset_expression _
  | Frontend.Ast.Defined_expression _
  | Frontend.Ast.Integer_literal _
  | Frontend.Ast.Float_literal _
  | Frontend.Ast.Character_literal _
  | Frontend.Ast.String_literal _
  | Frontend.Ast.Current_position_expression _ -> None

let take_occurrence state (identifier : Frontend.Ast.identifier) =
  if state.occurrence_cursor >= Array.length state.occurrences then
    Error "top-level expression has no bound identifier occurrence"
  else
    let occurrence = state.occurrences.(state.occurrence_cursor) in
    if
      Sema.Top_level_outer_expression_binding.occurrence_index occurrence
      <> state.next_occurrence
    then Error "top-level expression occurrence identities are not contiguous"
    else if
      not
        (String.equal identifier.spelling
           (Sema.Top_level_outer_expression_binding.occurrence_name occurrence))
    then
      Error
        "top-level expression identifier spelling does not match its binding"
    else if
      origin identifier.location
      <> Sema.Top_level_outer_expression_binding.occurrence_origin occurrence
    then
      Error "top-level expression identifier origin does not match its binding"
    else
      match increment "top-level occurrence" state.next_occurrence with
      | Error _ as error -> error
      | Ok next_occurrence ->
          Ok
            ( occurrence,
              {
                state with
                occurrence_cursor = state.occurrence_cursor + 1;
                next_occurrence;
              } )

let visible_aggregate state name =
  state.module_expressions |> Sema.Module_expression_binding.publications
  |> List.fold_left
       (fun found publication ->
         if
           Sema.Module_expression_binding.publication_item_index publication
           < state.item_index
           && Sema.Module_expression_binding.publication_kind publication
              = Sema.Module_expression_binding.Aggregate
           && String.equal
                (publication
               |> Sema.Module_expression_binding.publication_source_symbol
               |> Sema.Symbol.name)
                name
         then
           Some
             (Sema.Module_expression_binding.publication_canonical_symbol
                publication)
         else found)
       None

let cast_type_reference state = function
  | (cast : Frontend.Ast.postfix_cast_expression) -> (
      let pointer_origins =
        List.map
          (fun (layer : Frontend.Ast.pointer_layer) -> origin layer.location)
          cast.cast_pointer_layers
      in
      let pointer_depth = List.length pointer_origins in
      let make resolved_type =
        match resolved_type with
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
          make
            (Sema.Type.make_primitive ~form:Sema.Type.Public_spelling
               ~primitive:primitive.primitive ~pointer_depth)
      | Frontend.Ast.Internal_type_specifier internal ->
          make
            (Sema.Type.make_primitive ~form:Sema.Type.Internal_storage
               ~primitive:internal.primitive ~pointer_depth)
      | Frontend.Ast.Named_type_specifier identifier -> (
          match visible_aggregate state identifier.spelling with
          | None ->
              Error
                (Printf.sprintf
                   "named top-level postfix cast %S has no source-visible \
                    aggregate identity"
                   identifier.spelling)
          | Some symbol ->
              make (Sema.Type.make_aggregate ~symbol ~pointer_depth)))

let rec expression state (source : Frontend.Ast.expression) =
  let finish state kind =
    Ok
      ( state,
        Sema.Function_call_resolution.make_argument_expression ~kind
          ~origin:(origin (Frontend.Ast.expression_location source)) )
  in
  match source with
  | Frontend.Ast.Integer_literal _ ->
      finish state Sema.Function_call_resolution.Integer_literal
  | Frontend.Ast.Float_literal _ ->
      finish state Sema.Function_call_resolution.Float_literal
  | Frontend.Ast.Character_literal _ ->
      finish state Sema.Function_call_resolution.Character_literal
  | Frontend.Ast.String_literal _ ->
      finish state Sema.Function_call_resolution.String_literal
  | Frontend.Ast.Identifier_expression identifier -> (
      match take_occurrence state identifier with
      | Error _ as error -> error
      | Ok (occurrence, state) -> (
          match
            Sema.Function_call_resolution
            .make_top_level_bound_identifier_argument_expression ~occurrence
          with
          | Error _ as error -> error
          | Ok kind -> finish state kind))
  | Frontend.Ast.Parenthesized_expression grouped -> (
      match expression state grouped.grouped_expression with
      | Error _ as error -> error
      | Ok (state, grouped) ->
          finish state
            (Sema.Function_call_resolution.Parenthesized_expression grouped))
  | Frontend.Ast.Prefix_expression prefix -> (
      match expression state prefix.prefix_operand with
      | Error _ as error -> error
      | Ok (state, operand) -> (
          match
            Sema.Function_call_resolution.make_prefix_argument_expression
              ~operator:(prefix_operator prefix.prefix_operator_kind)
              ~operator_origin:(origin prefix.prefix_operator.operator_location)
              ~operand
          with
          | Error _ as error -> error
          | Ok kind -> finish state kind))
  | Frontend.Ast.Postfix_expression postfix -> (
      match expression state postfix.postfix_operand with
      | Error _ as error -> error
      | Ok (state, operand) -> (
          match
            Sema.Function_call_resolution.make_postfix_argument_expression
              ~operator:(postfix_operator postfix.postfix_operator_kind)
              ~operator_origin:
                (origin postfix.postfix_operator.operator_location)
              ~operand
          with
          | Error _ as error -> error
          | Ok kind -> finish state kind))
  | Frontend.Ast.Postfix_cast_expression cast -> (
      match expression state cast.cast_operand with
      | Error _ as error -> error
      | Ok (state, operand) -> (
          match cast_type_reference state cast with
          | Error _ as error -> error
          | Ok target ->
              finish state
                (Sema.Function_call_resolution.Postfix_cast_expression
                   (operand, target))))
  | Frontend.Ast.Binary_expression binary -> (
      match expression state binary.binary_left with
      | Error _ as error -> error
      | Ok (state, left) -> (
          match expression state binary.binary_right with
          | Error _ as error -> error
          | Ok (state, right) -> (
              match
                Generated.Intermediate_codes.of_source_name
                  binary.binary_operator_spec.ic_name
              with
              | None ->
                  Error
                    (Printf.sprintf
                       "binary operator %S has no checked IC identity"
                       binary.binary_operator_spec.ic_name)
              | Some operator -> (
                  match
                    Sema.Function_call_resolution
                    .make_binary_argument_expression ~operator
                      ~operator_origin:
                        (origin binary.binary_operator.operator_location)
                      ~left ~right
                  with
                  | Error _ as error -> error
                  | Ok kind -> finish state kind))))
  | Frontend.Ast.Index_expression index -> (
      match expression state index.index_base with
      | Error _ as error -> error
      | Ok (state, base) -> (
          match expression state index.index_value with
          | Error _ as error -> error
          | Ok (state, value) -> (
              match
                Sema.Function_call_resolution.make_index_argument_expression
                  ~base
                  ~opening_origin:(origin index.index_opening_bracket)
                  ~index:value
                  ~closing_origin:(origin index.index_closing_bracket)
              with
              | Error _ as error -> error
              | Ok kind -> finish state kind)))
  | Frontend.Ast.Member_expression member -> (
      match expression state member.member_base with
      | Error _ as error -> error
      | Ok (state, base) -> (
          let access_kind =
            match member.member_access_kind with
            | Frontend.Ast.Direct_member ->
                Sema.Function_call_resolution.Direct_member
            | Frontend.Ast.Pointer_member ->
                Sema.Function_call_resolution.Pointer_member
          in
          match
            Sema.Function_call_resolution.make_member_argument_expression ~base
              ~access_kind
              ~operator_origin:(origin member.member_operator.operator_location)
              ~member_name:member.member_name.spelling
              ~member_origin:(origin member.member_name.location)
          with
          | Error _ as error -> error
          | Ok kind -> finish state kind))
  | Frontend.Ast.Call_expression call -> call_expression state source call
  | Frontend.Ast.Current_position_expression _ ->
      finish state
        (Sema.Function_call_resolution.Unresolved_expression
           Sema.Function_call_resolution.Current_position_expression)
  | Frontend.Ast.Sizeof_expression _ ->
      finish state
        (Sema.Function_call_resolution.Unresolved_expression
           Sema.Function_call_resolution.Sizeof_expression)
  | Frontend.Ast.Offset_expression _ ->
      finish state
        (Sema.Function_call_resolution.Unresolved_expression
           Sema.Function_call_resolution.Offset_expression)
  | Frontend.Ast.Defined_expression _ ->
      finish state
        (Sema.Function_call_resolution.Unresolved_expression
           Sema.Function_call_resolution.Defined_expression)

and call_expression state source (call : Frontend.Ast.call_expression) =
  let result_expression =
    Sema.Function_call_resolution.make_argument_expression
      ~kind:
        (Sema.Function_call_resolution.Unresolved_expression
           Sema.Function_call_resolution.Call_expression)
      ~origin:(origin (Frontend.Ast.expression_location source))
  in
  let finish state = Ok (state, result_expression) in
  match first_identifier call.call_callee with
  | None -> (
      match expression state call.call_callee with
      | Error _ as error -> error
      | Ok (state, _) -> (
          match call_arguments state call.call_arguments with
          | Error _ as error -> error
          | Ok (state, _) -> finish state))
  | Some callee_identifier -> (
      let call_index = state.next_call in
      let callee_occurrence_index = state.next_occurrence in
      let callee_binding =
        if state.occurrence_cursor >= Array.length state.occurrences then None
        else Some state.occurrences.(state.occurrence_cursor)
      in
      match increment "top-level call" state.next_call with
      | Error _ as error -> error
      | Ok next_call -> (
          match expression { state with next_call } call.call_callee with
          | Error _ as error -> error
          | Ok (state, callee_expression) -> (
              match call_arguments state call.call_arguments with
              | Error _ as error -> error
              | Ok (state, arguments) -> (
                  match callee_binding with
                  | None -> Error "top-level call has no bound callee"
                  | Some callee -> (
                      let callee_form =
                        match identifier_callee 0 call.call_callee with
                        | Some (_, form) -> form
                        | None -> Sema.Function_call_resolution.Member_callee
                      in
                      let computed_callee =
                        match callee_form with
                        | Sema.Function_call_resolution.Member_callee ->
                            Some callee_expression
                        | Sema.Function_call_resolution.Identifier_callee
                        | Sema.Function_call_resolution
                          .Dereferenced_identifier_callee
                            _ -> None
                      in
                      match
                        Sema.Function_call_resolution.make_call
                          ~index:call_index ~callee_occurrence_index
                          ~callee_name:callee_identifier.spelling
                          ~callee_origin:(origin callee_identifier.location)
                          ~callee_form ?computed_callee
                          ~origin:(origin call.call_location)
                          ~syntax:(call_syntax call) arguments
                      with
                      | Error _ as error -> error
                      | Ok source_call -> (
                          match
                            Sema.Top_level_expression_tree.make_call
                              ~source:source_call ~callee ~callee_expression
                              ~result_expression
                          with
                          | Error error ->
                              Error
                                (Sema.Top_level_expression_tree.error_to_string
                                   error)
                          | Ok prepared ->
                              finish
                                {
                                  state with
                                  calls_rev = prepared :: state.calls_rev;
                                }))))))

and call_arguments state arguments =
  let rec loop index state rev = function
    | [] -> Ok (state, List.rev rev)
    | (argument : Frontend.Ast.call_argument) :: rest -> (
        let prepared =
          match argument.call_argument_value with
          | Frontend.Ast.Omitted_call_argument ->
              Ok (state, Sema.Function_call_resolution.Omitted, None)
          | Frontend.Ast.Provided_call_argument value -> (
              match expression state value with
              | Error _ as error -> error
              | Ok (state, expression) ->
                  Ok
                    ( state,
                      Sema.Function_call_resolution.Provided,
                      Some expression ))
        in
        match prepared with
        | Error _ as error -> error
        | Ok (state, kind, expression) -> (
            match
              Sema.Function_call_resolution.make_argument ~index ~kind
                ~expression
                ~origin:(origin argument.call_argument_location)
            with
            | Error _ as error -> error
            | Ok argument -> (
                match increment "top-level call argument" index with
                | Error _ as error -> error
                | Ok next_index -> loop next_index state (argument :: rev) rest)
            ))
  in
  loop 0 state [] arguments

let add_root state role source =
  match expression state source with
  | Error _ as error -> error
  | Ok (state, expression) -> (
      match increment "top-level expression root" state.next_root with
      | Error _ as error -> error
      | Ok next_root -> (
          match
            Sema.Top_level_expression_tree.make_root ~index:state.next_root
              ~role ~expression
              ~origin:(origin (Frontend.Ast.expression_location source))
          with
          | Error error ->
              Error (Sema.Top_level_expression_tree.error_to_string error)
          | Ok root ->
              Ok { state with next_root; roots_rev = root :: state.roots_rev }))

let record_expression_statement state
    (statement : Frontend.Ast.expression_statement) =
  let index = state.next_expression_statement in
  match increment "top-level expression statement" index with
  | Error _ as error -> error
  | Ok next_expression_statement ->
      add_root
        { state with next_expression_statement }
        (Sema.Top_level_expression_tree.Expression_statement
           { statement_index = index })
        statement.expression_statement_expression

let record_condition state role keyword source =
  let index = state.next_condition in
  match increment "top-level condition" index with
  | Error _ as error -> error
  | Ok next_condition ->
      add_root
        { state with next_condition }
        (Sema.Top_level_expression_tree.Condition
           { condition_index = index; role; keyword_origin = origin keyword })
        source

let record_selector state mode keyword source =
  let index = state.next_selector in
  match increment "top-level switch selector" index with
  | Error _ as error -> error
  | Ok next_selector ->
      add_root
        { state with next_selector }
        (Sema.Top_level_expression_tree.Switch_selector
           { selector_index = index; mode; keyword_origin = origin keyword })
        source

let record_output state (output : Frontend.Ast.implicit_output_statement) =
  let output_index = state.next_output in
  let target =
    match output.target with
    | Frontend.Ast.Print_target -> Sema.Function_call_resolution.Print_output
    | Frontend.Ast.Put_chars_target ->
        Sema.Function_call_resolution.Put_chars_output
  in
  let fixed, source =
    match output.fixed_argument with
    | Frontend.Ast.Marker_fixed_argument value ->
        (value, Sema.Function_call_resolution.Marker_fixed_output)
    | Frontend.Ast.Expression_fixed_argument value ->
        (value, Sema.Function_call_resolution.Following_expression_output)
  in
  match increment "top-level implicit output" output_index with
  | Error _ as error -> error
  | Ok next_output -> (
      match
        add_root { state with next_output }
          (Sema.Top_level_expression_tree.Implicit_output_fixed
             {
               output_index;
               target;
               source;
               marker_origin = origin output.marker.literal_location;
             })
          fixed
      with
      | Error _ as error -> error
      | Ok state ->
          output.arguments
          |> List.mapi (fun argument_index argument ->
              (argument_index, argument))
          |> fold_result
               (fun state
                    ( argument_index,
                      (argument : Frontend.Ast.implicit_output_argument) ) ->
                 add_root state
                   (Sema.Top_level_expression_tree.Implicit_output_argument
                      { output_index; argument_index })
                   argument.value)
               state)

let rec record_initializer state ~declaration_index ~declarator_index path =
  function
  | Frontend.Ast.Scalar_initializer value ->
      add_root state
        (Sema.Top_level_expression_tree.Local_initializer
           { declaration_index; declarator_index; element_path = path })
        value
  | Frontend.Ast.Braced_initializer braced ->
      braced.initializer_elements
      |> List.mapi (fun index element -> (index, element))
      |> fold_result
           (fun state (index, (element : Frontend.Ast.initializer_element)) ->
             record_initializer state ~declaration_index ~declarator_index
               (path @ [ index ]) element.initializer_element_value)
           state

let record_local_declaration state
    (declaration : Frontend.Ast.local_declaration) =
  let declaration_index = state.next_local_declaration in
  match increment "top-level local declaration" declaration_index with
  | Error _ as error -> error
  | Ok next_local_declaration ->
      declaration.local_declarators
      |> List.mapi (fun declarator_index declarator ->
          (declarator_index, declarator))
      |> fold_result
           (fun state
                (declarator_index, (declarator : Frontend.Ast.local_declarator))
              ->
             let dimensions =
               declarator.local_array_dimensions
               |> List.mapi (fun dimension_index dimension ->
                   (dimension_index, dimension))
             in
             match
               fold_result
                 (fun state
                      ( dimension_index,
                        (dimension : Frontend.Ast.array_dimension) ) ->
                   match dimension.dimension_expression with
                   | None -> Ok state
                   | Some value ->
                       add_root state
                         (Sema.Top_level_expression_tree.Local_array_dimension
                            {
                              declaration_index;
                              declarator_index;
                              dimension_index;
                            })
                         value)
                 state dimensions
             with
             | Error _ as error -> error
             | Ok state -> (
                 match declarator.local_initializer with
                 | None -> Ok state
                 | Some initial ->
                     record_initializer state ~declaration_index
                       ~declarator_index [] initial.local_initializer_value))
           { state with next_local_declaration }

let record_case state (case_ : Frontend.Ast.switch_case_label) =
  let case_index = state.next_case in
  match increment "top-level switch case" case_index with
  | Error _ as error -> error
  | Ok next_case -> (
      let state = { state with next_case } in
      match case_.switch_case_pattern with
      | Frontend.Ast.Implicit_case -> Ok state
      | Frontend.Ast.Single_case value ->
          add_root state
            (Sema.Top_level_expression_tree.Switch_case_value
               {
                 case_index;
                 position = Sema.Top_level_expression_tree.Single_case;
               })
            value
      | Frontend.Ast.Ranged_case range -> (
          match
            add_root state
              (Sema.Top_level_expression_tree.Switch_case_value
                 {
                   case_index;
                   position = Sema.Top_level_expression_tree.Range_start;
                 })
              range.case_range_start
          with
          | Error _ as error -> error
          | Ok state ->
              add_root state
                (Sema.Top_level_expression_tree.Switch_case_value
                   {
                     case_index;
                     position = Sema.Top_level_expression_tree.Range_end;
                   })
                range.case_range_end))

let selector_mode = function
  | Frontend.Ast.Bounded_switch -> Sema.Function_call_resolution.Bounded_switch
  | Frontend.Ast.No_bound_switch ->
      Sema.Function_call_resolution.No_bound_switch

let rec statement state = function
  | Frontend.Ast.Block_statement block ->
      fold_result statement state block.block_statements
  | Frontend.Ast.Do_while_statement do_while -> (
      match statement state do_while.do_body with
      | Error _ as error -> error
      | Ok state ->
          record_condition state
            Sema.Function_call_resolution.Do_while_condition
            do_while.do_while_keyword do_while.do_while_condition)
  | Frontend.Ast.Expression_statement expression ->
      record_expression_statement state expression
  | Frontend.Ast.For_statement for_ -> (
      match statement state for_.for_initializer with
      | Error _ as error -> error
      | Ok state -> (
          match
            record_condition state Sema.Function_call_resolution.For_condition
              for_.for_keyword for_.for_condition
          with
          | Error _ as error -> error
          | Ok state -> (
              match for_.for_update with
              | None -> statement state for_.for_body
              | Some update -> (
                  match statement state update with
                  | Error _ as error -> error
                  | Ok state -> statement state for_.for_body))))
  | Frontend.Ast.If_statement if_ -> (
      match
        record_condition state Sema.Function_call_resolution.If_condition
          if_.if_keyword if_.if_condition
      with
      | Error _ as error -> error
      | Ok state -> (
          match statement state if_.if_then_branch with
          | Error _ as error -> error
          | Ok state -> (
              match if_.if_else_clause with
              | None -> Ok state
              | Some else_ -> statement state else_.else_branch)))
  | Frontend.Ast.Implicit_output_statement output -> record_output state output
  | Frontend.Ast.Local_declaration_statement declaration ->
      record_local_declaration state declaration
  | Frontend.Ast.Lock_statement lock -> statement state lock.lock_body
  | Frontend.Ast.Return_statement return_ -> (
      match return_.return_value with
      | None -> Ok state
      | Some value -> (
          let return_index = state.next_return in
          match increment "top-level return" return_index with
          | Error _ as error -> error
          | Ok next_return ->
              add_root { state with next_return }
                (Sema.Top_level_expression_tree.Return_value { return_index })
                value))
  | Frontend.Ast.Sequence_statement sequence ->
      sequence.sequence_elements
      |> fold_result
           (fun state (element : Frontend.Ast.statement_sequence_element) ->
             statement state element.sequence_statement)
           state
  | Frontend.Ast.Switch_statement switch -> (
      match
        record_selector state
          (selector_mode switch.switch_mode)
          switch.switch_keyword switch.switch_expression
      with
      | Error _ as error -> error
      | Ok state -> switch_elements state switch.switch_elements)
  | Frontend.Ast.Try_catch_statement try_catch -> (
      match statement state try_catch.try_body with
      | Error _ as error -> error
      | Ok state -> statement state try_catch.catch_body)
  | Frontend.Ast.While_statement while_ -> (
      match
        record_condition state Sema.Function_call_resolution.While_condition
          while_.while_keyword while_.while_condition
      with
      | Error _ as error -> error
      | Ok state -> statement state while_.while_body)
  | Frontend.Ast.Assembly_block_statement _
  | Frontend.Ast.Inline_assembly_statement _
  | Frontend.Ast.Break_statement _
  | Frontend.Ast.Empty_statement _
  | Frontend.Ast.Goto_statement _
  | Frontend.Ast.Label_statement _
  | Frontend.Ast.No_warn_statement _ -> Ok state

and switch_elements state elements = fold_result switch_element state elements

and switch_element state = function
  | Frontend.Ast.Switch_case_element case_ -> record_case state case_
  | Frontend.Ast.Switch_default_element _ -> Ok state
  | Frontend.Ast.Switch_subswitch_element subswitch ->
      switch_elements state subswitch.subswitch_elements
  | Frontend.Ast.Switch_statement_element statement_ ->
      statement state statement_

let ast_statements (module_ : Frontend.Ast.module_) =
  module_.items
  |> List.mapi (fun item_index item -> (item_index, item))
  |> List.filter_map (function
    | item_index, Frontend.Ast.Top_level_statement statement ->
        Some (item_index, statement)
    | _ -> None)

let source_matches_ast expected item_index ast =
  Sema.Top_level_outer_expression_binding.statement_item_index expected
  = item_index
  && Sema.Top_level_outer_expression_binding.statement_origin expected
     = origin (Frontend.Ast.statement_location ast)

let statement_input counters expected (item_index, ast) =
  if not (source_matches_ast expected item_index ast) then
    Error "top-level expression statement does not match the supplied AST"
  else
    let occurrences =
      Sema.Top_level_outer_expression_binding.statement_occurrences expected
    in
    let state =
      initial_state ~next_occurrence:counters.next_occurrence
        ~next_root:counters.next_root ~next_call:counters.next_call
        ~next_expression_statement:counters.next_expression_statement
        ~next_output:counters.next_output
        ~next_condition:counters.next_condition
        ~next_selector:counters.next_selector ~next_case:counters.next_case
        ~next_local_declaration:counters.next_local_declaration
        ~next_return:counters.next_return
        ~module_expressions:counters.module_expressions ~item_index occurrences
    in
    match statement state ast with
    | Error _ as error -> error
    | Ok state -> (
        if state.occurrence_cursor <> Array.length state.occurrences then
          Error "top-level expression traversal did not consume every binding"
        else
          let roots = List.rev state.roots_rev in
          let calls =
            state.calls_rev
            |> List.sort (fun left right ->
                Int.compare
                  (left |> Sema.Top_level_expression_tree.call_source
                 |> Sema.Function_call_resolution.call_index)
                  (right |> Sema.Top_level_expression_tree.call_source
                 |> Sema.Function_call_resolution.call_index))
          in
          match
            Sema.Top_level_expression_tree.make_statement ~source:expected
              ~roots ~calls
          with
          | Error error ->
              Error (Sema.Top_level_expression_tree.error_to_string error)
          | Ok prepared -> Ok (state, prepared))

let build_statements source module_ =
  let rec loop counters rev expected ast =
    match (expected, ast) with
    | [], [] -> Ok (List.rev rev)
    | expected :: expected_rest, ast :: ast_rest -> (
        match statement_input counters expected ast with
        | Error _ as error -> error
        | Ok (counters, statement) ->
            loop counters (statement :: rev) expected_rest ast_rest)
    | _ -> Error "top-level expression statement count does not match the AST"
  in
  let counters =
    let module_expressions =
      source |> Sema.Top_level_outer_expression_binding.source
      |> Sema.Top_level_expression_binding.module_expressions
    in
    initial_state ~next_occurrence:0 ~next_root:0 ~next_call:0
      ~next_expression_statement:0 ~next_output:0 ~next_condition:0
      ~next_selector:0 ~next_case:0 ~next_local_declaration:0 ~next_return:0
      ~module_expressions ~item_index:0 []
  in
  loop counters []
    (Sema.Top_level_outer_expression_binding.statements source)
    (ast_statements module_)

let build ~table ~declarations ~compilation_mode ~expressions module_ =
  let parent = Sema.Declaration_collection.scope declarations in
  let environment =
    Sema.Top_level_outer_expression_binding.environment expressions
  in
  let result =
    if not (Sema.Symbol_table.owns_scope table parent) then
      Error "top-level expression declarations belong to another symbol table"
    else if Sema.Symbol_table.scope_kind parent <> Sema.Symbol_table.Module then
      Error "top-level expression trees require a module declaration scope"
    else if
      not (Sema.Top_level_outer_expression_binding.owns_table expressions table)
    then Error "top-level expression bindings belong to another symbol table"
    else if
      Sema.Outer_environment.compilation_mode environment <> compilation_mode
    then
      Error
        "top-level expression compilation mode does not match its outer \
         environment"
    else
      match build_statements expressions module_ with
      | Error _ as error -> error
      | Ok statements ->
          Sema.Top_level_expression_tree.create ~table ~source:expressions
            statements
          |> Result.map_error Sema.Top_level_expression_tree.error_to_string
  in
  Result.map_error
    (fun message ->
      if String.starts_with ~prefix:"HCSEMA" message then message
      else "HCSEMA0055: " ^ message)
    result
