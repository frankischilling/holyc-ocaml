let origin (location : Frontend.Ast.location) =
  Sema.Symbol.Source_location
    {
      span = location.span;
      source_segments = location.source_segments;
      generated_from = location.generated_from;
      defined_at = location.defined_at;
    }

module String_map = Map.Make (String)
module Int_map = Map.Make (Int)

type typed_value = {
  resolved_type : Sema.Type.t;
  shape : Sema.Function_call_resolution.identifier_value_shape;
  array_rank : int;
}

type typed_environment = typed_value Int_map.t

let symbol_number symbol = Sema.Symbol.id symbol |> Sema.Symbol.Id.to_int

let same_symbol left right =
  Sema.Symbol.Id.equal (Sema.Symbol.id left) (Sema.Symbol.id right)

let same_scope left right =
  Sema.Symbol.Scope_id.equal
    (Sema.Symbol_table.scope_id left)
    (Sema.Symbol_table.scope_id right)

let object_value reference =
  {
    resolved_type = Sema.Type_reference.resolved_type reference;
    shape = Sema.Function_call_resolution.Object_value;
    array_rank = 0;
  }

let parameter_value parameter =
  match Sema.Function_type_resolution.parameter_declarator_kind parameter with
  | Sema.Function_type_resolution.Object ->
      object_value
        (Sema.Function_type_resolution.parameter_type_reference parameter)
  | Sema.Function_type_resolution.Function_pointer _ ->
      {
        resolved_type =
          parameter |> Sema.Function_type_resolution.parameter_type_reference
          |> Sema.Type_reference.resolved_type;
        shape = Sema.Function_call_resolution.Function_pointer_value;
        array_rank = 0;
      }

let synthetic_value binding =
  let shape, array_rank =
    match Sema.Function_type_resolution.synthetic_binding_shape binding with
    | Sema.Function_type_resolution.Scalar ->
        (Sema.Function_call_resolution.Object_value, 0)
    | Sema.Function_type_resolution.Array _ ->
        (Sema.Function_call_resolution.Array_value, 1)
  in
  {
    resolved_type = Sema.Function_type_resolution.synthetic_binding_type binding;
    shape;
    array_rank;
  }

let local_value local =
  let dimensions = Sema.Local_type_resolution.local_array_dimensions local in
  let shape =
    match Sema.Local_type_resolution.local_declarator_kind local with
    | Sema.Local_type_resolution.Function_pointer _ ->
        Sema.Function_call_resolution.Function_pointer_value
    | Sema.Local_type_resolution.Object ->
        if dimensions = [] then
          Sema.Function_call_resolution.Object_value
        else Sema.Function_call_resolution.Array_value
  in
  {
    resolved_type =
      local |> Sema.Local_type_resolution.local_type_reference
      |> Sema.Type_reference.resolved_type;
    shape;
    array_rank = List.length dimensions;
  }

let global_value global =
  let dimensions = Sema.Global_type_resolution.global_array_dimensions global in
  let shape =
    match Sema.Global_type_resolution.global_declarator_kind global with
    | Sema.Global_type_resolution.Function_pointer _ ->
        Sema.Function_call_resolution.Function_pointer_value
    | Sema.Global_type_resolution.Object ->
        if dimensions = [] then
          Sema.Function_call_resolution.Object_value
        else Sema.Function_call_resolution.Array_value
  in
  {
    resolved_type =
      global |> Sema.Global_type_resolution.global_type_reference
      |> Sema.Type_reference.resolved_type;
    shape;
    array_rank = List.length dimensions;
  }

let add_typed_value symbol value values =
  Int_map.add (symbol_number symbol) value values

let typed_parameter_values function_ =
  let parameters =
    function_ |> Sema.Function_type_resolution.function_signature
    |> Sema.Function_type_resolution.signature_parameters
  in
  let rec add_named values = function
    | [] -> Ok values
    | binding :: rest -> (
        let index =
          Sema.Function_type_resolution.parameter_binding_index binding
        in
        match
          List.find_opt
            (fun parameter ->
              Sema.Function_type_resolution.parameter_index parameter = index)
            parameters
        with
        | None -> Error "call argument parameter binding has no checked type"
        | Some parameter ->
            add_named
              (add_typed_value
                 (Sema.Function_type_resolution.parameter_binding_symbol binding)
                 (parameter_value parameter)
                 values)
              rest)
  in
  match
    add_named Int_map.empty
      (Sema.Function_type_resolution.function_parameter_bindings function_)
  with
  | Error _ as error -> error
  | Ok values -> (
      match
        Sema.Function_type_resolution.function_variadic_bindings function_
      with
      | None -> Ok values
      | Some variadic ->
          let argc = Sema.Function_type_resolution.variadic_argc variadic in
          let argv = Sema.Function_type_resolution.variadic_argv variadic in
          Ok
            (values
            |> add_typed_value
                 (Sema.Function_type_resolution.synthetic_binding_symbol argc)
                 (synthetic_value argc)
            |> add_typed_value
                 (Sema.Function_type_resolution.synthetic_binding_symbol argv)
                 (synthetic_value argv)))

let function_typed_environment table expected typed locals =
  let expected_symbol =
    Sema.Module_expression_binding.function_symbol expected
  in
  let expected_scope = Sema.Module_expression_binding.function_scope expected in
  let expected_item =
    Sema.Module_expression_binding.function_item_index expected
  in
  let typed_symbol = Sema.Function_type_resolution.function_symbol typed in
  let typed_scope = Sema.Function_type_resolution.function_scope typed in
  let local_symbol = Sema.Local_type_resolution.function_symbol locals in
  let local_scope = Sema.Local_type_resolution.function_scope locals in
  if
    not
      (Sema.Symbol_table.owns_symbol table typed_symbol
      && Sema.Symbol_table.owns_symbol table local_symbol)
  then
    Error "call argument function type results belong to another symbol table"
  else if
    not
      (Sema.Symbol_table.owns_scope table typed_scope
      && Sema.Symbol_table.owns_scope table local_scope)
  then Error "call argument function type scopes belong to another symbol table"
  else if
    not
      (same_symbol expected_symbol typed_symbol
      && same_symbol expected_symbol local_symbol)
  then Error "call argument type results have different function symbols"
  else if
    not
      (same_scope expected_scope typed_scope
      && same_scope expected_scope local_scope)
  then Error "call argument type results have different function scopes"
  else if
    expected_item <> Sema.Function_type_resolution.function_item_index typed
    || expected_item <> Sema.Local_type_resolution.function_item_index locals
  then Error "call argument type results have different module positions"
  else
    match typed_parameter_values typed with
    | Error _ as error -> error
    | Ok values ->
        Ok
          (List.fold_left
             (fun values local ->
               add_typed_value
                 (Sema.Local_type_resolution.local_symbol local)
                 (local_value local) values)
             values
             (Sema.Local_type_resolution.function_locals locals))

let global_typed_environment table globals =
  let rec loop values = function
    | [] -> Ok values
    | global :: rest ->
        let symbol = Sema.Global_type_resolution.global_symbol global in
        if not (Sema.Symbol_table.owns_symbol table symbol) then
          Error "call argument global type belongs to another symbol table"
        else if Int_map.mem (symbol_number symbol) values then
          Error "call argument global type symbol is repeated"
        else loop (add_typed_value symbol (global_value global) values) rest
  in
  loop Int_map.empty (Sema.Global_type_resolution.globals globals)

let occurrence_map occurrences =
  List.fold_left
    (fun map occurrence ->
      Int_map.add
        (Sema.Module_expression_binding.occurrence_index occurrence)
        occurrence map)
    Int_map.empty occurrences

let occurrence_at occurrences index (identifier : Frontend.Ast.identifier) =
  match Int_map.find_opt index occurrences with
  | None -> Error "call argument identifier has no bound occurrence"
  | Some occurrence ->
      if
        not
          (String.equal identifier.Frontend.Ast.spelling
             (Sema.Module_expression_binding.occurrence_name occurrence))
      then
        Error "call argument identifier spelling does not match its occurrence"
      else if
        origin identifier.location
        <> Sema.Module_expression_binding.occurrence_origin occurrence
      then Error "call argument identifier origin does not match its occurrence"
      else Ok occurrence

let typed_value_for_occurrence locals globals occurrence =
  match Sema.Module_expression_binding.occurrence_resolution occurrence with
  | Sema.Module_expression_binding.Local_binding binding ->
      Int_map.find_opt
        (binding |> Sema.Function_binding_index.binding_symbol |> symbol_number)
        locals
  | Sema.Module_expression_binding.Module_binding publication
    when Sema.Module_expression_binding.publication_kind publication
         = Sema.Module_expression_binding.Global_variable ->
      Int_map.find_opt
        (publication |> Sema.Module_expression_binding.publication_source_symbol
       |> symbol_number)
        globals
  | Sema.Module_expression_binding.Module_binding _
  | Sema.Module_expression_binding.Outer_candidate -> None

type state = {
  next_occurrence : int;
  next_call : int;
  calls_rev : Sema.Function_call_resolution.call list;
  visible_aggregates : Sema.Symbol.t String_map.t;
  typed_values : typed_environment;
  global_values : typed_environment;
  occurrences : Sema.Module_expression_binding.occurrence Int_map.t;
}

let empty_state visible_aggregates typed_values global_values occurrences =
  {
    next_occurrence = 0;
    next_call = 0;
    calls_rev = [];
    visible_aggregates;
    typed_values;
    global_values;
    occurrences;
  }

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

let cast_type_reference visible (cast : Frontend.Ast.postfix_cast_expression) =
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
      match String_map.find_opt identifier.spelling visible with
      | None ->
          Error
            (Printf.sprintf
               "named postfix cast %S has no source-visible aggregate identity"
               identifier.spelling)
      | Some symbol -> make (Sema.Type.make_aggregate ~symbol ~pointer_depth))

let take_occurrence occurrences cursor identifier =
  match occurrence_at occurrences !cursor identifier with
  | Error _ as error -> error
  | Ok occurrence ->
      if !cursor = max_int then
        Error "function call occurrence space is exhausted"
      else (
        cursor := !cursor + 1;
        Ok occurrence)

let rec advance_expression_occurrences occurrences cursor = function
  | Frontend.Ast.Identifier_expression identifier ->
      Result.map (fun _ -> ()) (take_occurrence occurrences cursor identifier)
  | Frontend.Ast.Parenthesized_expression grouped ->
      advance_expression_occurrences occurrences cursor
        grouped.grouped_expression
  | Frontend.Ast.Prefix_expression prefix ->
      advance_expression_occurrences occurrences cursor prefix.prefix_operand
  | Frontend.Ast.Postfix_expression postfix ->
      advance_expression_occurrences occurrences cursor postfix.postfix_operand
  | Frontend.Ast.Postfix_cast_expression cast ->
      advance_expression_occurrences occurrences cursor cast.cast_operand
  | Frontend.Ast.Binary_expression binary -> (
      match
        advance_expression_occurrences occurrences cursor binary.binary_left
      with
      | Error _ as error -> error
      | Ok () ->
          advance_expression_occurrences occurrences cursor binary.binary_right)
  | Frontend.Ast.Call_expression call -> (
      match
        advance_expression_occurrences occurrences cursor call.call_callee
      with
      | Error _ as error -> error
      | Ok () ->
          fold_result
            (fun () argument ->
              match argument.Frontend.Ast.call_argument_value with
              | Frontend.Ast.Omitted_call_argument -> Ok ()
              | Frontend.Ast.Provided_call_argument value ->
                  advance_expression_occurrences occurrences cursor value)
            () call.call_arguments)
  | Frontend.Ast.Index_expression index -> (
      match
        advance_expression_occurrences occurrences cursor index.index_base
      with
      | Error _ as error -> error
      | Ok () ->
          advance_expression_occurrences occurrences cursor index.index_value)
  | Frontend.Ast.Member_expression member ->
      advance_expression_occurrences occurrences cursor member.member_base
  | Frontend.Ast.Sizeof_expression _
  | Frontend.Ast.Offset_expression _
  | Frontend.Ast.Defined_expression _
  | Frontend.Ast.Integer_literal _
  | Frontend.Ast.Float_literal _
  | Frontend.Ast.Character_literal _
  | Frontend.Ast.String_literal _
  | Frontend.Ast.Current_position_expression _ -> Ok ()

let rec argument_expression visible locals globals occurrences cursor
    (expression : Frontend.Ast.expression) =
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
        match
          argument_expression visible locals globals occurrences cursor
            grouped.grouped_expression
        with
        | Error _ as error -> error
        | Ok grouped ->
            Ok (Sema.Function_call_resolution.Parenthesized_expression grouped))
    | Frontend.Ast.Identifier_expression identifier -> (
        match take_occurrence occurrences cursor identifier with
        | Error _ as error -> error
        | Ok occurrence -> (
            match typed_value_for_occurrence locals globals occurrence with
            | None ->
                Ok
                  (Sema.Function_call_resolution.Unresolved_expression
                     Sema.Function_call_resolution.Identifier_expression)
            | Some value ->
                Sema.Function_call_resolution
                .make_bound_identifier_argument_expression ~occurrence
                  ~resolved_type:value.resolved_type ~shape:value.shape
                  ~array_rank:value.array_rank))
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
    | Frontend.Ast.Prefix_expression prefix -> (
        match
          argument_expression visible locals globals occurrences cursor
            prefix.prefix_operand
        with
        | Error _ as error -> error
        | Ok operand ->
            Sema.Function_call_resolution.make_prefix_argument_expression
              ~operator:(prefix_operator prefix.prefix_operator_kind)
              ~operator_origin:(origin prefix.prefix_operator.operator_location)
              ~operand)
    | Frontend.Ast.Postfix_expression postfix -> (
        match
          argument_expression visible locals globals occurrences cursor
            postfix.postfix_operand
        with
        | Error _ as error -> error
        | Ok operand ->
            Sema.Function_call_resolution.make_postfix_argument_expression
              ~operator:(postfix_operator postfix.postfix_operator_kind)
              ~operator_origin:
                (origin postfix.postfix_operator.operator_location)
              ~operand)
    | Frontend.Ast.Postfix_cast_expression cast -> (
        match cast_type_reference visible cast with
        | Error _ as error -> error
        | Ok target -> (
            match
              argument_expression visible locals globals occurrences cursor
                cast.cast_operand
            with
            | Error _ as error -> error
            | Ok operand ->
                Ok
                  (Sema.Function_call_resolution.Postfix_cast_expression
                     (operand, target))))
    | Frontend.Ast.Binary_expression binary -> (
        match
          Generated.Intermediate_codes.of_source_name
            binary.binary_operator_spec.ic_name
        with
        | None ->
            Error
              (Printf.sprintf "binary operator %S has no checked IC identity"
                 binary.binary_operator_spec.ic_name)
        | Some operator -> (
            match
              argument_expression visible locals globals occurrences cursor
                binary.binary_left
            with
            | Error _ as error -> error
            | Ok left -> (
                match
                  argument_expression visible locals globals occurrences cursor
                    binary.binary_right
                with
                | Error _ as error -> error
                | Ok right ->
                    Sema.Function_call_resolution
                    .make_binary_argument_expression ~operator
                      ~operator_origin:
                        (origin binary.binary_operator.operator_location)
                      ~left ~right)))
    | Frontend.Ast.Call_expression _ ->
        Result.map
          (fun () ->
            Sema.Function_call_resolution.Unresolved_expression
              Sema.Function_call_resolution.Call_expression)
          (advance_expression_occurrences occurrences cursor expression)
    | Frontend.Ast.Index_expression index -> (
        match
          argument_expression visible locals globals occurrences cursor
            index.index_base
        with
        | Error _ as error -> error
        | Ok base -> (
            match
              argument_expression visible locals globals occurrences cursor
                index.index_value
            with
            | Error _ as error -> error
            | Ok value ->
                Sema.Function_call_resolution.make_index_argument_expression
                  ~base
                  ~opening_origin:(origin index.index_opening_bracket)
                  ~index:value
                  ~closing_origin:(origin index.index_closing_bracket)))
    | Frontend.Ast.Member_expression _ ->
        Result.map
          (fun () ->
            Sema.Function_call_resolution.Unresolved_expression
              Sema.Function_call_resolution.Member_expression)
          (advance_expression_occurrences occurrences cursor expression)
  in
  Result.map
    (fun kind ->
      Sema.Function_call_resolution.make_argument_expression ~kind
        ~origin:(origin (Frontend.Ast.expression_location expression)))
    kind_result

let argument visible locals globals occurrences cursor index
    (argument : Frontend.Ast.call_argument) =
  let prepared =
    match argument.call_argument_value with
    | Frontend.Ast.Provided_call_argument expression ->
        Result.map
          (fun expression ->
            (Sema.Function_call_resolution.Provided, Some expression))
          (argument_expression visible locals globals occurrences cursor
             expression)
    | Frontend.Ast.Omitted_call_argument ->
        Ok (Sema.Function_call_resolution.Omitted, None)
  in
  match prepared with
  | Error _ as error -> error
  | Ok (kind, expression) ->
      Sema.Function_call_resolution.make_argument ~index ~kind ~expression
        ~origin:(origin argument.call_argument_location)

let call_arguments visible locals globals occurrences first_occurrence call =
  let cursor = ref first_occurrence in
  let rec loop index rev = function
    | [] -> Ok (List.rev rev)
    | argument_ast :: rest -> (
        match
          argument visible locals globals occurrences cursor index argument_ast
        with
        | Error _ as error -> error
        | Ok argument ->
            if index = max_int then
              Error "function call argument space is exhausted"
            else loop (index + 1) (argument :: rev) rest)
  in
  loop 0 [] call.Frontend.Ast.call_arguments

let collect_call visible locals globals occurrences state
    (call : Frontend.Ast.call_expression) =
  match call.call_callee with
  | Frontend.Ast.Identifier_expression callee -> (
      if state.next_occurrence = max_int then
        Error "function call occurrence space is exhausted"
      else
        match
          call_arguments visible locals globals occurrences
            (state.next_occurrence + 1)
            call
        with
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
      match
        collect_call state.visible_aggregates state.typed_values
          state.global_values state.occurrences state call
      with
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

let function_input table visible_aggregates global_values expected typed locals
    (item_index, ast) =
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
    match function_typed_environment table expected typed locals with
    | Error _ as error -> error
    | Ok typed_values -> (
        let expected_occurrences =
          Sema.Module_expression_binding.function_occurrences expected
        in
        let collected =
          let state =
            empty_state visible_aggregates typed_values global_values
              (occurrence_map expected_occurrences)
          in
          match body with
          | None -> Ok state
          | Some body -> statement state body
        in
        match collected with
        | Error _ as error -> error
        | Ok state ->
            if state.next_occurrence <> List.length expected_occurrences then
              Error
                "function call traversal does not match ordinary expression \
                 binding"
            else
              Sema.Function_call_resolution.make_function ~symbol ~scope
                ~item_index (List.rev state.calls_rev))

let publish_aggregates_before visible publications item_index =
  let rec loop visible = function
    | publication :: rest
      when Sema.Module_expression_binding.publication_item_index publication
           < item_index ->
        let visible =
          if
            Sema.Module_expression_binding.publication_kind publication
            = Sema.Module_expression_binding.Aggregate
          then
            let symbol =
              Sema.Module_expression_binding.publication_canonical_symbol
                publication
            in
            String_map.add (Sema.Symbol.name symbol) symbol visible
          else visible
        in
        loop visible rest
    | remaining -> (visible, remaining)
  in
  loop visible publications

let function_inputs table function_types local_types global_values expressions
    module_ =
  let rec pair inputs_rev visible publications expected typed locals ast =
    match (expected, typed, locals, ast) with
    | [], [], [], [] -> Ok (List.rev inputs_rev)
    | ( expected :: expected_rest,
        typed :: typed_rest,
        locals :: local_rest,
        ast :: ast_rest ) -> (
        let item_index =
          Sema.Module_expression_binding.function_item_index expected
        in
        let visible, publications =
          publish_aggregates_before visible publications item_index
        in
        match
          function_input table visible global_values expected typed locals ast
        with
        | Error _ as error -> error
        | Ok input ->
            pair (input :: inputs_rev) visible publications expected_rest
              typed_rest local_rest ast_rest)
    | [], _, _, _ | _, [], _, _ | _, _, [], _ | _, _, _, [] ->
        Error "function call inputs do not match the typed function count"
  in
  pair [] String_map.empty
    (Sema.Module_expression_binding.publications expressions)
    (Sema.Module_expression_binding.functions expressions)
    (Sema.Function_type_resolution.functions function_types)
    (Sema.Local_type_resolution.functions local_types)
    (ast_functions module_)

let resolve ~table ~declarations ~function_types ~local_types ~global_types
    ~functions ~expressions module_ =
  let parent = Sema.Declaration_collection.scope declarations in
  let result =
    if not (Sema.Symbol_table.owns_scope table parent) then
      Error "function call declarations belong to another symbol table"
    else if Sema.Symbol_table.scope_kind parent <> Sema.Symbol_table.Module then
      Error "function call resolution requires a module declaration scope"
    else if not (Sema.Module_expression_binding.owns_table expressions table)
    then Error "function call expressions belong to another symbol table"
    else
      match global_typed_environment table global_types with
      | Error _ as error -> error
      | Ok global_values -> (
          match
            function_inputs table function_types local_types global_values
              expressions module_
          with
          | Error _ as error -> error
          | Ok inputs ->
              Sema.Function_call_resolution.resolve ~table ~parent
                ~function_types ~functions ~expressions inputs
              |> Result.map_error Sema.Function_call_resolution.error_to_string)
  in
  Result.map_error
    (fun message ->
      if String.starts_with ~prefix:"HCSEMA" message then message
      else "HCSEMA0039: " ^ message)
    result
