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
  callable : Sema.Function_call_resolution.callable option;
  function_declaration : Sema.Function_resolution.resolved_declaration option;
  function_address_path :
    Sema.Function_call_resolution.direct_function_address_path option;
}

type typed_environment = typed_value Int_map.t

let symbol_number symbol = Sema.Symbol.id symbol |> Sema.Symbol.Id.to_int

let same_symbol left right =
  Sema.Symbol.Id.equal (Sema.Symbol.id left) (Sema.Symbol.id right)

let same_scope left right =
  Sema.Symbol.Scope_id.equal
    (Sema.Symbol_table.scope_id left)
    (Sema.Symbol_table.scope_id right)

let typed_value_of_identifier ?callable value =
  {
    resolved_type = Sema.Function_call_resolution.identifier_value_type value;
    shape = Sema.Function_call_resolution.identifier_value_shape value;
    array_rank = Sema.Function_call_resolution.identifier_value_array_rank value;
    callable;
    function_declaration =
      Sema.Function_call_resolution.identifier_value_function_declaration value;
    function_address_path =
      Sema.Function_call_resolution.identifier_value_function_address_path value;
  }

let object_value reference =
  {
    resolved_type = Sema.Type_reference.resolved_type reference;
    shape = Sema.Function_call_resolution.Object_value;
    array_rank = 0;
    callable = None;
    function_declaration = None;
    function_address_path = None;
  }

let callback_value reference pointer =
  {
    resolved_type = Sema.Type_reference.resolved_type reference;
    shape = Sema.Function_call_resolution.Function_pointer_value;
    array_rank = 0;
    callable =
      Some
        (Sema.Function_call_resolution.make_callable ~return_type:reference
           ~function_pointer:pointer);
    function_declaration = None;
    function_address_path = None;
  }

let direct_function_value declaration function_address_path =
  Sema.Function_call_resolution.direct_function_identifier_value ~declaration
    ~address_path:function_address_path
  |> Result.map typed_value_of_identifier

let parameter_value parameter =
  match Sema.Function_type_resolution.parameter_declarator_kind parameter with
  | Sema.Function_type_resolution.Object ->
      object_value
        (Sema.Function_type_resolution.parameter_type_reference parameter)
  | Sema.Function_type_resolution.Function_pointer pointer ->
      callback_value
        (Sema.Function_type_resolution.parameter_type_reference parameter)
        pointer

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
    callable = None;
    function_declaration = None;
    function_address_path = None;
  }

let local_value local =
  let dimensions = Sema.Local_type_resolution.local_array_dimensions local in
  let reference = Sema.Local_type_resolution.local_type_reference local in
  let shape, callable =
    match Sema.Local_type_resolution.local_declarator_kind local with
    | Sema.Local_type_resolution.Function_pointer pointer ->
        ( (if dimensions = [] then
             Sema.Function_call_resolution.Function_pointer_value
           else Sema.Function_call_resolution.Array_value),
          Some
            (Sema.Function_call_resolution.make_callable ~return_type:reference
               ~function_pointer:pointer) )
    | Sema.Local_type_resolution.Object ->
        ( (if dimensions = [] then Sema.Function_call_resolution.Object_value
           else Sema.Function_call_resolution.Array_value),
          None )
  in
  {
    resolved_type = Sema.Type_reference.resolved_type reference;
    shape;
    array_rank = List.length dimensions;
    callable;
    function_declaration = None;
    function_address_path = None;
  }

let global_value global =
  let reference = Sema.Global_type_resolution.global_type_reference global in
  let callable =
    match Sema.Global_type_resolution.global_declarator_kind global with
    | Sema.Global_type_resolution.Function_pointer pointer ->
        Some
          (Sema.Function_call_resolution.make_callable ~return_type:reference
             ~function_pointer:pointer)
    | Sema.Global_type_resolution.Object -> None
  in
  Sema.Function_call_resolution.global_identifier_value global
  |> Result.map (typed_value_of_identifier ?callable)

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
    | global :: rest -> (
        let symbol = Sema.Global_type_resolution.global_symbol global in
        if not (Sema.Symbol_table.owns_symbol table symbol) then
          Error "call argument global type belongs to another symbol table"
        else if Int_map.mem (symbol_number symbol) values then
          Error "call argument global type symbol is repeated"
        else
          match global_value global with
          | Error _ as error -> error
          | Ok value -> loop (add_typed_value symbol value values) rest)
  in
  loop Int_map.empty (Sema.Global_type_resolution.globals globals)

let add_function_typed_environment table functions values =
  let compilation_mode = Sema.Function_resolution.compilation_mode functions in
  let rec loop values = function
    | [] -> Ok values
    | declaration :: rest -> (
        let site =
          Sema.Function_resolution.resolved_declaration_site declaration
        in
        let symbol =
          site |> Sema.Function_resolution.declaration_site_function
          |> Sema.Function_type_resolution.function_symbol
        in
        if not (Sema.Symbol_table.owns_symbol table symbol) then
          Error
            "call argument function declaration belongs to another symbol table"
        else if Int_map.mem (symbol_number symbol) values then
          Error "call argument module value symbol is repeated"
        else
          match
            Sema.Function_call_resolution.direct_function_address_path
              compilation_mode declaration
          with
          | Error _ as error -> error
          | Ok path -> (
              match direct_function_value declaration path with
              | Error _ as error -> error
              | Ok value -> loop (add_typed_value symbol value values) rest))
  in
  loop values (Sema.Function_resolution.declarations functions)

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

let typed_value_for_occurrence locals module_values occurrence =
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
        module_values
  | Sema.Module_expression_binding.Module_binding publication
    when Sema.Module_expression_binding.publication_kind publication
         = Sema.Module_expression_binding.Function ->
      Int_map.find_opt
        (publication |> Sema.Module_expression_binding.publication_source_symbol
       |> symbol_number)
        module_values
  | Sema.Module_expression_binding.Module_binding _
  | Sema.Module_expression_binding.Outer_candidate -> None

type state = {
  next_occurrence : int;
  next_call : int;
  calls_rev : Sema.Function_call_resolution.call list;
  next_expression_statement : int;
  expression_statements_rev :
    Sema.Function_call_resolution.expression_statement_input list;
  next_implicit_output : int;
  implicit_outputs_rev :
    Sema.Function_call_resolution.implicit_output_input list;
  next_condition : int;
  conditions_rev : Sema.Function_call_resolution.condition_input list;
  next_selector : int;
  selectors_rev : Sema.Function_call_resolution.selector_input list;
  next_switch_case : int;
  switch_cases_rev : Sema.Function_call_resolution.switch_case_input list;
  next_return : int;
  returns_rev : Sema.Function_call_resolution.return_input list;
  visible_aggregates : Sema.Symbol.t String_map.t;
  typed_values : typed_environment;
  global_values : typed_environment;
  occurrences : Sema.Module_expression_binding.occurrence Int_map.t;
  defined_queries : Sema.Function_call_resolution.defined_function_query list;
}

let empty_state visible_aggregates typed_values global_values occurrences
    defined_queries =
  {
    next_occurrence = 0;
    next_call = 0;
    calls_rev = [];
    next_expression_statement = 0;
    expression_statements_rev = [];
    next_implicit_output = 0;
    implicit_outputs_rev = [];
    next_condition = 0;
    conditions_rev = [];
    next_selector = 0;
    selectors_rev = [];
    next_switch_case = 0;
    switch_cases_rev = [];
    next_return = 0;
    returns_rev = [];
    visible_aggregates;
    typed_values;
    global_values;
    occurrences;
    defined_queries;
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

let defined_query_role = function
  | Sema.Function_call_resolution.Module_query query ->
      Sema.Module_expression_binding.query_role query
  | Sema.Function_call_resolution.Outer_query query ->
      Sema.Outer_expression_binding.query_role query

let defined_query_name = function
  | Sema.Function_call_resolution.Module_query query ->
      Sema.Module_expression_binding.query_name query
  | Sema.Function_call_resolution.Outer_query query ->
      Sema.Outer_expression_binding.query_name query

let defined_query_origin = function
  | Sema.Function_call_resolution.Module_query query ->
      Sema.Module_expression_binding.query_origin query
  | Sema.Function_call_resolution.Outer_query query ->
      Sema.Outer_expression_binding.query_origin query

let defined_query queries (operand : Frontend.Ast.defined_operand) =
  let spelling = operand.defined_operand_spelling in
  let operand_origin = origin operand.defined_operand_location in
  let matches query =
    defined_query_role query = Sema.Function_expression_binding.Defined_operand
    && String.equal spelling (defined_query_name query)
    && operand_origin = defined_query_origin query
  in
  match List.filter matches queries with
  | [ query ] -> Ok query
  | [] -> Error "defined operand has no matching function query"
  | _ -> Error "defined operand has more than one matching function query"

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

let rec argument_expression visible locals globals occurrences defined_queries
    cursor (expression : Frontend.Ast.expression) =
  let kind_result =
    match expression with
    | Frontend.Ast.Integer_literal literal -> (
        match literal.literal_value with
        | Frontend.Ast.Integer_value value ->
            Ok (Sema.Function_call_resolution.Integer_literal value)
        | Frontend.Ast.Float_value _ | Frontend.Ast.Bytes_value _ ->
            Error "integer literal has a non-integer payload")
    | Frontend.Ast.Float_literal literal -> (
        match literal.literal_value with
        | Frontend.Ast.Float_value value ->
            Ok
              (Sema.Function_call_resolution.Float_literal
                 (Int64.bits_of_float value))
        | Frontend.Ast.Integer_value _ | Frontend.Ast.Bytes_value _ ->
            Error "F64 literal has a non-F64 payload")
    | Frontend.Ast.Character_literal literal -> (
        match literal.literal_value with
        | Frontend.Ast.Integer_value value ->
            Ok (Sema.Function_call_resolution.Character_literal value)
        | Frontend.Ast.Float_value _ | Frontend.Ast.Bytes_value _ ->
            Error "character literal has a non-integer payload")
    | Frontend.Ast.String_literal literal -> (
        match literal.literal_value with
        | Frontend.Ast.Bytes_value value ->
            Ok (Sema.Function_call_resolution.String_literal value)
        | Frontend.Ast.Integer_value _ | Frontend.Ast.Float_value _ ->
            Error "string literal has a non-byte payload")
    | Frontend.Ast.Parenthesized_expression grouped -> (
        match
          argument_expression visible locals globals occurrences defined_queries
            cursor grouped.grouped_expression
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
                  ~array_rank:value.array_rank
                  ?function_declaration:value.function_declaration
                  ?function_address_path:value.function_address_path ()))
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
    | Frontend.Ast.Defined_expression defined ->
        let operand = defined.defined_operand in
        let operand_kind =
          match operand.defined_operand_kind with
          | Frontend.Ast.Defined_name ->
              Sema.Function_call_resolution.Defined_name
          | Frontend.Ast.Defined_non_name ->
              Sema.Function_call_resolution.Defined_non_name
        in
        let resolution =
          match operand.defined_operand_kind with
          | Frontend.Ast.Defined_non_name ->
              Ok Sema.Function_call_resolution.Defined_non_name_false
          | Frontend.Ast.Defined_name ->
              Result.map
                (fun query ->
                  Sema.Function_call_resolution.Defined_function_query query)
                (defined_query defined_queries operand)
        in
        Result.bind resolution (fun operand_resolution ->
            Sema.Function_call_resolution.make_defined_argument_expression
              ~operand_kind ~operand_spelling:operand.defined_operand_spelling
              ~operand_origin:(origin operand.defined_operand_location)
              ~operand_resolution)
    | Frontend.Ast.Prefix_expression prefix -> (
        match
          argument_expression visible locals globals occurrences defined_queries
            cursor prefix.prefix_operand
        with
        | Error _ as error -> error
        | Ok operand ->
            Sema.Function_call_resolution.make_prefix_argument_expression
              ~operator:(prefix_operator prefix.prefix_operator_kind)
              ~operator_origin:(origin prefix.prefix_operator.operator_location)
              ~operand)
    | Frontend.Ast.Postfix_expression postfix -> (
        match
          argument_expression visible locals globals occurrences defined_queries
            cursor postfix.postfix_operand
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
              argument_expression visible locals globals occurrences
                defined_queries cursor cast.cast_operand
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
              argument_expression visible locals globals occurrences
                defined_queries cursor binary.binary_left
            with
            | Error _ as error -> error
            | Ok left -> (
                match
                  argument_expression visible locals globals occurrences
                    defined_queries cursor binary.binary_right
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
          argument_expression visible locals globals occurrences defined_queries
            cursor index.index_base
        with
        | Error _ as error -> error
        | Ok base -> (
            match
              argument_expression visible locals globals occurrences
                defined_queries cursor index.index_value
            with
            | Error _ as error -> error
            | Ok value ->
                Sema.Function_call_resolution.make_index_argument_expression
                  ~base
                  ~opening_origin:(origin index.index_opening_bracket)
                  ~index:value
                  ~closing_origin:(origin index.index_closing_bracket)))
    | Frontend.Ast.Member_expression member -> (
        match
          argument_expression visible locals globals occurrences defined_queries
            cursor member.member_base
        with
        | Error _ as error -> error
        | Ok base ->
            let access_kind =
              match member.member_access_kind with
              | Frontend.Ast.Direct_member ->
                  Sema.Function_call_resolution.Direct_member
              | Frontend.Ast.Pointer_member ->
                  Sema.Function_call_resolution.Pointer_member
            in
            Sema.Function_call_resolution.make_member_argument_expression ~base
              ~access_kind
              ~operator_origin:(origin member.member_operator.operator_location)
              ~member_name:member.member_name.spelling
              ~member_origin:(origin member.member_name.location))
  in
  Result.map
    (fun kind ->
      Sema.Function_call_resolution.make_argument_expression ~kind
        ~origin:(origin (Frontend.Ast.expression_location expression)))
    kind_result

let argument visible locals globals occurrences defined_queries cursor index
    (argument : Frontend.Ast.call_argument) =
  let prepared =
    match argument.call_argument_value with
    | Frontend.Ast.Provided_call_argument expression ->
        Result.map
          (fun expression ->
            (Sema.Function_call_resolution.Provided, Some expression))
          (argument_expression visible locals globals occurrences
             defined_queries cursor expression)
    | Frontend.Ast.Omitted_call_argument ->
        Ok (Sema.Function_call_resolution.Omitted, None)
  in
  match prepared with
  | Error _ as error -> error
  | Ok (kind, expression) ->
      Sema.Function_call_resolution.make_argument ~index ~kind ~expression
        ~origin:(origin argument.call_argument_location)

let call_arguments visible locals globals occurrences defined_queries
    first_occurrence call =
  let cursor = ref first_occurrence in
  let rec loop index rev = function
    | [] -> Ok (List.rev rev)
    | argument_ast :: rest -> (
        match
          argument visible locals globals occurrences defined_queries cursor
            index argument_ast
        with
        | Error _ as error -> error
        | Ok argument ->
            if index = max_int then
              Error "function call argument space is exhausted"
            else loop (index + 1) (argument :: rev) rest)
  in
  loop 0 [] call.Frontend.Ast.call_arguments

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

let rec computed_callee = function
  | Frontend.Ast.Member_expression _ | Frontend.Ast.Index_expression _ -> true
  | Frontend.Ast.Parenthesized_expression grouped ->
      computed_callee grouped.grouped_expression
  | _ -> false

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

let record_call state call =
  if state.next_call = max_int then
    Error "function call identity space is exhausted"
  else
    Ok
      {
        state with
        next_call = state.next_call + 1;
        calls_rev = call :: state.calls_rev;
      }

let collect_call visible locals globals occurrences defined_queries state
    (call : Frontend.Ast.call_expression) =
  match identifier_callee 0 call.call_callee with
  | Some (callee, callee_form) -> (
      if state.next_occurrence = max_int then
        Error "function call occurrence space is exhausted"
      else
        match
          call_arguments visible locals globals occurrences defined_queries
            (state.next_occurrence + 1)
            call
        with
        | Error _ as error -> error
        | Ok arguments -> (
            match occurrence_at occurrences state.next_occurrence callee with
            | Error _ as error -> error
            | Ok occurrence -> (
                let callable =
                  Option.bind
                    (typed_value_for_occurrence locals globals occurrence)
                    (fun value ->
                      if value.array_rank = 0 then value.callable else None)
                in
                match
                  Sema.Function_call_resolution.make_call ~index:state.next_call
                    ~callee_occurrence_index:state.next_occurrence
                    ~callee_name:callee.spelling
                    ~callee_origin:(origin callee.location) ~callee_form
                    ?callable
                    ~origin:(origin call.call_location)
                    ~syntax:(call_syntax call) arguments
                with
                | Error _ as error -> error
                | Ok call -> record_call state call)))
  | None when computed_callee call.call_callee -> (
      match first_identifier call.call_callee with
      | None -> Error "computed call callee has no bound base identifier"
      | Some callee -> (
          let cursor = ref state.next_occurrence in
          match
            argument_expression visible locals globals occurrences
              defined_queries cursor call.call_callee
          with
          | Error _ as error -> error
          | Ok computed_callee -> (
              match
                call_arguments visible locals globals occurrences
                  defined_queries !cursor call
              with
              | Error _ as error -> error
              | Ok arguments -> (
                  match
                    occurrence_at occurrences state.next_occurrence callee
                  with
                  | Error _ as error -> error
                  | Ok occurrence -> (
                      let callable =
                        Option.bind
                          (typed_value_for_occurrence locals globals occurrence)
                          (fun value -> value.callable)
                      in
                      match
                        Sema.Function_call_resolution.make_call
                          ~index:state.next_call
                          ~callee_occurrence_index:state.next_occurrence
                          ~callee_name:callee.spelling
                          ~callee_origin:(origin callee.location)
                          ~callee_form:
                            Sema.Function_call_resolution.Member_callee
                          ?callable ~computed_callee
                          ~origin:(origin call.call_location)
                          ~syntax:(call_syntax call) arguments
                      with
                      | Error _ as error -> error
                      | Ok call -> record_call state call)))))
  | None -> Ok state

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
          state.global_values state.occurrences state.defined_queries state call
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
  | Frontend.Ast.Unbraced_array_initializer unbraced ->
      fold_result initializer_element state
        unbraced.unbraced_initializer_elements

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

let record_implicit_output state
    (output : Frontend.Ast.implicit_output_statement) =
  let first_occurrence = state.next_occurrence in
  let visible = state.visible_aggregates in
  let locals = state.typed_values in
  let globals = state.global_values in
  let occurrences = state.occurrences in
  let defined_queries = state.defined_queries in
  let fixed_value, fixed_source =
    match output.fixed_argument with
    | Frontend.Ast.Marker_fixed_argument value ->
        (value, Sema.Function_call_resolution.Marker_fixed_output)
    | Frontend.Ast.Expression_fixed_argument value ->
        (value, Sema.Function_call_resolution.Following_expression_output)
  in
  match expression state fixed_value with
  | Error _ as error -> error
  | Ok state -> (
      match
        fold_result
          (fun state (argument : Frontend.Ast.implicit_output_argument) ->
            expression state argument.value)
          state output.arguments
      with
      | Error _ as error -> error
      | Ok state -> (
          let cursor = ref first_occurrence in
          match
            argument_expression visible locals globals occurrences
              defined_queries cursor fixed_value
          with
          | Error _ as error -> error
          | Ok fixed_expression -> (
              let rec prepare_arguments index rev = function
                | [] -> Ok (List.rev rev)
                | (argument : Frontend.Ast.implicit_output_argument) :: rest
                  -> (
                    match
                      argument_expression visible locals globals occurrences
                        defined_queries cursor argument.value
                    with
                    | Error _ as error -> error
                    | Ok expression -> (
                        match
                          Sema.Function_call_resolution
                          .make_implicit_output_argument ~index
                            ~leading_comma_origin:
                              (origin argument.leading_comma)
                            ~expression ~origin:(origin argument.location)
                        with
                        | Error _ as error -> error
                        | Ok prepared ->
                            if index = max_int then
                              Error
                                "implicit output argument space is exhausted"
                            else
                              prepare_arguments (index + 1) (prepared :: rev)
                                rest))
              in
              match prepare_arguments 0 [] output.arguments with
              | Error _ as error -> error
              | Ok _ when !cursor <> state.next_occurrence ->
                  Error
                    "function implicit output traversal disagrees with \
                     ordinary expression binding"
              | Ok arguments -> (
                  let target =
                    match output.target with
                    | Frontend.Ast.Print_target ->
                        Sema.Function_call_resolution.Print_output
                    | Frontend.Ast.Put_chars_target ->
                        Sema.Function_call_resolution.Put_chars_output
                  in
                  match
                    Sema.Function_call_resolution.make_implicit_output
                      ~index:state.next_implicit_output ~target
                      ~marker_origin:(origin output.marker.literal_location)
                      ~fixed_source ~fixed_expression ~arguments
                      ~origin:(origin output.location)
                  with
                  | Error _ as error -> error
                  | Ok prepared ->
                      if state.next_implicit_output = max_int then
                        Error "function implicit output space is exhausted"
                      else
                        Ok
                          {
                            state with
                            next_implicit_output =
                              state.next_implicit_output + 1;
                            implicit_outputs_rev =
                              prepared :: state.implicit_outputs_rev;
                          }))))

let case_pattern state = function
  | Frontend.Ast.Implicit_case -> Ok state
  | Frontend.Ast.Single_case value -> expression state value
  | Frontend.Ast.Ranged_case range -> (
      match expression state range.case_range_start with
      | Error _ as error -> error
      | Ok state -> expression state range.case_range_end)

let record_expression_statement state
    (statement : Frontend.Ast.expression_statement) =
  let first_occurrence = state.next_occurrence in
  let visible = state.visible_aggregates in
  let locals = state.typed_values in
  let globals = state.global_values in
  let occurrences = state.occurrences in
  let defined_queries = state.defined_queries in
  let value = statement.expression_statement_expression in
  match expression state value with
  | Error _ as error -> error
  | Ok state -> (
      let cursor = ref first_occurrence in
      match
        argument_expression visible locals globals occurrences defined_queries
          cursor value
      with
      | Error _ as error -> error
      | Ok _ when !cursor <> state.next_occurrence ->
          Error
            "function expression statement traversal disagrees with ordinary \
             expression binding"
      | Ok expression -> (
          match
            Sema.Function_call_resolution.make_expression_statement
              ~index:state.next_expression_statement ~expression
              ~origin:(origin statement.expression_statement_location)
          with
          | Error _ as error -> error
          | Ok input ->
              if state.next_expression_statement = max_int then
                Error "function expression statement space is exhausted"
              else
                Ok
                  {
                    state with
                    next_expression_statement =
                      state.next_expression_statement + 1;
                    expression_statements_rev =
                      input :: state.expression_statements_rev;
                  }))

let record_return state (return_ : Frontend.Ast.return_statement) =
  if state.next_return = max_int then
    Error "function return identity space is exhausted"
  else
    let finish state expression =
      match
        Sema.Function_call_resolution.make_return ~index:state.next_return
          ~keyword_origin:(origin return_.return_keyword)
          ~expression
          ~origin:(origin return_.return_location)
      with
      | Error _ as error -> error
      | Ok return_input ->
          Ok
            {
              state with
              next_return = state.next_return + 1;
              returns_rev = return_input :: state.returns_rev;
            }
    in
    match return_.return_value with
    | None -> finish state None
    | Some value -> (
        let first_occurrence = state.next_occurrence in
        let visible = state.visible_aggregates in
        let locals = state.typed_values in
        let globals = state.global_values in
        let occurrences = state.occurrences in
        let defined_queries = state.defined_queries in
        match expression state value with
        | Error _ as error -> error
        | Ok state -> (
            let cursor = ref first_occurrence in
            match
              argument_expression visible locals globals occurrences
                defined_queries cursor value
            with
            | Error _ as error -> error
            | Ok _ when !cursor <> state.next_occurrence ->
                Error
                  "function return expression traversal disagrees with \
                   ordinary expression binding"
            | Ok expression -> finish state (Some expression)))

let record_condition role keyword location state value =
  let first_occurrence = state.next_occurrence in
  let visible = state.visible_aggregates in
  let locals = state.typed_values in
  let globals = state.global_values in
  let occurrences = state.occurrences in
  let defined_queries = state.defined_queries in
  match expression state value with
  | Error _ as error -> error
  | Ok state -> (
      let cursor = ref first_occurrence in
      match
        argument_expression visible locals globals occurrences defined_queries
          cursor value
      with
      | Error _ as error -> error
      | Ok _ when !cursor <> state.next_occurrence ->
          Error
            "function condition traversal disagrees with ordinary expression \
             binding"
      | Ok expression -> (
          match
            Sema.Function_call_resolution.make_condition
              ~index:state.next_condition ~role ~keyword_origin:(origin keyword)
              ~expression ~origin:(origin location)
          with
          | Error _ as error -> error
          | Ok condition ->
              if state.next_condition = max_int then
                Error "function condition space is exhausted"
              else
                Ok
                  {
                    state with
                    next_condition = state.next_condition + 1;
                    conditions_rev = condition :: state.conditions_rev;
                  }))

let record_selector mode keyword location state value =
  let first_occurrence = state.next_occurrence in
  let visible = state.visible_aggregates in
  let locals = state.typed_values in
  let globals = state.global_values in
  let occurrences = state.occurrences in
  let defined_queries = state.defined_queries in
  match expression state value with
  | Error _ as error -> error
  | Ok state -> (
      let cursor = ref first_occurrence in
      match
        argument_expression visible locals globals occurrences defined_queries
          cursor value
      with
      | Error _ as error -> error
      | Ok _ when !cursor <> state.next_occurrence ->
          Error
            "function switch selector traversal disagrees with ordinary \
             expression binding"
      | Ok expression -> (
          match
            Sema.Function_call_resolution.make_selector
              ~index:state.next_selector ~mode ~keyword_origin:(origin keyword)
              ~expression ~origin:(origin location)
          with
          | Error _ as error -> error
          | Ok selector ->
              if state.next_selector = max_int then
                Error "function switch selector space is exhausted"
              else
                Ok
                  {
                    state with
                    next_selector = state.next_selector + 1;
                    selectors_rev = selector :: state.selectors_rev;
                  }))

let selector_mode = function
  | Frontend.Ast.Bounded_switch -> Sema.Function_call_resolution.Bounded_switch
  | Frontend.Ast.No_bound_switch ->
      Sema.Function_call_resolution.No_bound_switch

let record_switch_case state (case_ : Frontend.Ast.switch_case_label) =
  let first_occurrence = state.next_occurrence in
  let visible = state.visible_aggregates in
  let locals = state.typed_values in
  let globals = state.global_values in
  let occurrences = state.occurrences in
  let defined_queries = state.defined_queries in
  match case_pattern state case_.switch_case_pattern with
  | Error _ as error -> error
  | Ok state -> (
      let cursor = ref first_occurrence in
      let checked_expression value =
        argument_expression visible locals globals occurrences defined_queries
          cursor value
      in
      let pattern =
        match case_.switch_case_pattern with
        | Frontend.Ast.Implicit_case ->
            Ok Sema.Function_call_resolution.Implicit_case
        | Frontend.Ast.Single_case value -> (
            match checked_expression value with
            | Error _ as error -> error
            | Ok expression ->
                Ok (Sema.Function_call_resolution.Single_case expression))
        | Frontend.Ast.Ranged_case range -> (
            match checked_expression range.case_range_start with
            | Error _ as error -> error
            | Ok start_expression -> (
                match checked_expression range.case_range_end with
                | Error _ as error -> error
                | Ok end_expression ->
                    Sema.Function_call_resolution.make_ranged_case_pattern
                      ~start_expression
                      ~ellipsis_origin:(origin range.case_range_ellipsis)
                      ~end_expression))
      in
      match pattern with
      | Error _ as error -> error
      | Ok _ when !cursor <> state.next_occurrence ->
          Error
            "function switch case traversal disagrees with ordinary expression \
             binding"
      | Ok pattern -> (
          match
            Sema.Function_call_resolution.make_switch_case
              ~index:state.next_switch_case
              ~keyword_origin:(origin case_.switch_case_keyword)
              ~pattern
              ~origin:(origin case_.switch_case_location)
          with
          | Error _ as error -> error
          | Ok case_input ->
              if state.next_switch_case = max_int then
                Error "function switch case space is exhausted"
              else
                Ok
                  {
                    state with
                    next_switch_case = state.next_switch_case + 1;
                    switch_cases_rev = case_input :: state.switch_cases_rev;
                  }))

let rec statement state = function
  | Frontend.Ast.Block_statement block ->
      statements state block.block_statements
  | Frontend.Ast.Do_while_statement do_while -> (
      match statement state do_while.do_body with
      | Error _ as error -> error
      | Ok state ->
          record_condition Sema.Function_call_resolution.Do_while_condition
            do_while.do_while_keyword do_while.do_while_location state
            do_while.do_while_condition)
  | Frontend.Ast.Expression_statement statement ->
      record_expression_statement state statement
  | Frontend.Ast.For_statement for_ -> (
      match statement state for_.for_initializer with
      | Error _ as error -> error
      | Ok state -> (
          match
            record_condition Sema.Function_call_resolution.For_condition
              for_.for_keyword for_.for_location state for_.for_condition
          with
          | Error _ as error -> error
          | Ok state -> (
              match for_.for_update with
              | Some update -> (
                  match statement state update with
                  | Error _ as error -> error
                  | Ok state -> statement state for_.for_body)
              | None -> statement state for_.for_body)))
  | Frontend.Ast.If_statement if_ -> (
      match
        record_condition Sema.Function_call_resolution.If_condition
          if_.if_keyword if_.if_location state if_.if_condition
      with
      | Error _ as error -> error
      | Ok state -> (
          match statement state if_.if_then_branch with
          | Error _ as error -> error
          | Ok state -> (
              match if_.if_else_clause with
              | None -> Ok state
              | Some else_ -> statement state else_.else_branch)))
  | Frontend.Ast.Implicit_output_statement output ->
      record_implicit_output state output
  | Frontend.Ast.Local_declaration_statement declaration ->
      local_declaration state declaration
  | Frontend.Ast.Lock_statement lock -> statement state lock.lock_body
  | Frontend.Ast.Return_statement return -> record_return state return
  | Frontend.Ast.Sequence_statement sequence ->
      sequence_elements state sequence.sequence_elements
  | Frontend.Ast.Switch_statement switch -> (
      match
        record_selector
          (selector_mode switch.switch_mode)
          switch.switch_keyword switch.switch_location state
          switch.switch_expression
      with
      | Error _ as error -> error
      | Ok state -> switch_elements state switch.switch_elements)
  | Frontend.Ast.Try_catch_statement try_catch -> (
      match statement state try_catch.try_body with
      | Error _ as error -> error
      | Ok state -> statement state try_catch.catch_body)
  | Frontend.Ast.While_statement while_ -> (
      match
        record_condition Sema.Function_call_resolution.While_condition
          while_.while_keyword while_.while_location state
          while_.while_condition
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

and statements state statements = fold_result statement state statements

and sequence_elements state elements =
  fold_result
    (fun state (element : Frontend.Ast.statement_sequence_element) ->
      statement state element.sequence_statement)
    state elements

and switch_elements state elements = fold_result switch_element state elements

and switch_element state = function
  | Frontend.Ast.Switch_case_element case_ -> record_switch_case state case_
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

let function_input table visible_aggregates global_values defined_queries
    expected typed locals (item_index, ast) =
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
              defined_queries
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
                ~item_index
                ~expression_statements:
                  (List.rev state.expression_statements_rev)
                ~implicit_outputs:(List.rev state.implicit_outputs_rev)
                ~conditions:(List.rev state.conditions_rev)
                ~selectors:(List.rev state.selectors_rev)
                ~switch_cases:(List.rev state.switch_cases_rev)
                ~returns:(List.rev state.returns_rev)
                (List.rev state.calls_rev))

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
    outer module_ =
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
        let defined_queries =
          match outer with
          | None ->
              Ok
                (expected |> Sema.Module_expression_binding.function_queries
                |> List.map (fun query ->
                    Sema.Function_call_resolution.Module_query query))
          | Some outer -> (
              match
                Sema.Outer_expression_binding.find_function outer
                  (Sema.Module_expression_binding.function_symbol expected)
              with
              | None ->
                  Error
                    "outer expression binding has no matching function query \
                     set"
              | Some function_ ->
                  Ok
                    (function_ |> Sema.Outer_expression_binding.function_queries
                    |> List.map (fun query ->
                        Sema.Function_call_resolution.Outer_query query)))
        in
        match defined_queries with
        | Error _ as error -> error
        | Ok defined_queries -> (
            match
              function_input table visible global_values defined_queries
                expected typed locals ast
            with
            | Error _ as error -> error
            | Ok input ->
                pair (input :: inputs_rev) visible publications expected_rest
                  typed_rest local_rest ast_rest))
    | [], _, _, _ | _, [], _, _ | _, _, [], _ | _, _, _, [] ->
        Error "function call inputs do not match the typed function count"
  in
  pair [] String_map.empty
    (Sema.Module_expression_binding.publications expressions)
    (Sema.Module_expression_binding.functions expressions)
    (Sema.Function_type_resolution.functions function_types)
    (Sema.Local_type_resolution.functions local_types)
    (ast_functions module_)

let resolve ~table ~declarations ?members ~function_types ~local_types
    ~global_types ~functions ~expressions ?outer module_ =
  let parent = Sema.Declaration_collection.scope declarations in
  let result =
    if not (Sema.Symbol_table.owns_scope table parent) then
      Error "function call declarations belong to another symbol table"
    else if Sema.Symbol_table.scope_kind parent <> Sema.Symbol_table.Module then
      Error "function call resolution requires a module declaration scope"
    else if not (Sema.Module_expression_binding.owns_table expressions table)
    then Error "function call expressions belong to another symbol table"
    else if
      match outer with
      | None -> false
      | Some outer ->
          (not (Sema.Outer_expression_binding.owns_table outer table))
          || Sema.Outer_expression_binding.source outer != expressions
    then
      Error
        "function call outer expressions do not match the module expression \
         binding"
    else
      match global_typed_environment table global_types with
      | Error _ as error -> error
      | Ok global_values -> (
          match
            add_function_typed_environment table functions global_values
          with
          | Error _ as error -> error
          | Ok module_values -> (
              match
                function_inputs table function_types local_types module_values
                  expressions outer module_
              with
              | Error _ as error -> error
              | Ok inputs ->
                  Sema.Function_call_resolution.resolve ~table ~parent ?members
                    ~function_types ~functions ~expressions ?outer inputs
                  |> Result.map_error
                       Sema.Function_call_resolution.error_to_string))
  in
  Result.map_error
    (fun message ->
      if String.starts_with ~prefix:"HCSEMA" message then message
      else "HCSEMA0039: " ^ message)
    result
