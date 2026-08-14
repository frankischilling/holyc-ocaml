let origin_of_location (location : Frontend.Ast.location) =
  Sema.Symbol.Source_location
    {
      span = location.span;
      source_segments = location.source_segments;
      generated_from = location.generated_from;
      defined_at = location.defined_at;
    }

let same_symbol left right =
  Sema.Symbol.Id.equal (Sema.Symbol.id left) (Sema.Symbol.id right)

let same_scope left right =
  Sema.Symbol.Scope_id.equal
    (Sema.Symbol_table.scope_id left)
    (Sema.Symbol_table.scope_id right)

type ast_function = {
  item_index : int;
  name : Frontend.Ast.identifier;
  is_definition : bool;
}

let ast_functions (module_ : Frontend.Ast.module_) =
  module_.items
  |> List.mapi (fun item_index item ->
      match item with
      | Frontend.Ast.Function_prototype prototype ->
          Some { item_index; name = prototype.name; is_definition = false }
      | Frontend.Ast.Function_definition definition ->
          Some { item_index; name = definition.name; is_definition = true }
      | Frontend.Ast.Aggregate_forward_declaration _
      | Frontend.Ast.Aggregate_definition _
      | Frontend.Ast.Global_variable _
      | Frontend.Ast.Global_declaration _
      | Frontend.Ast.Top_level_statement _ -> None)
  |> List.filter_map Fun.id

type flag_fact = {
  symbol : Sema.Symbol.t;
  kind : Sema.Function_binding_index.binding_kind;
  parameter_index : int option;
  local_declaration_index : int option;
  local_declarator_index : int option;
  flag_mask : int64;
}

let parameter_at signature index =
  Sema.Function_type_resolution.signature_parameters signature
  |> List.find_opt (fun parameter ->
      Sema.Function_type_resolution.parameter_index parameter = index)

let named_parameter_facts typed =
  let signature = Sema.Function_type_resolution.function_signature typed in
  let rec loop facts_rev = function
    | [] -> Ok (List.rev facts_rev)
    | binding :: rest ->
        let index =
          Sema.Function_type_resolution.parameter_binding_index binding
        in
        let symbol =
          Sema.Function_type_resolution.parameter_binding_symbol binding
        in
        (match parameter_at signature index with
        | None -> Error "local warning analysis cannot find a typed parameter"
        | Some parameter ->
            if
              Sema.Function_type_resolution.parameter_name parameter
              <> Some (Sema.Symbol.name symbol)
            then
              Error
                "local warning typed parameter does not match its binding name"
            else
              loop
                ({
                   symbol;
                   kind = Sema.Function_binding_index.Named_parameter;
                   parameter_index = Some index;
                   local_declaration_index = None;
                   local_declarator_index = None;
                   flag_mask =
                     Sema.Function_type_resolution.parameter_flag_mask parameter;
                 }
                :: facts_rev)
                rest)
  in
  loop []
    (Sema.Function_type_resolution.function_parameter_bindings typed)

let synthetic_fact kind binding =
  {
    symbol = Sema.Function_type_resolution.synthetic_binding_symbol binding;
    kind;
    parameter_index =
      Some (Sema.Function_type_resolution.synthetic_binding_index binding);
    local_declaration_index = None;
    local_declarator_index = None;
    flag_mask =
      Sema.Function_type_resolution.synthetic_binding_flag_mask binding;
  }

let synthetic_parameter_facts typed =
  match Sema.Function_type_resolution.function_variadic_bindings typed with
  | None -> []
  | Some variadic ->
      [
        synthetic_fact Sema.Function_binding_index.Variadic_argc
          (Sema.Function_type_resolution.variadic_argc variadic);
        synthetic_fact Sema.Function_binding_index.Variadic_argv
          (Sema.Function_type_resolution.variadic_argv variadic);
      ]

let local_fact local =
  let kind =
    match Sema.Local_type_resolution.local_storage local with
    | Sema.Local_type_resolution.Automatic ->
        Sema.Function_binding_index.Automatic_local
    | Sema.Local_type_resolution.Static ->
        Sema.Function_binding_index.Static_local
  in
  {
    symbol = Sema.Local_type_resolution.local_symbol local;
    kind;
    parameter_index = None;
    local_declaration_index =
      Some (Sema.Local_type_resolution.local_declaration_index local);
    local_declarator_index =
      Some (Sema.Local_type_resolution.local_declarator_index local);
    flag_mask = Sema.Local_type_resolution.local_flag_mask local;
  }

let flag_facts typed local_types =
  match named_parameter_facts typed with
  | Error _ as error -> error
  | Ok named ->
      let synthetic = synthetic_parameter_facts typed in
      let locals =
        Sema.Local_type_resolution.function_locals local_types
        |> List.map local_fact
      in
      Ok (named @ synthetic @ locals)

let fact_matches_binding fact
    (binding : Sema.Function_binding_index.binding) =
  same_symbol fact.symbol binding.symbol
  && fact.kind = binding.kind
  && fact.parameter_index = binding.parameter_index
  && fact.local_declaration_index = binding.local_declaration_index
  && fact.local_declarator_index = binding.local_declarator_index

let binding_inputs indexed facts =
  let rec pair inputs_rev bindings facts =
    match (bindings, facts) with
    | [], [] -> Ok (List.rev inputs_rev)
    | binding :: binding_rest, fact :: fact_rest ->
        if not (fact_matches_binding fact binding) then
          Error
            "local warning member flags do not match the function binding index"
        else
          pair
            (Sema.Local_warning_analysis.make_binding_input ~binding
               ~initial_flag_mask:fact.flag_mask
            :: inputs_rev)
            binding_rest fact_rest
    | [], _ :: _ | _ :: _, [] ->
        Error
          "local warning member flags do not match the function binding count"
  in
  pair [] (Sema.Function_binding_index.function_bindings indexed) facts

let validate_function table indexed typed local_types expressions ast =
  let indexed_symbol = Sema.Function_binding_index.function_symbol indexed in
  let indexed_scope = Sema.Function_binding_index.function_scope indexed in
  let indexed_item = Sema.Function_binding_index.function_item_index indexed in
  let typed_symbol = Sema.Function_type_resolution.function_symbol typed in
  let typed_scope = Sema.Function_type_resolution.function_scope typed in
  let typed_item = Sema.Function_type_resolution.function_item_index typed in
  let local_symbol = Sema.Local_type_resolution.function_symbol local_types in
  let local_scope = Sema.Local_type_resolution.function_scope local_types in
  let local_item = Sema.Local_type_resolution.function_item_index local_types in
  let expression_symbol =
    Sema.Function_expression_binding.function_symbol expressions
  in
  let expression_scope =
    Sema.Function_expression_binding.function_scope expressions
  in
  let expression_item =
    Sema.Function_expression_binding.function_item_index expressions
  in
  if
    not
      (Sema.Symbol_table.owns_symbol table indexed_symbol
      && Sema.Symbol_table.owns_symbol table typed_symbol
      && Sema.Symbol_table.owns_symbol table local_symbol
      && Sema.Symbol_table.owns_symbol table expression_symbol)
  then Error "local warning semantic functions belong to another symbol table"
  else if
    not
      (Sema.Symbol_table.owns_scope table indexed_scope
      && Sema.Symbol_table.owns_scope table typed_scope
      && Sema.Symbol_table.owns_scope table local_scope
      && Sema.Symbol_table.owns_scope table expression_scope)
  then
    Error "local warning semantic scopes belong to another symbol table"
  else if
    not
      (same_symbol indexed_symbol typed_symbol
      && same_symbol indexed_symbol local_symbol
      && same_symbol indexed_symbol expression_symbol)
  then Error "local warning semantic function identities do not match"
  else if
    not
      (same_scope indexed_scope typed_scope
      && same_scope indexed_scope local_scope
      && same_scope indexed_scope expression_scope)
  then Error "local warning semantic function scopes do not match"
  else if
    indexed_item <> typed_item || indexed_item <> local_item
    || indexed_item <> expression_item || indexed_item <> ast.item_index
  then Error "local warning semantic function positions do not match"
  else if not (String.equal (Sema.Symbol.name indexed_symbol) ast.name.spelling)
  then Error "local warning function does not match the AST name"
  else if Sema.Symbol.origin indexed_symbol <> origin_of_location ast.name.location
  then Error "local warning function does not match the AST origin"
  else
    match flag_facts typed local_types with
    | Error _ as error -> error
    | Ok facts -> (
        match binding_inputs indexed facts with
        | Error _ as error -> error
        | Ok bindings ->
            Sema.Local_warning_analysis.make_function_input
              ~symbol:indexed_symbol ~scope:indexed_scope
              ~item_index:indexed_item ~is_definition:ast.is_definition bindings)

let function_inputs table bindings function_types local_types expressions ast =
  let rec loop inputs_rev bindings function_types local_types expressions ast =
    match (bindings, function_types, local_types, expressions, ast) with
    | [], [], [], [], [] -> Ok (List.rev inputs_rev)
    | ( indexed :: binding_rest,
        typed :: type_rest,
        local :: local_rest,
        expression :: expression_rest,
        ast_function :: ast_rest ) -> (
        match
          validate_function table indexed typed local expression ast_function
        with
        | Error _ as error -> error
        | Ok input ->
            loop (input :: inputs_rev) binding_rest type_rest local_rest
              expression_rest ast_rest)
    | [], _, _, _, _
    | _, [], _, _, _
    | _, _, [], _, _
    | _, _, _, [], _
    | _, _, _, _, [] ->
        Error "local warning semantic inputs contain different function counts"
  in
  loop [] bindings function_types local_types expressions ast

let analyze ?(compiler_option_mask = Sema.Compiler_option.initial_mask) ~table
    ~declarations ~function_types ~local_types ~bindings ~expressions module_ =
  let parent = Sema.Declaration_collection.scope declarations in
  let result =
    if not (Sema.Symbol_table.owns_scope table parent) then
      Error "local warning declarations belong to another symbol table"
    else if Sema.Symbol_table.scope_kind parent <> Sema.Symbol_table.Module then
      Error "local warning analysis requires a module declaration collection"
    else
      match
        function_inputs table
          (Sema.Function_binding_index.functions bindings)
          (Sema.Function_type_resolution.functions function_types)
          (Sema.Local_type_resolution.functions local_types)
          (Sema.Function_expression_binding.functions expressions)
          (ast_functions module_)
      with
      | Error _ as error -> error
      | Ok inputs ->
          Sema.Local_warning_analysis.analyze ~table ~parent ~bindings
            ~expressions ~compiler_option_mask inputs
          |> Result.map_error Sema.Local_warning_analysis.error_to_string
  in
  Result.map_error
    (fun message ->
      if String.starts_with ~prefix:"HCSEMA" message then message
      else "HCSEMA0022: " ^ message)
    result
