type context = {
  table : Sema.Symbol_table.t;
  parent : Sema.Symbol_table.scope;
  expressions : Sema.Module_expression_binding.t;
  members : Sema.Aggregate_member_index.t;
  globals : Sema.Global_type_resolution.t;
  functions : Sema.Function_resolution.t;
  policies : Sema.Function_call_conversion_policy.t;
}

let ( let* ) = Result.bind

let create_context ~table ~parent =
  let* headers = Sema.Aggregate_header_resolution.resolve ~table ~parent [] in
  let* members =
    Sema.Aggregate_member_index.build ~table ~parent []
    |> Result.map_error Sema.Aggregate_member_index.error_to_string
  in
  let* bindings =
    Sema.Function_binding_index.build ~table ~parent []
    |> Result.map_error Sema.Function_binding_index.error_to_string
  in
  let* expressions =
    Sema.Function_expression_binding.resolve ~table ~parent ~bindings []
    |> Result.map_error Sema.Function_expression_binding.error_to_string
  in
  let* expressions =
    Sema.Module_expression_binding.resolve ~table ~parent ~compilation_mode:Jit
      ~expressions []
    |> Result.map_error Sema.Module_expression_binding.error_to_string
  in
  let* function_types =
    Sema.Function_type_resolution.resolve ~table ~parent []
  in
  let* functions =
    Sema.Function_resolution.resolve ~table ~parent ~compilation_mode:Jit []
  in
  let* calls =
    Sema.Function_call_resolution.resolve ~table ~parent ~members
      ~function_types ~functions ~expressions []
    |> Result.map_error Sema.Function_call_resolution.error_to_string
  in
  let* policies =
    Sema.Function_call_conversion_policy.analyze ~table ~parent ~headers ~calls
    |> Result.map_error Sema.Function_call_conversion_policy.error_to_string
  in
  let* globals = Sema.Global_type_resolution.resolve ~table ~parent [] in
  Ok { table; parent; expressions; members; globals; functions; policies }

let prepare context fragment =
  let table = context.table in
  let* bindings =
    Top_level_expression_binding.resolve_initializer_fragment ~table
      ~parent:context.parent ~module_expressions:context.expressions fragment
  in
  let* expressions =
    Sema.Top_level_outer_expression_binding.resolve ~table
      ~environment:(Sema.Initializer_fragment.environment fragment)
      ~expressions:bindings
    |> Result.map_error Sema.Top_level_outer_expression_binding.error_to_string
  in
  let* expressions =
    Top_level_expression_tree.build_initializer_fragment ~table ~expressions
      fragment
  in
  let* identifiers =
    Top_level_identifier_resolution.classify ~table ~globals:context.globals
      ~functions:context.functions ~expressions
  in
  Sema.Function_call_expression_result.analyze_top_level ~table
    ~members:context.members ~policies:context.policies ~identifiers expressions
  |> Result.map_error Sema.Function_call_expression_result.error_to_string
