module Typed = Sema.Function_call_expression_result
module Resolution = Sema.Function_call_resolution

let direct_expression value =
  let selection =
    match Resolution.argument_expression_kind (Typed.result_source value) with
    | Resolution.Sizeof_expression sizeof -> (
        match Resolution.sizeof_root_resolution sizeof with
        | Resolution.Sizeof_top_level_query query ->
            Sema.Top_level_outer_expression_binding.query_selection query
        | Resolution.Sizeof_function_query (Resolution.Module_query query) ->
            Sema.Module_expression_binding.query_selection query
        | Resolution.Sizeof_function_query (Resolution.Outer_query query) ->
            Sema.Outer_expression_binding.query_selection query)
    | _ -> None
  in
  Option.fold ~none:[]
    ~some:(fun selection ->
      Sema.Query_selection.checked_read selection
      |> Sema.Compiler_record.query_runtime_dependencies)
    selection

let rec expression value =
  let pair =
    Option.fold ~none:[] ~some:(fun (left, right) ->
        expression left @ expression right)
  in
  direct_expression value
  @ Option.fold ~none:[] ~some:expression (Typed.result_operand value)
  @ pair (Typed.result_binary_operands value)
  @ pair (Typed.result_index_operands value)

let top_level typed =
  List.concat_map direct_expression (Typed.top_level_all_results typed)

let functions typed =
  List.concat_map direct_expression (Typed.all_results typed)

let frame frame =
  Sema.Function_frame_layout.function_locations frame
  |> List.concat_map (fun location ->
      Sema.Function_frame_layout.location_dimensions location
      |> List.concat_map
           Sema.Function_frame_layout.dimension_runtime_dependencies)
