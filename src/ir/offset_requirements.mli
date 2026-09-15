val expression :
  Sema.Function_call_expression_result.expression_result ->
  Sema.Compiler_record.aggregate_offset list

val top_level :
  Sema.Function_call_expression_result.top_level_t ->
  Sema.Compiler_record.aggregate_offset list

val functions :
  Sema.Function_call_expression_result.t ->
  Sema.Compiler_record.aggregate_offset list

val frame :
  Sema.Function_frame_layout.function_layout ->
  Sema.Compiler_record.aggregate_offset list
