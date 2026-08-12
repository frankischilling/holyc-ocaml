type output = {
  ast : Ast.module_ option;
  diagnostics : Common.Diagnostic.t list;
}

val max_pointer_depth : int
val max_expression_depth : int
val max_block_depth : int
val max_conditional_depth : int
val max_loop_depth : int
val max_lock_depth : int
val max_try_depth : int

val parse :
  sources:Common.Source_manager.t ->
  definitions:Definition.Environment.t ->
  symbols:Symbol_visibility.Environment.t ->
  config:Preprocessor.Config.t ->
  Common.Source_file.t ->
  output

val has_errors : output -> bool
