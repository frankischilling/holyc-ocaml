val resolve :
  table:Sema.Symbol_table.t ->
  declarations:Sema.Declaration_collection.t ->
  function_types:Sema.Function_type_resolution.t ->
  local_types:Sema.Local_type_resolution.t ->
  global_types:Sema.Global_type_resolution.t ->
  functions:Sema.Function_resolution.t ->
  expressions:Sema.Module_expression_binding.t ->
  Frontend.Ast.module_ ->
  (Sema.Function_call_resolution.t, string) result
(** Collect syntactically direct calls from function bodies, associate each
    callee with its ordinary-expression occurrence, retain audited argument
    source classes, typed bound identifiers, recursive prefix, postfix, and
    binary operands, and source-visible cast targets, and resolve fixed and
    variadic slots against the source-visible function header. *)
