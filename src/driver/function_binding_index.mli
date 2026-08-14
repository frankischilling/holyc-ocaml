val build :
  table:Sema.Symbol_table.t ->
  declarations:Sema.Declaration_collection.t ->
  functions:Sema.Function_collection.t ->
  function_types:Sema.Function_type_resolution.t ->
  local_types:Sema.Local_type_resolution.t ->
  (Sema.Function_binding_index.t, string) result
(** Reconcile collected bindings with resolved parameter and local types, then
    validate and index each function-wide namespace. *)
