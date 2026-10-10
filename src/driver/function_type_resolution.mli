val resolve :
  ?retained_headers:Sema.Function_type_resolution.resolved_function list ->
  ?retained_item_index_offset:int ->
  ?selected_types:
    Sema.Declaration_collection.namespace
    * Sema.Function_type_resolution.selected_aggregate_resolver ->
  table:Sema.Symbol_table.t ->
  declarations:Sema.Declaration_collection.t ->
  aggregates:Sema.Aggregate_resolution.t ->
  functions:Sema.Function_collection.t ->
  Frontend.Ast.module_ ->
  (Sema.Function_type_resolution.t, string) result

val resolve_completed_callback :
  ?selected_aggregate:Sema.Function_type_resolution.selected_aggregate_resolver ->
  table:Sema.Symbol_table.t ->
  namespace:Sema.Declaration_collection.namespace ->
  Frontend.Parser.completed_callback_signature ->
  (Sema.Function_type_resolution.function_pointer, string) result
(** Type the original completed anonymous header without publishing storage,
    allocating a function scope or granting executable identity. *)

val resolve_provisional_call :
  ?scope:Sema.Symbol_table.scope ->
  ?selected_aggregate:Sema.Function_type_resolution.selected_aggregate_resolver ->
  table:Sema.Symbol_table.t ->
  namespace:Sema.Declaration_collection.namespace ->
  Sema.Function_record_phase.checked_call_shape ->
  (Sema.Function_type_resolution.resolved_function, string) result
(** Type the exact native member cursor while preserving explicit call-only
    evidence. Type and ownership failures precede scope allocation. An existing
    owning function scope can be reused across phases; no parameter or synthetic
    locals are created, and this projection cannot authorize a function body. *)

val resolve_completed_header :
  ?selected_aggregate:Sema.Function_type_resolution.selected_aggregate_resolver ->
  table:Sema.Symbol_table.t ->
  namespace:Sema.Declaration_collection.namespace ->
  Sema.Compiler_record.declared_function ->
  (Sema.Function_type_resolution.resolved_function, string) result
(** Resolve the original completed header under its declaration's exact table
    and namespace ownership, retaining its parameter AST children. This creates
    a parameter-only function scope with item index zero, without constructing a
    module, body, command seal, executable frame or runtime admission. Named
    aggregate pointers require selected source-type evidence; aggregate values
    remain outside this retained-header path. *)

val resolve_completed_header_with_collection :
  ?selected_aggregate:Sema.Function_type_resolution.selected_aggregate_resolver ->
  table:Sema.Symbol_table.t ->
  namespace:Sema.Declaration_collection.namespace ->
  Sema.Compiler_record.declared_function ->
  ( Sema.Function_collection.collected_function
    * Sema.Function_type_resolution.resolved_function,
    string )
  result
(** Resolve one completed header while retaining the exact parameter collection
    needed by eventual body completion. Named aggregate pointers require their
    original occurrence proof; no current namespace lookup is performed. *)

val resolve_native_header_types :
  ?selected_aggregate:Sema.Function_type_resolution.selected_aggregate_resolver ->
  table:Sema.Symbol_table.t ->
  namespace:Sema.Declaration_collection.namespace ->
  Sema.Function_record_phase.snapshot ->
  ( Sema.Type_reference.t
    * (Sema.Function_record_phase.header_member * Sema.Type.t) list,
    string )
  result
(** Resolve the full original header cursor without applying arg_cnt or
    allocating a call scope. Incomplete members retain their original published
    type source; argc/argv retain the pinned internal I64 member class. *)

val resolve_publication_return_type :
  ?selected_aggregate:Sema.Function_type_resolution.selected_aggregate_resolver ->
  table:Sema.Symbol_table.t ->
  namespace:Sema.Declaration_collection.namespace ->
  Frontend.Parser.function_publication ->
  (Sema.Type_reference.t, string) result
(** Resolve the retained return occurrence without constructing a body or call.
*)

val resolve_native_header_return_type :
  ?selected_aggregate:Sema.Function_type_resolution.selected_aggregate_resolver ->
  table:Sema.Symbol_table.t ->
  namespace:Sema.Declaration_collection.namespace ->
  Sema.Function_record_phase.snapshot ->
  (Sema.Type_reference.t, string) result

val resolve_native_header_member_type :
  ?selected_aggregate:Sema.Function_type_resolution.selected_aggregate_resolver ->
  table:Sema.Symbol_table.t ->
  namespace:Sema.Declaration_collection.namespace ->
  Sema.Function_record_phase.snapshot ->
  Sema.Function_record_phase.header_member ->
  (Sema.Type.t, string) result
(** Resolve one original member after checking its physical snapshot membership.
    These separate reads preserve the compiler's comparison short circuits. *)
