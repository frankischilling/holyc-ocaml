val layout :
  ?original_definitions:
    (Frontend.Ast.aggregate_definition * Frontend.Ast.aggregate_definition) list ->
  ?inherited_metadata:Sema.Compiler_record.inherited_metadata list ->
  ?offsets:
    (Frontend.Ast.expression ->
    (Sema.Compiler_record.aggregate_offset, string) result) ->
  ?prepared:
    (Frontend.Ast.array_dimension ->
    (Sema.Compiler_record.declared_dimension, string) result) ->
  table:Sema.Symbol_table.t ->
  declarations:Sema.Declaration_collection.t ->
  aggregates:Sema.Aggregate_resolution.t ->
  headers:Sema.Aggregate_header_resolution.t ->
  members:Sema.Member_type_resolution.t ->
  Frontend.Ast.module_ ->
  (Sema.Aggregate_layout.t, string) result
(** Validate that all semantic inputs describe the same AST, retain the layout
    expressions from that AST, and calculate the closed aggregate layouts. A
    source command supplies its original checked member bounds through
    [prepared] and its original checked offsets through [offsets]. Both
    resolvers must return receipts for the exact expression/dimension and
    semantic table. Original inherited metadata proofs preserve their reached
    sizes separately; they cannot grant an object layout or member index. *)
