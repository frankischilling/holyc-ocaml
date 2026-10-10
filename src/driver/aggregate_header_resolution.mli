val resolve :
  ?original_definitions:
    (Frontend.Ast.aggregate_definition * Frontend.Ast.aggregate_definition) list ->
  ?inherited_metadata:Sema.Compiler_record.inherited_metadata list ->
  table:Sema.Symbol_table.t ->
  declarations:Sema.Declaration_collection.t ->
  aggregates:Sema.Aggregate_resolution.t ->
  Frontend.Ast.module_ ->
  (Sema.Aggregate_header_resolution.t, string) result
(** Resolve aggregate backing and base types at their TempleOS publication
    points, using canonical identities from [aggregates]. Original inherited
    metadata proofs exclude only their exact definitions from object admission.
*)

val resolve_metadata :
  table:Sema.Symbol_table.t ->
  parent:Sema.Symbol_table.scope ->
  (int * Sema.Compiler_record.aggregate_value_header) list ->
  (Sema.Aggregate_header_resolution.t, string) result
(** Resolve raw class metadata from original completed definitions in source
    order. These headers grant no member layout or object storage. *)
