val resolve :
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
