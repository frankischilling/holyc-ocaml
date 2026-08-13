val build :
  table:Sema.Symbol_table.t ->
  declarations:Sema.Declaration_collection.t ->
  headers:Sema.Aggregate_header_resolution.t ->
  members:Sema.Member_type_resolution.t ->
  layouts:Sema.Aggregate_layout.t ->
  (Sema.Aggregate_member_index.t, string) result
(** Reconcile the checked header, member-type, and byte-layout results, then
    build the source-ordered direct and inherited member index. *)
