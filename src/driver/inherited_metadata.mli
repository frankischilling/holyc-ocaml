val contains :
  table:Sema.Symbol_table.t ->
  scope:Sema.Symbol_table.scope ->
  Sema.Compiler_record.inherited_metadata list ->
  Frontend.Ast.aggregate_definition ->
  bool
(** Only an opaque proof of this original definition's completed inherited
    metadata can remove it from object layout and member index admission. *)
