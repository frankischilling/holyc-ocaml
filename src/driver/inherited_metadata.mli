val contains :
  table:Sema.Symbol_table.t ->
  scope:Sema.Symbol_table.scope ->
  Sema.Compiler_record.inherited_metadata list ->
  Frontend.Ast.aggregate_definition ->
  bool
(** Only an opaque proof of this original definition's completed inherited
    metadata can remove it from object layout and member index admission. *)

type storage

val prepare_storage :
  table:Sema.Symbol_table.t ->
  scope:Sema.Symbol_table.scope ->
  aggregates:Sema.Aggregate_resolution.t ->
  ast:Frontend.Ast.module_ ->
  Sema.Compiler_record.inherited_metadata list ->
  storage
(** Admit only original completed base definitions already encountered in this
    module. Each selected base must itself have object layout admission. *)

val metadata_only : storage -> Sema.Compiler_record.inherited_metadata list

val validate_storage :
  storage -> layouts:Sema.Aggregate_layout.t -> (unit, string) result
(** Check the completed child and base sizes and their exact identities before
    publishing a member index or executing storage. *)
