val classify :
  ?compiler_option_mask:int64 ->
  resolution:Sema.Global_resolution.t ->
  Frontend.Ast.module_ ->
  (Sema.Global_record_classification.t, string) result
(** Pair the AST's ordered modifier state with the resolved global records.
    Until compile-time option execution supplies source-position snapshots, the
    driver applies one explicit option mask to the batch. *)
