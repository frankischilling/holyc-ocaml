val classify :
  ?compiler_option_mask:int64 ->
  resolution:Sema.Function_resolution.t ->
  Frontend.Ast.module_ ->
  (Sema.Function_record_classification.t, string) result
(** Pair source-ordered modifier and import state with resolved functions. Until
    compile-time option execution supplies source-position snapshots, the driver
    applies one explicit option mask to the batch. *)
