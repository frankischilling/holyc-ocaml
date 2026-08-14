val classify :
  ?compiler_option_mask:int64 ->
  resolution:Sema.Global_resolution.t ->
  Frontend.Ast.module_ ->
  (Sema.Global_record_classification.t, string) result
(** Pair the AST's ordered modifier state with the resolved global records. When
    no batch override is supplied, each record contributes its retained
    compiler-option snapshot. *)
