val classify :
  ?compiler_option_mask:int64 ->
  resolution:Sema.Function_resolution.t ->
  Frontend.Ast.module_ ->
  (Sema.Function_record_classification.t, string) result
(** Pair source-ordered modifier and import state with resolved functions. When
    no batch override is supplied, each resolved declaration contributes its
    retained compiler-option snapshot. *)
