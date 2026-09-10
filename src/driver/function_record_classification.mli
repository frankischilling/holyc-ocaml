val classify :
  ?previous:Sema.Function_record_classification.classified_declaration list ->
  ?compiler_option_mask:int64 ->
  resolution:Sema.Function_resolution.t ->
  Frontend.Ast.module_ ->
  (Sema.Function_record_classification.t, string) result
(** Pair source-ordered modifier and import state with resolved functions. When
    no batch override is supplied, each resolved declaration contributes its
    retained compiler-option snapshot. *)

val classify_completed_header :
  ?previous:Sema.Function_record_classification.classified_declaration list ->
  resolution:Sema.Function_resolution.t ->
  Sema.Compiler_record.declared_function ->
  (Sema.Function_record_classification.t, string) result
