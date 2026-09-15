type result = private {
  image : Backend.X86_64_expression.t;
  bits : int64;
  platform : Runtime.Native_execution.platform;
}

val compile :
  ?max_ir_instructions:int ->
  ?max_code_bytes:int ->
  Session.t ->
  config:Frontend.Preprocessor.Config.t ->
  source:Common.Source_file.t ->
  (Backend.X86_64_expression.t, Common.Diagnostic.t list) Stdlib.result
(** Check one ordinary expression through the existing source/typed/IR pipeline,
    then compile the bounded native subset. This never allocates executable
    memory or evaluates the expression. Configuration is checked before parsing.
*)

val evaluate :
  ?max_ir_instructions:int ->
  ?max_code_bytes:int ->
  Session.t ->
  config:Frontend.Preprocessor.Config.t ->
  source:Common.Source_file.t ->
  (result, Common.Diagnostic.t list) Stdlib.result
(** Explicitly compile and execute the checked native expression using the host
    bridge. This is separate from ordinary interpreter and compile-time work. *)
