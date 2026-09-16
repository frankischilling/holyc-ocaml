type result = private {
  image : Backend.X86_64_expression.t;
  bits : int64;
  platform : Runtime.Native_execution.platform;
}

val compile :
  ?max_ir_instructions:int ->
  ?max_code_bytes:int ->
  ?max_stack_bytes:int ->
  ?status_abi:Backend.X86_64_expression.status_abi ->
  Session.t ->
  config:Frontend.Preprocessor.Config.t ->
  source:Common.Source_file.t ->
  (Backend.X86_64_expression.t, Common.Diagnostic.t list) Stdlib.result
(** Check one ordinary expression through the existing source/typed/IR pipeline,
    then compile the bounded native subset. This never allocates executable
    memory or evaluates the expression. Configuration is checked before parsing.
    [max_stack_bytes] defaults to 4088 and accepts 0 through 4088. Zero disables
    spilling; a nonzero frame includes the padding required by the host ABI.
    [status_abi] selects the private fault-result calling convention for code
    inspection or execution. It defaults to the host OS convention and has no
    effect on images without division/remainder. Execution rejects a foreign
    convention before allocating executable memory. *)

val evaluate :
  ?max_ir_instructions:int ->
  ?max_code_bytes:int ->
  ?max_stack_bytes:int ->
  ?status_abi:Backend.X86_64_expression.status_abi ->
  Session.t ->
  config:Frontend.Preprocessor.Config.t ->
  source:Common.Source_file.t ->
  (result, Common.Diagnostic.t list) Stdlib.result
(** Explicitly compile and execute the checked native expression using the host
    bridge. Reached divide-by-zero and signed-overflow faults become diagnostics
    with the original operation span after host teardown. This is separate from
    ordinary interpreter and compile-time work. *)
