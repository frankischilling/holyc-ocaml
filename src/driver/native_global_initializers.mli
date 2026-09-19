type t

val create :
  span:Common.Span.t ->
  static_completions:Native_default_preparation.static_completion list ->
  completions:Native_default_preparation.initializer_completion list ->
  preparation:Integer_initializers.t ->
  runtime_calls:Ir.Runtime_call_context.t ->
  initialization:Ir.Global_initialization.t ->
  entry:Ir.X87_stack.t ->
  functions:Ir.Integer_interpreter.function_definition list ->
  (t, string) result
(** Seal successful original scalar preparation to its exact compiled bundle.
    Reconstructed values, missing receipts and scheduled initialization reject.
*)

val matches :
  t ->
  runtime_calls:Ir.Runtime_call_context.t ->
  initialization:Ir.Global_initialization.t ->
  entry:Ir.X87_stack.t ->
  functions:Ir.Integer_interpreter.function_definition list ->
  bool

val matches_storage :
  t -> initialization:Ir.Global_initialization.t -> entry:Ir.X87_stack.t -> bool
