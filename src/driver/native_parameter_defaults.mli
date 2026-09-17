type t

val create :
  globals:Ir.Integer_globals.t ->
  runtime_calls:Ir.Runtime_call_context.t ->
  initialization:Ir.Global_initialization.t ->
  entry:Ir.X87_stack.t ->
  functions:Ir.Integer_interpreter.function_definition list ->
  prepared:Ir.Prepared_parameter_default.t list ->
  completions:Native_default_preparation.completion list ->
  (t, string) result
(** Seal the exact declaration-time scalar defaults admitted for one native
    callable bundle. Every default-bearing selected/source header in every
    supplied definition, including unreachable definitions, must be covered by
    its original prepared value and opaque charged native preparation receipt.
    Foreign, duplicate and unused evidence is rejected. Saved values retain
    their full bits and exact declared parameter type, including narrow types.
*)

val matches :
  t ->
  globals:Ir.Integer_globals.t ->
  runtime_calls:Ir.Runtime_call_context.t ->
  initialization:Ir.Global_initialization.t ->
  entry:Ir.X87_stack.t ->
  functions:Ir.Integer_interpreter.function_definition list ->
  bool

val admits :
  t ->
  prepared:Ir.Prepared_parameter_default.t ->
  header:Sema.Function_type_resolution.resolved_function ->
  parameter:Sema.Function_type_resolution.parameter ->
  bool
(** Require the exact prepared object sealed into this proof and its original
    header/parameter authority. A value with equal bits is not sufficient. *)
