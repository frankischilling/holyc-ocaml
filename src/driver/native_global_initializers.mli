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
(** Seal successful original scalar and array-leaf preparation to its exact
    compiled bundle. Prepared JIT publications require their original lowerer
    receipt and must all precede the first entry instruction. AOT load regions
    require their original ordered leaf and destination receipts in the same
    bundle. Reconstructed values and missing or foreign receipts reject. *)

val matches :
  t ->
  runtime_calls:Ir.Runtime_call_context.t ->
  initialization:Ir.Global_initialization.t ->
  entry:Ir.X87_stack.t ->
  functions:Ir.Integer_interpreter.function_definition list ->
  bool

val is_load_slot : t -> Ir.Integer_globals.slot -> bool
(** Read exact storage containing an original checked AOT load-time region. *)

val is_load_root :
  t ->
  Ir.Integer_globals.slot ->
  Sema.Function_call_expression_result.top_level_root_result ->
  bool
(** Check physical membership of one load-time root in its original storage. *)

val matches_storage :
  t -> initialization:Ir.Global_initialization.t -> entry:Ir.X87_stack.t -> bool
