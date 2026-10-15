type t

val is_scalar_evaluation : t -> bool

val create :
  ?scalar_lowered:Integer_program_lowering.t ->
  destination:Static_initializer_destination.t ->
  entry:X87_stack.t ->
  initialization:Global_initialization.t ->
  runtime_calls:Runtime_call_context.t ->
  unit ->
  (t, string) result

val destination : t -> Static_initializer_destination.t
val entry : t -> X87_stack.t
val initialization : t -> Global_initialization.t
val runtime_calls : t -> Runtime_call_context.t
