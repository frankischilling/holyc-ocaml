type t

val create :
  authority:Sema.Dimension_fragment.authority ->
  destination:Dimension_fragment_destination.t ->
  entry:X87_stack.t ->
  initialization:Global_initialization.t ->
  runtime_calls:Runtime_call_context.t ->
  (t, string) result

val destination : t -> Dimension_fragment_destination.t
val entry : t -> X87_stack.t
val initialization : t -> Global_initialization.t
val runtime_calls : t -> Runtime_call_context.t

type code = Scheduled of t
type execution

val prepare :
  authority:Sema.Dimension_fragment.authority ->
  destination:Dimension_fragment_destination.t ->
  code:code ->
  steps:int ->
  (execution, string) result

val authority : execution -> Sema.Dimension_fragment.authority
val execution_destination : execution -> Dimension_fragment_destination.t
val code : execution -> code
val steps : execution -> int
