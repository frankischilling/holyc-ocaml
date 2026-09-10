type t

val create :
  authority:Sema.Default_fragment.authority ->
  destination:Default_fragment_destination.t ->
  entry:X87_stack.t ->
  initialization:Global_initialization.t ->
  runtime_calls:Runtime_call_context.t ->
  (t, string) result

val destination : t -> Default_fragment_destination.t
val entry : t -> X87_stack.t
val initialization : t -> Global_initialization.t
val runtime_calls : t -> Runtime_call_context.t

type code = Prepared of int64 | Scheduled of t
type execution

val prepare :
  authority:Sema.Default_fragment.authority ->
  destination:Default_fragment_destination.t ->
  code:code ->
  steps:int ->
  (execution, string) result

val authority : execution -> Sema.Default_fragment.authority
val execution_destination : execution -> Default_fragment_destination.t
val code : execution -> code
val steps : execution -> int
