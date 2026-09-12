type t

val create :
  authority:Sema.Offset_fragment.authority ->
  destination:Offset_fragment_destination.t ->
  lowered:Integer_program_lowering.t ->
  entry:X87_stack.t ->
  initialization:Global_initialization.t ->
  runtime_calls:Runtime_call_context.t ->
  (t, string) result

val destination : t -> Offset_fragment_destination.t
val lowered : t -> Integer_program_lowering.t
val entry : t -> X87_stack.t
val initialization : t -> Global_initialization.t
val runtime_calls : t -> Runtime_call_context.t

type code = Scheduled of t
type execution

val prepare :
  authority:Sema.Offset_fragment.authority ->
  destination:Offset_fragment_destination.t ->
  code:code ->
  steps:int ->
  (execution, string) result

val authority : execution -> Sema.Offset_fragment.authority
val execution_destination : execution -> Offset_fragment_destination.t
val code : execution -> code
val steps : execution -> int
