type t

val create :
  authority:Sema.Internal_binding_fragment.authority ->
  destination:Internal_binding_fragment_destination.t ->
  lowered:Integer_program_lowering.t ->
  entry:X87_stack.t ->
  initialization:Global_initialization.t ->
  runtime_calls:Runtime_call_context.t ->
  (t, string) result

val destination : t -> Internal_binding_fragment_destination.t
val lowering : t -> Integer_program_lowering.t
val entry : t -> X87_stack.t
val initialization : t -> Global_initialization.t
val runtime_calls : t -> Runtime_call_context.t
val source_authority : t -> Sema.Internal_binding_fragment.authority

type code = Scheduled of t
type execution

val prepare :
  authority:Sema.Internal_binding_fragment.authority ->
  destination:Internal_binding_fragment_destination.t ->
  code:code ->
  steps:int ->
  (execution, string) result

val authority : execution -> Sema.Internal_binding_fragment.authority
val execution_destination : execution -> Internal_binding_fragment_destination.t
val code : execution -> code
val steps : execution -> int
