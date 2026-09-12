type t

val create :
  authority:Sema.Initializer_fragment.authority ->
  destination:Initializer_fragment_destination.t ->
  entry:X87_stack.t ->
  initialization:Global_initialization.t ->
  runtime_calls:Runtime_call_context.t ->
  (t, string) result
(** Internal driver seal; this constructor is omitted from the maintained public
    library interface. Exact source, graph and call objects are retained. *)

val authority : t -> Sema.Initializer_fragment.authority
val destination : t -> Initializer_fragment_destination.t
val entry : t -> X87_stack.t
val initialization : t -> Global_initialization.t
val runtime_calls : t -> Runtime_call_context.t

type execution_code = private
  | Prepared of Integer_array_initializers.payload
  | Scheduled of t

type execution

val prepared_code : Integer_array_initializers.payload -> execution_code
val scheduled_code : t -> execution_code

val prepare :
  authority:Sema.Initializer_fragment.authority ->
  destination:Initializer_fragment_destination.t ->
  code:execution_code ->
  steps:int ->
  (execution, string) result
(** Internal driver seal after the common optimizer-domain and preparation
    checks. These constructors are absent from the public library interface. *)

val execution_authority : execution -> Sema.Initializer_fragment.authority
val execution_destination : execution -> Initializer_fragment_destination.t
val execution_code : execution -> execution_code
val execution_steps : execution -> int
