type word_type = X86_64_expression.word_type = I64 | U64
type status_abi = X86_64_encoder.status_abi = Windows_x64 | System_v_x64

type error = X86_64_expression.error = {
  code : string;
  message : string;
  span : Common.Span.t option;
}

type word = private { type_ : word_type; bits : int64 }

type fault_kind =
  | Division_by_zero
  | Signed_division_overflow
  | Step_limit_exceeded
  | Call_depth_exceeded
  | Frame_limit_exceeded
  | Native_stack_limit_exceeded
  | Uninitialized_read

type arithmetic_operation = X86_64_expression.arithmetic_operation =
  | Divide
  | Remainder

type fault = private {
  kind : fault_kind;
  operation : arithmetic_operation option;
  block_id : int;
  instruction_id : int;
  position : int;
  global_position : int;
  span : Common.Span.t option;
  executed_steps : int;
  function_id : int option;
  function_name : string option;
}

type execution = private { executed_steps : int; final_value : word option }
type outcome = Completed of execution | Fault of fault
type t

val hard_max_stack_bytes : int
(** Maximum shared spill frame size: 4088 bytes. *)

val validate_limits :
  max_ir_instructions:int -> max_code_bytes:int -> (unit, error list) result

val validate_stack_limit : max_stack_bytes:int -> (unit, error list) result

val validate_block_limit : max_blocks:int -> (unit, error list) result
(** [max_blocks] must be positive and at most 100,000. *)

val compile :
  ?status_abi:status_abi ->
  ?max_stack_bytes:int ->
  ?max_blocks:int ->
  max_ir_instructions:int ->
  max_code_bytes:int ->
  Ir.X87_stack.t ->
  (t, error list) result
(** Compile every block of one verified closed integer program. Block-local word
    values are allocated independently while the emitted image uses one spill
    frame sized to the maximum block requirement. R11 holds the private context
    and R10 the remaining step budget, leaving five value registers. The default
    block bound is 4096. *)

val compile_callable :
  ?status_abi:status_abi ->
  ?max_stack_bytes:int ->
  ?max_blocks:int ->
  ?parameter_defaults:Driver.Native_parameter_defaults.t ->
  max_ir_instructions:int ->
  max_code_bytes:int ->
  runtime_calls:Ir.Runtime_call_context.t ->
  initialization:Ir.Global_initialization.t ->
  entry:Ir.X87_stack.t ->
  functions:Ir.Integer_interpreter.function_definition list ->
  unit ->
  (t, error list) result
(** Compile a checked entry and its exact fixed scalar integer functions and U0
    procedures as one native callable bundle. Parameters and automatic objects
    use their declared integer widths; word returns retain full register bits.
    Every named definition is preflighted, including definitions unreachable
    from the entry. Calls are emitted only from exact sealed runtime-call
    metadata and preserve the shared native status context. The exact
    initialization context must be supplied even when empty; persistent storage,
    initialization regions, publications and their preparation work are not
    admitted. Declaration-time parameter defaults require [parameter_defaults]
    from the exact source preparation; omitting it preserves the low-level
    rejection. *)

val code : t -> string
val windows_unwind_info : t -> string
val windows_unwind_functions : t -> (int * int * string) list
val status_abi : t -> status_abi
val ir_instructions : t -> int
val machine_instructions : t -> int

val register_peak : t -> int
(** Peak allocator-visible register pressure. Callable images reserve R10/R11
    for the shared runtime context/budget; dedicated RBP/RSP frame bases are not
    allocator-visible and are excluded from this metric. *)

val frame_bytes : t -> int
val block_count : t -> int

val function_count : t -> int
(** Number of emitted named source functions. Closed native programs return 0.
*)

val entry_stack_bytes : t -> int
(** Exact root stack footprint charged by the host bridge before entry: the host
    CALL return slot plus the entry's generated saved-RBP/allocation footprint.
*)

val decode_runtime_status :
  t ->
  max_steps:int ->
  kind:int64 ->
  site:int64 ->
  executed_steps:int64 ->
  value_site:int64 ->
  bits:int64 ->
  (outcome, string) result
(** Validate the status projection from the private execution context.
    [value_site] is the dense one-based site of the last reached entry
    IC_END_EXP; its checked metadata supplies the result type. Zero value-site
    requires zero bits. Clean completion is kind/site zero with at least one
    executed IR instruction. Step-limit faults require an executed count exactly
    equal to [max_steps]; arithmetic, call-quota and uninitialized-read faults
    must name a matching checked dense site. The native bridge validates the
    three restored callable quota words before invoking this decoder. *)
