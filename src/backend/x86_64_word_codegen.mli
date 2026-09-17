type word_type = I64 | U64
type status_abi = X86_64_encoder.status_abi = Windows_x64 | System_v_x64
type arithmetic_operation = Divide | Remainder
type error = { code : string; message : string; span : Common.Span.t option }

type arithmetic_fault_site = {
  site : int;
  operation : arithmetic_operation;
  instruction_id : int;
  position : int;
  span : Common.Span.t option;
  signed : bool;
}

type expression_image

type program_owner =
  | Entry_owner
  | Function_owner of { function_id : int; function_name : string }

type program_site = {
  site : int;
  owner : program_owner;
  block_id : int;
  instruction_id : int;
  position : int;
  global_position : int;
  span : Common.Span.t option;
  arithmetic : (arithmetic_operation * bool) option;
  value_type : word_type option;
  call_site : bool;
  uninitialized_read_site : bool;
}

type program_image

val hard_ir_limit : int
val hard_max_stack_bytes : int
val hard_block_limit : int

val validate_limits :
  max_ir_instructions:int -> max_code_bytes:int -> (unit, error list) result

val validate_stack_limit : max_stack_bytes:int -> (unit, error list) result
val validate_block_limit : max_blocks:int -> (unit, error list) result
val default_status_abi : unit -> status_abi

val compile_expression :
  ?status_abi:status_abi ->
  ?max_stack_bytes:int ->
  max_ir_instructions:int ->
  max_code_bytes:int ->
  Ir.X87_stack.t ->
  (expression_image, error list) result

val expression_code : expression_image -> string
val expression_word_type : expression_image -> word_type
val expression_ir_instructions : expression_image -> int
val expression_machine_instructions : expression_image -> int
val expression_register_peak : expression_image -> int
val expression_frame_bytes : expression_image -> int
val expression_windows_unwind_info : expression_image -> string
val expression_status_abi : expression_image -> status_abi option
val expression_fault_sites : expression_image -> arithmetic_fault_site list

val compile_program :
  ?status_abi:status_abi ->
  ?max_stack_bytes:int ->
  ?max_blocks:int ->
  max_ir_instructions:int ->
  max_code_bytes:int ->
  Ir.X87_stack.t ->
  (program_image, error list) result

val compile_callable :
  ?status_abi:status_abi ->
  ?max_stack_bytes:int ->
  ?max_blocks:int ->
  max_ir_instructions:int ->
  max_code_bytes:int ->
  runtime_calls:Ir.Runtime_call_context.t ->
  initialization:Ir.Global_initialization.t ->
  entry:Ir.X87_stack.t ->
  functions:Ir.Integer_interpreter.function_definition list ->
  unit ->
  (program_image, error list) result
(** Compile one checked ordinary-source entry plus its exact fixed scalar source
    functions as one callable native bundle. All entry and named-function
    graphs, including unreachable definitions, pass preflight before machine
    allocation. Direct calls are resolved only through the supplied sealed
    runtime-call context and exact function/frame ownership. *)

val program_code : program_image -> string
val program_windows_unwind_info : program_image -> string

val program_windows_unwind_functions :
  program_image -> (int * int * string) list
(** [begin_offset,end_offset,unwind_info] for every emitted machine function.
    Callable images return the entry first followed by source functions; closed
    images retain one entry record. Returned unwind strings are fresh copies. *)

val program_status_abi : program_image -> status_abi
val program_ir_instructions : program_image -> int
val program_machine_instructions : program_image -> int
val program_register_peak : program_image -> int
val program_frame_bytes : program_image -> int
val program_block_count : program_image -> int
val program_function_count : program_image -> int
val program_entry_stack_bytes : program_image -> int
val program_sites : program_image -> program_site list
