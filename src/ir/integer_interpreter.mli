type word_type = I64 | U64
type word = private { type_ : word_type; bits : int64 }

type function_definition = {
  frame : Sema.Function_frame_layout.function_layout;
  body : Function_body.t;
}

type termination = Stream_end | Returned of word option
type error_stage = Configuration | Preflight | Execution

type error = private {
  stage : error_stage;
  code : string;
  message : string;
  executed_steps : int;
  block_id : int option;
  instruction_id : int option;
  span : Common.Span.t option;
  function_id : int option;
  function_name : string option;
  initializer_phase : Global_initialization.phase option;
  initializer_symbol_id : int option;
  initializer_name : string option;
}

type t

val reference_commit : string

val execute_program :
  ?globals:Integer_globals.t ->
  ?initialization:Global_initialization.t ->
  ?max_global_bytes:int ->
  max_steps:int ->
  max_frame_bytes:int ->
  max_call_depth:int ->
  functions:function_definition list ->
  X87_stack.t ->
  (t, error list) result
(** Preflight the entry and every checked definition, then execute direct calls
    with explicit continuations and independent slots. Limits cover the total
    instruction count, simultaneously active frame bytes and active calls.
    Checked public I64/U64 call results may participate in top-level unary and
    binary operations without requiring a local frame. [globals] supplies exact
    shared scalar objects, bounded separately by positive [max_global_bytes]
    (default 1,048,576). Every execution owns fresh words; calls and block
    transfers preserve them. Prepared constants supply initial-image bits; other
    AOT code-heap words start at zero, while a reached unknown JIT word produces
    a labeled hosted diagnostic. Initializer-bearing storage requires the exact
    [initialization] context. Its regions preserve declaration owner and
    compile/load phase through calls and faults, and do not replace the last
    ordinary top-level expression value. Canonical scalar compound assignments
    and prefix/postfix updates read storage at their own instruction, after RHS
    effects. They store only successful arithmetic results and retain the
    destination class (including division and right-shift signedness).
    Prefix/compound results are new words; postfix results are old words. *)

val final_value : t -> word option
(** Last reached top-level expression value from [execute_program], separate
    from stream termination and function return values. *)

val execute : max_steps:int -> X87_stack.t -> (t, error list) result
(** Preflight and execute the source-audited integer subset. Every executed
    instruction consumes one step, and all unsupported instructions are rejected
    before execution, including instructions in unreachable blocks. Division and
    remainder use signed truncation for two I64 operands and unsigned arithmetic
    otherwise. Zero divisors and signed minimum divided or reduced modulo minus
    one fail at execution, consuming the faulting instruction's step. Zero-flag
    [IC_HOLYC_TYPECAST] accepts internal I64/U64 word views with a zero/one
    parenthesis payload and preserves the exact bits. Other cast domains remain
    unsupported. *)

val execute_function :
  max_steps:int ->
  max_frame_bytes:int ->
  frame:Sema.Function_frame_layout.function_layout ->
  arguments:int64 list ->
  Function_body.t ->
  (t, error list) result
(** Execute one verified ordinary I64/U64 function with its exact checked frame.
    Argument bits initialize the named parameter slots in source order.
    Automatic locals begin uninitialized. The allocation bound includes
    parameter slots and the checked local frame size. Canonical frame addresses,
    loads, assignments and scalar updates are preflighted before execution; slot
    contents survive block transfers. Every invocation owns independent storage.
    Public U64 negation retains U64, while internal U64 negation yields internal
    I64. Same-width integer returns preserve bits and adopt the declared type.
    This entry point does not execute calls or arbitrary pointer operations. *)

val termination : t -> termination
val executed_steps : t -> int

val compiled_initializer_steps : t -> int
(** Constant preparation evidence retained by the initialization context. These
    steps are separate from runtime [executed_steps] and its instruction limit.
*)

val human : t -> string
(** Render the versioned, deterministic execution result. *)
