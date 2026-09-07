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
    Prefix/compound results are new words; postfix results are old words. Static
    locals share persistent words and the global byte bound, occupy no
    invocation slots, and require their exact declaring frame and options.
    Static address producers and storage consumers also accept their owner's
    checked entry initializer region. Other entry instructions and functions
    fail preflight; region authority never enters a called function. JIT static
    calls require transitively earlier definitions in the supplied bodies.
    Checked one-level I64/U64/U8 pointer locals and fixed parameters hold
    references to scalar objects and automatic I64/U64/U8 array elements. U8
    memory stores retain the low eight bits and reads zero-extend them. Plain
    assignment expressions retain the full RHS bits independently of narrowed
    storage. Arithmetic retains checked raw classes: U8 with I64 selects I64,
    while two U8 operands retain U8 computation class without truncating the
    register result. U8 compound and prefix/postfix updates remain unsupported,
    as does U8 unary minus, whose pinned result class is the unsupported I8.
    Checked indexing retains declared-object extents and remaining array
    strides; grouping/materialization consumes dimensions. Final pointer values
    may be one-past; memory access must be within the original object. Scale/add
    overflow and object bounds report HCIRVM0020 and HCIRVM0019 respectively.
    Offsets, strides and object extents use actual element byte widths. Active
    allocation charges the checked padded local frame plus eight-byte parameter
    slots. Cell counts and allocation bytes are bounded before expansion.
    [IC_ADDR] materializes a reference without reading the object, with its own
    static-consumer ownership check. Dereferences and updates retain the actual
    caller or recursive activation, even when slots have identical offsets.
    Explicit references can pass to callees without granting canonical
    static-address authority. Returned frames are invalidated; pointer returns,
    arbitrary integer addresses and pointer arithmetic remain unsupported.
    Public results remain words. *)

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
    Checked pointer locals can reference this invocation's scalar slots and
    automatic I64/U64/U8 array elements through checked indexing. U8 automatic
    scalars and elements have independent byte initialization state. The
    [arguments] bit interface cannot supply pointer parameters. This entry point
    does not execute calls or arbitrary pointer operations. *)

val termination : t -> termination
val executed_steps : t -> int

val compiled_initializer_steps : t -> int
(** Constant preparation evidence retained by the initialization context. These
    steps are separate from runtime [executed_steps] and its instruction limit.
*)

val human : t -> string
(** Render the versioned, deterministic execution result. *)
