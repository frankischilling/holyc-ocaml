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
type report

val reference_commit : string

val execute_program :
  ?runtime_calls:Runtime_call_context.t ->
  ?globals:Integer_globals.t ->
  ?initialization:Global_initialization.t ->
  ?max_global_bytes:int ->
  ?max_literal_bytes:int ->
  ?max_output_bytes:int ->
  ?max_output_work:int ->
  max_steps:int ->
  max_frame_bytes:int ->
  max_call_depth:int ->
  functions:function_definition list ->
  X87_stack.t ->
  (t, error list) result
(** Preflight the entry and every checked definition, then execute direct calls
    with explicit continuations and independent slots. Limits cover the total
    instruction count, simultaneously active frame bytes and active calls.
    Ordinary U0 functions complete without a word through bare return or
    fallthrough. Call completion distinguishes pending, no-value and word
    results; the canonical U0 call-end result remains a private no-value marker
    accepted only by checked expression discard. U0 is not a word, memory value
    or numeric argument. Word functions still require their own reached return
    value, regardless of results from other calls. These are hosted execution
    boundaries; the pinned compiler warns for missing word return values and
    value-returning U0 functions rather than rejecting those source forms.
    Checked nonzero integer call results may participate in supported top-level
    operations without requiring a local frame. Narrow returns preserve full
    register bits as runtime I64/U64 according to declared signedness; narrow
    parameters normalize to declared width and signedness at each entry.
    [globals] supplies exact shared scalar objects and fixed arrays, bounded
    separately by positive [max_global_bytes] (default 1,048,576). Every
    execution owns fresh words; calls and block transfers preserve them.
    Prepared AOT array leaves supply sparse initial-image overlays. Prepared JIT
    array leaves publish once at their checked module-entry instruction
    boundary, before that instruction, without runtime step charges or changes
    to the final expression value. Callees cannot trigger or replay those
    publications. Scalar prepared constants supply initial-image bits; other AOT
    code-heap words start at zero, while a reached unknown JIT word produces a
    labeled hosted diagnostic. Initializer-bearing storage requires the exact
    [initialization] context. Its regions preserve declaration owner and
    compile/load phase through calls and faults, and do not replace the last
    ordinary top-level expression value. Canonical scalar compound assignments
    and prefix/postfix updates read storage at their own instruction, after RHS
    effects. They store only successful arithmetic results and retain the
    destination class (including division and right-shift signedness). Compound
    results retain the full computed word; prefix/postfix results are the
    new/old stored values. Static locals share persistent words and the global
    byte bound, occupy no invocation slots, and require their exact declaring
    frame and options. Static address producers and storage consumers also
    accept their owner's checked entry initializer region. Other entry
    instructions and functions fail preflight; region authority never enters a
    called function. JIT static calls require transitively earlier definitions
    in the supplied bodies. Checked one-level integer pointer locals and fixed
    parameters hold references to nonzero scalar integer objects and automatic
    array elements. Memory stores retain low declared bits and reads sign- or
    zero-extend them. Plain assignment expressions retain the full RHS bits
    independently of narrowed storage. Arithmetic retains checked raw classes
    and separately derived native computation classes. Ordinary binary selection
    takes the greater raw ID; comparisons retain their unsigned-operand rule.
    Direct public unsigned calls, forwarded storage and COM node/result classes
    remain distinct. Updates store low declared bits and normalize
    prefix/postfix results. This canonical reference path retains zero flags and
    a materialized result; it does not select native BY_VAL or discarded-result
    optimizations. Unsigned internal negation selects the signed partner while
    retaining full register bits. Checked indexing retains declared-object
    extents and remaining array strides; grouping/materialization consumes
    dimensions. Final pointer values may be one-past; memory access must be
    within the original object. Scale/add overflow and object bounds report
    HCIRVM0020 and HCIRVM0019 respectively. Offsets, strides and object extents
    use actual element byte widths. Active allocation charges the checked padded
    local frame plus eight-byte parameter slots. Cell counts and allocation
    bytes are bounded before expansion. [IC_ADDR] materializes a reference
    without reading the object, with its own static-consumer ownership check.
    Dereferences and updates retain the actual caller or recursive activation,
    even when slots have identical offsets. Explicit references can pass to
    callees without granting canonical static-address authority. Returned frames
    are invalidated; pointer returns, arbitrary integer addresses and pointer
    arithmetic remain unsupported. Canonical [IC_STR_CONST] instructions own
    mutable byte regions containing the exact payload followed by one zero byte.
    Every literal site in every definition and the entry is checked before
    execution, including unreachable sites. The positive [max_literal_bytes]
    limit (default 1,048,576) includes their terminators and is separate from
    frame and global bytes. Capacity is checked before byte-cell allocation;
    excess reports HCIRVM0021. Each site has independent identity, even for
    identical payloads or shared graph objects in distinct definitions. Its
    region persists across calls and returns within one execution; each
    execution starts with a fresh image. Internal/public U8 pointer forms may
    convert at checked stores and fixed arguments because U8 denotes a native
    internal class. Conversion preserves object identity, extent and byte offset
    while adopting the destination pointee type. Canonical producer and
    memory-operation types remain exact. Public results remain words. *)

val execute_program_report :
  ?runtime_calls:Runtime_call_context.t ->
  ?globals:Integer_globals.t ->
  ?initialization:Global_initialization.t ->
  ?max_global_bytes:int ->
  ?max_literal_bytes:int ->
  ?max_output_bytes:int ->
  ?max_output_work:int ->
  max_steps:int ->
  max_frame_bytes:int ->
  max_call_depth:int ->
  functions:function_definition list ->
  X87_stack.t ->
  report
(** Execute with immutable captured bytes and charged formatting work retained
    on failure as well as success. Exact graph-owned [runtime_calls] authorize
    checked Print/PutChars sites and identify implicit-output discards, which
    preserve the last ordinary expression value. Explicit U0 calls still clear
    it. Configuration and preflight failures capture no bytes and charge no
    output work. [execute_program] projects this report's established outcome.

    Positive output-byte and work limits independently default to 1,048,576; the
    byte limit must fit a host string. Each fetched format/string byte,
    including terminators and failed reads, inspected packed-byte position and
    candidate output byte costs one work unit. An exhausted charge leaves the
    count at the limit; append work precedes capacity checking. Print publishes
    only its complete successful draft. PutChars retains bytes published before
    a later fault. Providers consume one active call-depth level and eight
    active frame bytes per ABI argument slot, including hidden counts, without
    allocating source locals or a return slot. Output/work/format/argument
    faults use HCIRVM0022 through HCIRVM0025; pointer faults retain their
    existing object/lifetime/initialization diagnostics. *)

val report_outcome : report -> (t, error list) result
val report_output_bytes : report -> string
val report_output_work : report -> int

val final_value : t -> word option
(** Last reached top-level expression value from [execute_program], separate
    from stream termination and function return values. A last U0 expression has
    no final word: [42; Set(1);] yields [None], while [Set(1); 42;] yields the
    word 42. Function-internal discards do not replace the top-level value. *)

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
  ?max_literal_bytes:int ->
  max_steps:int ->
  max_frame_bytes:int ->
  frame:Sema.Function_frame_layout.function_layout ->
  arguments:int64 list ->
  Function_body.t ->
  (t, error list) result
(** Execute one verified ordinary I64/U64/U8/U0 function with its exact checked
    frame. Argument bits initialize the named parameter slots in source order.
    Automatic locals begin uninitialized. The allocation bound includes
    parameter slots and the checked local frame size. Canonical frame addresses,
    loads, assignments and scalar updates are preflighted before execution; slot
    contents survive block transfers. Every invocation owns independent storage.
    Public U64 negation retains U64, while internal U64 negation yields internal
    I64. Integer returns preserve full bits; a U8 return reports runtime U64. U8
    parameters narrow incoming bits to one initialized byte while retaining an
    eight-byte ABI allocation. Parameter padding remains inaccessible. Checked
    pointer locals can reference this invocation's scalar slots and automatic
    I64/U64/U8 array elements through checked indexing. U8 automatic scalars and
    elements have independent byte initialization state. The [arguments] bit
    interface cannot supply pointer parameters. This entry point does not
    execute calls or arbitrary pointer operations. Canonical string literals use
    a fresh, mutable, terminated byte image independent of frame storage. The
    positive [max_literal_bytes] limit defaults to 1,048,576 and counts every
    literal site, including unreachable sites, before allocation. U8 pointer
    storage conversion preserves the referenced object's identity, extent and
    offset. U0 bare return and fallthrough complete as [Returned None] without
    allocating a synthetic return word or frame slot. An I64/U64/U8 function
    that returns without a value still reports HCIRVM0013. *)

val termination : t -> termination
val executed_steps : t -> int

val compiled_initializer_steps : t -> int
(** Constant preparation evidence retained by the initialization context. These
    steps are separate from runtime [executed_steps] and its instruction limit.
*)

val human : t -> string
(** Render the versioned, deterministic execution result. *)
