type word = private { type_ : Backend.X86_64_program.word_type; bits : int64 }
type result = private { final_value : word option }
type 'a checked = { value : 'a; diagnostics : Common.Diagnostic.t list }

type fragment_kind =
  | Initializer
  | Default
  | Internal_binding
  | Dimension
  | Offset
  | Command
  | Aot_module

type image = private {
  status_abi : Backend.X86_64_program.status_abi;
  ir_instructions : int;
  code_bytes : int;
  global_bytes : int;
  global_arena_bytes : int;
  literal_bytes : int;
  arena_metadata_bytes : int;
  entry_stack_bytes : int;
  function_count : int;
}
(** Detached immutable metadata from the exact compiled fragment. This is not an
    executable image and retains no live request, task arena or source task. *)

type fragment = private {
  kind : fragment_kind;
  image : image;
  native_outcome : (Backend.X86_64_program.outcome, string) Stdlib.result option;
}

type report

type static_copy = private {
  cell_offset : int;
  byte_offset : int;
  byte_count : int;
  outcome : (unit, string) Stdlib.result;
}
(** Detached observations of the compiler's original direct literal-copy branch.
    These are native storage writes, without expression IR or code images. The
    byte count is charged to initializer work on entry. *)

val evaluate :
  ?max_ir_instructions:int ->
  ?max_code_bytes:int ->
  ?max_stack_bytes:int ->
  ?max_blocks:int ->
  ?max_initializer_steps:int ->
  ?max_default_bytes:int ->
  ?max_switch_work:int ->
  ?max_dimension_work:int ->
  ?max_global_bytes:int ->
  ?max_literal_bytes:int ->
  ?max_frame_bytes:int ->
  ?max_call_depth:int ->
  ?max_output_bytes:int ->
  ?max_output_work:int ->
  ?max_active_stack_bytes:int ->
  ?status_abi:Backend.X86_64_program.status_abi ->
  Driver.Session.t ->
  config:Frontend.Preprocessor.Config.t ->
  source:Common.Source_file.t ->
  max_steps:int ->
  report
(** Execute original JIT integer scalar and fixed-array initializer leaves and
    resumed source commands through one shared native task arena. Each fragment
    retains its original checked source request and claims its live entry once.
    Earlier data and initialization flags remain in that arena when later
    fragments execute. Source admission retains declaration and command metadata
    without an IR storage copy. The report records actual native outcomes and
    cumulative native work separately from closed declaration preparation.
    Fragment observations retain detached image metadata after code and arena
    release.

    One-star callback cells and fixed arrays retain numeric words with separate
    zero executable-owner lanes. Automatic callback storage, fixed callback
    parameters and saved integer or callback defaults use the same native
    ownership path. Reached numeric calls preserve reverse argument effects
    before the native unowned-target fault. Original function-address producers
    retain canonical native entries with separate executable owners and source
    lifetime checks.

    Named direct functions retain their original admitted source body, frame,
    call context and global references. Each caller fragment compiles its exact
    direct-call closure, including historical definitions. The task retains no
    persistent executable address for these source records. Original checked
    Print/PutChars calls keep their cumulative output contracts. A joined extern
    body cannot fall back to its earlier provider.

    Code bytes and verified IR instructions are bounded cumulatively across
    emitted fragments. Runtime steps and output share one native allowance;
    frame, call-depth and active native stack limits apply to each activation.
    Original literal bytes and reference descriptors append to the shared arena
    and preserve mutation across fragments. Logical literal and metadata counts
    appear separately in each detached image report. Integer scalar and
    fixed-array function statics append their original allocation to that arena.
    Each original initializer leaf executes once during its live parser
    callback; completion joins the same allocation to its declaring frame.
    Literal byte-array leaves use the compiler's direct-copy branch and charge
    their checked byte count to the owning initializer allowance. Those native
    writes appear in [static_copies], with no expression IR or code image.
    Static values are not computed by the interpreter or copied from prepared
    storage. Named and anonymous integer, one-star callback or one-level
    primitive data-pointer parameter defaults execute once in their original
    native expression fragment and retain full words, original callback values
    or owned data descriptors in the same header. Calls require those exact
    saved objects and completed native receipts. Each word charges eight
    [default_bytes]; actual expression work consumes both the shared native
    allowance and remaining initializer steps. Static one-star callback cells
    and fixed arrays retain their original live allocation and completed frame,
    physical RT_PTR shape, anonymous signature and private owner lanes. Their
    defaults execute once at the original header. String-producing defaults copy
    their terminated result into fresh task storage. Copied bytes charge literal
    storage; attempted scans charge initializer work. Data descriptors charge 32
    private arena bytes and expose no host pointer. Direct static callback
    initializers and F64 defaults remain unsupported. Unsupported task
    declarations return diagnostics. There is no interpreter fallback.

    AOT mode freezes a separate directive task before module publication.
    Original [#exe] commands run in that task and feed committed generated text
    into the original outer parser. Its checked native module follows as one
    [Aot_module] fragment with a separate arena. Instruction/output,
    preparation, saved-default, code/IR and combined logical storage allowances
    cover both contexts. [source_progress] observes only the directive task; the
    native report retains actual work and output for the complete invocation.
    Successful synchronous StreamExePrint in either outer mode, AOT saved-table
    selection, runtime AOT dimensions, general outer aggregates and
    reference-default relocation remain open. *)

val outcome : report -> (result checked, Common.Diagnostic.t list) Stdlib.result
val fragments : report -> fragment list
val static_copies : report -> static_copy list
val platform : report -> Runtime.Native_program_execution.platform
val executed_steps : report -> int
val preparation_steps : report -> int
val default_bytes : report -> int
val dimension_work : report -> int
val switch_work : report -> int
val output_bytes : report -> string
val output_work : report -> int

val source_progress : report -> Driver.Integer_task.progress option
(** Original source admission and preparation progress. Its interpreter runtime
    counters do not stand in for native execution; use [executed_steps]. *)

val compiler_exceptions : report -> Frontend.Parser.compiler_exception list
(** Original counted parser [Compiler] failures from the outer input or a
    reached nested input. General native execution errors do not create one. *)
