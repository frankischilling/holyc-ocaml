type word_type = I64 | U64
type word = private { type_ : word_type; bits : int64 }

type function_definition = {
  frame : Sema.Function_frame_layout.function_layout;
  body : Function_body.t;
}

type task_function_source = {
  source_globals : Integer_globals.t;
  source_runtime_calls : Runtime_call_context.t;
  source_functions : function_definition list;
  source_definition : function_definition;
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
type task_state
type task_call_start

val observe_task_function_selection :
  task_state ->
  namespace:Sema.Declaration_collection.namespace ->
  selection:Frontend.Parser.reference_selection ->
  selected:Retained_function.t ->
  (unit, string) result

val capture_task_call_start :
  task_state ->
  namespace:Sema.Declaration_collection.namespace ->
  capture:Sema.Function_record_phase.call_start_snapshot ->
  selected:Retained_function.t ->
  arguments:Sema.Function_type_resolution.resolved_function ->
  (task_call_start, string) result

val capture_task_call_emission :
  task_state ->
  table:Sema.Symbol_table.t ->
  capture:Sema.Function_record_phase.call_emission_snapshot ->
  task_call_start ->
  (Sema.Function_call_phase.t, string) result

val owns_call_phase : task_state -> Sema.Function_call_phase.t -> bool

type task_stream
type task_admission
type initializer_attempt
type dimension_attempt

val prepare_task_closed_dimension :
  task_state ->
  table:Sema.Symbol_table.t ->
  namespace:Sema.Declaration_collection.namespace ->
  preparation:Frontend.Parser.array_dimension_preparation ->
  queries:Sema.Compiler_record.query_read list ->
  (Sema.Compiler_record.dimension_preparation, string) result * int

val complete_task_dimension :
  task_state ->
  namespace:Sema.Declaration_collection.namespace ->
  Sema.Compiler_record.declared_dimension ->
  (unit, string) result

val task_dimension_is_completed :
  task_state -> Frontend.Parser.completed_array_dimension -> bool

val begin_task_dimension :
  task_state ->
  Sema.Dimension_fragment.authority ->
  (dimension_attempt, string) result

val fail_task_dimension :
  task_state -> dimension_attempt -> (unit, string) result

val task_dimension_bits :
  task_state -> Frontend.Parser.array_dimension_preparation -> int64 option

val execute_task_dimension :
  task_state ->
  dimension_attempt ->
  Dimension_fragment_program.execution ->
  (unit, error list) result

type default_attempt

val begin_task_default :
  task_state ->
  namespace:Sema.Declaration_collection.namespace ->
  publication:Sema.Declaration_collection.publication ->
  Frontend.Parser.completed_parameter_default ->
  (default_attempt, string) result

val fail_task_default : task_state -> default_attempt -> (unit, string) result

val task_default_bits :
  task_state -> Frontend.Parser.completed_parameter_default -> int64 option

val complete_task_defaults :
  task_state ->
  namespace:Sema.Declaration_collection.namespace ->
  Frontend.Parser.completed_function_header ->
  (unit, string) result

val execute_task_default :
  task_state ->
  default_attempt ->
  Default_fragment_program.execution ->
  (unit, error list) result

val begin_task_initializer :
  task_state ->
  namespace:Sema.Declaration_collection.namespace ->
  Sema.Compiler_record.declared_global ->
  Frontend.Parser.global_initializer_start ->
  (unit, string) result

val observe_task_initializer_delimiter :
  task_state ->
  namespace:Sema.Declaration_collection.namespace ->
  Frontend.Parser.completed_initializer_delimiter ->
  (unit, string) result

val begin_task_initializer_leaf :
  task_state ->
  namespace:Sema.Declaration_collection.namespace ->
  Sema.Initializer_source.leaf ->
  (initializer_attempt, string) result

val initializer_attempt_destination :
  initializer_attempt -> Integer_initializer_layout.entry

val fail_task_initializer_attempt :
  task_state -> initializer_attempt -> (unit, string) result

val complete_task_initializer :
  task_state ->
  namespace:Sema.Declaration_collection.namespace ->
  Frontend.Parser.global_initializer_start ->
  Sema.Initializer_source.t ->
  (unit, string) result

val execute_task_initializer :
  task_state ->
  initializer_attempt ->
  Initializer_fragment_program.execution ->
  (unit, error list) result

type task_progress = private {
  executed_steps : int;
  initializer_steps : int;
  global_bytes : int;
  literal_bytes : int;
  output_bytes : string;
  output_work : int;
  generated_bytes : int;
  final_value : word option;
}

val task_progress : task_state -> task_progress
(** Immutable observation of task-lifetime work, allocated storage, captured
    output and the last reached outer expression value. Reached faults retain
    that value; declarations and implicit output leave it alone, while an
    explicit no-value expression clears it. Active stream commands do not alter
    it. Snapshots grant no runtime, source or admission authority and do not
    describe a successful whole-invocation outcome. *)

type admitted_publication = private
  | Admitted_declared_global of
      Retained_global.t * Integer_globals.declared_slot
  | Admitted_global of Retained_global.t * Integer_globals.slot
  | Admitted_function of Retained_function.t

val create_task_state :
  ?max_steps:int ->
  ?max_initializer_steps:int ->
  ?max_global_bytes:int ->
  ?max_literal_bytes:int ->
  ?max_frame_bytes:int ->
  ?max_call_depth:int ->
  ?max_output_bytes:int ->
  ?max_output_work:int ->
  ?max_generated_bytes:int ->
  ?max_stream_depth:int ->
  table:Sema.Symbol_table.t ->
  unit ->
  (task_state, string) result

val begin_task_stream : task_state -> (task_stream, string) result

val task_stream_is_active : task_state -> task_stream -> bool
(** Read-only exact top-buffer ownership check for parser callback admission. *)

val finish_task_stream : task_state -> task_stream -> (string, string) result

val abort_task_stream : task_state -> task_stream -> (unit, string) result
(** Exact LIFO task-owned buffers. StreamPrint uses the active buffer and shares
    ordinary formatting work. Generated bytes have a separate cumulative limit;
    successful fragments remain charged after finish or abort. An abort returns
    no text. Foreign, consumed and out-of-order tokens leave the stack intact.
*)

val task_snapshot : task_state -> (Integer_globals.task_view, string) result

val admit_declared_global :
  task_state -> Sema.Compiler_record.declared_global -> (unit, string) result

val admit_function_header :
  task_state ->
  namespace:Sema.Declaration_collection.namespace ->
  source:Sema.Compiler_record.declared_function ->
  records:Sema.Function_record_classification.t ->
  (unit, string) result

val check_function_header_source :
  task_state ->
  namespace:Sema.Declaration_collection.namespace ->
  Sema.Compiler_record.declared_function ->
  (unit, string) result

val function_record_head :
  task_state ->
  Sema.Function_record_phase.snapshot ->
  Retained_function.t option

val check_function_phase_source :
  task_state ->
  namespace:Sema.Declaration_collection.namespace ->
  event:Frontend.Parser.declaration_event ->
  Sema.Function_record_phase.snapshot ->
  (unit, string) result

val admit_function_phase :
  task_state ->
  namespace:Sema.Declaration_collection.namespace ->
  event:Frontend.Parser.declaration_event ->
  snapshot:Sema.Function_record_phase.snapshot ->
  records:Sema.Function_record_classification.t ->
  (unit, string) result

val bind_task_namespace :
  task_state -> Sema.Declaration_collection.namespace -> (unit, string) result
(** Internal single-assignment ledger binding. Driver-owned namespaces remain
    private; an unrelated semantic publication cannot allocate in their task. *)

val task_source_order : task_state -> Sema.Task_command_order.t

val observe_task_source_event :
  task_state -> Frontend.Parser.command_event -> (unit, string) result
(** Observe original source order and retain immutable result counters at a root
    completion boundary. Result projection still requires parser acceptance,
    completed runtime admission and successful execution. *)

val start_task_compilation : task_state -> unit
(** Close the fresh-runtime source-promotion boundary before compiling a task
    unit, including a unit with no preparation work or runtime effects. *)

val promote_task_source :
  ?dimensions:Sema.Compiler_record.dimension_preparation list ->
  ?completed_dimensions:Sema.Compiler_record.declared_dimension list ->
  task_state ->
  namespace:Sema.Declaration_collection.namespace ->
  events:Frontend.Parser.command_event list ->
  dimension_steps:int ->
  (unit, string) result
(** Internal source-ledger join. A fresh runtime imports already validated
    command receipts and their reached dimension work atomically. Failed
    preflight leaves the runtime unchanged; success consumes promotion once. *)

val promote_task_source_activation :
  ?pending_runtime_dimension:Frontend.Parser.array_dimension_preparation ->
  task_state ->
  namespace:Sema.Declaration_collection.namespace ->
  activation:Sema.Source_activation.t ->
  dimensions:Sema.Compiler_record.dimension_preparation list ->
  (unit, string) result
(** Bind the complete activation journal and its checked preparation manifest
    atomically. Each closed dimension is charged at its original active event.
    One runtime-dependent preparation may be deferred only when it is the exact
    final observation and its original parser callback is live. It is neither
    evaluated nor charged here; replay requires normal runtime authority. *)

val charge_source_dimension :
  task_state ->
  Frontend.Parser.array_dimension_preparation ->
  (unit, string) result

val bind_task_source_program :
  task_state ->
  runtime_calls:Runtime_call_context.t ->
  globals:Integer_globals.t ->
  initialization:Global_initialization.t ->
  functions:function_definition list ->
  X87_stack.t ->
  (unit, string) result
(** Internal compiler join. Bind source order to the exact final compilation
    bundle before exposing it. Registration admits no runtime effects. *)

val task_owns_snapshot : task_state -> Integer_globals.task_view -> bool
val task_owns_table : task_state -> Sema.Symbol_table.t -> bool

val task_admission :
  task_state ->
  globals:Integer_globals.t ->
  entry:X87_stack.t ->
  task_admission option

val owns_task_admission : task_state -> task_admission -> bool
val admission_publications : task_admission -> admitted_publication list
val admitted_source_symbol : admitted_publication -> Sema.Symbol.t

val admitted_publication_for_symbol :
  task_state -> Sema.Symbol.t -> admitted_publication option
(** Read the already admitted publication for this exact source symbol,
    including the original declaration of a joined function. Names and canonical
    function identities cannot substitute for source identity. *)

val latest_task_admission : task_state -> task_admission option
(** Exact successful preflight/admission evidence for this task and compiled
    storage/entry pair. Compilation and failed preflight issue no receipt.
    Reached faults retain the original receipt and ordered global/function
    publication links. Reading a receipt grants no execution or storage access.
*)

val task_function_source :
  task_state -> Retained_function.t -> task_function_source option
(** Read the original checked source owner for an exact admitted executable
    link. Unadmitted definitions and foreign links have no source publication.
*)

val task_output_bytes : task_state -> string
val task_output_work : task_state -> int
val task_generated_bytes : task_state -> int
val task_executed_steps : task_state -> int
val task_initializer_steps : task_state -> int
val task_initializer_limit : task_state -> int
val record_task_preparation : task_state -> before:int -> steps:int -> unit

val execute_task_program :
  task_state ->
  runtime_calls:Runtime_call_context.t ->
  globals:Integer_globals.t ->
  initialization:Global_initialization.t ->
  functions:function_definition list ->
  X87_stack.t ->
  (t, error list) result
(** Admit the fully preflighted command into its task before execution. Checked
    function publications retain exact executable links, their original direct
    callees and mutable literal images. Later calls preserve that owner across
    nested calls and returns; original globals and statics retain their storage
    identities. Failed preflight publishes nothing, while reached faults retain
    admitted functions and storage effects. Instruction, storage and output
    budgets remain cumulative across commands. *)

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

val execute_isolated_program_in_task :
  task_state ->
  runtime_calls:Runtime_call_context.t ->
  globals:Integer_globals.t ->
  initialization:Global_initialization.t ->
  functions:function_definition list ->
  X87_stack.t ->
  (t, error list) result
(** Execute a fresh isolated output image using the invocation's remaining
    instruction, storage, literal and output/work allowances. It receives no
    retained task bindings and publishes none. Preflight preserves earlier task
    effects; reached execution charges its allocations and instructions once.
    Success reports cumulative instructions/preparation and the actual outer
    result. Active streams and replayed output images reject before effects. *)

type isolated_preparation

val begin_isolated_preparation : task_state -> isolated_preparation

val record_isolated_preparation :
  task_state -> isolated_preparation -> steps:int -> unit

val abort_isolated_preparation : task_state -> isolated_preparation -> unit

val finish_isolated_preparation :
  task_state ->
  isolated_preparation ->
  runtime_calls:Runtime_call_context.t ->
  globals:Integer_globals.t ->
  initialization:Global_initialization.t ->
  functions:function_definition list ->
  X87_stack.t ->
  (unit, string) result
(** Account preparation through this exact invocation-owned ticket, then seal
    its actual ordinary entry/storage/initializer/call/body bundle. The charged
    work must equal the bundle's checked preparation. Abort retains charges;
    foreign, closed or undercharged tickets cannot authorize isolated execution.
*)

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
(** Execute one verified I64/U64/U8/U0 function with its exact checked frame.
    Argument bits initialize the named parameter slots in source order.
    Automatic locals begin uninitialized. The allocation bound includes
    parameter slots and the checked local frame size. Canonical frame addresses,
    loads, assignments and scalar updates are preflighted before execution; slot
    contents survive block transfers. Every invocation owns independent storage.
    A variadic frame consumes the fixed argument prefix followed by integer tail
    bits, synthesizes argc and exposes argv with the actual tail extent. Its
    arbitrary declared extent of 127 is not an allocation or indexing bound. The
    allocation limit includes the hidden count and every actual tail word.
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

val bind_source_activation :
  task_state ->
  namespace:Sema.Declaration_collection.namespace ->
  Sema.Source_activation.t ->
  (unit, string) result

val task_result :
  task_state ->
  sequence:Frontend.Parser.completed_sequence ->
  (t, string) result

(** Cumulative outer result for the exact accepted and admitted root sequence.
    Failed execution, active streams and unfinished initializer/default work
    cannot produce a successful result. *)

val task_input_result :
  task_state ->
  sequence:Frontend.Parser.completed_sequence ->
  (t, string) result
(** Frozen result for one original root input. Prior failed inputs and existing
    generation buffers remain task state; this input must finish all its work
    successfully and restore the same buffer stack. Work totals are cumulative,
    while the final value belongs only to this input's root commands. *)
