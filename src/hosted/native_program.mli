open Driver

type 'a checked = { value : 'a; diagnostics : Common.Diagnostic.t list }

type result = private {
  image : Backend.X86_64_program.t;
  execution : Backend.X86_64_program.execution;
  platform : Runtime.Native_program_execution.platform;
}

type report

val compile :
  ?max_ir_instructions:int ->
  ?max_code_bytes:int ->
  ?max_stack_bytes:int ->
  ?max_blocks:int ->
  ?max_initializer_steps:int ->
  ?max_switch_work:int ->
  ?max_dimension_work:int ->
  ?max_default_bytes:int ->
  ?max_global_bytes:int ->
  ?max_literal_bytes:int ->
  ?status_abi:Backend.X86_64_program.status_abi ->
  Session.t ->
  config:Frontend.Preprocessor.Config.t ->
  source:Common.Source_file.t ->
  (Backend.X86_64_program.t checked, Common.Diagnostic.t list) Stdlib.result
(** Parse ordinary source without command or [#exe] executors. Compile the exact
    checked entry and its fixed direct scalar integer or U0 source functions,
    with width-correct automatic/global/static integer storage, one-level scalar
    pointer locals and fixed parameters, and function-local language goto/labels
    and bounded integer switches, through the shared native word backend. Closed
    case endpoints prepare at their original parser callbacks under an
    independent 100,000-node work limit. Closed automatic scalar array
    dimensions prepare at their original callbacks under [max_dimension_work].
    Their full aligned frames, per-element flags and reference records are
    bounded before expansion. Indexed reads, assignments and updates retain the
    original object extent and declared element width. Intermediate flat offsets
    are checked for signed overflow; materialization permits aligned one-past
    references, while reads and writes require an actual element. Supported
    scalar parameter defaults are prepared once at their original declaration
    callbacks through the checked constant-preparation engine. Closed scalar and
    array global/static initializer leaves prepare after original expression
    lookahead under that same work budget and require completed declarations.
    Their exact receipts authorize the native initial image, which restores
    declared-width values on each execution. Persistent integer arrays retain
    original extents, strides and per-element state. Mutable string literals
    retain their exact producer and terminated byte image; their canonical
    reference tables occupy private arena bytes. Prepared JIT publications must
    precede entry. The ordinary [extern U0 PutChars(U64)] and
    [extern U0 Print(U8 *fmt,...)] provider headers may authorize explicit calls
    and their corresponding implicit output statements through sealed runtime
    call metadata. Native Print retains the bounded interpreter format domain:
    ordinary bytes plus [%%], [%d], [%s] and [%c], with dynamic owned format and
    string pointers. A source-defined [Print] or [PutChars] remains an ordinary
    direct source call. Effectful initializers, escaping/deeper pointers,
    automatic array initializers and other unsupported declarations/defaults
    reject before native entry. This never interprets ordinary commands or
    allocates executable memory. Parser warnings retain their original source
    identities. Defaults are 4096 total IR instructions, 65536 code bytes, 4088
    private frame bytes, 4096 total blocks, 100,000 declaration-preparation
    steps and 65,536 bytes of saved default payloads (eight bytes per prepared
    value). Global/static storage defaults to 1,048,576 bytes, with statics
    rounded to eight; its separate host cap is 16,777,216 bytes. Literal bytes
    have the same default and hard bound, independently. The combined arena,
    including private flags and reference tables, is limited to 33,554,432
    bytes. *)

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
  Session.t ->
  config:Frontend.Preprocessor.Config.t ->
  source:Common.Source_file.t ->
  max_steps:int ->
  report
(** Compile and execute one program using the native host bridge. The report
    retains the compiled image and actual native outcome after runtime faults,
    so callers can inspect exact executed-step counts without inventing an
    interpreter result. Configuration failures occur before parsing and host
    failures occur without interpreter fallback. Simultaneous semantic frame
    bytes default to 1,048,576 and active named calls to 128. The separate
    physical native stack limit defaults to its hard maximum of 65,536 bytes and
    includes compiler-private storage, return addresses and saved frame
    pointers. Native output bytes and formatting/scan work are independently
    bounded; both default to 1,048,576, and the byte limit may not exceed 16
    MiB. Reached output and work are retained on success and checked execution
    faults. PutChars commits each emitted byte, while Print publishes its draft
    only after the complete dynamic format succeeds. Each execution owns fresh
    global/static data and initialization flags, shared by entry and generated
    calls. Persistent AOT objects without initializers start at zero; reached
    JIT reads before assignment retain the hosted uninitialized-object fault.
    Automatic scalar and array elements start uninitialized in both modes.
    Prepared globals restore their initial values in both modes. *)

val outcome : report -> (result checked, Common.Diagnostic.t list) Stdlib.result
val image : report -> Backend.X86_64_program.t option
val native_outcome : report -> Backend.X86_64_program.outcome option
val platform : report -> Runtime.Native_program_execution.platform
val executed_steps : report -> int option

val preparation_steps : report -> int
(** Reached declaration-time preparation work, retained independently of native
    entry steps after preparation, parsing, compilation or execution failures.
*)

val switch_work : report -> int
(** Reached closed case endpoint node visits, preserved after failures and
    separately bounded by [max_switch_work] (positive, default 100,000). These
    values are prepared once; native dispatch never reevaluates case source. *)

val default_bytes : report -> int
(** Saved scalar default payload bytes successfully prepared before completion
    or failure. Repeated calls do not prepare or charge the saved value again.
*)

val dimension_work : report -> int
(** Original closed dimension preparation work, retained after failure and
    bounded independently by [max_dimension_work] (positive, default 100,000).
*)

val output_bytes : report -> string
(** Bytes committed by reached native output providers. A failed Print call does
    not expose bytes from its current atomic draft. Reports that fail before
    native execution return the empty string. *)

val output_work : report -> int
(** Reached native output formatting and byte-scan work. Reports that fail
    before native execution return zero. *)
