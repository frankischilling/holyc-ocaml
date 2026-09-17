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
  ?max_default_bytes:int ->
  ?status_abi:Backend.X86_64_program.status_abi ->
  Session.t ->
  config:Frontend.Preprocessor.Config.t ->
  source:Common.Source_file.t ->
  (Backend.X86_64_program.t checked, Common.Diagnostic.t list) Stdlib.result
(** Parse ordinary source without command or [#exe] executors. Compile the exact
    checked entry and its fixed direct scalar integer or U0 source functions,
    with width-correct automatic integer storage and function-local language
    goto/labels, through the shared native word backend. Supported scalar
    parameter defaults are prepared once at their original declaration callbacks
    through the checked constant-preparation engine. Unsupported declarations,
    defaults and persistent storage reject before native entry. This never
    interprets ordinary commands or allocates executable memory. Parser warnings
    retain their original source identities. Defaults are 4096 total IR
    instructions, 65536 code bytes, 4088 private frame bytes, 4096 total blocks,
    100,000 declaration-preparation steps and 65,536 bytes of saved default
    payloads (eight bytes per prepared value). *)

val evaluate :
  ?max_ir_instructions:int ->
  ?max_code_bytes:int ->
  ?max_stack_bytes:int ->
  ?max_blocks:int ->
  ?max_initializer_steps:int ->
  ?max_default_bytes:int ->
  ?max_frame_bytes:int ->
  ?max_call_depth:int ->
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
    pointers. *)

val outcome : report -> (result checked, Common.Diagnostic.t list) Stdlib.result
val image : report -> Backend.X86_64_program.t option
val native_outcome : report -> Backend.X86_64_program.outcome option
val platform : report -> Runtime.Native_program_execution.platform
val executed_steps : report -> int option

val preparation_steps : report -> int
(** Reached declaration-time preparation work, retained independently of native
    entry steps after preparation, parsing, compilation or execution failures.
*)

val default_bytes : report -> int
(** Saved scalar default payload bytes successfully prepared before completion
    or failure. Repeated calls do not prepare or charge the saved value again.
*)
