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
  ?status_abi:Backend.X86_64_program.status_abi ->
  Session.t ->
  config:Frontend.Preprocessor.Config.t ->
  source:Common.Source_file.t ->
  (Backend.X86_64_program.t checked, Common.Diagnostic.t list) Stdlib.result
(** Parse ordinary source without command or [#exe] execution callbacks, reject
    source forms outside the closed native-program gate, then compile the exact
    checked top-level graph. This never executes the IR interpreter or allocates
    executable memory. Parser warnings retain their original source identities.
    Defaults are 4096 IR instructions, 65536 code bytes, 4088 private spill
    bytes and 4096 blocks. *)

val evaluate :
  ?max_ir_instructions:int ->
  ?max_code_bytes:int ->
  ?max_stack_bytes:int ->
  ?max_blocks:int ->
  ?status_abi:Backend.X86_64_program.status_abi ->
  Session.t ->
  config:Frontend.Preprocessor.Config.t ->
  source:Common.Source_file.t ->
  max_steps:int ->
  report
(** Compile and execute one closed program using the native host bridge. The
    report retains the compiled image and actual native outcome after runtime
    faults, so callers can inspect exact executed-step counts without inventing
    an interpreter result. Configuration failures occur before parsing and host
    failures occur without interpreter fallback. *)

val outcome : report -> (result checked, Common.Diagnostic.t list) Stdlib.result
val image : report -> Backend.X86_64_program.t option
val native_outcome : report -> Backend.X86_64_program.outcome option
val platform : report -> Runtime.Native_program_execution.platform
val executed_steps : report -> int option
