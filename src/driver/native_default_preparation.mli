type t

type completion
(** A receipt issued only after actual checked preparation and successful
    completion charged to the original native source invocation. *)

val create :
  compilation_mode:Frontend.Preprocessor.compilation_mode ->
  max_initializer_steps:int ->
  ?max_default_bytes:int ->
  Session.t ->
  (t, string) result
(** An isolated preparation budget. It has no source-command executor, retained
    task bindings or executable-memory capability. Saved payloads default to
    65,536 bytes and charge eight bytes per successful scalar integer default.
    Saved bits retain the full register value even for narrow parameter types;
    the callee's parameter storage applies its declared width. *)

val prepare :
  t ->
  session:Session.t ->
  ledger:Task_declarations.t ->
  Frontend.Parser.completed_parameter_default ->
  (unit, Common.Diagnostic.t list) result

val work : t -> int

val prepare_initializer :
  t ->
  session:Session.t ->
  ledger:Task_declarations.t ->
  Frontend.Parser.completed_initializer_leaf ->
  (unit, Common.Diagnostic.t list) result
(** Prepare one original global initializer leaf under the same declaration-work
    budget as defaults. The exact live layout decides scalar stores versus
    source-owned byte copies. Copy work is charged to [max_initializer_steps].
*)

val initializers : t -> Integer_initializers.native_preparation list

type initializer_completion
(** Successful original preparation charged to the source invocation budget. *)

val initializer_completions : t -> initializer_completion list

val initializer_preparation :
  initializer_completion -> Integer_initializers.native_preparation

val bytes : t -> int

val completions : t -> completion list
(** Successfully completed original default fragments, in declaration order.
    Failed preparation retains reached work but publishes no execution or saved
    word. *)

val execution : completion -> Ir.Default_fragment_program.execution
(** Read-only fragment facts; a raw fragment execution cannot create a receipt.
*)

type static_completion

val prepare_static :
  t ->
  session:Session.t ->
  ledger:Task_declarations.t ->
  Frontend.Parser.static_initializer_preparation ->
  (unit, Common.Diagnostic.t list) result

val static_completions : t -> static_completion list

val static_preparation :
  static_completion -> Integer_initializers.native_static_preparation

val static_initializers :
  t -> Integer_initializers.native_static_preparation list
