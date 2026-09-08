type slot

val create :
  span:Common.Span.t ->
  mode:Sema.Function_resolution.compilation_mode ->
  start:int ->
  frames:Sema.Function_frame_layout.t ->
  functions:Sema.Function_call_expression_result.t ->
  records:Sema.Function_record_classification.t ->
  (slot list, Common.Diagnostic.t list) result
(** Check retained static roots against exact frame/local evidence. Exact
    declaration snapshots distinguish prototypes, which own no frame or static
    initializer, from definitions, whose checked frame remains mandatory.
    Completeness of the initializer set is checked when the source driver
    consumes every AST declaration and requires its exact retained root; absence
    here alone is not proof that a source declaration has no initializer. *)

val index : slot -> int
val frame : slot -> Sema.Function_frame_layout.function_layout
val location : slot -> Sema.Function_frame_layout.location
val symbol : slot -> Sema.Symbol.t
val type_ : slot -> Sema.Type.t
val compiler_options : slot -> int64
val opcode : slot -> Opcode.t

val initial :
  slot -> Sema.Function_call_expression_result.initializer_result option

val initial_bits : slot -> int64 option
val preparation_steps : slot -> int
val materialized : slot -> bool
val with_initial_value : slot -> bits:int64 -> steps:int -> slot
