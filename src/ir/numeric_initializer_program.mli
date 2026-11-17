module Typed = Sema.Function_call_expression_result

type source =
  | Global of Sema.Initializer_fragment.authority * Typed.top_level_root_result
  | Static of Sema.Static_initializer_fragment.t * Typed.top_level_root_result

type t

val create :
  source:source ->
  globals:Integer_globals.t ->
  lowered:Integer_program_lowering.t ->
  initialization:Global_initialization.t ->
  runtime_calls:Runtime_call_context.t ->
  (t, string) result
(** Retain one original closed numeric initializer expression and its exact
    lowered graph. This seal carries no execution or destination-copy authority.
*)

val source : t -> source
val root : t -> Typed.top_level_root_result
val globals : t -> Integer_globals.t
val lowered : t -> Integer_program_lowering.t
val entry : t -> X87_stack.t
val initialization : t -> Global_initialization.t
val runtime_calls : t -> Runtime_call_context.t
val span : t -> Common.Span.t
