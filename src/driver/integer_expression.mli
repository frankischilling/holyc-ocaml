val lower :
  Session.t ->
  config:Frontend.Preprocessor.Config.t ->
  source:Common.Source_file.t ->
  (Ir.X87_stack.t, Common.Diagnostic.t list) result
(** Preprocess and type exactly one ordinary [EXPR;] statement, then lower its
    exact value into a verified return harness. VM domain checks run only during
    evaluation, so a graph can describe unsupported VM operations. *)

val evaluate :
  Session.t ->
  config:Frontend.Preprocessor.Config.t ->
  source:Common.Source_file.t ->
  max_steps:int ->
  (Ir.Integer_interpreter.t, Common.Diagnostic.t list) result
(** Execute the verified harness through the bounded integer interpreter. A
    positive budget is required and checked before parsing. Return preparation
    and return each consume a step. *)
