val unary :
  Frontend.Ast.unary_operator_kind ->
  Sema.Aggregate_layout.unary_operator option

val binary :
  Frontend.Operator.binary_operator ->
  Sema.Aggregate_layout.binary_operator option
(** Map the parser's checked operator records to the closed-layout evaluator's
    supported operator set. Assignment forms and other unsupported operators
    return [None]. *)
