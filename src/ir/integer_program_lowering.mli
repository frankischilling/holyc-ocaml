type statement =
  | Empty of Common.Span.t
  | Expression of Sema.Function_call_expression_result.expression_result
  | Block of statement list
  | If of
      Sema.Function_call_expression_result.expression_result
      * statement
      * statement option
  | While of Sema.Function_call_expression_result.expression_result * statement
  | Do_while of
      statement * Sema.Function_call_expression_result.expression_result
  | For of
      statement
      * Sema.Function_call_expression_result.expression_result
      * statement option
      * statement
  | Break of Common.Span.t

val lower :
  span:Common.Span.t ->
  statement list ->
  (X87_stack.t, Common.Diagnostic.t list) result
(** Lower integer top-level control flow with block-local values, conditional
    short-circuit branches and one stream-end marker. The complete graph passes
    graph and x87 verification. Integer VM preflight belongs to execution. *)
