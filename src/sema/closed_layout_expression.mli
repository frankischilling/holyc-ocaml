val equal : Aggregate_layout.expression -> Aggregate_layout.expression -> bool
(** Exact recursive source agreement, including floating payload bits and
    origins. Signed zero must not collapse under OCaml float equality. *)

val unary :
  Frontend.Ast.unary_operator_kind -> Aggregate_layout.unary_operator option

val binary :
  Frontend.Operator.binary_operator -> Aggregate_layout.binary_operator option

val of_ast :
  ?allow_floating:bool -> Frontend.Ast.expression -> Aggregate_layout.expression
(** Convert the retained source expression without binding or evaluating names.
    High-bit integer and character literals retain their internal U64 class.
    Unparenthesized comparison chains are explicitly unsupported.
    [allow_floating] defaults to [true]; the aggregate-layout syntax boundary
    supplies [false]. Unsupported forms retain their source origins. *)
