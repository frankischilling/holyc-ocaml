type role = Sizeof_root | Offset_root | Defined_operand

val origin : Frontend.Ast.location -> Symbol.origin
val source_queries : Frontend.Ast.expression -> Frontend.Ast.expression list

val name_query_facts :
  Frontend.Ast.expression -> (role * string * Symbol.origin) option
(** Inspect original query source children in source order. These shape facts
    confer no checked value, table ownership or execution authority. *)
