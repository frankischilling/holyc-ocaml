type role = Sizeof_root | Offset_root | Defined_operand
type t

val make :
  ?sizeof_read:Compiler_record.sizeof_read ->
  table:Symbol_table.t ->
  receipt:Frontend.Parser.completed_query ->
  unit ->
  (t, string) result
(** Retain a parser-completed query for semantic event validation. The source
    driver must first establish its original command seal and table ownership.
    This is query-read evidence, not storage, executable or layout authority. *)

val validate :
  table:Symbol_table.t ->
  role:role ->
  name:string ->
  origin:Symbol.origin ->
  t ->
  (unit, string) result

val expression : t -> Frontend.Ast.expression
val owns_table : t -> Symbol_table.t -> bool

val name_query_facts :
  Frontend.Ast.expression -> (role * string * Symbol.origin) option
(** Source facts for named queries; non-name defined operands have no name
    event. *)

val is_local : t -> bool
val presence : t -> bool option
val sizeof : t -> (Primitive_type.t option * int64 * bool) option
val constant : t -> int64 option
val source_queries : Frontend.Ast.expression -> Frontend.Ast.expression list

val validate_manifest :
  table:Symbol_table.t ->
  expression:Frontend.Ast.expression ->
  t list ->
  (unit, string) result
