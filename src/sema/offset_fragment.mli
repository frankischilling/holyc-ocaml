type t
type authority
type position

val create :
  table:Symbol_table.t ->
  namespace:Declaration_collection.namespace ->
  progress:Compiler_record.aggregate_progress ->
  receipt:Frontend.Parser.aggregate_phase ->
  environment:Outer_environment.t ->
  references:(Frontend.Ast.identifier * Reference_selection.t) list ->
  queries:Query_selection.t list ->
  (t, string) result

val authorize : t -> (authority, string) result
val authorized_fragment : authority -> t
val owns_table : t -> Symbol_table.t -> bool
val namespace : t -> Declaration_collection.namespace
val receipt : t -> Frontend.Parser.aggregate_phase
val expression : t -> Frontend.Ast.expression
val origin : t -> Symbol.origin
val environment : t -> Outer_environment.t
val references : t -> (Frontend.Ast.identifier * Reference_selection.t) list
val queries : t -> Query_selection.t list

val reference_for :
  t -> Frontend.Ast.identifier -> (Reference_selection.t, string) result

val query_for :
  t -> Frontend.Ast.expression -> (Query_selection.t, string) result

val preparation : authority -> Compiler_record.runtime_aggregate_offset
val position_for : t -> Frontend.Ast.expression -> (position, string) result
val position_matches : position -> t -> Frontend.Ast.expression -> bool
val position_value : position -> int64

val position_dependencies : position -> Compiler_record.aggregate_offset list
(** Original aggregate-position evidence, distinct from ordinary instruction
    pointers. It is immutable metadata, not task execution authority. *)
