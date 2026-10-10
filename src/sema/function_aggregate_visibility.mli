type t
type function_

val create :
  table:Symbol_table.t ->
  declarations:Declaration_collection.t ->
  bodies:
    (Frontend.Parser.completed_function_header
    * Frontend.Ast.function_definition)
    list ->
  ?aggregate_references:
    (Frontend.Ast.identifier ->
    Compiler_record.aggregate_reference_visibility option) ->
  Frontend.Ast.module_ ->
  (t, string) result
(** Immutable lexical layout positions for the exact declaration tree. This is
    semantic evidence, not permission to execute a reconstructed source tree. *)

val owns : t -> table:Symbol_table.t -> parent:Symbol_table.scope -> bool

val function_owns :
  function_ -> table:Symbol_table.t -> parent:Symbol_table.scope -> bool

val function_table : function_ -> Symbol_table.t
val find_function : t -> symbol:Symbol.t -> item_index:int -> function_ option
val function_item_index : function_ -> int
val function_symbol : function_ -> Symbol.t
val before_expression : function_ -> Frontend.Ast.expression -> int option

val before_local : function_ -> Frontend.Ast.identifier -> int option
(** The layout frontier at one original body occurrence. Ambiguous or foreign
    nodes have no frontier. The function's own declaration index is unchanged.
*)

val permits_aggregate :
  function_ ->
  source:Frontend.Ast.expression ->
  item_index:int ->
  symbol:Symbol.t ->
  bool
(** Imported layouts must also have been complete at the original identifier
    observation. This veto cannot grant a layout or replay a later completion.
*)
