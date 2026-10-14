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

val create_references :
  table:Symbol_table.t ->
  parent:Symbol_table.scope ->
  (Frontend.Ast.identifier * Compiler_record.aggregate_reference_visibility)
  list ->
  (t, string) result
(** Immutable metadata view of original observations in one namespace, without
    reconstructing an AST or granting function, storage or execution authority.
    Foreign occurrences and duplicate observations reject. *)

val owns : t -> table:Symbol_table.t -> parent:Symbol_table.scope -> bool

val member_reference :
  t ->
  source:Frontend.Ast.expression ->
  (Frontend.Ast.identifier * Compiler_record.aggregate_reference_visibility)
  option
(** Resolve the original member-name observation from this source namespace.
    Copied names and expressions without a member occurrence have no proof. *)

val reference_allows :
  t ->
  identifier:Frontend.Ast.identifier ->
  symbol:Symbol.t ->
  Compiler_record.aggregate_reference_visibility ->
  bool
(** Check the original completed-class observation in this exact namespace. *)

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

val partial_aggregate :
  function_ ->
  source:Frontend.Ast.expression ->
  symbol:Symbol.t ->
  Compiler_record.t option
(** Original reached metadata at the exact member-name observation. Other
    expression forms retain their original identifier observation. *)
