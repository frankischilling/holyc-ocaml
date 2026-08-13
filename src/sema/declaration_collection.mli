(** The AST shape that introduced a top-level semantic symbol. This does not
    imply that a forward and definition have been reconciled. *)
type declaration_kind =
  | Aggregate_forward
  | Aggregate_definition
  | Aggregate_attached_global
  | Global_variable
  | Function_prototype
  | Function_definition

type declaration
type entry
type t

val make_declaration :
  name:string ->
  declaration_kind:declaration_kind ->
  origin:Symbol.origin ->
  item_index:int ->
  ?declarator_index:int ->
  unit ->
  (declaration, string) result
(** Build one checked declaration fact without mutating a symbol table. *)

val collect :
  table:Symbol_table.t ->
  ?module_name:string ->
  declaration list ->
  (t, string) result
(** Create one module scope and insert the checked facts in list order. Repeated
    names remain separate symbols. *)

val scope : t -> Symbol_table.scope
val entries : t -> entry list
val entry_symbol : entry -> Symbol.t
val entry_kind : entry -> declaration_kind
val entry_item_index : entry -> int
val entry_declarator_index : entry -> int option
val declaration_kind_name : declaration_kind -> string
