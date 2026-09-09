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

type namespace
type publication

val create_namespace :
  table:Symbol_table.t ->
  ?module_name:string ->
  unit ->
  (namespace, string) result

val namespace_scope : namespace -> Symbol_table.scope

val publish :
  namespace ->
  name:string ->
  kind:Symbol.kind ->
  origin:Symbol.origin ->
  (publication, string) result
(** Allocate one declaration symbol in this namespace. This is semantic
    identity, not executable or storage admission. *)

val publication_symbol : publication -> Symbol.t

val publish_global :
  namespace ->
  Frontend.Parser.global_publication ->
  (publication, string) result
(** Allocate a global symbol and retain the exact parser publication at that
    allocation. Later matching names or origins cannot attach source metadata.
*)

val publication_source_global :
  publication -> Frontend.Parser.global_publication option

val publish_function :
  namespace ->
  Frontend.Parser.function_publication ->
  (publication, string) result

val publication_source_function :
  publication -> Frontend.Parser.function_publication option

val namespace_owns_publication : namespace -> publication -> bool
val namespace_owns_table : namespace -> Symbol_table.t -> bool

val view : namespace -> (publication * declaration) list -> (t, string) result
(** Make a command-local collection without allocating symbols. Every
    publication must belong to this exact namespace and match its declaration's
    kind, name and origin. Reject duplicates and non-increasing command-local
    positions. Earlier views and table lookup order remain unchanged. *)
