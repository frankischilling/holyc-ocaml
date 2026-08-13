type aggregate_kind = Class | Union
type declaration_kind = Forward | Definition
type declaration
type declaration_site
type identity
type resolved_declaration
type t

val make_declaration :
  symbol:Symbol.t ->
  declaration_kind:declaration_kind ->
  aggregate_kind:aggregate_kind ->
  item_index:int ->
  (declaration, string) result
(** Build one checked aggregate declaration fact without changing the symbol
    table. *)

val resolve :
  table:Symbol_table.t ->
  parent:Symbol_table.scope ->
  declaration list ->
  (t, string) result
(** Reconcile declarations in source order. A definition completes only the
    newest same-name identity when that identity is still a forward. *)

val identities : t -> identity list
val declarations : t -> resolved_declaration list
val identity_symbol : identity -> Symbol.t
val identity_forward : identity -> declaration_site option
val identity_definition : identity -> declaration_site option
val identity_kind : identity -> aggregate_kind
val identity_first_item_index : identity -> int
val declaration_site_symbol : declaration_site -> Symbol.t
val declaration_site_kind : declaration_site -> declaration_kind
val declaration_site_aggregate_kind : declaration_site -> aggregate_kind
val declaration_site_item_index : declaration_site -> int
val resolved_declaration_site : resolved_declaration -> declaration_site
val resolved_declaration_identity_symbol : resolved_declaration -> Symbol.t
val aggregate_kind_name : aggregate_kind -> string
val declaration_kind_name : declaration_kind -> string
