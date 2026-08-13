type scope_kind =
  | Task
  | Module
  | Function
  | Block
  | Aggregate
  | Assembler_block

type scope
type t

val create : ?root_name:string -> unit -> t
val root : t -> scope

val create_scope :
  t ->
  parent:scope ->
  kind:scope_kind ->
  ?name:string ->
  unit ->
  (scope, string) result

val add :
  t ->
  scope:scope ->
  name:string ->
  kind:Symbol.kind ->
  origin:Symbol.origin ->
  (Symbol.t, string) result

val lookup_local :
  t ->
  scope:scope ->
  name:string ->
  kinds:Symbol.kind list ->
  ?instance:int ->
  unit ->
  (Symbol.t option, string) result

val lookup :
  t ->
  scope:scope ->
  name:string ->
  kinds:Symbol.kind list ->
  ?instance:int ->
  unit ->
  (Symbol.t option, string) result

val scope_id : scope -> Symbol.Scope_id.t
val scope_kind : scope -> scope_kind
val scope_name : scope -> string option
val parent : scope -> scope option
val scope_kind_name : scope_kind -> string
val all_scopes : t -> scope list
val all_symbols : t -> Symbol.t list
val to_yojson : Common.Source_manager.t -> t -> Yojson.Safe.t
val human : Common.Source_manager.t -> t -> string
val json : Common.Source_manager.t -> t -> string
