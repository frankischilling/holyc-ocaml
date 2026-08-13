type backing_site
type base_site
type header
type t

val make_backing_site :
  spelling:string ->
  origin:Symbol.origin ->
  spelling_origin:Symbol.origin ->
  pointer_origins:Symbol.origin list ->
  resolved_type:Type.t ->
  (backing_site, string) result

val make_base_site :
  spelling:string ->
  origin:Symbol.origin ->
  colon_origin:Symbol.origin ->
  name_origin:Symbol.origin ->
  symbol:Symbol.t ->
  (base_site, string) result

val make_header :
  symbol:Symbol.t ->
  aggregate_kind:Aggregate_resolution.aggregate_kind ->
  item_index:int ->
  origin:Symbol.origin ->
  keyword_origin:Symbol.origin ->
  backing:backing_site option ->
  base:base_site option ->
  (header, string) result

val resolve :
  table:Symbol_table.t ->
  parent:Symbol_table.scope ->
  header list ->
  (t, string) result
(** Validate definition headers without changing the symbol table. Headers and
    their referenced aggregate identities must belong to [parent]. *)

val headers : t -> header list
val header_symbol : header -> Symbol.t
val header_aggregate_kind : header -> Aggregate_resolution.aggregate_kind
val header_item_index : header -> int
val header_origin : header -> Symbol.origin
val header_keyword_origin : header -> Symbol.origin
val header_backing : header -> backing_site option
val header_base : header -> base_site option
val backing_spelling : backing_site -> string
val backing_origin : backing_site -> Symbol.origin
val backing_spelling_origin : backing_site -> Symbol.origin
val backing_pointer_origins : backing_site -> Symbol.origin list
val backing_type : backing_site -> Type.t
val base_spelling : base_site -> string
val base_origin : base_site -> Symbol.origin
val base_colon_origin : base_site -> Symbol.origin
val base_name_origin : base_site -> Symbol.origin
val base_symbol : base_site -> Symbol.t
