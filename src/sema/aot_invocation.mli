type t

val create :
  context:Frontend.Parser.command_context ->
  events_rev:Frontend.Parser.command_event list ->
  namespace:Declaration_collection.namespace ->
  fork:Symbol_table.empty_fork ->
  owner:unit ref ->
  (t, string) result

val claim_peer :
  t -> parent:Symbol_table.t -> child:Symbol_table.t -> (unit, string) result

val fork : t -> Symbol_table.empty_fork
val context : t -> Frontend.Parser.command_context
val owns_namespace : t -> Declaration_collection.namespace -> bool
val namespace : t -> Declaration_collection.namespace

val consume_peer :
  t ->
  parent:Symbol_table.t ->
  child:Symbol_table.t ->
  owner:unit ref ->
  (unit, string) result

val claim_consumer :
  t -> owner:unit ref -> child:Symbol_table.t -> (unit, string) result
