type t
(** Original source phases of a named function header. These records retain
    partial members independently of completed signatures. They do not establish
    native argument counts, evaluated defaults, layout or runtime admission. *)

type snapshot
type member

val create :
  ?activation:Source_activation.t ->
  table:Symbol_table.t ->
  namespace:Declaration_collection.namespace ->
  Declaration_collection.publication ->
  Frontend.Parser.function_publication ->
  (t, string) result
(** Requires the exact namespace publication during its original declaration
    callback or matching source activation event. *)

val observe :
  ?activation:Source_activation.t ->
  t ->
  Frontend.Parser.declaration_event ->
  (unit, string) result
(** Validates ownership, phase and original children before updating the record.
    A rejected, delayed or repeated event leaves the current snapshot unchanged.
    Default events describe parsed source, not successful default evaluation. *)

val event_belongs : t -> Frontend.Parser.declaration_event -> bool
val snapshot : t -> snapshot
val source : snapshot -> Frontend.Parser.function_publication
val publication : snapshot -> Declaration_collection.publication
val owns_table : snapshot -> Symbol_table.t -> bool
val owns_namespace : snapshot -> Declaration_collection.namespace -> bool
val previous_lookup : snapshot -> Frontend.Symbol_visibility.lookup
val members : snapshot -> member list
val member_source : member -> Frontend.Parser.function_parameter_publication

val member_default_source :
  member -> Frontend.Parser.completed_parameter_default option

val member_completion :
  member -> Frontend.Parser.completed_function_parameter option

val variadic_source :
  snapshot -> Frontend.Parser.function_variadic_publication option

val variadic_members_present : snapshot -> bool

val completed_header :
  snapshot -> Frontend.Parser.completed_function_header option
(** Snapshots are immutable. Earlier reads retain the exact phase and original
    source children they observed, including through later header completion. *)
