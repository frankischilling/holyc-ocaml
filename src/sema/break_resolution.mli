type region_kind =
  | While_region
  | Do_while_region
  | For_body_region
  | Switch_region
  | Subswitch_region

module Region_id : sig
  type t

  val to_int : t -> int
  val compare : t -> t -> int
  val equal : t -> t -> bool
end

type region_input
type break_input
type function_input
type region
type resolved_break
type resolved_function
type t
type outcome = (t, string) result

val reference_commit : string

val make_region :
  region_index:int ->
  kind:region_kind ->
  origin:Symbol.origin ->
  (region_input, string) result

val make_break :
  occurrence_index:int ->
  origin:Symbol.origin ->
  target_region_index:int ->
  (break_input, string) result

val make_function :
  symbol:Symbol.t ->
  scope:Symbol_table.scope ->
  item_index:int ->
  regions:region_input list ->
  breaks:break_input list ->
  (function_input, string) result

val resolve : table:Symbol_table.t -> function_input list -> outcome
val functions : t -> resolved_function list
val function_symbol : resolved_function -> Symbol.t
val function_scope : resolved_function -> Symbol_table.scope
val function_item_index : resolved_function -> int
val function_regions : resolved_function -> region list
val function_breaks : resolved_function -> resolved_break list
val region_id : region -> Region_id.t
val region_kind : region -> region_kind
val region_origin : region -> Symbol.origin
val region_break_count : region -> int
val break_occurrence_index : resolved_break -> int
val break_origin : resolved_break -> Symbol.origin
val break_target : resolved_break -> Region_id.t
val region_kind_name : region_kind -> string

val human : t -> string
(** Render the versioned deterministic break-resolution form. *)
