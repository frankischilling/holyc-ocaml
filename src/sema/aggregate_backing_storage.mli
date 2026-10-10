type t

val create :
  table:Symbol_table.t ->
  members:Aggregate_member_index.t ->
  policies:Function_call_conversion_policy.t ->
  before_item_index:int ->
  source_type:Type.t ->
  t option
(** Select the original completed aggregate and its source-visible integer
    backing, or signed RT_PTR value when the chain ends at an ordinary class.
    The class byte extent remains separate from the scalar width. *)

val source_type : t -> Type.t
val value_type : t -> Type.t
val aggregate_symbol : t -> Symbol.t

val matches :
  t ->
  function_symbol:Symbol.t ->
  function_scope:Symbol.Scope_id.t ->
  before_item_index:int ->
  source_type:Type.t ->
  value_type:Type.t ->
  bool
