type t

val integer_value_type :
  table:Symbol_table.t ->
  members:Aggregate_member_index.t ->
  policies:Function_call_conversion_policy.t ->
  before_item_index:int ->
  source_type:Type.t ->
  Type.t option
(** Read the integer raw class of an exact completed aggregate in this checked
    namespace. This grants no object storage or byte extent. Empty classes may
    have an integer raw class even though automatic storage cannot admit them.
*)

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

val create_visible :
  visibility:Function_aggregate_visibility.function_ ->
  source:Frontend.Ast.expression ->
  table:Symbol_table.t ->
  members:Aggregate_member_index.t ->
  policies:Function_call_conversion_policy.t ->
  source_type:Type.t ->
  t option

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
