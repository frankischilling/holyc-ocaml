type t

val create :
  members:Aggregate_member_index.t ->
  before_item_index:int ->
  pointer_type:Type.t ->
  t option
(** Retain the exact nonempty, completed earlier aggregate selected by a
    one-level pointer, including its completed inherited layout. *)

val create_visible :
  visibility:Function_aggregate_visibility.function_ ->
  source:Frontend.Ast.expression ->
  members:Aggregate_member_index.t ->
  pointer_type:Type.t ->
  t option

val create_index :
  visibility:Function_aggregate_visibility.function_ ->
  source:Frontend.Ast.expression ->
  pointer_type:Type.t ->
  stride:int64 ->
  t option
(** Authenticate the original index's reached class size independently of its
    earlier object allocation. This grants no additional byte extent. *)

val byte_size : t -> int64
val aggregate_symbol : t -> Symbol.t

val matches :
  ?function_symbol:Symbol.t ->
  t ->
  before_item_index:int ->
  pointer_type:Type.t ->
  stride:int64 ->
  bool
