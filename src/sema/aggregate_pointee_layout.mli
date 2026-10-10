type t

val create :
  members:Aggregate_member_index.t ->
  before_item_index:int ->
  pointer_type:Type.t ->
  t option
(** Retain the exact nonempty, completed earlier aggregate selected by a
    one-level pointer, including its completed inherited layout. *)

val byte_size : t -> int64
val aggregate_symbol : t -> Symbol.t

val matches :
  t -> before_item_index:int -> pointer_type:Type.t -> stride:int64 -> bool
