type t

val create :
  lookup:Sema.Aggregate_member_index.lookup ->
  base_pointer:Sema.Type.t ->
  pointer_type:Sema.Type.t ->
  (t, string) result
(** Retain the immutable selected field, its exact aggregate and member types,
    and representable array strides. Callback storage needs its separate owner.
*)

val matches :
  t ->
  base_pointer:Sema.Type.t ->
  pointer_type:Sema.Type.t ->
  offset:int64 ->
  bool

val offset : t -> int64
val strides : t -> int64 list
val member_symbol : t -> Sema.Symbol.t
