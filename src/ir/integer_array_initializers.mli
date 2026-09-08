type payload = Word of int64 | Bytes of string
type 'root entry
type 'root t

val create :
  shape:Integer_storage_shape.t ->
  source:Sema.Initializer_source.t ->
  roots:'root list ->
  source_leaf:('root -> Sema.Initializer_source.leaf option) ->
  ('root t, string) result

val entries : 'root t -> 'root entry list
val root : 'root entry -> 'root
val destination : 'root entry -> Integer_initializer_layout.entry
val prepared : 'root entry -> (payload * int) option
val find : 'root t -> 'root -> 'root entry option
val steps : 'root t -> int
val has_unprepared : 'root t -> bool

val publish :
  'root t -> ('root * payload * int) list -> ('root t, string) result
(** Internal immutable publication requires an original root, a determined copy
    or scalar word, and positive bounded preparation work. *)
