type t
type scalar
type element
type view = Scalar of scalar | Aggregate of element list

val of_type : Type.t -> (t, string) result

val of_record :
  ?resolve:(Type.t -> Compiler_record.t option) ->
  type_:Type.t ->
  Compiler_record.t ->
  (t, string) result

val of_members :
  headers:Aggregate_header_resolution.t ->
  members:Aggregate_member_index.t ->
  before_item_index:int ->
  type_:Type.t ->
  (t, string) result
(** Retain checked initializer type metadata, without allocating storage or
    granting source or execution authority. Class references keep their original
    nominal identity; initializer traversal reads direct members and forwarding,
    independently of field-access offsets and the root's allocation extent.
    Callback leaves retain their original anonymous header and pointer storage
    width; numeric initializer values grant no callable field identity. *)

val view : t -> (view, string) result
val element_type : element -> t
val element_dimensions : element -> int64 list
val scalar_type : scalar -> Type.t
val scalar_value_type : scalar -> Type.t
val scalar_byte_size : scalar -> int
val scalar_equal : scalar -> scalar -> bool
