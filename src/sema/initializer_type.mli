type t
type scalar
type element
type view = Scalar of scalar | Aggregate of element list
type scalar_evaluation

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
val begin_scalar_evaluation : t -> (scalar_evaluation, string) result
val scalar_evaluation_before : scalar_evaluation -> scalar

val finish_scalar_evaluation : scalar_evaluation -> (scalar, string) result
(** Retain the scalar class selected before expression evaluation. Backing is
    followed at that selection; settlement reads the cached class's reached
    nominal width without repeating member traversal or backing selection. The
    cached raw representation remains selected. A forward class with no members
    can supply this metadata phase. Settlement is consumed once in its original
    domain. These reads do not execute an expression or authorize storage
    writes. *)
