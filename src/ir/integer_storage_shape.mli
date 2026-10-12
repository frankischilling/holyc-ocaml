type t
type error = Unsupported_type | Invalid_extent | Overflow

val create : type_:Sema.Type.t -> dimensions:int64 list -> (t, error) result
val dimensions : t -> int64 list
val strides : t -> int64 list
val element_count : t -> int
val byte_size : t -> int
val scalar : t -> Integer_scalar_storage.t
val element_size : t -> int
val cell_count : t -> int
val is_aggregate : t -> bool

val matches :
  t ->
  symbol:Sema.Symbol.t ->
  type_:Sema.Type.t ->
  dimensions:int64 list ->
  bool

(** Match an aggregate shape to its exact original root symbol, nominal type and
    source dimensions. Scalar shapes have no aggregate owner and reject. *)

val create_global :
  members:Sema.Aggregate_member_index.t ->
  global:Sema.Global_type_resolution.global ->
  dimensions:int64 list ->
  (t, error) result

val create_static :
  frame:Sema.Function_frame_layout.function_layout ->
  location:Sema.Function_frame_layout.location ->
  (t, error) result

val create_static_allocation :
  allocation:Sema.Compiler_record.static_allocation ->
  symbol:Sema.Symbol.t ->
  dimensions:int64 list ->
  (t, error) result

val create_declared_global :
  Sema.Compiler_record.declared_global -> (t, error) result

val padded_byte_size : t -> int option
