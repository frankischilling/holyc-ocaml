type t
type error = Unsupported_type | Invalid_extent | Overflow

val create : type_:Sema.Type.t -> dimensions:int64 list -> (t, error) result
val dimensions : t -> int64 list
val strides : t -> int64 list
val element_count : t -> int
val byte_size : t -> int
val padded_byte_size : t -> int option
