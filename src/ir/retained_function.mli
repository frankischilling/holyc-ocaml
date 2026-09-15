type t

val create : Sema.Outer_environment.function_metadata -> t
val metadata : t -> Sema.Outer_environment.function_metadata
val symbol : t -> Sema.Symbol.t
val same : t -> t -> bool
