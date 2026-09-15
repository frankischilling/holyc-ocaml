type t

val create : Sema.Symbol.t -> t
(** Internal identity allocation. A reference grants access only when an exact
    task catalog and compiled storage view retain it. *)

val symbol : t -> Sema.Symbol.t
val same : t -> t -> bool
