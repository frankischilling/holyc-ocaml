type 'copy t

val consume : 'copy t -> copy:'copy -> bytes:int -> (unit, string) result
(** One-use completion produced by the actual native compiler copy. A caller
    cannot replace its original copy token or reached byte count. *)
