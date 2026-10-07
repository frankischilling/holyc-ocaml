type t
(** A C-created scope for the physical caller suspended at its native source
    callback. It expires before the caller resumes. No address is exposed. *)

val check : t -> (unit, string) result

val owns_generation : t -> 'target -> (bool, string) result
(** Compare the physical original target, without accepting copied metadata. *)

val limits : t -> (int * int * int * int, string) result
(** Remaining instructions, semantic frame bytes, call depth and native stack
    bytes at this original suspended call. Foreign domains and expired scopes
    are rejected. *)

val open_raw : 'bridge -> t * string
(** Open this actual callback once. Runtime callback adapters alone receive the
    opaque C bridge argument. Other values, duplicates and replay are rejected.
*)
