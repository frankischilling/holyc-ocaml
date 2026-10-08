type t

val create_function : name:string -> t
val use_count : t -> int64
val increment : t -> unit
val reset : t -> unit

val matches_function : t -> name:string -> bool
(** Private native storage for an owned function phase's CHash prefix. The
    original source consumer must establish record and table ownership before
    mutation. These buffers expose no function payload, hash-table search,
    executable address or exported ABI authority. Operations require the
    allocation's original domain. *)

val verify_storage : unit -> bool
(** Compare the U32 field operation with the pinned INC instruction, including
    wrapping, and check the 64-bit CHash prefix layout. *)
