type t

val create_function : name:string -> t
val create_prefix : name:string -> type_bits:int32 -> t

val hash_string : string -> int64
(** Pinned byte hash, including outgoing shift carry and final bit-15 carry.
    Like HashStr, input stops at its first NUL. *)

val use_count : t -> int64
val increment : t -> unit
val reset : t -> unit

val matches_function : t -> name:string -> bool
(** Private native storage for an owned function phase's CHash prefix. The
    original source consumer must establish record and table ownership before
    mutation. These buffers expose no function payload, executable address or
    exported ABI authority. Operations require the allocation's original domain.
*)

val verify_storage : unit -> bool
(** Compare the U32 field operation with the pinned INC instruction, including
    wrapping, and check the 64-bit CHash prefix layout. *)

module Table : sig
  type record = t
  type t

  val create : size:int -> t

  val add : t -> record -> unit
  (** Head insertion into the original bucket. A record may belong to only one
      live table; reinserting it or inserting it in another table rejects. *)

  val set_next : t -> t -> unit
  (** Retain the original successor allocation; cycles reject before mutation.
      Table and record allocations survive independently collected handles. *)

  val selects :
    ?instance:int64 ->
    ?chain:bool ->
    t ->
    expected:record ->
    name:string ->
    mask:int32 ->
    bool

  val find :
    ?instance:int64 ->
    ?chain:bool ->
    t ->
    expected:record ->
    name:string ->
    mask:int32 ->
    bool
  (** Search native buckets and optionally their next-table chain. The selected
      instance spans the chain. Only a physically selected [expected] allocation
      is incremented; a miss or another allocation leaves every count unchanged.
      [selects] performs the same search without mutation. These primitives
      grant no source, task-table or executable authority. All operations
      require the original domain. *)

  val verify_storage : unit -> bool
  (** Independent x86-64 comparison of HashStr and bucket/chain selection with
      the pinned instruction sequence, including count changes and U32 wrap. *)
end
