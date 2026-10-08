type t
(** A C-created scope for the physical caller suspended at its native source
    callback. It expires before the caller resumes. No address is exposed. *)

type request = private
  | Execute_source of string
  | Read_option of int64
  | Write_option of int64 * bool
      (** Original source or compiler operation captured by an entered native
          call. The private C bridge produces these requests from actual machine
          values. *)

val check : t -> (unit, string) result

val owns_request : t -> request -> (bool, string) result
(** Require the exact C-created request and unchanged actual machine payload of
    this current callback. Copies, changed payloads, foreign domains and expired
    callbacks cannot authorize a compiler operation. *)

val owns_generation : t -> 'target -> (bool, string) result
(** Compare the physical original target, without accepting copied metadata. *)

val limits : t -> (int * int * int * int, string) result
(** Remaining instructions, semantic frame bytes, call depth and native stack
    bytes at this original suspended call. Foreign domains and expired scopes
    are rejected. *)

val open_raw : 'bridge -> t * request
(** Open this actual callback once. Runtime callback adapters alone receive the
    opaque C bridge argument. Other values, duplicates and replay are rejected.
*)
