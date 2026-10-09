type t
(** Private native storage for the option and diagnostic fields of an original
    parser input. The allocation has the pinned [CCmpCtrl] prefix through
    [warning_cnt]; [opts], [error_cnt], [warning_cnt] and [CCF_HAS_RETURN] are
    used. It does not provide the complete record, task queue, table context,
    lexical buffers or an exported ABI.

    Parser contexts retain their focus, source and domain checks. Sharing this
    storage does not grant permission to act through another context. *)

val create : options:int64 -> t

val child : t -> t
(** Copy live options into a distinct input with a fresh zero warning count. *)

val options : t -> int64

val get_option : t -> bit_index:int -> bool
(** These private field operations accept indices 0 through 63. The parser
    separately restricts source calls to the twelve checked compiler options.
    Invalid indices and foreign domains reject before mutation. *)

val set_option : t -> bit_index:int -> bool -> bool
(** Return the previous bit value, as [BEqu] does. *)

val warning_count : t -> int64
val increment_warning : t -> unit
val error_count : t -> int64

val increment_error : t -> unit
(** Increment the pinned I64 error field at byte 344. The original parser
    producer supplies its authority; an arbitrary diagnostic does not. *)

val has_return : t -> bool

val set_has_return : t -> bool -> unit
(** The pinned bit 22 in [flags] at byte 24. Child inputs start clear;
    directives share the original field. Foreign domains reject before access.
*)

val verify_storage : unit -> bool
(** Compare production fields and operations with literal-offset x86-64
    BT/BTS/BTR/INC instructions. False when that oracle is unavailable. *)
