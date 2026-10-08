type t
(** Private native storage for the option and warning fields of an original
    parser input. The allocation has the pinned [CCmpCtrl] prefix through
    [warning_cnt]; other fields remain zero. It does not provide the complete
    record, task queue, table context, lexical buffers or an exported ABI.

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

val verify_storage : unit -> bool
(** Compare production fields and operations with literal-offset x86-64
    BT/BTS/BTR/INC instructions. False when that oracle is unavailable. *)
