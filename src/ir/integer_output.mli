type t
type 'pointer argument = Word of int64 | Pointer of 'pointer

type 'error failure =
  | Memory of 'error
  | Output_limit
  | Work_limit
  | Offset_overflow
  | Invalid_format of string
  | Invalid_argument of string

val create : max_output_bytes:int -> max_output_work:int -> t
(** The caller validates positive limits. Capture is private to one execution.
*)

val print :
  t ->
  read_byte:('pointer -> int64 -> (char, 'error) result) ->
  format:'pointer ->
  'pointer argument array ->
  (unit, 'error failure) result
(** Format ordinary bytes, percent, signed decimal, strings and packed chars.
    Publish the complete draft only on success. Reads include the terminating
    zero and are charged before the callback, including failed read attempts. *)

val put_chars : t -> int64 -> (unit, 'error failure) result
(** Visit low-to-high packed bytes, skipping interior zeros. Each successful
    byte is committed immediately; later faults retain that prefix. *)

val contents : t -> string
val work : t -> int
