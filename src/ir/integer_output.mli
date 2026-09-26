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

val share_work : t -> max_output_bytes:int -> t
(** New capture and byte budget, sharing the parent's work budget. The caller
    validates a nonnegative byte limit that fits a host string. *)

val fork : t -> t
(** New capture sharing both budgets. Successful output remains charged after
    its capture is discarded; each Print call still commits atomically. *)

val print :
  t ->
  read_byte:('pointer -> int64 -> (char, 'error) result) ->
  format:'pointer ->
  'pointer argument array ->
  (unit, 'error failure) result
(** Format ordinary bytes, percent, signed/unsigned decimal, hexadecimal,
    binary, strings, NUL-delimited list subscripts, escaped/decoded byte strings
    and packed chars, including ASCII uppercase packed output. The checked
    subset accepts source justification/zero flags, bounded literal or dynamic
    widths, ignored literal/dynamic precision, comma/truncate/harmless integer
    modifiers, and source-style [h] auxiliary fields with wrapping I64
    accumulation. Dynamic fields consume their integer variadic slots. Lowercase
    [z] consumes a word index and owned U8 list, preserving the pinned [LstSub]
    alias/sentinel probe order before applying ordinary string layout. [h]
    repeats packed [c]/[C] fields; auxiliary decimal engineering formatting is
    rejected explicitly. Publish the complete draft only on success. Reads
    include terminating zero bytes and are charged before the callback,
    including failed read attempts. Quoted fields first scan and measure the
    complete conversion, retaining only its visible prefix length, then
    regenerate the selected output through fixed four-byte chunks. Decoded NUL
    does not suppress later source reads in the first pass. *)

val put_chars : t -> int64 -> (unit, 'error failure) result
(** Visit low-to-high packed bytes, skipping interior zeros. Each successful
    byte is committed immediately; later faults retain that prefix. *)

val discard_print :
  t ->
  read_byte:('pointer -> int64 -> (char, 'error) result) ->
  format:'pointer ->
  'pointer argument array ->
  (unit, 'error failure) result
(** Perform bounded formatting without committing text or bytes. The pinned
    StreamPrint formats before testing for an active stream block. *)

val contents : t -> string
val work : t -> int
val committed_bytes : t -> int
val capacity : t -> int
