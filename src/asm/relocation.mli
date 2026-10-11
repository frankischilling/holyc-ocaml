type kind =
  | Add_base
  | Subtract_base
  | Relative_import of string
  | Absolute_import of string

type t

val make : offset:int -> width:int -> kind:kind -> (t, string) result
(** A relocation names a byte offset and a field of one, two, four or eight
    bytes. Offsets must be nonnegative, and import names must be nonempty.
    [apply] checks that the complete field belongs to its input image. *)

type error = { relocation_index : int option; message : string }
(** A present index is zero-based in the supplied relocation list. Errors in
    coordinates, symbol definitions or the initial image have no index. *)

val apply :
  ?max_work:int ->
  position:int64 ->
  base:int64 ->
  symbols:(string * int64) list ->
  bytes:string ->
  t list ->
  (string, error) result
(** Apply checked relocations to a fresh copy of an unrelocated image.

    [position] is the raw coordinate of byte zero. [base] is the nonnegative
    load bias added to raw coordinates. Both coordinates and both image ends
    must fit nonnegative signed 64-bit values. Symbol values retain their full
    [int64] representation. Duplicate or empty symbol names are errors, even
    when the image does not use them.

    Add/subtract records run first, in their supplied order, and change each
    little-endian word modulo its width. Imports then run in their supplied
    order and overwrite the field. Absolute imports store the low source-width
    bits. Relative imports store the symbol value minus the loaded field end,
    with checked signed arithmetic and field range. Their original placeholder
    bytes and expression addends do not enter this calculation. These rules
    follow [Compiler/CMain.HC:174-295], including the field-end base used when
    an instruction has bytes after its imported displacement.

    Validation and import resolution finish before the output buffer is
    allocated. Failure leaves [bytes] untouched and returns no partial image.
    The default [max_work] is 1000000; valid limits are 1 through 100000000.
    Work is the image byte count, one per relocation, one plus the name length
    per supplied symbol, and the name length of every import relocation.
    Repeated and unused names contribute to their respective counts. *)
