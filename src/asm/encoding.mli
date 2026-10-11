type range = Signed | Unsigned | Bit_pattern
type field

val field : bytes:int -> range:range -> int64 -> (field, string) result
(** Construct a checked little-endian numeric field of one, two, four or eight
    bytes. [Signed] accepts the signed range; [Unsigned] accepts the unsigned
    range; [Bit_pattern] accepts either representation. All three admit every
    [int64] bit pattern at width eight. Values are never silently narrowed. *)

type opcode_patch = Or_low3 of int | Add_low3 of int
type plan

val max_prefix_bytes : int
(** At most four separate prefix bytes precede the pinned opcode bytes.
    Mandatory prefixes already present in the generated opcode remain part of
    that opcode. *)

val plan :
  mode:Operand.mode ->
  form:Generated.Opcode_keywords.instruction ->
  ?prefixes:int list ->
  ?opcode_patch:opcode_patch ->
  ?modrm:int ->
  ?sib:int ->
  ?displacement:field ->
  ?immediate:field ->
  ?trailing_zero:bool ->
  unit ->
  (plan, string) result
(** Validate a complete encoding recipe before allocating an output buffer. The
    form must be the owned, nonempty generated record. Prefix order is retained;
    REX is permitted only at the end of the prefix list in USE64. Opcode patches
    address only the low register bits of a generated plus-opcode form. Required
    operand-size and REX.W prefixes, numeric widths, ModR/M, SIB and the ENTER
    trailing zero must match the form and mode. Operand selection, register
    extension bits, address-size selection and relative-address calculation
    belong to the caller. This interface produces bytes and grants no executable
    or storage ownership. *)

val size : plan -> int
(** The exact byte length of the validated recipe. *)

val encode : plan -> string
(** Emit the recipe into one fresh immutable string. *)

val encode_all : max_bytes:int -> plan list -> (string, string) result
(** Check the positive limit and complete aggregate length before allocating a
    buffer. Empty input produces an empty string when the limit is valid. *)
