type symbol = { name : string; address : int64 }

type operand = private {
  argument_type : int;
  size : int;
  text : string;
  value : int64 option;
  target : int64 option;
}

type instruction = private {
  offset : int;
  address : int64;
  bytes : string;
  opcode : string;
  form : Generated.Opcode_keywords.instruction;
  prefixes : int list;
  operands : operand list;
  jump_target : int64 option;
}

type error = { offset : int; message : string }
type t

val max_input_bytes : int

val decode :
  ?max_bytes:int ->
  ?max_instructions:int ->
  ?symbols:symbol list ->
  mode:Operand.mode ->
  position:int64 ->
  string ->
  (t, error) result
(** Decode a complete immutable stream using the pinned UAsm instruction forms,
    operand widths and dialect. Unknown, truncated and overlong instructions
    fail with their original byte offset. No partial report or executable
    authority is returned. Both limits are positive and checked before output.
    Signed address arithmetic is checked, including relative targets.

    This follows UAsm's legacy address and MOFFS rules; it is not a general x86
    decoder. In particular, the pinned UAsm warns that USE16 is not well
    supported. Bytes outside the pinned forms are rejected rather than decoded
    through UAsm's unchecked nearest-entry fallback. Interrupt annotations use
    the kernel's initial ST_INT_NAMES table. *)

val instructions : t -> instruction list
val mode : t -> Operand.mode
val position : t -> int64
val byte_count : t -> int
val text : instruction -> string
val listing : t -> string
val map : t -> string
val to_yojson : t -> Yojson.Safe.t
