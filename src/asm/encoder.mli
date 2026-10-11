type error = { operand_index : int option; message : string }
(** [operand_index], when present, is zero-based within the instruction. *)

type plan

type emission = private {
  bytes : string;
  size : int;
  selection : Instruction.selection;
}

val max_code_bytes : int
(** The output limit shared with the native encoder, bounded by the host string
    limit and 16 MiB. Explicit limits must be positive and no greater than this.
*)

val plan :
  ?max_bytes:int ->
  position:int64 ->
  Instruction.selection ->
  (plan, error) result
(** Resolve every numeric field of an already selected source instruction and
    validate the complete recipe without allocating a byte buffer. Positions are
    signed [int64] coordinates; instruction-end and relative arithmetic must not
    overflow. The selected form is retained when a branch is out of range.

    All operand expressions and address categories must have known values.
    Numeric fields must fit their selected width; signed and unsigned immediate
    classes, base displacements and relative targets keep their distinct ranges.
    Absolute address fields accept signed or unsigned bit-pattern spelling.

    Source prefix order, opcode patches, x87 rules and mode/address quirks are
    preserved. Emission rejects non-x86 scales, empty memory addresses, ignored
    stack-pointer indices, MM/XMM operands in general-register forms, extended
    registers or 64-bit integer operands outside USE64 and byte-register
    combinations that require or conflict with the selected REX prefix. These
    checks do not alter parsing or instruction selection. *)

val size : plan -> int

val emit : plan -> string
(** Emit a validated plan into one fresh immutable string. *)

val encode :
  ?max_bytes:int ->
  position:int64 ->
  Instruction.selection ->
  (emission, error) result
(** Plan and emit one instruction, retaining its selected source form facts. *)

val encode_all : max_bytes:int -> plan list -> (string, error) result
(** Check the complete aggregate output size before allocating one buffer. Each
    plan retains the explicit position supplied when it was constructed. *)
