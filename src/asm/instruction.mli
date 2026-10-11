type error = { operand_index : int option; message : string }
(** [operand_index], when present, is zero-based. *)

type warning = Unspecified_size

type numeric_field = private {
  bytes : int;
  operand_index : int option;
  relative : bool;
  address : bool;
}
(** A missing [operand_index] denotes the source routine's implicit zero.
    [address] distinguishes an absolute address from a base displacement.
    [relative] fields contain targets and require a position before emission. *)

type selection = private {
  opcode : Opcode.resolved;
  mode : Operand.mode;
  operands : Operand.t list;
  form : Generated.Opcode_keywords.instruction;
  candidate_count : int;
  warnings : warning list;
  selection_bytes : int;
  encoded_bytes : int;
  operand_prefix : bool;
  address_prefix : bool;
  segment_prefix : int option;
  rex : int option;
  modrm : int option;
  sib : int option;
  immediate_bytes : int;
  displacement_bytes : int;
  displacement_field : numeric_field option;
  immediate_field : numeric_field option;
  unresolved : bool;
}

val select :
  mode:Operand.mode ->
  opcode:Opcode.resolved ->
  operands:Operand.t list ->
  (selection, error) result
(** Selection preserves the original shortest-form rule and first-form ties.
    [selection_bytes] is the original comparison cost; [encoded_bytes] also
    includes emitted size/segment prefixes and the ENTER trailing zero. An
    [unresolved] selection reserves its numeric fields before its expressions
    have values. Relative range checks require the eventual instruction
    position. *)

val warning_message : warning -> string

val select_form :
  mode:Operand.mode ->
  opcode:Opcode.resolved ->
  operands:Operand.t list ->
  entry_index:int ->
  (selection, error) result
(** Selects one original form by its opcode-local index after the same operand
    count, argument-mask and mode checks as [select]. The form is retrieved from
    the pinned opcode database. This explicit path does not search alternative
    forms or inherit the last candidate's emission state. *)

val numeric_fields : selection -> numeric_field option * numeric_field option
(** Returns the selected displacement and immediate descriptions, in emission
    order. Their operand ownership follows [PrsAsmInst], including implicit zero
    displacements and the source's single-argument and SIB distinctions. These
    descriptions retain the widths reserved by the original selection. *)

val resolve_values :
  selection -> (int64 * bool) option list -> (selection, error) result
(** Supply one slot per selected operand. [Some (value, absolute)] resolves that
    operand's original immediate expression and address-count parity; [None]
    retains the operand. An operand without an immediate expression rejects a
    supplied value. A value without an owned numeric field must be unchanged.

    Resolution preserves the original form, prefixes, ModRM, SIB and numeric
    field widths and ownership. It never selects a shorter form or changes an
    implicit zero field. Remaining unresolved operands and numeric ranges are
    checked by [Encoder] when the instruction position is available. *)
