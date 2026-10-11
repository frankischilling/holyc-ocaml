type error = { operand_index : int option; message : string }
(** [operand_index], when present, is zero-based. *)

type warning = Unspecified_size

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
    [unresolved] selection is provisional until its expressions have values.
    Relative range checks still require the eventual instruction position. *)

val warning_message : warning -> string
