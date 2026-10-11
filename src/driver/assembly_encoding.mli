type instruction = private {
  item_index : int;
  operation : Frontend.Ast.inline_assembly_operation;
  selection : Asm.Instruction.selection;
  position : int64;
  offset : int;
  size : int;
}

type entry = private {
  item_index : int;
  kind : string;
  position : int64;
  offset : int;
  size : int;
  listing : bool;
  location : Frontend.Ast.location;
}

type symbol = private {
  name : string;
  scope : string option;
  exported : bool;
  position : int64;
  item_index : int;
}

type relocation = private {
  offset : int;
  width : int;
  kind : string;
  symbol : string option;
}

type binary_input = private {
  item_index : int;
  provenance : Frontend.Assembly_binary.provenance;
}

type t = private {
  assembly : Frontend.Ast.assembly_unit;
  bytes : string;
  position : int64;
  end_position : int64;
  instructions : instruction list;
  entries : entry list;
  origin : int64 option;
  max_align_bits : int;
  imports : string list;
  symbols : symbol list;
  relocations : relocation list;
  binary_inputs : binary_input list;
  work : int;
  input_bytes : int;
}

val encode :
  ?position:int64 ->
  ?config:Frontend.Preprocessor.Config.t ->
  ?max_work:int ->
  ?max_binary_input_bytes:int ->
  max_code_bytes:int ->
  Frontend.Ast.assembly_unit ->
  (t, Common.Diagnostic.t) result
(** Preflight instructions and ordered assembler directives before allocating
    the complete output. Data words preserve the original low-byte stores;
    instruction operands retain checked widths. The optional configuration
    supplies compilation mode and explicit BINFILE input roots. No file is read
    without it. Limits cover output bytes, directive work and cumulative raw
    binary input. ORG remains metadata, and absolute fields have relocation
    records; returned bytes are an unrelocated assembly image. *)

val schema : string
val to_yojson : Common.Source_manager.t -> t -> Yojson.Safe.t
val human : t -> string
