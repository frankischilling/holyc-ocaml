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
  id : int;
  name : string;
  scope : string option;
  exported : bool;
  position : int64;
  item_index : int;
  definition_item_indices : int list;
}

type relocation = private {
  offset : int;
  width : int;
  kind : string;
  symbol : string option;
  type_id : int;
  position : int64;
  relative_base : int64 option;
  value : int64;
  location : Frontend.Ast.location;
}

type fixup = private {
  item_index : int;
  offset : int;
  position : int64;
  width : int;
  relative_base : int64 option;
  value : int64;
  symbols : string list;
  symbol_ids : int list;
  imported : string option;
  location : Frontend.Ast.location;
}

type binary_input = private {
  item_index : int;
  provenance : Frontend.Assembly_binary.provenance;
}

type t = private {
  assembly : Frontend.Ast.assembly_unit;
  bytes : string;
  initial_bytes : string;
  position : int64;
  end_position : int64;
  instructions : instruction list;
  entries : entry list;
  origin : int64 option;
  max_align_bits : int;
  imports : string list;
  symbols : symbol list;
  relocations : relocation list;
  fixups : fixup list;
  external_symbols : (string * int64) list;
  binary_inputs : binary_input list;
  work : int;
  input_bytes : int;
}

val encode :
  ?position:int64 ->
  ?config:Frontend.Preprocessor.Config.t ->
  ?symbols:(string * int64) list ->
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
    records; returned bytes are an unrelocated assembly image. Supplied symbols
    are already loaded external addresses. Explicit IMPORT declarations shadow
    them and produce deferred import records. Forward fields retain their
    original width; unknown required labels and out-of-range branches fail.
    [initial_bytes] is a normalized image before forward fixups: deferred
    absolute fields contain zero, and deferred relative fields contain the
    negated raw relative base. It does not reproduce the original DU routine's
    retained or uninitialized provisional scalar. *)

type linked = private {
  image : t;
  bytes : string;
  base : int64;
  position : int64;
  end_position : int64;
  symbols : symbol list;
}

val link :
  ?symbols:(string * int64) list ->
  ?max_work:int ->
  base:int64 ->
  t ->
  (linked, Common.Diagnostic.t) result
(** Apply the source's raw-image AAT and import fixups into a fresh byte string.
    [base] is added to raw coordinates. AAT stores wrap at their field width;
    import stores replace the provisional expression value. Relative imports use
    the field end and must fit their signed width. Every used import must have
    an explicit supplied binding. The original image remains reusable. This is
    neither a BIN serializer nor an executable-memory loader. *)

val schema : string
val to_yojson : Common.Source_manager.t -> t -> Yojson.Safe.t
val human : t -> string
val linked_to_yojson : Common.Source_manager.t -> linked -> Yojson.Safe.t
val linked_human : linked -> string
val map : t -> string
val listing : Common.Source_manager.t -> t -> string
