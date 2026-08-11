type kind = Language | Assembly

type entry = private {
  kind : kind;
  spelling : string;
  templeos_id : int;
  source_line : int;
}

type register_kind = R8 | R16 | R32 | R64 | Segment | Float_stack | Mm | Xmm

type register = private {
  register_kind : register_kind;
  register_type : int;
  spelling : string;
  register_number : int;
  source_line : int;
}

type instruction = private {
  entry_index : int;
  opcode_bytes : int list;
  flags : int;
  slash_value : int;
  uasm_slash_value : int;
  opcode_modifier : int;
  argument1 : int;
  argument2 : int;
  size1 : int;
  size2 : int;
  source_line : int;
}

type opcode_alias = private { spelling : string; source_line : int }

type opcode = private {
  spelling : string;
  instructions : instruction list;
  aliases : opcode_alias list;
  source_line : int;
}

val reference_commit : string
val source_path : string
val source_sha256 : string
val registers : register list
val language : entry list
val assembly : entry list
val opcodes : opcode list
