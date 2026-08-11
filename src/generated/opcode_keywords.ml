(* This table is generated from the pinned TempleOS opcode database.
   Run the opcode table generator after an approved reference update. *)

[@@@ocamlformat "disable"]

let reference_commit = "c26482bb6ad3f80106d28504ec5db3c6a360732c"
let source_path = "Compiler/OpCodes.DD"
let source_sha256 = "b814c439af73b080e7584dd32734241724d95f072b6ca7c805759fbd870f635d"

type kind = Language | Assembly

type entry = {
  kind : kind;
  spelling : string;
  templeos_id : int;
  source_line : int;
}

type register_kind = R8 | R16 | R32 | R64 | Segment | Float_stack | Mm | Xmm

type register = {
  register_kind : register_kind;
  register_type : int;
  spelling : string;
  register_number : int;
  source_line : int;
}

type instruction = {
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

type opcode_alias = { spelling : string; source_line : int }

type opcode = {
  spelling : string;
  instructions : instruction list;
  aliases : opcode_alias list;
  source_line : int;
}

let registers =
  [
    { register_kind = R8; register_type = 1; spelling = "AL"; register_number = 0; source_line = 26 };
    { register_kind = R8; register_type = 1; spelling = "CL"; register_number = 1; source_line = 27 };
    { register_kind = R8; register_type = 1; spelling = "DL"; register_number = 2; source_line = 28 };
    { register_kind = R8; register_type = 1; spelling = "BL"; register_number = 3; source_line = 29 };
    { register_kind = R8; register_type = 1; spelling = "AH"; register_number = 4; source_line = 30 };
    { register_kind = R8; register_type = 1; spelling = "CH"; register_number = 5; source_line = 31 };
    { register_kind = R8; register_type = 1; spelling = "DH"; register_number = 6; source_line = 32 };
    { register_kind = R8; register_type = 1; spelling = "BH"; register_number = 7; source_line = 33 };
    { register_kind = R8; register_type = 1; spelling = "R8u8"; register_number = 8; source_line = 34 };
    { register_kind = R8; register_type = 1; spelling = "R9u8"; register_number = 9; source_line = 35 };
    { register_kind = R8; register_type = 1; spelling = "R10u8"; register_number = 10; source_line = 36 };
    { register_kind = R8; register_type = 1; spelling = "R11u8"; register_number = 11; source_line = 37 };
    { register_kind = R8; register_type = 1; spelling = "R12u8"; register_number = 12; source_line = 38 };
    { register_kind = R8; register_type = 1; spelling = "R13u8"; register_number = 13; source_line = 39 };
    { register_kind = R8; register_type = 1; spelling = "R14u8"; register_number = 14; source_line = 40 };
    { register_kind = R8; register_type = 1; spelling = "R15u8"; register_number = 15; source_line = 41 };
    { register_kind = R8; register_type = 1; spelling = "RSPu8"; register_number = 20; source_line = 42 };
    { register_kind = R8; register_type = 1; spelling = "RBPu8"; register_number = 21; source_line = 43 };
    { register_kind = R8; register_type = 1; spelling = "RSIu8"; register_number = 22; source_line = 44 };
    { register_kind = R8; register_type = 1; spelling = "RDIu8"; register_number = 23; source_line = 45 };
    { register_kind = R16; register_type = 2; spelling = "AX"; register_number = 0; source_line = 47 };
    { register_kind = R16; register_type = 2; spelling = "CX"; register_number = 1; source_line = 48 };
    { register_kind = R16; register_type = 2; spelling = "DX"; register_number = 2; source_line = 49 };
    { register_kind = R16; register_type = 2; spelling = "BX"; register_number = 3; source_line = 50 };
    { register_kind = R16; register_type = 2; spelling = "SP"; register_number = 4; source_line = 51 };
    { register_kind = R16; register_type = 2; spelling = "BP"; register_number = 5; source_line = 52 };
    { register_kind = R16; register_type = 2; spelling = "SI"; register_number = 6; source_line = 53 };
    { register_kind = R16; register_type = 2; spelling = "DI"; register_number = 7; source_line = 54 };
    { register_kind = R16; register_type = 2; spelling = "R8u16"; register_number = 8; source_line = 55 };
    { register_kind = R16; register_type = 2; spelling = "R9u16"; register_number = 9; source_line = 56 };
    { register_kind = R16; register_type = 2; spelling = "R10u16"; register_number = 10; source_line = 57 };
    { register_kind = R16; register_type = 2; spelling = "R11u16"; register_number = 11; source_line = 58 };
    { register_kind = R16; register_type = 2; spelling = "R12u16"; register_number = 12; source_line = 59 };
    { register_kind = R16; register_type = 2; spelling = "R13u16"; register_number = 13; source_line = 60 };
    { register_kind = R16; register_type = 2; spelling = "R14u16"; register_number = 14; source_line = 61 };
    { register_kind = R16; register_type = 2; spelling = "R15u16"; register_number = 15; source_line = 62 };
    { register_kind = R32; register_type = 3; spelling = "EAX"; register_number = 0; source_line = 64 };
    { register_kind = R32; register_type = 3; spelling = "ECX"; register_number = 1; source_line = 65 };
    { register_kind = R32; register_type = 3; spelling = "EDX"; register_number = 2; source_line = 66 };
    { register_kind = R32; register_type = 3; spelling = "EBX"; register_number = 3; source_line = 67 };
    { register_kind = R32; register_type = 3; spelling = "ESP"; register_number = 4; source_line = 68 };
    { register_kind = R32; register_type = 3; spelling = "EBP"; register_number = 5; source_line = 69 };
    { register_kind = R32; register_type = 3; spelling = "ESI"; register_number = 6; source_line = 70 };
    { register_kind = R32; register_type = 3; spelling = "EDI"; register_number = 7; source_line = 71 };
    { register_kind = R32; register_type = 3; spelling = "R8u32"; register_number = 8; source_line = 72 };
    { register_kind = R32; register_type = 3; spelling = "R9u32"; register_number = 9; source_line = 73 };
    { register_kind = R32; register_type = 3; spelling = "R10u32"; register_number = 10; source_line = 74 };
    { register_kind = R32; register_type = 3; spelling = "R11u32"; register_number = 11; source_line = 75 };
    { register_kind = R32; register_type = 3; spelling = "R12u32"; register_number = 12; source_line = 76 };
    { register_kind = R32; register_type = 3; spelling = "R13u32"; register_number = 13; source_line = 77 };
    { register_kind = R32; register_type = 3; spelling = "R14u32"; register_number = 14; source_line = 78 };
    { register_kind = R32; register_type = 3; spelling = "R15u32"; register_number = 15; source_line = 79 };
    { register_kind = R64; register_type = 4; spelling = "RAX"; register_number = 0; source_line = 81 };
    { register_kind = R64; register_type = 4; spelling = "RCX"; register_number = 1; source_line = 82 };
    { register_kind = R64; register_type = 4; spelling = "RDX"; register_number = 2; source_line = 83 };
    { register_kind = R64; register_type = 4; spelling = "RBX"; register_number = 3; source_line = 84 };
    { register_kind = R64; register_type = 4; spelling = "RSP"; register_number = 4; source_line = 85 };
    { register_kind = R64; register_type = 4; spelling = "RBP"; register_number = 5; source_line = 86 };
    { register_kind = R64; register_type = 4; spelling = "RSI"; register_number = 6; source_line = 87 };
    { register_kind = R64; register_type = 4; spelling = "RDI"; register_number = 7; source_line = 88 };
    { register_kind = R64; register_type = 4; spelling = "R8"; register_number = 8; source_line = 89 };
    { register_kind = R64; register_type = 4; spelling = "R9"; register_number = 9; source_line = 90 };
    { register_kind = R64; register_type = 4; spelling = "R10"; register_number = 10; source_line = 91 };
    { register_kind = R64; register_type = 4; spelling = "R11"; register_number = 11; source_line = 92 };
    { register_kind = R64; register_type = 4; spelling = "R12"; register_number = 12; source_line = 93 };
    { register_kind = R64; register_type = 4; spelling = "R13"; register_number = 13; source_line = 94 };
    { register_kind = R64; register_type = 4; spelling = "R14"; register_number = 14; source_line = 95 };
    { register_kind = R64; register_type = 4; spelling = "R15"; register_number = 15; source_line = 96 };
    { register_kind = R64; register_type = 4; spelling = "R8u64"; register_number = 8; source_line = 97 };
    { register_kind = R64; register_type = 4; spelling = "R9u64"; register_number = 9; source_line = 98 };
    { register_kind = R64; register_type = 4; spelling = "R10u64"; register_number = 10; source_line = 99 };
    { register_kind = R64; register_type = 4; spelling = "R11u64"; register_number = 11; source_line = 100 };
    { register_kind = R64; register_type = 4; spelling = "R12u64"; register_number = 12; source_line = 101 };
    { register_kind = R64; register_type = 4; spelling = "R13u64"; register_number = 13; source_line = 102 };
    { register_kind = R64; register_type = 4; spelling = "R14u64"; register_number = 14; source_line = 103 };
    { register_kind = R64; register_type = 4; spelling = "R15u64"; register_number = 15; source_line = 104 };
    { register_kind = Segment; register_type = 5; spelling = "ES"; register_number = 0; source_line = 106 };
    { register_kind = Segment; register_type = 5; spelling = "CS"; register_number = 1; source_line = 107 };
    { register_kind = Segment; register_type = 5; spelling = "SS"; register_number = 2; source_line = 108 };
    { register_kind = Segment; register_type = 5; spelling = "DS"; register_number = 3; source_line = 109 };
    { register_kind = Segment; register_type = 5; spelling = "FS"; register_number = 4; source_line = 110 };
    { register_kind = Segment; register_type = 5; spelling = "GS"; register_number = 5; source_line = 111 };
    { register_kind = Float_stack; register_type = 6; spelling = "ST0"; register_number = 0; source_line = 113 };
    { register_kind = Float_stack; register_type = 6; spelling = "ST1"; register_number = 1; source_line = 114 };
    { register_kind = Float_stack; register_type = 6; spelling = "ST2"; register_number = 2; source_line = 115 };
    { register_kind = Float_stack; register_type = 6; spelling = "ST3"; register_number = 3; source_line = 116 };
    { register_kind = Float_stack; register_type = 6; spelling = "ST4"; register_number = 4; source_line = 117 };
    { register_kind = Float_stack; register_type = 6; spelling = "ST5"; register_number = 5; source_line = 118 };
    { register_kind = Float_stack; register_type = 6; spelling = "ST6"; register_number = 6; source_line = 119 };
    { register_kind = Float_stack; register_type = 6; spelling = "ST7"; register_number = 7; source_line = 120 };
    { register_kind = Mm; register_type = 7; spelling = "MM0"; register_number = 0; source_line = 122 };
    { register_kind = Mm; register_type = 7; spelling = "MM1"; register_number = 1; source_line = 123 };
    { register_kind = Mm; register_type = 7; spelling = "MM2"; register_number = 2; source_line = 124 };
    { register_kind = Mm; register_type = 7; spelling = "MM3"; register_number = 3; source_line = 125 };
    { register_kind = Mm; register_type = 7; spelling = "MM4"; register_number = 4; source_line = 126 };
    { register_kind = Mm; register_type = 7; spelling = "MM5"; register_number = 5; source_line = 127 };
    { register_kind = Mm; register_type = 7; spelling = "MM6"; register_number = 6; source_line = 128 };
    { register_kind = Mm; register_type = 7; spelling = "MM7"; register_number = 7; source_line = 129 };
    { register_kind = Xmm; register_type = 8; spelling = "XMM0"; register_number = 0; source_line = 131 };
    { register_kind = Xmm; register_type = 8; spelling = "XMM1"; register_number = 1; source_line = 132 };
    { register_kind = Xmm; register_type = 8; spelling = "XMM2"; register_number = 2; source_line = 133 };
    { register_kind = Xmm; register_type = 8; spelling = "XMM3"; register_number = 3; source_line = 134 };
    { register_kind = Xmm; register_type = 8; spelling = "XMM4"; register_number = 4; source_line = 135 };
    { register_kind = Xmm; register_type = 8; spelling = "XMM5"; register_number = 5; source_line = 136 };
    { register_kind = Xmm; register_type = 8; spelling = "XMM6"; register_number = 6; source_line = 137 };
    { register_kind = Xmm; register_type = 8; spelling = "XMM7"; register_number = 7; source_line = 138 };
  ]

let language =
  [
    { kind = Language; spelling = "include"; templeos_id = 0; source_line = 140 };
    { kind = Language; spelling = "define"; templeos_id = 1; source_line = 141 };
    { kind = Language; spelling = "union"; templeos_id = 2; source_line = 142 };
    { kind = Language; spelling = "catch"; templeos_id = 3; source_line = 143 };
    { kind = Language; spelling = "class"; templeos_id = 4; source_line = 144 };
    { kind = Language; spelling = "try"; templeos_id = 5; source_line = 145 };
    { kind = Language; spelling = "if"; templeos_id = 6; source_line = 146 };
    { kind = Language; spelling = "else"; templeos_id = 7; source_line = 147 };
    { kind = Language; spelling = "for"; templeos_id = 8; source_line = 148 };
    { kind = Language; spelling = "while"; templeos_id = 9; source_line = 149 };
    { kind = Language; spelling = "extern"; templeos_id = 10; source_line = 150 };
    { kind = Language; spelling = "_extern"; templeos_id = 11; source_line = 151 };
    { kind = Language; spelling = "return"; templeos_id = 12; source_line = 152 };
    { kind = Language; spelling = "sizeof"; templeos_id = 13; source_line = 153 };
    { kind = Language; spelling = "_intern"; templeos_id = 14; source_line = 154 };
    { kind = Language; spelling = "do"; templeos_id = 15; source_line = 155 };
    { kind = Language; spelling = "asm"; templeos_id = 16; source_line = 156 };
    { kind = Language; spelling = "goto"; templeos_id = 17; source_line = 157 };
    { kind = Language; spelling = "exe"; templeos_id = 18; source_line = 158 };
    { kind = Language; spelling = "break"; templeos_id = 19; source_line = 159 };
    { kind = Language; spelling = "switch"; templeos_id = 20; source_line = 160 };
    { kind = Language; spelling = "start"; templeos_id = 21; source_line = 161 };
    { kind = Language; spelling = "end"; templeos_id = 22; source_line = 162 };
    { kind = Language; spelling = "case"; templeos_id = 23; source_line = 163 };
    { kind = Language; spelling = "default"; templeos_id = 24; source_line = 164 };
    { kind = Language; spelling = "public"; templeos_id = 25; source_line = 165 };
    { kind = Language; spelling = "offset"; templeos_id = 26; source_line = 166 };
    { kind = Language; spelling = "import"; templeos_id = 27; source_line = 167 };
    { kind = Language; spelling = "_import"; templeos_id = 28; source_line = 168 };
    { kind = Language; spelling = "ifdef"; templeos_id = 29; source_line = 169 };
    { kind = Language; spelling = "ifndef"; templeos_id = 30; source_line = 170 };
    { kind = Language; spelling = "ifaot"; templeos_id = 31; source_line = 171 };
    { kind = Language; spelling = "ifjit"; templeos_id = 32; source_line = 172 };
    { kind = Language; spelling = "endif"; templeos_id = 33; source_line = 173 };
    { kind = Language; spelling = "assert"; templeos_id = 34; source_line = 174 };
    { kind = Language; spelling = "reg"; templeos_id = 35; source_line = 175 };
    { kind = Language; spelling = "noreg"; templeos_id = 36; source_line = 176 };
    { kind = Language; spelling = "lastclass"; templeos_id = 37; source_line = 177 };
    { kind = Language; spelling = "no_warn"; templeos_id = 38; source_line = 178 };
    { kind = Language; spelling = "help_index"; templeos_id = 39; source_line = 179 };
    { kind = Language; spelling = "help_file"; templeos_id = 40; source_line = 180 };
    { kind = Language; spelling = "static"; templeos_id = 41; source_line = 181 };
    { kind = Language; spelling = "lock"; templeos_id = 42; source_line = 182 };
    { kind = Language; spelling = "defined"; templeos_id = 43; source_line = 183 };
    { kind = Language; spelling = "interrupt"; templeos_id = 44; source_line = 184 };
    { kind = Language; spelling = "haserrcode"; templeos_id = 45; source_line = 185 };
    { kind = Language; spelling = "argpop"; templeos_id = 46; source_line = 186 };
    { kind = Language; spelling = "noargpop"; templeos_id = 47; source_line = 187 };
  ]

let assembly =
  [
    { kind = Assembly; spelling = "ALIGN"; templeos_id = 64; source_line = 189 };
    { kind = Assembly; spelling = "ORG"; templeos_id = 65; source_line = 190 };
    { kind = Assembly; spelling = "I0"; templeos_id = 66; source_line = 191 };
    { kind = Assembly; spelling = "I8"; templeos_id = 67; source_line = 192 };
    { kind = Assembly; spelling = "I16"; templeos_id = 68; source_line = 193 };
    { kind = Assembly; spelling = "I32"; templeos_id = 69; source_line = 194 };
    { kind = Assembly; spelling = "I64"; templeos_id = 70; source_line = 195 };
    { kind = Assembly; spelling = "U0"; templeos_id = 71; source_line = 196 };
    { kind = Assembly; spelling = "U8"; templeos_id = 72; source_line = 197 };
    { kind = Assembly; spelling = "U16"; templeos_id = 73; source_line = 198 };
    { kind = Assembly; spelling = "U32"; templeos_id = 74; source_line = 199 };
    { kind = Assembly; spelling = "U64"; templeos_id = 75; source_line = 200 };
    { kind = Assembly; spelling = "F64"; templeos_id = 76; source_line = 201 };
    { kind = Assembly; spelling = "DU8"; templeos_id = 77; source_line = 202 };
    { kind = Assembly; spelling = "DU16"; templeos_id = 78; source_line = 203 };
    { kind = Assembly; spelling = "DU32"; templeos_id = 79; source_line = 204 };
    { kind = Assembly; spelling = "DU64"; templeos_id = 80; source_line = 205 };
    { kind = Assembly; spelling = "DUP"; templeos_id = 81; source_line = 206 };
    { kind = Assembly; spelling = "USE16"; templeos_id = 82; source_line = 207 };
    { kind = Assembly; spelling = "USE32"; templeos_id = 83; source_line = 208 };
    { kind = Assembly; spelling = "USE64"; templeos_id = 84; source_line = 209 };
    { kind = Assembly; spelling = "IMPORT"; templeos_id = 85; source_line = 210 };
    { kind = Assembly; spelling = "LIST"; templeos_id = 86; source_line = 211 };
    { kind = Assembly; spelling = "NOLIST"; templeos_id = 87; source_line = 212 };
    { kind = Assembly; spelling = "BINFILE"; templeos_id = 88; source_line = 213 };
  ]

let opcodes =
  [
    { spelling = "PUSH";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0E]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 45; argument2 = 0; size1 = 0; size2 = 0; source_line = 216 };
        { entry_index = 1; opcode_bytes = [0x16]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 40; argument2 = 0; size1 = 0; size2 = 0; source_line = 217 };
        { entry_index = 2; opcode_bytes = [0x1E]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 41; argument2 = 0; size1 = 0; size2 = 0; source_line = 218 };
        { entry_index = 3; opcode_bytes = [0x06]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 42; argument2 = 0; size1 = 0; size2 = 0; source_line = 219 };
        { entry_index = 4; opcode_bytes = [0x0F; 0xA0]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 43; argument2 = 0; size1 = 0; size2 = 0; source_line = 220 };
        { entry_index = 5; opcode_bytes = [0x0F; 0xA8]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 44; argument2 = 0; size1 = 0; size2 = 0; source_line = 221 };
        { entry_index = 6; opcode_bytes = [0x6A]; flags = 0x010; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 5; argument1 = 4; argument2 = 0; size1 = 8; size2 = 0; source_line = 222 };
        { entry_index = 7; opcode_bytes = [0x68]; flags = 0x009; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 6; argument1 = 5; argument2 = 0; size1 = 16; size2 = 0; source_line = 223 };
        { entry_index = 8; opcode_bytes = [0x68]; flags = 0x00A; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 7; argument1 = 6; argument2 = 0; size1 = 32; size2 = 0; source_line = 224 };
        { entry_index = 9; opcode_bytes = [0x50]; flags = 0x025; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 13; argument2 = 0; size1 = 16; size2 = 0; source_line = 225 };
        { entry_index = 10; opcode_bytes = [0x50]; flags = 0x006; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 14; argument2 = 0; size1 = 32; size2 = 0; source_line = 226 };
        { entry_index = 11; opcode_bytes = [0x50]; flags = 0x086; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 15; argument2 = 0; size1 = 64; size2 = 0; source_line = 227 };
        { entry_index = 12; opcode_bytes = [0xFF]; flags = 0x021; slash_value = 6; uasm_slash_value = 6; opcode_modifier = 0; argument1 = 17; argument2 = 0; size1 = 16; size2 = 0; source_line = 228 };
        { entry_index = 13; opcode_bytes = [0xFF]; flags = 0x002; slash_value = 6; uasm_slash_value = 6; opcode_modifier = 0; argument1 = 18; argument2 = 0; size1 = 32; size2 = 0; source_line = 229 };
        { entry_index = 14; opcode_bytes = [0xFF]; flags = 0x002; slash_value = 6; uasm_slash_value = 6; opcode_modifier = 0; argument1 = 19; argument2 = 0; size1 = 64; size2 = 0; source_line = 230 };
        ];
      aliases =
        [
        ];
      source_line = 215 };
    { spelling = "PUSHAW";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x60]; flags = 0x001; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 231 };
        ];
      aliases =
        [
        ];
      source_line = 231 };
    { spelling = "PUSHAD";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x60]; flags = 0x002; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 232 };
        ];
      aliases =
        [
        ];
      source_line = 232 };
    { spelling = "PUSHFW";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x9C]; flags = 0x001; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 233 };
        ];
      aliases =
        [
        ];
      source_line = 233 };
    { spelling = "PUSHFD";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x9C]; flags = 0x002; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 234 };
        ];
      aliases =
        [
        ];
      source_line = 234 };
    { spelling = "POP";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x1F]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 41; argument2 = 0; size1 = 0; size2 = 0; source_line = 236 };
        { entry_index = 1; opcode_bytes = [0x07]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 42; argument2 = 0; size1 = 0; size2 = 0; source_line = 237 };
        { entry_index = 2; opcode_bytes = [0x17]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 40; argument2 = 0; size1 = 0; size2 = 0; source_line = 238 };
        { entry_index = 3; opcode_bytes = [0x0F; 0xA1]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 43; argument2 = 0; size1 = 0; size2 = 0; source_line = 239 };
        { entry_index = 4; opcode_bytes = [0x0F; 0xA9]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 44; argument2 = 0; size1 = 0; size2 = 0; source_line = 240 };
        { entry_index = 5; opcode_bytes = [0x58]; flags = 0x005; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 13; argument2 = 0; size1 = 16; size2 = 0; source_line = 241 };
        { entry_index = 6; opcode_bytes = [0x58]; flags = 0x006; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 14; argument2 = 0; size1 = 32; size2 = 0; source_line = 242 };
        { entry_index = 7; opcode_bytes = [0x58]; flags = 0x086; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 15; argument2 = 0; size1 = 64; size2 = 0; source_line = 243 };
        { entry_index = 8; opcode_bytes = [0x8F]; flags = 0x001; slash_value = 0; uasm_slash_value = 0; opcode_modifier = 0; argument1 = 17; argument2 = 0; size1 = 16; size2 = 0; source_line = 244 };
        { entry_index = 9; opcode_bytes = [0x8F]; flags = 0x002; slash_value = 0; uasm_slash_value = 0; opcode_modifier = 0; argument1 = 18; argument2 = 0; size1 = 32; size2 = 0; source_line = 245 };
        { entry_index = 10; opcode_bytes = [0x8F]; flags = 0x002; slash_value = 0; uasm_slash_value = 0; opcode_modifier = 0; argument1 = 19; argument2 = 0; size1 = 64; size2 = 0; source_line = 246 };
        ];
      aliases =
        [
        ];
      source_line = 235 };
    { spelling = "POPAW";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x61]; flags = 0x001; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 247 };
        ];
      aliases =
        [
        ];
      source_line = 247 };
    { spelling = "POPAD";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x61]; flags = 0x002; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 248 };
        ];
      aliases =
        [
        ];
      source_line = 248 };
    { spelling = "POPFW";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x9D]; flags = 0x001; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 249 };
        ];
      aliases =
        [
        ];
      source_line = 249 };
    { spelling = "POPFD";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x9D]; flags = 0x002; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 250 };
        ];
      aliases =
        [
        ];
      source_line = 250 };
    { spelling = "MOV";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xA1]; flags = 0x001; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 33; argument2 = 29; size1 = 16; size2 = 16; source_line = 253 };
        { entry_index = 1; opcode_bytes = [0xA1]; flags = 0x002; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 34; argument2 = 30; size1 = 32; size2 = 32; source_line = 254 };
        { entry_index = 2; opcode_bytes = [0xA3]; flags = 0x001; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 29; argument2 = 33; size1 = 16; size2 = 16; source_line = 256 };
        { entry_index = 3; opcode_bytes = [0xA3]; flags = 0x002; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 30; argument2 = 34; size1 = 32; size2 = 32; source_line = 257 };
        { entry_index = 4; opcode_bytes = [0x8A]; flags = 0x000; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 12; argument2 = 16; size1 = 8; size2 = 8; source_line = 258 };
        { entry_index = 5; opcode_bytes = [0x8B]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 13; argument2 = 17; size1 = 16; size2 = 16; source_line = 259 };
        { entry_index = 6; opcode_bytes = [0x8B]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 14; argument2 = 18; size1 = 32; size2 = 32; source_line = 260 };
        { entry_index = 7; opcode_bytes = [0x8B]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 15; argument2 = 19; size1 = 64; size2 = 64; source_line = 261 };
        { entry_index = 8; opcode_bytes = [0x88]; flags = 0x000; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 16; argument2 = 12; size1 = 8; size2 = 8; source_line = 262 };
        { entry_index = 9; opcode_bytes = [0x89]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 17; argument2 = 13; size1 = 16; size2 = 16; source_line = 263 };
        { entry_index = 10; opcode_bytes = [0x89]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 18; argument2 = 14; size1 = 32; size2 = 32; source_line = 264 };
        { entry_index = 11; opcode_bytes = [0x89]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 19; argument2 = 15; size1 = 64; size2 = 64; source_line = 265 };
        { entry_index = 12; opcode_bytes = [0x8C]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 17; argument2 = 39; size1 = 16; size2 = 0; source_line = 266 };
        { entry_index = 13; opcode_bytes = [0x8E]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 39; argument2 = 17; size1 = 0; size2 = 16; source_line = 267 };
        { entry_index = 14; opcode_bytes = [0xB0]; flags = 0x014; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 12; argument2 = 8; size1 = 8; size2 = 8; source_line = 268 };
        { entry_index = 15; opcode_bytes = [0xB0]; flags = 0x004; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 12; argument2 = 4; size1 = 8; size2 = 8; source_line = 269 };
        { entry_index = 16; opcode_bytes = [0xB8]; flags = 0x015; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 13; argument2 = 9; size1 = 16; size2 = 16; source_line = 270 };
        { entry_index = 17; opcode_bytes = [0xB8]; flags = 0x005; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 13; argument2 = 5; size1 = 16; size2 = 16; source_line = 271 };
        { entry_index = 18; opcode_bytes = [0xB8]; flags = 0x016; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 14; argument2 = 10; size1 = 32; size2 = 32; source_line = 272 };
        { entry_index = 19; opcode_bytes = [0xB8]; flags = 0x006; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 14; argument2 = 6; size1 = 32; size2 = 32; source_line = 273 };
        { entry_index = 20; opcode_bytes = [0xB8]; flags = 0x086; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 15; argument2 = 10; size1 = 64; size2 = 32; source_line = 274 };
        { entry_index = 21; opcode_bytes = [0xB8]; flags = 0x016; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 15; argument2 = 11; size1 = 64; size2 = 64; source_line = 275 };
        { entry_index = 22; opcode_bytes = [0xB8]; flags = 0x006; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 15; argument2 = 7; size1 = 64; size2 = 64; source_line = 276 };
        { entry_index = 23; opcode_bytes = [0xC6]; flags = 0x010; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 16; argument2 = 8; size1 = 8; size2 = 8; source_line = 277 };
        { entry_index = 24; opcode_bytes = [0xC6]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 16; argument2 = 4; size1 = 8; size2 = 8; source_line = 278 };
        { entry_index = 25; opcode_bytes = [0xC7]; flags = 0x011; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 17; argument2 = 9; size1 = 16; size2 = 16; source_line = 279 };
        { entry_index = 26; opcode_bytes = [0xC7]; flags = 0x001; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 17; argument2 = 5; size1 = 16; size2 = 16; source_line = 280 };
        { entry_index = 27; opcode_bytes = [0xC7]; flags = 0x012; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 18; argument2 = 10; size1 = 32; size2 = 32; source_line = 281 };
        { entry_index = 28; opcode_bytes = [0xC7]; flags = 0x002; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 18; argument2 = 6; size1 = 32; size2 = 32; source_line = 282 };
        { entry_index = 29; opcode_bytes = [0xC7]; flags = 0x082; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 19; argument2 = 10; size1 = 64; size2 = 32; source_line = 283 };
        { entry_index = 30; opcode_bytes = [0xC7]; flags = 0x002; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 19; argument2 = 6; size1 = 64; size2 = 32; source_line = 284 };
        ];
      aliases =
        [
        ];
      source_line = 251 };
    { spelling = "ADC";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x14]; flags = 0x010; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 5; argument1 = 32; argument2 = 8; size1 = 8; size2 = 8; source_line = 287 };
        { entry_index = 1; opcode_bytes = [0x14]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 5; argument1 = 32; argument2 = 4; size1 = 8; size2 = 8; source_line = 288 };
        { entry_index = 2; opcode_bytes = [0x15]; flags = 0x011; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 6; argument1 = 33; argument2 = 9; size1 = 16; size2 = 16; source_line = 289 };
        { entry_index = 3; opcode_bytes = [0x15]; flags = 0x001; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 6; argument1 = 33; argument2 = 5; size1 = 16; size2 = 16; source_line = 290 };
        { entry_index = 4; opcode_bytes = [0x15]; flags = 0x012; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 7; argument1 = 34; argument2 = 10; size1 = 32; size2 = 32; source_line = 291 };
        { entry_index = 5; opcode_bytes = [0x15]; flags = 0x002; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 7; argument1 = 34; argument2 = 6; size1 = 32; size2 = 32; source_line = 292 };
        { entry_index = 6; opcode_bytes = [0x80]; flags = 0x000; slash_value = 2; uasm_slash_value = 2; opcode_modifier = 5; argument1 = 16; argument2 = 4; size1 = 8; size2 = 8; source_line = 293 };
        { entry_index = 7; opcode_bytes = [0x83]; flags = 0x001; slash_value = 2; uasm_slash_value = 2; opcode_modifier = 5; argument1 = 17; argument2 = 4; size1 = 16; size2 = 8; source_line = 294 };
        { entry_index = 8; opcode_bytes = [0x83]; flags = 0x002; slash_value = 2; uasm_slash_value = 2; opcode_modifier = 5; argument1 = 18; argument2 = 4; size1 = 32; size2 = 8; source_line = 295 };
        { entry_index = 9; opcode_bytes = [0x83]; flags = 0x002; slash_value = 2; uasm_slash_value = 2; opcode_modifier = 5; argument1 = 19; argument2 = 4; size1 = 64; size2 = 8; source_line = 296 };
        { entry_index = 10; opcode_bytes = [0x81]; flags = 0x001; slash_value = 2; uasm_slash_value = 2; opcode_modifier = 6; argument1 = 17; argument2 = 5; size1 = 16; size2 = 16; source_line = 297 };
        { entry_index = 11; opcode_bytes = [0x81]; flags = 0x002; slash_value = 2; uasm_slash_value = 2; opcode_modifier = 7; argument1 = 18; argument2 = 6; size1 = 32; size2 = 32; source_line = 298 };
        { entry_index = 12; opcode_bytes = [0x81]; flags = 0x002; slash_value = 2; uasm_slash_value = 2; opcode_modifier = 7; argument1 = 19; argument2 = 6; size1 = 64; size2 = 32; source_line = 299 };
        { entry_index = 13; opcode_bytes = [0x12]; flags = 0x000; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 12; argument2 = 16; size1 = 8; size2 = 8; source_line = 300 };
        { entry_index = 14; opcode_bytes = [0x13]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 13; argument2 = 17; size1 = 16; size2 = 16; source_line = 301 };
        { entry_index = 15; opcode_bytes = [0x13]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 14; argument2 = 18; size1 = 32; size2 = 32; source_line = 302 };
        { entry_index = 16; opcode_bytes = [0x13]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 15; argument2 = 19; size1 = 64; size2 = 64; source_line = 303 };
        { entry_index = 17; opcode_bytes = [0x10]; flags = 0x000; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 16; argument2 = 12; size1 = 8; size2 = 8; source_line = 304 };
        { entry_index = 18; opcode_bytes = [0x11]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 17; argument2 = 13; size1 = 16; size2 = 16; source_line = 305 };
        { entry_index = 19; opcode_bytes = [0x11]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 18; argument2 = 14; size1 = 32; size2 = 32; source_line = 306 };
        { entry_index = 20; opcode_bytes = [0x11]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 19; argument2 = 15; size1 = 64; size2 = 64; source_line = 307 };
        ];
      aliases =
        [
        ];
      source_line = 286 };
    { spelling = "ADD";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x04]; flags = 0x010; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 5; argument1 = 32; argument2 = 8; size1 = 8; size2 = 8; source_line = 309 };
        { entry_index = 1; opcode_bytes = [0x04]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 5; argument1 = 32; argument2 = 4; size1 = 8; size2 = 8; source_line = 310 };
        { entry_index = 2; opcode_bytes = [0x05]; flags = 0x011; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 6; argument1 = 33; argument2 = 9; size1 = 16; size2 = 16; source_line = 311 };
        { entry_index = 3; opcode_bytes = [0x05]; flags = 0x001; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 6; argument1 = 33; argument2 = 5; size1 = 16; size2 = 16; source_line = 312 };
        { entry_index = 4; opcode_bytes = [0x05]; flags = 0x012; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 7; argument1 = 34; argument2 = 10; size1 = 32; size2 = 32; source_line = 313 };
        { entry_index = 5; opcode_bytes = [0x05]; flags = 0x002; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 7; argument1 = 34; argument2 = 6; size1 = 32; size2 = 32; source_line = 314 };
        { entry_index = 6; opcode_bytes = [0x80]; flags = 0x010; slash_value = 0; uasm_slash_value = 0; opcode_modifier = 5; argument1 = 16; argument2 = 8; size1 = 8; size2 = 8; source_line = 315 };
        { entry_index = 7; opcode_bytes = [0x80]; flags = 0x000; slash_value = 0; uasm_slash_value = 0; opcode_modifier = 5; argument1 = 16; argument2 = 4; size1 = 8; size2 = 8; source_line = 316 };
        { entry_index = 8; opcode_bytes = [0x83]; flags = 0x001; slash_value = 0; uasm_slash_value = 0; opcode_modifier = 5; argument1 = 17; argument2 = 4; size1 = 16; size2 = 8; source_line = 317 };
        { entry_index = 9; opcode_bytes = [0x83]; flags = 0x002; slash_value = 0; uasm_slash_value = 0; opcode_modifier = 5; argument1 = 18; argument2 = 4; size1 = 32; size2 = 8; source_line = 318 };
        { entry_index = 10; opcode_bytes = [0x83]; flags = 0x002; slash_value = 0; uasm_slash_value = 0; opcode_modifier = 5; argument1 = 19; argument2 = 4; size1 = 64; size2 = 8; source_line = 319 };
        { entry_index = 11; opcode_bytes = [0x81]; flags = 0x001; slash_value = 0; uasm_slash_value = 0; opcode_modifier = 6; argument1 = 17; argument2 = 5; size1 = 16; size2 = 16; source_line = 320 };
        { entry_index = 12; opcode_bytes = [0x81]; flags = 0x002; slash_value = 0; uasm_slash_value = 0; opcode_modifier = 7; argument1 = 18; argument2 = 6; size1 = 32; size2 = 32; source_line = 321 };
        { entry_index = 13; opcode_bytes = [0x81]; flags = 0x002; slash_value = 0; uasm_slash_value = 0; opcode_modifier = 7; argument1 = 19; argument2 = 6; size1 = 64; size2 = 32; source_line = 322 };
        { entry_index = 14; opcode_bytes = [0x02]; flags = 0x000; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 12; argument2 = 16; size1 = 8; size2 = 8; source_line = 323 };
        { entry_index = 15; opcode_bytes = [0x03]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 13; argument2 = 17; size1 = 16; size2 = 16; source_line = 324 };
        { entry_index = 16; opcode_bytes = [0x03]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 14; argument2 = 18; size1 = 32; size2 = 32; source_line = 325 };
        { entry_index = 17; opcode_bytes = [0x03]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 15; argument2 = 19; size1 = 64; size2 = 64; source_line = 326 };
        { entry_index = 18; opcode_bytes = [0x00]; flags = 0x000; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 16; argument2 = 12; size1 = 8; size2 = 8; source_line = 327 };
        { entry_index = 19; opcode_bytes = [0x01]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 17; argument2 = 13; size1 = 16; size2 = 16; source_line = 328 };
        { entry_index = 20; opcode_bytes = [0x01]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 18; argument2 = 14; size1 = 32; size2 = 32; source_line = 329 };
        { entry_index = 21; opcode_bytes = [0x01]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 19; argument2 = 15; size1 = 64; size2 = 64; source_line = 330 };
        ];
      aliases =
        [
        ];
      source_line = 308 };
    { spelling = "AND";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x24]; flags = 0x010; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 5; argument1 = 32; argument2 = 8; size1 = 8; size2 = 8; source_line = 332 };
        { entry_index = 1; opcode_bytes = [0x24]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 5; argument1 = 32; argument2 = 4; size1 = 8; size2 = 8; source_line = 333 };
        { entry_index = 2; opcode_bytes = [0x25]; flags = 0x011; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 6; argument1 = 33; argument2 = 9; size1 = 16; size2 = 16; source_line = 334 };
        { entry_index = 3; opcode_bytes = [0x25]; flags = 0x001; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 6; argument1 = 33; argument2 = 5; size1 = 16; size2 = 16; source_line = 335 };
        { entry_index = 4; opcode_bytes = [0x25]; flags = 0x012; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 7; argument1 = 34; argument2 = 10; size1 = 32; size2 = 32; source_line = 336 };
        { entry_index = 5; opcode_bytes = [0x25]; flags = 0x002; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 7; argument1 = 34; argument2 = 6; size1 = 32; size2 = 32; source_line = 337 };
        { entry_index = 6; opcode_bytes = [0x80]; flags = 0x010; slash_value = 4; uasm_slash_value = 4; opcode_modifier = 5; argument1 = 16; argument2 = 8; size1 = 8; size2 = 8; source_line = 338 };
        { entry_index = 7; opcode_bytes = [0x80]; flags = 0x000; slash_value = 4; uasm_slash_value = 4; opcode_modifier = 5; argument1 = 16; argument2 = 4; size1 = 8; size2 = 8; source_line = 339 };
        { entry_index = 8; opcode_bytes = [0x83]; flags = 0x001; slash_value = 4; uasm_slash_value = 4; opcode_modifier = 5; argument1 = 17; argument2 = 4; size1 = 16; size2 = 8; source_line = 340 };
        { entry_index = 9; opcode_bytes = [0x83]; flags = 0x002; slash_value = 4; uasm_slash_value = 4; opcode_modifier = 5; argument1 = 18; argument2 = 4; size1 = 32; size2 = 8; source_line = 341 };
        { entry_index = 10; opcode_bytes = [0x83]; flags = 0x002; slash_value = 4; uasm_slash_value = 4; opcode_modifier = 5; argument1 = 19; argument2 = 4; size1 = 64; size2 = 8; source_line = 342 };
        { entry_index = 11; opcode_bytes = [0x81]; flags = 0x001; slash_value = 4; uasm_slash_value = 4; opcode_modifier = 6; argument1 = 17; argument2 = 5; size1 = 16; size2 = 16; source_line = 343 };
        { entry_index = 12; opcode_bytes = [0x81]; flags = 0x002; slash_value = 4; uasm_slash_value = 4; opcode_modifier = 7; argument1 = 18; argument2 = 6; size1 = 32; size2 = 32; source_line = 344 };
        { entry_index = 13; opcode_bytes = [0x81]; flags = 0x002; slash_value = 4; uasm_slash_value = 4; opcode_modifier = 7; argument1 = 19; argument2 = 6; size1 = 64; size2 = 32; source_line = 345 };
        { entry_index = 14; opcode_bytes = [0x22]; flags = 0x000; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 12; argument2 = 16; size1 = 8; size2 = 8; source_line = 346 };
        { entry_index = 15; opcode_bytes = [0x23]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 13; argument2 = 17; size1 = 16; size2 = 16; source_line = 347 };
        { entry_index = 16; opcode_bytes = [0x23]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 14; argument2 = 18; size1 = 32; size2 = 32; source_line = 348 };
        { entry_index = 17; opcode_bytes = [0x23]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 15; argument2 = 19; size1 = 64; size2 = 64; source_line = 349 };
        { entry_index = 18; opcode_bytes = [0x20]; flags = 0x000; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 16; argument2 = 12; size1 = 8; size2 = 8; source_line = 350 };
        { entry_index = 19; opcode_bytes = [0x21]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 17; argument2 = 13; size1 = 16; size2 = 16; source_line = 351 };
        { entry_index = 20; opcode_bytes = [0x21]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 18; argument2 = 14; size1 = 32; size2 = 32; source_line = 352 };
        { entry_index = 21; opcode_bytes = [0x21]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 19; argument2 = 15; size1 = 64; size2 = 64; source_line = 353 };
        ];
      aliases =
        [
        ];
      source_line = 331 };
    { spelling = "CMP";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x3C]; flags = 0x010; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 5; argument1 = 32; argument2 = 8; size1 = 8; size2 = 8; source_line = 355 };
        { entry_index = 1; opcode_bytes = [0x3C]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 5; argument1 = 32; argument2 = 4; size1 = 8; size2 = 8; source_line = 356 };
        { entry_index = 2; opcode_bytes = [0x3D]; flags = 0x011; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 6; argument1 = 33; argument2 = 9; size1 = 16; size2 = 16; source_line = 357 };
        { entry_index = 3; opcode_bytes = [0x3D]; flags = 0x001; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 6; argument1 = 33; argument2 = 5; size1 = 16; size2 = 16; source_line = 358 };
        { entry_index = 4; opcode_bytes = [0x3D]; flags = 0x012; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 7; argument1 = 34; argument2 = 10; size1 = 32; size2 = 32; source_line = 359 };
        { entry_index = 5; opcode_bytes = [0x3D]; flags = 0x002; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 7; argument1 = 34; argument2 = 6; size1 = 32; size2 = 32; source_line = 360 };
        { entry_index = 6; opcode_bytes = [0x80]; flags = 0x010; slash_value = 7; uasm_slash_value = 7; opcode_modifier = 5; argument1 = 16; argument2 = 8; size1 = 8; size2 = 8; source_line = 361 };
        { entry_index = 7; opcode_bytes = [0x80]; flags = 0x000; slash_value = 7; uasm_slash_value = 7; opcode_modifier = 5; argument1 = 16; argument2 = 4; size1 = 8; size2 = 8; source_line = 362 };
        { entry_index = 8; opcode_bytes = [0x83]; flags = 0x001; slash_value = 7; uasm_slash_value = 7; opcode_modifier = 5; argument1 = 17; argument2 = 4; size1 = 16; size2 = 8; source_line = 363 };
        { entry_index = 9; opcode_bytes = [0x83]; flags = 0x002; slash_value = 7; uasm_slash_value = 7; opcode_modifier = 5; argument1 = 18; argument2 = 4; size1 = 32; size2 = 8; source_line = 364 };
        { entry_index = 10; opcode_bytes = [0x83]; flags = 0x002; slash_value = 7; uasm_slash_value = 7; opcode_modifier = 5; argument1 = 19; argument2 = 4; size1 = 64; size2 = 8; source_line = 365 };
        { entry_index = 11; opcode_bytes = [0x81]; flags = 0x001; slash_value = 7; uasm_slash_value = 7; opcode_modifier = 6; argument1 = 17; argument2 = 5; size1 = 16; size2 = 16; source_line = 366 };
        { entry_index = 12; opcode_bytes = [0x81]; flags = 0x002; slash_value = 7; uasm_slash_value = 7; opcode_modifier = 7; argument1 = 18; argument2 = 6; size1 = 32; size2 = 32; source_line = 367 };
        { entry_index = 13; opcode_bytes = [0x81]; flags = 0x002; slash_value = 7; uasm_slash_value = 7; opcode_modifier = 7; argument1 = 19; argument2 = 6; size1 = 64; size2 = 32; source_line = 368 };
        { entry_index = 14; opcode_bytes = [0x3A]; flags = 0x000; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 12; argument2 = 16; size1 = 8; size2 = 8; source_line = 369 };
        { entry_index = 15; opcode_bytes = [0x3B]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 13; argument2 = 17; size1 = 16; size2 = 16; source_line = 370 };
        { entry_index = 16; opcode_bytes = [0x3B]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 14; argument2 = 18; size1 = 32; size2 = 32; source_line = 371 };
        { entry_index = 17; opcode_bytes = [0x3B]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 15; argument2 = 19; size1 = 64; size2 = 64; source_line = 372 };
        { entry_index = 18; opcode_bytes = [0x38]; flags = 0x000; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 16; argument2 = 12; size1 = 8; size2 = 8; source_line = 373 };
        { entry_index = 19; opcode_bytes = [0x39]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 17; argument2 = 13; size1 = 16; size2 = 16; source_line = 374 };
        { entry_index = 20; opcode_bytes = [0x39]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 18; argument2 = 14; size1 = 32; size2 = 32; source_line = 375 };
        { entry_index = 21; opcode_bytes = [0x39]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 19; argument2 = 15; size1 = 64; size2 = 64; source_line = 376 };
        ];
      aliases =
        [
        ];
      source_line = 354 };
    { spelling = "OR";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0C]; flags = 0x010; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 5; argument1 = 32; argument2 = 8; size1 = 8; size2 = 8; source_line = 378 };
        { entry_index = 1; opcode_bytes = [0x0C]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 5; argument1 = 32; argument2 = 4; size1 = 8; size2 = 8; source_line = 379 };
        { entry_index = 2; opcode_bytes = [0x0D]; flags = 0x011; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 6; argument1 = 33; argument2 = 9; size1 = 16; size2 = 16; source_line = 380 };
        { entry_index = 3; opcode_bytes = [0x0D]; flags = 0x001; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 6; argument1 = 33; argument2 = 5; size1 = 16; size2 = 16; source_line = 381 };
        { entry_index = 4; opcode_bytes = [0x0D]; flags = 0x012; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 7; argument1 = 34; argument2 = 10; size1 = 32; size2 = 32; source_line = 382 };
        { entry_index = 5; opcode_bytes = [0x0D]; flags = 0x002; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 7; argument1 = 34; argument2 = 6; size1 = 32; size2 = 32; source_line = 383 };
        { entry_index = 6; opcode_bytes = [0x80]; flags = 0x010; slash_value = 1; uasm_slash_value = 1; opcode_modifier = 5; argument1 = 16; argument2 = 8; size1 = 8; size2 = 8; source_line = 384 };
        { entry_index = 7; opcode_bytes = [0x80]; flags = 0x000; slash_value = 1; uasm_slash_value = 1; opcode_modifier = 5; argument1 = 16; argument2 = 4; size1 = 8; size2 = 8; source_line = 385 };
        { entry_index = 8; opcode_bytes = [0x83]; flags = 0x001; slash_value = 1; uasm_slash_value = 1; opcode_modifier = 5; argument1 = 17; argument2 = 4; size1 = 16; size2 = 8; source_line = 386 };
        { entry_index = 9; opcode_bytes = [0x83]; flags = 0x002; slash_value = 1; uasm_slash_value = 1; opcode_modifier = 5; argument1 = 18; argument2 = 4; size1 = 32; size2 = 8; source_line = 387 };
        { entry_index = 10; opcode_bytes = [0x83]; flags = 0x002; slash_value = 1; uasm_slash_value = 1; opcode_modifier = 5; argument1 = 19; argument2 = 4; size1 = 64; size2 = 8; source_line = 388 };
        { entry_index = 11; opcode_bytes = [0x81]; flags = 0x001; slash_value = 1; uasm_slash_value = 1; opcode_modifier = 6; argument1 = 17; argument2 = 5; size1 = 16; size2 = 16; source_line = 389 };
        { entry_index = 12; opcode_bytes = [0x81]; flags = 0x002; slash_value = 1; uasm_slash_value = 1; opcode_modifier = 7; argument1 = 18; argument2 = 6; size1 = 32; size2 = 32; source_line = 390 };
        { entry_index = 13; opcode_bytes = [0x81]; flags = 0x002; slash_value = 1; uasm_slash_value = 1; opcode_modifier = 7; argument1 = 19; argument2 = 6; size1 = 64; size2 = 32; source_line = 391 };
        { entry_index = 14; opcode_bytes = [0x0A]; flags = 0x000; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 12; argument2 = 16; size1 = 8; size2 = 8; source_line = 392 };
        { entry_index = 15; opcode_bytes = [0x0B]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 13; argument2 = 17; size1 = 16; size2 = 16; source_line = 393 };
        { entry_index = 16; opcode_bytes = [0x0B]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 14; argument2 = 18; size1 = 32; size2 = 32; source_line = 394 };
        { entry_index = 17; opcode_bytes = [0x0B]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 15; argument2 = 19; size1 = 64; size2 = 64; source_line = 395 };
        { entry_index = 18; opcode_bytes = [0x08]; flags = 0x000; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 16; argument2 = 12; size1 = 8; size2 = 8; source_line = 396 };
        { entry_index = 19; opcode_bytes = [0x09]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 17; argument2 = 13; size1 = 16; size2 = 16; source_line = 397 };
        { entry_index = 20; opcode_bytes = [0x09]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 18; argument2 = 14; size1 = 32; size2 = 32; source_line = 398 };
        { entry_index = 21; opcode_bytes = [0x09]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 19; argument2 = 15; size1 = 64; size2 = 64; source_line = 399 };
        ];
      aliases =
        [
        ];
      source_line = 377 };
    { spelling = "SBB";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x1C]; flags = 0x010; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 5; argument1 = 32; argument2 = 8; size1 = 8; size2 = 8; source_line = 401 };
        { entry_index = 1; opcode_bytes = [0x1C]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 5; argument1 = 32; argument2 = 4; size1 = 8; size2 = 8; source_line = 402 };
        { entry_index = 2; opcode_bytes = [0x1D]; flags = 0x011; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 6; argument1 = 33; argument2 = 9; size1 = 16; size2 = 16; source_line = 403 };
        { entry_index = 3; opcode_bytes = [0x1D]; flags = 0x001; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 6; argument1 = 33; argument2 = 5; size1 = 16; size2 = 16; source_line = 404 };
        { entry_index = 4; opcode_bytes = [0x1D]; flags = 0x012; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 7; argument1 = 34; argument2 = 10; size1 = 32; size2 = 32; source_line = 405 };
        { entry_index = 5; opcode_bytes = [0x1D]; flags = 0x002; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 7; argument1 = 34; argument2 = 6; size1 = 32; size2 = 32; source_line = 406 };
        { entry_index = 6; opcode_bytes = [0x80]; flags = 0x010; slash_value = 3; uasm_slash_value = 3; opcode_modifier = 5; argument1 = 16; argument2 = 8; size1 = 8; size2 = 8; source_line = 407 };
        { entry_index = 7; opcode_bytes = [0x80]; flags = 0x000; slash_value = 3; uasm_slash_value = 3; opcode_modifier = 5; argument1 = 16; argument2 = 4; size1 = 8; size2 = 8; source_line = 408 };
        { entry_index = 8; opcode_bytes = [0x83]; flags = 0x001; slash_value = 3; uasm_slash_value = 3; opcode_modifier = 5; argument1 = 17; argument2 = 4; size1 = 16; size2 = 8; source_line = 409 };
        { entry_index = 9; opcode_bytes = [0x83]; flags = 0x002; slash_value = 3; uasm_slash_value = 3; opcode_modifier = 5; argument1 = 18; argument2 = 4; size1 = 32; size2 = 8; source_line = 410 };
        { entry_index = 10; opcode_bytes = [0x83]; flags = 0x002; slash_value = 3; uasm_slash_value = 3; opcode_modifier = 5; argument1 = 19; argument2 = 4; size1 = 64; size2 = 8; source_line = 411 };
        { entry_index = 11; opcode_bytes = [0x81]; flags = 0x001; slash_value = 3; uasm_slash_value = 3; opcode_modifier = 6; argument1 = 17; argument2 = 5; size1 = 16; size2 = 16; source_line = 412 };
        { entry_index = 12; opcode_bytes = [0x81]; flags = 0x002; slash_value = 3; uasm_slash_value = 3; opcode_modifier = 7; argument1 = 18; argument2 = 6; size1 = 32; size2 = 32; source_line = 413 };
        { entry_index = 13; opcode_bytes = [0x81]; flags = 0x002; slash_value = 3; uasm_slash_value = 3; opcode_modifier = 7; argument1 = 19; argument2 = 6; size1 = 64; size2 = 32; source_line = 414 };
        { entry_index = 14; opcode_bytes = [0x1A]; flags = 0x000; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 12; argument2 = 16; size1 = 8; size2 = 8; source_line = 415 };
        { entry_index = 15; opcode_bytes = [0x1B]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 13; argument2 = 17; size1 = 16; size2 = 16; source_line = 416 };
        { entry_index = 16; opcode_bytes = [0x1B]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 14; argument2 = 18; size1 = 32; size2 = 32; source_line = 417 };
        { entry_index = 17; opcode_bytes = [0x1B]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 15; argument2 = 19; size1 = 64; size2 = 64; source_line = 418 };
        { entry_index = 18; opcode_bytes = [0x18]; flags = 0x000; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 16; argument2 = 12; size1 = 8; size2 = 8; source_line = 419 };
        { entry_index = 19; opcode_bytes = [0x19]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 17; argument2 = 13; size1 = 16; size2 = 16; source_line = 420 };
        { entry_index = 20; opcode_bytes = [0x19]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 18; argument2 = 14; size1 = 32; size2 = 32; source_line = 421 };
        { entry_index = 21; opcode_bytes = [0x19]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 19; argument2 = 15; size1 = 64; size2 = 64; source_line = 422 };
        ];
      aliases =
        [
        ];
      source_line = 400 };
    { spelling = "SUB";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x2C]; flags = 0x010; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 5; argument1 = 32; argument2 = 8; size1 = 8; size2 = 8; source_line = 424 };
        { entry_index = 1; opcode_bytes = [0x2C]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 5; argument1 = 32; argument2 = 4; size1 = 8; size2 = 8; source_line = 425 };
        { entry_index = 2; opcode_bytes = [0x2D]; flags = 0x011; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 6; argument1 = 33; argument2 = 9; size1 = 16; size2 = 16; source_line = 426 };
        { entry_index = 3; opcode_bytes = [0x2D]; flags = 0x001; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 6; argument1 = 33; argument2 = 5; size1 = 16; size2 = 16; source_line = 427 };
        { entry_index = 4; opcode_bytes = [0x2D]; flags = 0x012; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 7; argument1 = 34; argument2 = 10; size1 = 32; size2 = 32; source_line = 428 };
        { entry_index = 5; opcode_bytes = [0x2D]; flags = 0x002; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 7; argument1 = 34; argument2 = 6; size1 = 32; size2 = 32; source_line = 429 };
        { entry_index = 6; opcode_bytes = [0x80]; flags = 0x010; slash_value = 5; uasm_slash_value = 5; opcode_modifier = 5; argument1 = 16; argument2 = 8; size1 = 8; size2 = 8; source_line = 430 };
        { entry_index = 7; opcode_bytes = [0x80]; flags = 0x000; slash_value = 5; uasm_slash_value = 5; opcode_modifier = 5; argument1 = 16; argument2 = 4; size1 = 8; size2 = 8; source_line = 431 };
        { entry_index = 8; opcode_bytes = [0x83]; flags = 0x001; slash_value = 5; uasm_slash_value = 5; opcode_modifier = 5; argument1 = 17; argument2 = 4; size1 = 16; size2 = 8; source_line = 432 };
        { entry_index = 9; opcode_bytes = [0x83]; flags = 0x002; slash_value = 5; uasm_slash_value = 5; opcode_modifier = 5; argument1 = 18; argument2 = 4; size1 = 32; size2 = 8; source_line = 433 };
        { entry_index = 10; opcode_bytes = [0x83]; flags = 0x002; slash_value = 5; uasm_slash_value = 5; opcode_modifier = 5; argument1 = 19; argument2 = 4; size1 = 64; size2 = 8; source_line = 434 };
        { entry_index = 11; opcode_bytes = [0x81]; flags = 0x001; slash_value = 5; uasm_slash_value = 5; opcode_modifier = 6; argument1 = 17; argument2 = 5; size1 = 16; size2 = 16; source_line = 435 };
        { entry_index = 12; opcode_bytes = [0x81]; flags = 0x002; slash_value = 5; uasm_slash_value = 5; opcode_modifier = 7; argument1 = 18; argument2 = 6; size1 = 32; size2 = 32; source_line = 436 };
        { entry_index = 13; opcode_bytes = [0x81]; flags = 0x002; slash_value = 5; uasm_slash_value = 5; opcode_modifier = 7; argument1 = 19; argument2 = 6; size1 = 64; size2 = 32; source_line = 437 };
        { entry_index = 14; opcode_bytes = [0x2A]; flags = 0x000; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 12; argument2 = 16; size1 = 8; size2 = 8; source_line = 438 };
        { entry_index = 15; opcode_bytes = [0x2B]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 13; argument2 = 17; size1 = 16; size2 = 16; source_line = 439 };
        { entry_index = 16; opcode_bytes = [0x2B]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 14; argument2 = 18; size1 = 32; size2 = 32; source_line = 440 };
        { entry_index = 17; opcode_bytes = [0x2B]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 15; argument2 = 19; size1 = 64; size2 = 64; source_line = 441 };
        { entry_index = 18; opcode_bytes = [0x28]; flags = 0x000; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 16; argument2 = 12; size1 = 8; size2 = 8; source_line = 442 };
        { entry_index = 19; opcode_bytes = [0x29]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 17; argument2 = 13; size1 = 16; size2 = 16; source_line = 443 };
        { entry_index = 20; opcode_bytes = [0x29]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 18; argument2 = 14; size1 = 32; size2 = 32; source_line = 444 };
        { entry_index = 21; opcode_bytes = [0x29]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 19; argument2 = 15; size1 = 64; size2 = 64; source_line = 445 };
        ];
      aliases =
        [
        ];
      source_line = 423 };
    { spelling = "TEST";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xA8]; flags = 0x010; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 5; argument1 = 32; argument2 = 8; size1 = 8; size2 = 8; source_line = 447 };
        { entry_index = 1; opcode_bytes = [0xA8]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 5; argument1 = 32; argument2 = 4; size1 = 8; size2 = 8; source_line = 448 };
        { entry_index = 2; opcode_bytes = [0xA9]; flags = 0x011; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 6; argument1 = 33; argument2 = 9; size1 = 16; size2 = 16; source_line = 449 };
        { entry_index = 3; opcode_bytes = [0xA9]; flags = 0x001; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 6; argument1 = 33; argument2 = 5; size1 = 16; size2 = 16; source_line = 450 };
        { entry_index = 4; opcode_bytes = [0xA9]; flags = 0x012; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 7; argument1 = 34; argument2 = 10; size1 = 32; size2 = 32; source_line = 451 };
        { entry_index = 5; opcode_bytes = [0xA9]; flags = 0x002; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 7; argument1 = 34; argument2 = 6; size1 = 32; size2 = 32; source_line = 452 };
        { entry_index = 6; opcode_bytes = [0xF6]; flags = 0x010; slash_value = 0; uasm_slash_value = 0; opcode_modifier = 5; argument1 = 16; argument2 = 8; size1 = 8; size2 = 8; source_line = 453 };
        { entry_index = 7; opcode_bytes = [0xF6]; flags = 0x000; slash_value = 0; uasm_slash_value = 0; opcode_modifier = 5; argument1 = 16; argument2 = 4; size1 = 8; size2 = 8; source_line = 454 };
        { entry_index = 8; opcode_bytes = [0xF7]; flags = 0x001; slash_value = 0; uasm_slash_value = 0; opcode_modifier = 6; argument1 = 17; argument2 = 5; size1 = 16; size2 = 16; source_line = 455 };
        { entry_index = 9; opcode_bytes = [0xF7]; flags = 0x002; slash_value = 0; uasm_slash_value = 0; opcode_modifier = 7; argument1 = 18; argument2 = 6; size1 = 32; size2 = 32; source_line = 456 };
        { entry_index = 10; opcode_bytes = [0xF7]; flags = 0x002; slash_value = 0; uasm_slash_value = 0; opcode_modifier = 7; argument1 = 19; argument2 = 6; size1 = 64; size2 = 32; source_line = 457 };
        { entry_index = 11; opcode_bytes = [0x84]; flags = 0x000; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 16; argument2 = 12; size1 = 8; size2 = 8; source_line = 458 };
        { entry_index = 12; opcode_bytes = [0x85]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 17; argument2 = 13; size1 = 16; size2 = 16; source_line = 459 };
        { entry_index = 13; opcode_bytes = [0x85]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 18; argument2 = 14; size1 = 32; size2 = 32; source_line = 460 };
        { entry_index = 14; opcode_bytes = [0x85]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 19; argument2 = 15; size1 = 64; size2 = 64; source_line = 461 };
        ];
      aliases =
        [
        ];
      source_line = 446 };
    { spelling = "NOP";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x90]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 462 };
        ];
      aliases =
        [
        ];
      source_line = 462 };
    { spelling = "NOP2";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x66; 0x90]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 463 };
        ];
      aliases =
        [
        ];
      source_line = 463 };
    { spelling = "XCHG";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x90]; flags = 0x005; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 13; argument2 = 33; size1 = 16; size2 = 16; source_line = 465 };
        { entry_index = 1; opcode_bytes = [0x90]; flags = 0x005; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 33; argument2 = 13; size1 = 16; size2 = 16; source_line = 466 };
        { entry_index = 2; opcode_bytes = [0x90]; flags = 0x006; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 14; argument2 = 34; size1 = 32; size2 = 32; source_line = 467 };
        { entry_index = 3; opcode_bytes = [0x90]; flags = 0x006; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 34; argument2 = 14; size1 = 32; size2 = 32; source_line = 468 };
        { entry_index = 4; opcode_bytes = [0x90]; flags = 0x006; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 15; argument2 = 35; size1 = 64; size2 = 64; source_line = 469 };
        { entry_index = 5; opcode_bytes = [0x90]; flags = 0x006; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 35; argument2 = 15; size1 = 64; size2 = 64; source_line = 470 };
        { entry_index = 6; opcode_bytes = [0x86]; flags = 0x000; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 12; argument2 = 16; size1 = 8; size2 = 8; source_line = 471 };
        { entry_index = 7; opcode_bytes = [0x87]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 13; argument2 = 17; size1 = 16; size2 = 16; source_line = 472 };
        { entry_index = 8; opcode_bytes = [0x87]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 14; argument2 = 18; size1 = 32; size2 = 32; source_line = 473 };
        { entry_index = 9; opcode_bytes = [0x87]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 15; argument2 = 19; size1 = 64; size2 = 64; source_line = 474 };
        { entry_index = 10; opcode_bytes = [0x86]; flags = 0x000; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 16; argument2 = 12; size1 = 8; size2 = 8; source_line = 475 };
        { entry_index = 11; opcode_bytes = [0x87]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 17; argument2 = 13; size1 = 16; size2 = 16; source_line = 476 };
        { entry_index = 12; opcode_bytes = [0x87]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 18; argument2 = 14; size1 = 32; size2 = 32; source_line = 477 };
        { entry_index = 13; opcode_bytes = [0x87]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 19; argument2 = 15; size1 = 64; size2 = 64; source_line = 478 };
        ];
      aliases =
        [
        ];
      source_line = 464 };
    { spelling = "XOR";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x34]; flags = 0x010; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 5; argument1 = 32; argument2 = 8; size1 = 8; size2 = 8; source_line = 480 };
        { entry_index = 1; opcode_bytes = [0x34]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 5; argument1 = 32; argument2 = 4; size1 = 8; size2 = 8; source_line = 481 };
        { entry_index = 2; opcode_bytes = [0x35]; flags = 0x011; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 6; argument1 = 33; argument2 = 9; size1 = 16; size2 = 16; source_line = 482 };
        { entry_index = 3; opcode_bytes = [0x35]; flags = 0x001; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 6; argument1 = 33; argument2 = 5; size1 = 16; size2 = 16; source_line = 483 };
        { entry_index = 4; opcode_bytes = [0x35]; flags = 0x012; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 7; argument1 = 34; argument2 = 10; size1 = 32; size2 = 32; source_line = 484 };
        { entry_index = 5; opcode_bytes = [0x35]; flags = 0x002; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 7; argument1 = 34; argument2 = 6; size1 = 32; size2 = 32; source_line = 485 };
        { entry_index = 6; opcode_bytes = [0x80]; flags = 0x010; slash_value = 6; uasm_slash_value = 6; opcode_modifier = 5; argument1 = 16; argument2 = 8; size1 = 8; size2 = 8; source_line = 486 };
        { entry_index = 7; opcode_bytes = [0x80]; flags = 0x000; slash_value = 6; uasm_slash_value = 6; opcode_modifier = 5; argument1 = 16; argument2 = 4; size1 = 8; size2 = 8; source_line = 487 };
        { entry_index = 8; opcode_bytes = [0x83]; flags = 0x001; slash_value = 6; uasm_slash_value = 6; opcode_modifier = 5; argument1 = 17; argument2 = 4; size1 = 16; size2 = 8; source_line = 488 };
        { entry_index = 9; opcode_bytes = [0x83]; flags = 0x002; slash_value = 6; uasm_slash_value = 6; opcode_modifier = 5; argument1 = 18; argument2 = 4; size1 = 32; size2 = 8; source_line = 489 };
        { entry_index = 10; opcode_bytes = [0x83]; flags = 0x002; slash_value = 6; uasm_slash_value = 6; opcode_modifier = 5; argument1 = 19; argument2 = 4; size1 = 64; size2 = 8; source_line = 490 };
        { entry_index = 11; opcode_bytes = [0x81]; flags = 0x001; slash_value = 6; uasm_slash_value = 6; opcode_modifier = 6; argument1 = 17; argument2 = 5; size1 = 16; size2 = 16; source_line = 491 };
        { entry_index = 12; opcode_bytes = [0x81]; flags = 0x002; slash_value = 6; uasm_slash_value = 6; opcode_modifier = 7; argument1 = 18; argument2 = 6; size1 = 32; size2 = 32; source_line = 492 };
        { entry_index = 13; opcode_bytes = [0x81]; flags = 0x002; slash_value = 6; uasm_slash_value = 6; opcode_modifier = 7; argument1 = 19; argument2 = 6; size1 = 64; size2 = 32; source_line = 493 };
        { entry_index = 14; opcode_bytes = [0x32]; flags = 0x000; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 12; argument2 = 16; size1 = 8; size2 = 8; source_line = 494 };
        { entry_index = 15; opcode_bytes = [0x33]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 13; argument2 = 17; size1 = 16; size2 = 16; source_line = 495 };
        { entry_index = 16; opcode_bytes = [0x33]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 14; argument2 = 18; size1 = 32; size2 = 32; source_line = 496 };
        { entry_index = 17; opcode_bytes = [0x33]; flags = 0x102; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 15; argument2 = 19; size1 = 64; size2 = 64; source_line = 497 };
        { entry_index = 18; opcode_bytes = [0x30]; flags = 0x000; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 16; argument2 = 12; size1 = 8; size2 = 8; source_line = 498 };
        { entry_index = 19; opcode_bytes = [0x31]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 17; argument2 = 13; size1 = 16; size2 = 16; source_line = 499 };
        { entry_index = 20; opcode_bytes = [0x31]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 18; argument2 = 14; size1 = 32; size2 = 32; source_line = 500 };
        { entry_index = 21; opcode_bytes = [0x31]; flags = 0x102; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 19; argument2 = 15; size1 = 64; size2 = 64; source_line = 501 };
        ];
      aliases =
        [
        ];
      source_line = 479 };
    { spelling = "CMOVO";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x40]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 13; argument2 = 17; size1 = 16; size2 = 16; source_line = 504 };
        { entry_index = 1; opcode_bytes = [0x0F; 0x40]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 14; argument2 = 18; size1 = 32; size2 = 32; source_line = 505 };
        { entry_index = 2; opcode_bytes = [0x0F; 0x40]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 15; argument2 = 19; size1 = 64; size2 = 64; source_line = 506 };
        ];
      aliases =
        [
        ];
      source_line = 503 };
    { spelling = "CMOVNO";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x41]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 13; argument2 = 17; size1 = 16; size2 = 16; source_line = 508 };
        { entry_index = 1; opcode_bytes = [0x0F; 0x41]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 14; argument2 = 18; size1 = 32; size2 = 32; source_line = 509 };
        { entry_index = 2; opcode_bytes = [0x0F; 0x41]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 15; argument2 = 19; size1 = 64; size2 = 64; source_line = 510 };
        ];
      aliases =
        [
        ];
      source_line = 507 };
    { spelling = "CMOVB";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x42]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 13; argument2 = 17; size1 = 16; size2 = 16; source_line = 512 };
        { entry_index = 1; opcode_bytes = [0x0F; 0x42]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 14; argument2 = 18; size1 = 32; size2 = 32; source_line = 513 };
        { entry_index = 2; opcode_bytes = [0x0F; 0x42]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 15; argument2 = 19; size1 = 64; size2 = 64; source_line = 514 };
        ];
      aliases =
        [
        { spelling = "CMOVC"; source_line = 514 };
        { spelling = "CMOVNAE"; source_line = 514 };
        ];
      source_line = 511 };
    { spelling = "CMOVAE";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x43]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 13; argument2 = 17; size1 = 16; size2 = 16; source_line = 516 };
        { entry_index = 1; opcode_bytes = [0x0F; 0x43]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 14; argument2 = 18; size1 = 32; size2 = 32; source_line = 517 };
        { entry_index = 2; opcode_bytes = [0x0F; 0x43]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 15; argument2 = 19; size1 = 64; size2 = 64; source_line = 518 };
        ];
      aliases =
        [
        { spelling = "CMOVNB"; source_line = 518 };
        { spelling = "CMOVNC"; source_line = 518 };
        ];
      source_line = 515 };
    { spelling = "CMOVE";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x44]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 13; argument2 = 17; size1 = 16; size2 = 16; source_line = 520 };
        { entry_index = 1; opcode_bytes = [0x0F; 0x44]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 14; argument2 = 18; size1 = 32; size2 = 32; source_line = 521 };
        { entry_index = 2; opcode_bytes = [0x0F; 0x44]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 15; argument2 = 19; size1 = 64; size2 = 64; source_line = 522 };
        ];
      aliases =
        [
        { spelling = "CMOVZ"; source_line = 522 };
        ];
      source_line = 519 };
    { spelling = "CMOVNE";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x45]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 13; argument2 = 17; size1 = 16; size2 = 16; source_line = 524 };
        { entry_index = 1; opcode_bytes = [0x0F; 0x45]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 14; argument2 = 18; size1 = 32; size2 = 32; source_line = 525 };
        { entry_index = 2; opcode_bytes = [0x0F; 0x45]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 15; argument2 = 19; size1 = 64; size2 = 64; source_line = 526 };
        ];
      aliases =
        [
        { spelling = "CMOVNZ"; source_line = 526 };
        ];
      source_line = 523 };
    { spelling = "CMOVBE";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x46]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 13; argument2 = 17; size1 = 16; size2 = 16; source_line = 528 };
        { entry_index = 1; opcode_bytes = [0x0F; 0x46]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 14; argument2 = 18; size1 = 32; size2 = 32; source_line = 529 };
        { entry_index = 2; opcode_bytes = [0x0F; 0x46]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 15; argument2 = 19; size1 = 64; size2 = 64; source_line = 530 };
        ];
      aliases =
        [
        { spelling = "CMOVNA"; source_line = 530 };
        ];
      source_line = 527 };
    { spelling = "CMOVA";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x47]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 13; argument2 = 17; size1 = 16; size2 = 16; source_line = 532 };
        { entry_index = 1; opcode_bytes = [0x0F; 0x47]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 14; argument2 = 18; size1 = 32; size2 = 32; source_line = 533 };
        { entry_index = 2; opcode_bytes = [0x0F; 0x47]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 15; argument2 = 19; size1 = 64; size2 = 64; source_line = 534 };
        ];
      aliases =
        [
        { spelling = "CMOVNBE"; source_line = 534 };
        ];
      source_line = 531 };
    { spelling = "CMOVS";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x48]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 13; argument2 = 17; size1 = 16; size2 = 16; source_line = 536 };
        { entry_index = 1; opcode_bytes = [0x0F; 0x48]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 14; argument2 = 18; size1 = 32; size2 = 32; source_line = 537 };
        { entry_index = 2; opcode_bytes = [0x0F; 0x48]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 15; argument2 = 19; size1 = 64; size2 = 64; source_line = 538 };
        ];
      aliases =
        [
        ];
      source_line = 535 };
    { spelling = "CMOVNS";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x49]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 13; argument2 = 17; size1 = 16; size2 = 16; source_line = 540 };
        { entry_index = 1; opcode_bytes = [0x0F; 0x49]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 14; argument2 = 18; size1 = 32; size2 = 32; source_line = 541 };
        { entry_index = 2; opcode_bytes = [0x0F; 0x49]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 15; argument2 = 19; size1 = 64; size2 = 64; source_line = 542 };
        ];
      aliases =
        [
        ];
      source_line = 539 };
    { spelling = "CMOVP";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x4A]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 13; argument2 = 17; size1 = 16; size2 = 16; source_line = 544 };
        { entry_index = 1; opcode_bytes = [0x0F; 0x4A]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 14; argument2 = 18; size1 = 32; size2 = 32; source_line = 545 };
        { entry_index = 2; opcode_bytes = [0x0F; 0x4A]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 15; argument2 = 19; size1 = 64; size2 = 64; source_line = 546 };
        ];
      aliases =
        [
        { spelling = "CMOVPE"; source_line = 546 };
        ];
      source_line = 543 };
    { spelling = "CMOVNP";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x4B]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 13; argument2 = 17; size1 = 16; size2 = 16; source_line = 548 };
        { entry_index = 1; opcode_bytes = [0x0F; 0x4B]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 14; argument2 = 18; size1 = 32; size2 = 32; source_line = 549 };
        { entry_index = 2; opcode_bytes = [0x0F; 0x4B]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 15; argument2 = 19; size1 = 64; size2 = 64; source_line = 550 };
        ];
      aliases =
        [
        { spelling = "CMOVPO"; source_line = 550 };
        ];
      source_line = 547 };
    { spelling = "CMOVL";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x4C]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 13; argument2 = 17; size1 = 16; size2 = 16; source_line = 552 };
        { entry_index = 1; opcode_bytes = [0x0F; 0x4C]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 14; argument2 = 18; size1 = 32; size2 = 32; source_line = 553 };
        { entry_index = 2; opcode_bytes = [0x0F; 0x4C]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 15; argument2 = 19; size1 = 64; size2 = 64; source_line = 554 };
        ];
      aliases =
        [
        { spelling = "CMOVNGE"; source_line = 554 };
        ];
      source_line = 551 };
    { spelling = "CMOVGE";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x4D]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 13; argument2 = 17; size1 = 16; size2 = 16; source_line = 556 };
        { entry_index = 1; opcode_bytes = [0x0F; 0x4D]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 14; argument2 = 18; size1 = 32; size2 = 32; source_line = 557 };
        { entry_index = 2; opcode_bytes = [0x0F; 0x4D]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 15; argument2 = 19; size1 = 64; size2 = 64; source_line = 558 };
        ];
      aliases =
        [
        { spelling = "CMOVNL"; source_line = 558 };
        ];
      source_line = 555 };
    { spelling = "CMOVLE";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x4E]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 13; argument2 = 17; size1 = 16; size2 = 16; source_line = 560 };
        { entry_index = 1; opcode_bytes = [0x0F; 0x4E]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 14; argument2 = 18; size1 = 32; size2 = 32; source_line = 561 };
        { entry_index = 2; opcode_bytes = [0x0F; 0x4E]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 15; argument2 = 19; size1 = 64; size2 = 64; source_line = 562 };
        ];
      aliases =
        [
        { spelling = "CMOVNG"; source_line = 562 };
        ];
      source_line = 559 };
    { spelling = "CMOVG";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x4F]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 13; argument2 = 17; size1 = 16; size2 = 16; source_line = 564 };
        { entry_index = 1; opcode_bytes = [0x0F; 0x4F]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 14; argument2 = 18; size1 = 32; size2 = 32; source_line = 565 };
        { entry_index = 2; opcode_bytes = [0x0F; 0x4F]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 15; argument2 = 19; size1 = 64; size2 = 64; source_line = 566 };
        ];
      aliases =
        [
        { spelling = "CMOVNLE"; source_line = 566 };
        ];
      source_line = 563 };
    { spelling = "CALL";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xE8]; flags = 0x019; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 2; argument1 = 2; argument2 = 0; size1 = 16; size2 = 0; source_line = 569 };
        { entry_index = 1; opcode_bytes = [0xFF]; flags = 0x009; slash_value = 2; uasm_slash_value = 2; opcode_modifier = 0; argument1 = 17; argument2 = 0; size1 = 16; size2 = 0; source_line = 570 };
        { entry_index = 2; opcode_bytes = [0xE8]; flags = 0x01A; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 3; argument1 = 3; argument2 = 0; size1 = 32; size2 = 0; source_line = 571 };
        { entry_index = 3; opcode_bytes = [0xFF]; flags = 0x00A; slash_value = 2; uasm_slash_value = 2; opcode_modifier = 0; argument1 = 18; argument2 = 0; size1 = 32; size2 = 0; source_line = 572 };
        { entry_index = 4; opcode_bytes = [0xFF]; flags = 0x08A; slash_value = 2; uasm_slash_value = 2; opcode_modifier = 0; argument1 = 19; argument2 = 0; size1 = 64; size2 = 0; source_line = 573 };
        ];
      aliases =
        [
        ];
      source_line = 568 };
    { spelling = "JMP";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xEB]; flags = 0x010; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 1; argument1 = 1; argument2 = 0; size1 = 8; size2 = 0; source_line = 582 };
        { entry_index = 1; opcode_bytes = [0xE9]; flags = 0x009; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 2; argument1 = 2; argument2 = 0; size1 = 16; size2 = 0; source_line = 583 };
        { entry_index = 2; opcode_bytes = [0xE9]; flags = 0x00A; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 3; argument1 = 3; argument2 = 0; size1 = 32; size2 = 0; source_line = 584 };
        { entry_index = 3; opcode_bytes = [0xFF]; flags = 0x009; slash_value = 4; uasm_slash_value = 4; opcode_modifier = 0; argument1 = 17; argument2 = 0; size1 = 16; size2 = 0; source_line = 585 };
        { entry_index = 4; opcode_bytes = [0xFF]; flags = 0x00A; slash_value = 4; uasm_slash_value = 4; opcode_modifier = 0; argument1 = 18; argument2 = 0; size1 = 32; size2 = 0; source_line = 586 };
        { entry_index = 5; opcode_bytes = [0xFF]; flags = 0x00A; slash_value = 4; uasm_slash_value = 4; opcode_modifier = 0; argument1 = 19; argument2 = 0; size1 = 64; size2 = 0; source_line = 587 };
        ];
      aliases =
        [
        ];
      source_line = 581 };
    { spelling = "JO";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x70]; flags = 0x010; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 1; argument1 = 1; argument2 = 0; size1 = 8; size2 = 0; source_line = 590 };
        { entry_index = 1; opcode_bytes = [0x0F; 0x80]; flags = 0x009; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 2; argument1 = 2; argument2 = 0; size1 = 16; size2 = 0; source_line = 591 };
        { entry_index = 2; opcode_bytes = [0x0F; 0x80]; flags = 0x00A; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 3; argument1 = 3; argument2 = 0; size1 = 32; size2 = 0; source_line = 592 };
        ];
      aliases =
        [
        ];
      source_line = 589 };
    { spelling = "JNO";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x71]; flags = 0x010; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 1; argument1 = 1; argument2 = 0; size1 = 8; size2 = 0; source_line = 594 };
        { entry_index = 1; opcode_bytes = [0x0F; 0x81]; flags = 0x009; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 2; argument1 = 2; argument2 = 0; size1 = 16; size2 = 0; source_line = 595 };
        { entry_index = 2; opcode_bytes = [0x0F; 0x81]; flags = 0x00A; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 3; argument1 = 3; argument2 = 0; size1 = 32; size2 = 0; source_line = 596 };
        ];
      aliases =
        [
        ];
      source_line = 593 };
    { spelling = "JB";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x72]; flags = 0x010; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 1; argument1 = 1; argument2 = 0; size1 = 8; size2 = 0; source_line = 598 };
        { entry_index = 1; opcode_bytes = [0x0F; 0x82]; flags = 0x009; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 2; argument1 = 2; argument2 = 0; size1 = 16; size2 = 0; source_line = 599 };
        { entry_index = 2; opcode_bytes = [0x0F; 0x82]; flags = 0x00A; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 3; argument1 = 3; argument2 = 0; size1 = 32; size2 = 0; source_line = 600 };
        ];
      aliases =
        [
        { spelling = "JC"; source_line = 600 };
        { spelling = "JNAE"; source_line = 600 };
        ];
      source_line = 597 };
    { spelling = "JAE";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x73]; flags = 0x010; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 1; argument1 = 1; argument2 = 0; size1 = 8; size2 = 0; source_line = 602 };
        { entry_index = 1; opcode_bytes = [0x0F; 0x83]; flags = 0x009; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 2; argument1 = 2; argument2 = 0; size1 = 16; size2 = 0; source_line = 603 };
        { entry_index = 2; opcode_bytes = [0x0F; 0x83]; flags = 0x00A; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 3; argument1 = 3; argument2 = 0; size1 = 32; size2 = 0; source_line = 604 };
        ];
      aliases =
        [
        { spelling = "JNB"; source_line = 604 };
        { spelling = "JNC"; source_line = 604 };
        ];
      source_line = 601 };
    { spelling = "JE";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x74]; flags = 0x010; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 1; argument1 = 1; argument2 = 0; size1 = 8; size2 = 0; source_line = 606 };
        { entry_index = 1; opcode_bytes = [0x0F; 0x84]; flags = 0x009; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 2; argument1 = 2; argument2 = 0; size1 = 16; size2 = 0; source_line = 607 };
        { entry_index = 2; opcode_bytes = [0x0F; 0x84]; flags = 0x00A; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 3; argument1 = 3; argument2 = 0; size1 = 32; size2 = 0; source_line = 608 };
        ];
      aliases =
        [
        { spelling = "JZ"; source_line = 608 };
        ];
      source_line = 605 };
    { spelling = "JNE";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x75]; flags = 0x010; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 1; argument1 = 1; argument2 = 0; size1 = 8; size2 = 0; source_line = 610 };
        { entry_index = 1; opcode_bytes = [0x0F; 0x85]; flags = 0x009; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 2; argument1 = 2; argument2 = 0; size1 = 16; size2 = 0; source_line = 611 };
        { entry_index = 2; opcode_bytes = [0x0F; 0x85]; flags = 0x00A; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 3; argument1 = 3; argument2 = 0; size1 = 32; size2 = 0; source_line = 612 };
        ];
      aliases =
        [
        { spelling = "JNZ"; source_line = 612 };
        ];
      source_line = 609 };
    { spelling = "JBE";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x76]; flags = 0x010; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 1; argument1 = 1; argument2 = 0; size1 = 8; size2 = 0; source_line = 614 };
        { entry_index = 1; opcode_bytes = [0x0F; 0x86]; flags = 0x009; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 2; argument1 = 2; argument2 = 0; size1 = 16; size2 = 0; source_line = 615 };
        { entry_index = 2; opcode_bytes = [0x0F; 0x86]; flags = 0x00A; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 3; argument1 = 3; argument2 = 0; size1 = 32; size2 = 0; source_line = 616 };
        ];
      aliases =
        [
        { spelling = "JNA"; source_line = 616 };
        ];
      source_line = 613 };
    { spelling = "JA";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x77]; flags = 0x010; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 1; argument1 = 1; argument2 = 0; size1 = 8; size2 = 0; source_line = 618 };
        { entry_index = 1; opcode_bytes = [0x0F; 0x87]; flags = 0x009; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 2; argument1 = 2; argument2 = 0; size1 = 16; size2 = 0; source_line = 619 };
        { entry_index = 2; opcode_bytes = [0x0F; 0x87]; flags = 0x00A; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 3; argument1 = 3; argument2 = 0; size1 = 32; size2 = 0; source_line = 620 };
        ];
      aliases =
        [
        { spelling = "JNBE"; source_line = 620 };
        ];
      source_line = 617 };
    { spelling = "JS";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x78]; flags = 0x010; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 1; argument1 = 1; argument2 = 0; size1 = 8; size2 = 0; source_line = 622 };
        { entry_index = 1; opcode_bytes = [0x0F; 0x88]; flags = 0x009; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 2; argument1 = 2; argument2 = 0; size1 = 16; size2 = 0; source_line = 623 };
        { entry_index = 2; opcode_bytes = [0x0F; 0x88]; flags = 0x00A; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 3; argument1 = 3; argument2 = 0; size1 = 32; size2 = 0; source_line = 624 };
        ];
      aliases =
        [
        ];
      source_line = 621 };
    { spelling = "JNS";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x79]; flags = 0x010; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 1; argument1 = 1; argument2 = 0; size1 = 8; size2 = 0; source_line = 626 };
        { entry_index = 1; opcode_bytes = [0x0F; 0x89]; flags = 0x009; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 2; argument1 = 2; argument2 = 0; size1 = 16; size2 = 0; source_line = 627 };
        { entry_index = 2; opcode_bytes = [0x0F; 0x89]; flags = 0x00A; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 3; argument1 = 3; argument2 = 0; size1 = 32; size2 = 0; source_line = 628 };
        ];
      aliases =
        [
        ];
      source_line = 625 };
    { spelling = "JP";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x7A]; flags = 0x010; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 1; argument1 = 1; argument2 = 0; size1 = 8; size2 = 0; source_line = 630 };
        { entry_index = 1; opcode_bytes = [0x0F; 0x8A]; flags = 0x009; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 2; argument1 = 2; argument2 = 0; size1 = 16; size2 = 0; source_line = 631 };
        { entry_index = 2; opcode_bytes = [0x0F; 0x8A]; flags = 0x00A; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 3; argument1 = 3; argument2 = 0; size1 = 32; size2 = 0; source_line = 632 };
        ];
      aliases =
        [
        { spelling = "JPE"; source_line = 632 };
        ];
      source_line = 629 };
    { spelling = "JNP";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x7B]; flags = 0x010; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 1; argument1 = 1; argument2 = 0; size1 = 8; size2 = 0; source_line = 634 };
        { entry_index = 1; opcode_bytes = [0x0F; 0x8B]; flags = 0x009; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 2; argument1 = 2; argument2 = 0; size1 = 16; size2 = 0; source_line = 635 };
        { entry_index = 2; opcode_bytes = [0x0F; 0x8B]; flags = 0x00A; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 3; argument1 = 3; argument2 = 0; size1 = 32; size2 = 0; source_line = 636 };
        ];
      aliases =
        [
        { spelling = "JPO"; source_line = 636 };
        ];
      source_line = 633 };
    { spelling = "JL";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x7C]; flags = 0x010; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 1; argument1 = 1; argument2 = 0; size1 = 8; size2 = 0; source_line = 638 };
        { entry_index = 1; opcode_bytes = [0x0F; 0x8C]; flags = 0x009; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 2; argument1 = 2; argument2 = 0; size1 = 16; size2 = 0; source_line = 639 };
        { entry_index = 2; opcode_bytes = [0x0F; 0x8C]; flags = 0x00A; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 3; argument1 = 3; argument2 = 0; size1 = 32; size2 = 0; source_line = 640 };
        ];
      aliases =
        [
        { spelling = "JNGE"; source_line = 640 };
        ];
      source_line = 637 };
    { spelling = "JGE";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x7D]; flags = 0x010; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 1; argument1 = 1; argument2 = 0; size1 = 8; size2 = 0; source_line = 642 };
        { entry_index = 1; opcode_bytes = [0x0F; 0x8D]; flags = 0x009; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 2; argument1 = 2; argument2 = 0; size1 = 16; size2 = 0; source_line = 643 };
        { entry_index = 2; opcode_bytes = [0x0F; 0x8D]; flags = 0x00A; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 3; argument1 = 3; argument2 = 0; size1 = 32; size2 = 0; source_line = 644 };
        ];
      aliases =
        [
        { spelling = "JNL"; source_line = 644 };
        ];
      source_line = 641 };
    { spelling = "JLE";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x7E]; flags = 0x010; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 1; argument1 = 1; argument2 = 0; size1 = 8; size2 = 0; source_line = 646 };
        { entry_index = 1; opcode_bytes = [0x0F; 0x8E]; flags = 0x009; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 2; argument1 = 2; argument2 = 0; size1 = 16; size2 = 0; source_line = 647 };
        { entry_index = 2; opcode_bytes = [0x0F; 0x8E]; flags = 0x00A; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 3; argument1 = 3; argument2 = 0; size1 = 32; size2 = 0; source_line = 648 };
        ];
      aliases =
        [
        { spelling = "JNG"; source_line = 648 };
        ];
      source_line = 645 };
    { spelling = "JG";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x7F]; flags = 0x010; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 1; argument1 = 1; argument2 = 0; size1 = 8; size2 = 0; source_line = 650 };
        { entry_index = 1; opcode_bytes = [0x0F; 0x8F]; flags = 0x009; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 2; argument1 = 2; argument2 = 0; size1 = 16; size2 = 0; source_line = 651 };
        { entry_index = 2; opcode_bytes = [0x0F; 0x8F]; flags = 0x00A; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 3; argument1 = 3; argument2 = 0; size1 = 32; size2 = 0; source_line = 652 };
        ];
      aliases =
        [
        { spelling = "JNLE"; source_line = 652 };
        ];
      source_line = 649 };
    { spelling = "JCXZ";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xE3]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 1; argument1 = 1; argument2 = 0; size1 = 8; size2 = 0; source_line = 655 };
        ];
      aliases =
        [
        { spelling = "JECXZ"; source_line = 655 };
        { spelling = "JRCXZ"; source_line = 655 };
        ];
      source_line = 654 };
    { spelling = "INC";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x40]; flags = 0x025; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 13; argument2 = 0; size1 = 16; size2 = 0; source_line = 658 };
        { entry_index = 1; opcode_bytes = [0x40]; flags = 0x026; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 14; argument2 = 0; size1 = 32; size2 = 0; source_line = 659 };
        { entry_index = 2; opcode_bytes = [0xFE]; flags = 0x000; slash_value = 0; uasm_slash_value = 0; opcode_modifier = 0; argument1 = 16; argument2 = 0; size1 = 8; size2 = 0; source_line = 660 };
        { entry_index = 3; opcode_bytes = [0xFF]; flags = 0x001; slash_value = 0; uasm_slash_value = 0; opcode_modifier = 0; argument1 = 17; argument2 = 0; size1 = 16; size2 = 0; source_line = 661 };
        { entry_index = 4; opcode_bytes = [0xFF]; flags = 0x002; slash_value = 0; uasm_slash_value = 0; opcode_modifier = 0; argument1 = 18; argument2 = 0; size1 = 32; size2 = 0; source_line = 662 };
        { entry_index = 5; opcode_bytes = [0xFF]; flags = 0x002; slash_value = 0; uasm_slash_value = 0; opcode_modifier = 0; argument1 = 19; argument2 = 0; size1 = 64; size2 = 0; source_line = 663 };
        ];
      aliases =
        [
        ];
      source_line = 657 };
    { spelling = "DEC";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x48]; flags = 0x025; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 13; argument2 = 0; size1 = 16; size2 = 0; source_line = 665 };
        { entry_index = 1; opcode_bytes = [0x48]; flags = 0x026; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 14; argument2 = 0; size1 = 32; size2 = 0; source_line = 666 };
        { entry_index = 2; opcode_bytes = [0xFE]; flags = 0x000; slash_value = 1; uasm_slash_value = 1; opcode_modifier = 0; argument1 = 16; argument2 = 0; size1 = 8; size2 = 0; source_line = 667 };
        { entry_index = 3; opcode_bytes = [0xFF]; flags = 0x001; slash_value = 1; uasm_slash_value = 1; opcode_modifier = 0; argument1 = 17; argument2 = 0; size1 = 16; size2 = 0; source_line = 668 };
        { entry_index = 4; opcode_bytes = [0xFF]; flags = 0x002; slash_value = 1; uasm_slash_value = 1; opcode_modifier = 0; argument1 = 18; argument2 = 0; size1 = 32; size2 = 0; source_line = 669 };
        { entry_index = 5; opcode_bytes = [0xFF]; flags = 0x002; slash_value = 1; uasm_slash_value = 1; opcode_modifier = 0; argument1 = 19; argument2 = 0; size1 = 64; size2 = 0; source_line = 670 };
        ];
      aliases =
        [
        ];
      source_line = 664 };
    { spelling = "NOT";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xF6]; flags = 0x000; slash_value = 2; uasm_slash_value = 2; opcode_modifier = 0; argument1 = 16; argument2 = 0; size1 = 8; size2 = 0; source_line = 672 };
        { entry_index = 1; opcode_bytes = [0xF7]; flags = 0x001; slash_value = 2; uasm_slash_value = 2; opcode_modifier = 0; argument1 = 17; argument2 = 0; size1 = 16; size2 = 0; source_line = 673 };
        { entry_index = 2; opcode_bytes = [0xF7]; flags = 0x002; slash_value = 2; uasm_slash_value = 2; opcode_modifier = 0; argument1 = 18; argument2 = 0; size1 = 32; size2 = 0; source_line = 674 };
        { entry_index = 3; opcode_bytes = [0xF7]; flags = 0x002; slash_value = 2; uasm_slash_value = 2; opcode_modifier = 0; argument1 = 19; argument2 = 0; size1 = 64; size2 = 0; source_line = 675 };
        ];
      aliases =
        [
        ];
      source_line = 671 };
    { spelling = "NEG";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xF6]; flags = 0x000; slash_value = 3; uasm_slash_value = 3; opcode_modifier = 0; argument1 = 16; argument2 = 0; size1 = 8; size2 = 0; source_line = 677 };
        { entry_index = 1; opcode_bytes = [0xF7]; flags = 0x001; slash_value = 3; uasm_slash_value = 3; opcode_modifier = 0; argument1 = 17; argument2 = 0; size1 = 16; size2 = 0; source_line = 678 };
        { entry_index = 2; opcode_bytes = [0xF7]; flags = 0x002; slash_value = 3; uasm_slash_value = 3; opcode_modifier = 0; argument1 = 18; argument2 = 0; size1 = 32; size2 = 0; source_line = 679 };
        { entry_index = 3; opcode_bytes = [0xF7]; flags = 0x002; slash_value = 3; uasm_slash_value = 3; opcode_modifier = 0; argument1 = 19; argument2 = 0; size1 = 64; size2 = 0; source_line = 680 };
        ];
      aliases =
        [
        ];
      source_line = 676 };
    { spelling = "MUL";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xF6]; flags = 0x000; slash_value = 4; uasm_slash_value = 4; opcode_modifier = 0; argument1 = 16; argument2 = 0; size1 = 8; size2 = 0; source_line = 682 };
        { entry_index = 1; opcode_bytes = [0xF7]; flags = 0x001; slash_value = 4; uasm_slash_value = 4; opcode_modifier = 0; argument1 = 17; argument2 = 0; size1 = 16; size2 = 0; source_line = 683 };
        { entry_index = 2; opcode_bytes = [0xF7]; flags = 0x002; slash_value = 4; uasm_slash_value = 4; opcode_modifier = 0; argument1 = 18; argument2 = 0; size1 = 32; size2 = 0; source_line = 684 };
        { entry_index = 3; opcode_bytes = [0xF7]; flags = 0x002; slash_value = 4; uasm_slash_value = 4; opcode_modifier = 0; argument1 = 19; argument2 = 0; size1 = 64; size2 = 0; source_line = 685 };
        ];
      aliases =
        [
        ];
      source_line = 681 };
    { spelling = "IMUL";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xF6]; flags = 0x000; slash_value = 5; uasm_slash_value = 5; opcode_modifier = 0; argument1 = 16; argument2 = 0; size1 = 8; size2 = 0; source_line = 687 };
        { entry_index = 1; opcode_bytes = [0xF7]; flags = 0x001; slash_value = 5; uasm_slash_value = 5; opcode_modifier = 0; argument1 = 17; argument2 = 0; size1 = 16; size2 = 0; source_line = 688 };
        { entry_index = 2; opcode_bytes = [0xF7]; flags = 0x002; slash_value = 5; uasm_slash_value = 5; opcode_modifier = 0; argument1 = 18; argument2 = 0; size1 = 32; size2 = 0; source_line = 689 };
        { entry_index = 3; opcode_bytes = [0xF7]; flags = 0x002; slash_value = 5; uasm_slash_value = 5; opcode_modifier = 0; argument1 = 19; argument2 = 0; size1 = 64; size2 = 0; source_line = 690 };
        ];
      aliases =
        [
        ];
      source_line = 686 };
    { spelling = "IMUL2";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0xAF]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 13; argument2 = 17; size1 = 16; size2 = 16; source_line = 692 };
        { entry_index = 1; opcode_bytes = [0x0F; 0xAF]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 14; argument2 = 18; size1 = 32; size2 = 32; source_line = 693 };
        { entry_index = 2; opcode_bytes = [0x0F; 0xAF]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 15; argument2 = 19; size1 = 64; size2 = 64; source_line = 694 };
        { entry_index = 3; opcode_bytes = [0x6B]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 5; argument1 = 17; argument2 = 4; size1 = 16; size2 = 8; source_line = 695 };
        { entry_index = 4; opcode_bytes = [0x6B]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 5; argument1 = 18; argument2 = 4; size1 = 32; size2 = 8; source_line = 696 };
        { entry_index = 5; opcode_bytes = [0x6B]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 5; argument1 = 19; argument2 = 4; size1 = 64; size2 = 8; source_line = 697 };
        { entry_index = 6; opcode_bytes = [0x69]; flags = 0x011; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 6; argument1 = 17; argument2 = 9; size1 = 16; size2 = 16; source_line = 698 };
        { entry_index = 7; opcode_bytes = [0x69]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 6; argument1 = 17; argument2 = 5; size1 = 16; size2 = 16; source_line = 699 };
        { entry_index = 8; opcode_bytes = [0x69]; flags = 0x012; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 7; argument1 = 18; argument2 = 10; size1 = 32; size2 = 32; source_line = 700 };
        { entry_index = 9; opcode_bytes = [0x69]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 7; argument1 = 18; argument2 = 6; size1 = 32; size2 = 32; source_line = 701 };
        { entry_index = 10; opcode_bytes = [0x69]; flags = 0x012; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 7; argument1 = 19; argument2 = 10; size1 = 64; size2 = 32; source_line = 702 };
        { entry_index = 11; opcode_bytes = [0x69]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 7; argument1 = 19; argument2 = 6; size1 = 64; size2 = 32; source_line = 703 };
        ];
      aliases =
        [
        ];
      source_line = 691 };
    { spelling = "DIV";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xF6]; flags = 0x000; slash_value = 6; uasm_slash_value = 6; opcode_modifier = 0; argument1 = 16; argument2 = 0; size1 = 8; size2 = 0; source_line = 705 };
        { entry_index = 1; opcode_bytes = [0xF7]; flags = 0x001; slash_value = 6; uasm_slash_value = 6; opcode_modifier = 0; argument1 = 17; argument2 = 0; size1 = 16; size2 = 0; source_line = 706 };
        { entry_index = 2; opcode_bytes = [0xF7]; flags = 0x002; slash_value = 6; uasm_slash_value = 6; opcode_modifier = 0; argument1 = 18; argument2 = 0; size1 = 32; size2 = 0; source_line = 707 };
        { entry_index = 3; opcode_bytes = [0xF7]; flags = 0x002; slash_value = 6; uasm_slash_value = 6; opcode_modifier = 0; argument1 = 19; argument2 = 0; size1 = 64; size2 = 0; source_line = 708 };
        ];
      aliases =
        [
        ];
      source_line = 704 };
    { spelling = "IDIV";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xF6]; flags = 0x000; slash_value = 7; uasm_slash_value = 7; opcode_modifier = 0; argument1 = 16; argument2 = 0; size1 = 8; size2 = 0; source_line = 710 };
        { entry_index = 1; opcode_bytes = [0xF7]; flags = 0x001; slash_value = 7; uasm_slash_value = 7; opcode_modifier = 0; argument1 = 17; argument2 = 0; size1 = 16; size2 = 0; source_line = 711 };
        { entry_index = 2; opcode_bytes = [0xF7]; flags = 0x002; slash_value = 7; uasm_slash_value = 7; opcode_modifier = 0; argument1 = 18; argument2 = 0; size1 = 32; size2 = 0; source_line = 712 };
        { entry_index = 3; opcode_bytes = [0xF7]; flags = 0x002; slash_value = 7; uasm_slash_value = 7; opcode_modifier = 0; argument1 = 19; argument2 = 0; size1 = 64; size2 = 0; source_line = 713 };
        ];
      aliases =
        [
        ];
      source_line = 709 };
    { spelling = "AAA";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x37]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 715 };
        ];
      aliases =
        [
        ];
      source_line = 715 };
    { spelling = "AAD";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD5; 0x0A]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 716 };
        ];
      aliases =
        [
        ];
      source_line = 716 };
    { spelling = "AAM";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD4; 0x0A]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 717 };
        ];
      aliases =
        [
        ];
      source_line = 717 };
    { spelling = "AAS";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x3F]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 718 };
        ];
      aliases =
        [
        ];
      source_line = 718 };
    { spelling = "ARPL";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x63]; flags = 0x000; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 17; argument2 = 13; size1 = 16; size2 = 16; source_line = 719 };
        ];
      aliases =
        [
        ];
      source_line = 719 };
    { spelling = "BOUND";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x62]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 17; argument2 = 13; size1 = 16; size2 = 16; source_line = 721 };
        { entry_index = 1; opcode_bytes = [0x62]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 18; argument2 = 14; size1 = 32; size2 = 32; source_line = 722 };
        { entry_index = 2; opcode_bytes = [0x62]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 19; argument2 = 15; size1 = 64; size2 = 64; source_line = 723 };
        ];
      aliases =
        [
        ];
      source_line = 720 };
    { spelling = "BSF";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0xBC]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 13; argument2 = 17; size1 = 16; size2 = 16; source_line = 725 };
        { entry_index = 1; opcode_bytes = [0x0F; 0xBC]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 14; argument2 = 18; size1 = 32; size2 = 32; source_line = 726 };
        { entry_index = 2; opcode_bytes = [0x0F; 0xBC]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 15; argument2 = 19; size1 = 64; size2 = 64; source_line = 727 };
        ];
      aliases =
        [
        ];
      source_line = 724 };
    { spelling = "BSR";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0xBD]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 13; argument2 = 17; size1 = 16; size2 = 16; source_line = 729 };
        { entry_index = 1; opcode_bytes = [0x0F; 0xBD]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 14; argument2 = 18; size1 = 32; size2 = 32; source_line = 730 };
        { entry_index = 2; opcode_bytes = [0x0F; 0xBD]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 15; argument2 = 19; size1 = 64; size2 = 64; source_line = 731 };
        ];
      aliases =
        [
        ];
      source_line = 728 };
    { spelling = "BSWAP";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0xC8]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 14; argument2 = 0; size1 = 32; size2 = 0; source_line = 733 };
        { entry_index = 1; opcode_bytes = [0x0F; 0xC8]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 15; argument2 = 0; size1 = 64; size2 = 0; source_line = 734 };
        ];
      aliases =
        [
        ];
      source_line = 732 };
    { spelling = "BT";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0xA3]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 17; argument2 = 13; size1 = 16; size2 = 16; source_line = 736 };
        { entry_index = 1; opcode_bytes = [0x0F; 0xA3]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 18; argument2 = 14; size1 = 32; size2 = 32; source_line = 737 };
        { entry_index = 2; opcode_bytes = [0x0F; 0xA3]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 19; argument2 = 15; size1 = 64; size2 = 64; source_line = 738 };
        { entry_index = 3; opcode_bytes = [0x0F; 0xBA]; flags = 0x011; slash_value = 4; uasm_slash_value = 4; opcode_modifier = 5; argument1 = 17; argument2 = 8; size1 = 16; size2 = 8; source_line = 739 };
        { entry_index = 4; opcode_bytes = [0x0F; 0xBA]; flags = 0x001; slash_value = 4; uasm_slash_value = 4; opcode_modifier = 5; argument1 = 17; argument2 = 4; size1 = 16; size2 = 8; source_line = 740 };
        { entry_index = 5; opcode_bytes = [0x0F; 0xBA]; flags = 0x012; slash_value = 4; uasm_slash_value = 4; opcode_modifier = 5; argument1 = 18; argument2 = 8; size1 = 32; size2 = 8; source_line = 741 };
        { entry_index = 6; opcode_bytes = [0x0F; 0xBA]; flags = 0x002; slash_value = 4; uasm_slash_value = 4; opcode_modifier = 5; argument1 = 18; argument2 = 4; size1 = 32; size2 = 8; source_line = 742 };
        { entry_index = 7; opcode_bytes = [0x0F; 0xBA]; flags = 0x012; slash_value = 4; uasm_slash_value = 4; opcode_modifier = 5; argument1 = 19; argument2 = 8; size1 = 64; size2 = 8; source_line = 743 };
        { entry_index = 8; opcode_bytes = [0x0F; 0xBA]; flags = 0x002; slash_value = 4; uasm_slash_value = 4; opcode_modifier = 5; argument1 = 19; argument2 = 4; size1 = 64; size2 = 8; source_line = 744 };
        ];
      aliases =
        [
        ];
      source_line = 735 };
    { spelling = "BTC";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0xBB]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 17; argument2 = 13; size1 = 16; size2 = 16; source_line = 746 };
        { entry_index = 1; opcode_bytes = [0x0F; 0xBB]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 18; argument2 = 14; size1 = 32; size2 = 32; source_line = 747 };
        { entry_index = 2; opcode_bytes = [0x0F; 0xBB]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 19; argument2 = 15; size1 = 64; size2 = 64; source_line = 748 };
        { entry_index = 3; opcode_bytes = [0x0F; 0xBA]; flags = 0x011; slash_value = 7; uasm_slash_value = 7; opcode_modifier = 5; argument1 = 17; argument2 = 8; size1 = 16; size2 = 8; source_line = 749 };
        { entry_index = 4; opcode_bytes = [0x0F; 0xBA]; flags = 0x001; slash_value = 7; uasm_slash_value = 7; opcode_modifier = 5; argument1 = 17; argument2 = 4; size1 = 16; size2 = 8; source_line = 750 };
        { entry_index = 5; opcode_bytes = [0x0F; 0xBA]; flags = 0x012; slash_value = 7; uasm_slash_value = 7; opcode_modifier = 5; argument1 = 18; argument2 = 8; size1 = 32; size2 = 8; source_line = 751 };
        { entry_index = 6; opcode_bytes = [0x0F; 0xBA]; flags = 0x002; slash_value = 7; uasm_slash_value = 7; opcode_modifier = 5; argument1 = 18; argument2 = 4; size1 = 32; size2 = 8; source_line = 752 };
        { entry_index = 7; opcode_bytes = [0x0F; 0xBA]; flags = 0x012; slash_value = 7; uasm_slash_value = 7; opcode_modifier = 5; argument1 = 19; argument2 = 8; size1 = 64; size2 = 8; source_line = 753 };
        { entry_index = 8; opcode_bytes = [0x0F; 0xBA]; flags = 0x002; slash_value = 7; uasm_slash_value = 7; opcode_modifier = 5; argument1 = 19; argument2 = 4; size1 = 64; size2 = 8; source_line = 754 };
        ];
      aliases =
        [
        ];
      source_line = 745 };
    { spelling = "BTR";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0xB3]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 17; argument2 = 13; size1 = 16; size2 = 16; source_line = 756 };
        { entry_index = 1; opcode_bytes = [0x0F; 0xB3]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 18; argument2 = 14; size1 = 32; size2 = 32; source_line = 757 };
        { entry_index = 2; opcode_bytes = [0x0F; 0xB3]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 19; argument2 = 15; size1 = 64; size2 = 64; source_line = 758 };
        { entry_index = 3; opcode_bytes = [0x0F; 0xBA]; flags = 0x011; slash_value = 6; uasm_slash_value = 6; opcode_modifier = 5; argument1 = 17; argument2 = 8; size1 = 16; size2 = 8; source_line = 759 };
        { entry_index = 4; opcode_bytes = [0x0F; 0xBA]; flags = 0x001; slash_value = 6; uasm_slash_value = 6; opcode_modifier = 5; argument1 = 17; argument2 = 4; size1 = 16; size2 = 8; source_line = 760 };
        { entry_index = 5; opcode_bytes = [0x0F; 0xBA]; flags = 0x012; slash_value = 6; uasm_slash_value = 6; opcode_modifier = 5; argument1 = 18; argument2 = 8; size1 = 32; size2 = 8; source_line = 761 };
        { entry_index = 6; opcode_bytes = [0x0F; 0xBA]; flags = 0x002; slash_value = 6; uasm_slash_value = 6; opcode_modifier = 5; argument1 = 18; argument2 = 4; size1 = 32; size2 = 8; source_line = 762 };
        { entry_index = 7; opcode_bytes = [0x0F; 0xBA]; flags = 0x012; slash_value = 6; uasm_slash_value = 6; opcode_modifier = 5; argument1 = 19; argument2 = 8; size1 = 64; size2 = 8; source_line = 763 };
        { entry_index = 8; opcode_bytes = [0x0F; 0xBA]; flags = 0x002; slash_value = 6; uasm_slash_value = 6; opcode_modifier = 5; argument1 = 19; argument2 = 4; size1 = 64; size2 = 8; source_line = 764 };
        ];
      aliases =
        [
        ];
      source_line = 755 };
    { spelling = "BTS";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0xAB]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 17; argument2 = 13; size1 = 16; size2 = 16; source_line = 766 };
        { entry_index = 1; opcode_bytes = [0x0F; 0xAB]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 18; argument2 = 14; size1 = 32; size2 = 32; source_line = 767 };
        { entry_index = 2; opcode_bytes = [0x0F; 0xAB]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 19; argument2 = 15; size1 = 64; size2 = 64; source_line = 768 };
        { entry_index = 3; opcode_bytes = [0x0F; 0xBA]; flags = 0x011; slash_value = 5; uasm_slash_value = 5; opcode_modifier = 5; argument1 = 17; argument2 = 8; size1 = 16; size2 = 8; source_line = 769 };
        { entry_index = 4; opcode_bytes = [0x0F; 0xBA]; flags = 0x001; slash_value = 5; uasm_slash_value = 5; opcode_modifier = 5; argument1 = 17; argument2 = 4; size1 = 16; size2 = 8; source_line = 770 };
        { entry_index = 5; opcode_bytes = [0x0F; 0xBA]; flags = 0x012; slash_value = 5; uasm_slash_value = 5; opcode_modifier = 5; argument1 = 18; argument2 = 8; size1 = 32; size2 = 8; source_line = 771 };
        { entry_index = 6; opcode_bytes = [0x0F; 0xBA]; flags = 0x002; slash_value = 5; uasm_slash_value = 5; opcode_modifier = 5; argument1 = 18; argument2 = 4; size1 = 32; size2 = 8; source_line = 772 };
        { entry_index = 7; opcode_bytes = [0x0F; 0xBA]; flags = 0x012; slash_value = 5; uasm_slash_value = 5; opcode_modifier = 5; argument1 = 19; argument2 = 8; size1 = 64; size2 = 8; source_line = 773 };
        { entry_index = 8; opcode_bytes = [0x0F; 0xBA]; flags = 0x002; slash_value = 5; uasm_slash_value = 5; opcode_modifier = 5; argument1 = 19; argument2 = 4; size1 = 64; size2 = 8; source_line = 774 };
        ];
      aliases =
        [
        ];
      source_line = 765 };
    { spelling = "CBW";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x98]; flags = 0x001; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 775 };
        ];
      aliases =
        [
        ];
      source_line = 775 };
    { spelling = "CWDE";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x98]; flags = 0x002; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 776 };
        ];
      aliases =
        [
        ];
      source_line = 776 };
    { spelling = "CDQE";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x98]; flags = 0x042; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 777 };
        ];
      aliases =
        [
        ];
      source_line = 777 };
    { spelling = "CWD";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x99]; flags = 0x001; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 778 };
        ];
      aliases =
        [
        ];
      source_line = 778 };
    { spelling = "CDQ";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x99]; flags = 0x002; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 779 };
        ];
      aliases =
        [
        ];
      source_line = 779 };
    { spelling = "CQO";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x99]; flags = 0x042; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 780 };
        ];
      aliases =
        [
        ];
      source_line = 780 };
    { spelling = "CLC";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xF8]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 781 };
        ];
      aliases =
        [
        ];
      source_line = 781 };
    { spelling = "CLD";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xFC]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 782 };
        ];
      aliases =
        [
        ];
      source_line = 782 };
    { spelling = "CLI";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xFA]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 783 };
        ];
      aliases =
        [
        ];
      source_line = 783 };
    { spelling = "CLTS";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x06]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 784 };
        ];
      aliases =
        [
        ];
      source_line = 784 };
    { spelling = "CMC";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xF5]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 785 };
        ];
      aliases =
        [
        ];
      source_line = 785 };
    { spelling = "CMPSB";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xA6]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 786 };
        ];
      aliases =
        [
        ];
      source_line = 786 };
    { spelling = "CMPSW";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xA7]; flags = 0x001; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 787 };
        ];
      aliases =
        [
        ];
      source_line = 787 };
    { spelling = "CMPSD";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xA7]; flags = 0x002; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 788 };
        ];
      aliases =
        [
        ];
      source_line = 788 };
    { spelling = "CMPSQ";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xA7]; flags = 0x042; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 789 };
        ];
      aliases =
        [
        ];
      source_line = 789 };
    { spelling = "CMPXCHG";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0xB0]; flags = 0x000; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 16; argument2 = 12; size1 = 8; size2 = 8; source_line = 791 };
        { entry_index = 1; opcode_bytes = [0x0F; 0xB1]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 17; argument2 = 13; size1 = 16; size2 = 16; source_line = 792 };
        { entry_index = 2; opcode_bytes = [0x0F; 0xB1]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 18; argument2 = 14; size1 = 32; size2 = 32; source_line = 793 };
        { entry_index = 3; opcode_bytes = [0x0F; 0xB1]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 19; argument2 = 15; size1 = 64; size2 = 64; source_line = 794 };
        ];
      aliases =
        [
        ];
      source_line = 790 };
    { spelling = "CHPXCHG8B";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0xC7]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 19; argument2 = 0; size1 = 64; size2 = 0; source_line = 795 };
        ];
      aliases =
        [
        ];
      source_line = 795 };
    { spelling = "DAA";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x27]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 796 };
        ];
      aliases =
        [
        ];
      source_line = 796 };
    { spelling = "DAS";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x2F]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 797 };
        ];
      aliases =
        [
        ];
      source_line = 797 };
    { spelling = "ENTER";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xC8]; flags = 0x400; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 6; argument1 = 5; argument2 = 0; size1 = 16; size2 = 0; source_line = 799 };
        ];
      aliases =
        [
        ];
      source_line = 798 };
    { spelling = "HLT";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xF4]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 800 };
        ];
      aliases =
        [
        ];
      source_line = 800 };
    { spelling = "IN";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xE4]; flags = 0x010; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 5; argument1 = 32; argument2 = 8; size1 = 8; size2 = 8; source_line = 802 };
        { entry_index = 1; opcode_bytes = [0xE4]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 5; argument1 = 32; argument2 = 4; size1 = 8; size2 = 8; source_line = 803 };
        { entry_index = 2; opcode_bytes = [0xE5]; flags = 0x011; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 5; argument1 = 33; argument2 = 8; size1 = 16; size2 = 8; source_line = 804 };
        { entry_index = 3; opcode_bytes = [0xE5]; flags = 0x001; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 5; argument1 = 33; argument2 = 4; size1 = 16; size2 = 8; source_line = 805 };
        { entry_index = 4; opcode_bytes = [0xE5]; flags = 0x012; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 5; argument1 = 34; argument2 = 8; size1 = 32; size2 = 8; source_line = 806 };
        { entry_index = 5; opcode_bytes = [0xE5]; flags = 0x002; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 5; argument1 = 34; argument2 = 4; size1 = 32; size2 = 8; source_line = 807 };
        { entry_index = 6; opcode_bytes = [0xEC]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 32; argument2 = 37; size1 = 8; size2 = 16; source_line = 808 };
        { entry_index = 7; opcode_bytes = [0xED]; flags = 0x001; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 33; argument2 = 37; size1 = 16; size2 = 16; source_line = 809 };
        { entry_index = 8; opcode_bytes = [0xED]; flags = 0x002; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 34; argument2 = 37; size1 = 32; size2 = 16; source_line = 810 };
        ];
      aliases =
        [
        ];
      source_line = 801 };
    { spelling = "INS";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x6C]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 16; argument2 = 37; size1 = 8; size2 = 16; source_line = 812 };
        { entry_index = 1; opcode_bytes = [0x6D]; flags = 0x001; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 17; argument2 = 37; size1 = 16; size2 = 16; source_line = 813 };
        { entry_index = 2; opcode_bytes = [0x6D]; flags = 0x002; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 18; argument2 = 37; size1 = 32; size2 = 16; source_line = 814 };
        ];
      aliases =
        [
        ];
      source_line = 811 };
    { spelling = "INSB";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x6C]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 815 };
        ];
      aliases =
        [
        ];
      source_line = 815 };
    { spelling = "INSW";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x6D]; flags = 0x001; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 816 };
        ];
      aliases =
        [
        ];
      source_line = 816 };
    { spelling = "INSD";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x6D]; flags = 0x002; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 817 };
        ];
      aliases =
        [
        ];
      source_line = 817 };
    { spelling = "INTO";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xCE]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 818 };
        ];
      aliases =
        [
        ];
      source_line = 818 };
    { spelling = "INT3";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xCC]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 819 };
        ];
      aliases =
        [
        { spelling = "BPT"; source_line = 819 };
        ];
      source_line = 819 };
    { spelling = "INT";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xCD]; flags = 0x010; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 5; argument1 = 8; argument2 = 0; size1 = 8; size2 = 0; source_line = 821 };
        { entry_index = 1; opcode_bytes = [0xCD]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 5; argument1 = 4; argument2 = 0; size1 = 8; size2 = 0; source_line = 822 };
        ];
      aliases =
        [
        ];
      source_line = 820 };
    { spelling = "INVD";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x08]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 823 };
        ];
      aliases =
        [
        ];
      source_line = 823 };
    { spelling = "IRET";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xCF]; flags = 0x042; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 824 };
        ];
      aliases =
        [
        ];
      source_line = 824 };
    { spelling = "LAHF";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x9F]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 825 };
        ];
      aliases =
        [
        ];
      source_line = 825 };
    { spelling = "LAR";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x02]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 13; argument2 = 17; size1 = 16; size2 = 16; source_line = 827 };
        { entry_index = 1; opcode_bytes = [0x0F; 0x02]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 14; argument2 = 18; size1 = 32; size2 = 32; source_line = 828 };
        { entry_index = 2; opcode_bytes = [0x0F; 0x02]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 15; argument2 = 19; size1 = 64; size2 = 64; source_line = 829 };
        ];
      aliases =
        [
        ];
      source_line = 826 };
    { spelling = "LEA";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x8D]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 13; argument2 = 17; size1 = 16; size2 = 16; source_line = 831 };
        { entry_index = 1; opcode_bytes = [0x8D]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 14; argument2 = 18; size1 = 32; size2 = 32; source_line = 832 };
        { entry_index = 2; opcode_bytes = [0x8D]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 15; argument2 = 19; size1 = 64; size2 = 64; source_line = 833 };
        ];
      aliases =
        [
        ];
      source_line = 830 };
    { spelling = "LEAVE";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xC9]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 834 };
        ];
      aliases =
        [
        ];
      source_line = 834 };
    { spelling = "LGDT";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x01]; flags = 0x001; slash_value = 2; uasm_slash_value = 2; opcode_modifier = 0; argument1 = 21; argument2 = 0; size1 = 16; size2 = 0; source_line = 836 };
        { entry_index = 1; opcode_bytes = [0x0F; 0x01]; flags = 0x002; slash_value = 2; uasm_slash_value = 2; opcode_modifier = 0; argument1 = 22; argument2 = 0; size1 = 32; size2 = 0; source_line = 837 };
        { entry_index = 2; opcode_bytes = [0x0F; 0x01]; flags = 0x002; slash_value = 2; uasm_slash_value = 2; opcode_modifier = 0; argument1 = 23; argument2 = 0; size1 = 64; size2 = 0; source_line = 838 };
        ];
      aliases =
        [
        ];
      source_line = 835 };
    { spelling = "SGDT";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x01]; flags = 0x001; slash_value = 0; uasm_slash_value = 0; opcode_modifier = 0; argument1 = 21; argument2 = 0; size1 = 16; size2 = 0; source_line = 840 };
        { entry_index = 1; opcode_bytes = [0x0F; 0x01]; flags = 0x002; slash_value = 0; uasm_slash_value = 0; opcode_modifier = 0; argument1 = 22; argument2 = 0; size1 = 32; size2 = 0; source_line = 841 };
        { entry_index = 2; opcode_bytes = [0x0F; 0x01]; flags = 0x002; slash_value = 0; uasm_slash_value = 0; opcode_modifier = 0; argument1 = 23; argument2 = 0; size1 = 64; size2 = 0; source_line = 842 };
        ];
      aliases =
        [
        ];
      source_line = 839 };
    { spelling = "LIDT";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x01]; flags = 0x001; slash_value = 3; uasm_slash_value = 3; opcode_modifier = 0; argument1 = 21; argument2 = 0; size1 = 16; size2 = 0; source_line = 844 };
        { entry_index = 1; opcode_bytes = [0x0F; 0x01]; flags = 0x002; slash_value = 3; uasm_slash_value = 3; opcode_modifier = 0; argument1 = 22; argument2 = 0; size1 = 32; size2 = 0; source_line = 845 };
        { entry_index = 2; opcode_bytes = [0x0F; 0x01]; flags = 0x002; slash_value = 3; uasm_slash_value = 3; opcode_modifier = 0; argument1 = 23; argument2 = 0; size1 = 64; size2 = 0; source_line = 846 };
        ];
      aliases =
        [
        ];
      source_line = 843 };
    { spelling = "SIDT";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x01]; flags = 0x001; slash_value = 1; uasm_slash_value = 1; opcode_modifier = 0; argument1 = 21; argument2 = 0; size1 = 16; size2 = 0; source_line = 848 };
        { entry_index = 1; opcode_bytes = [0x0F; 0x01]; flags = 0x002; slash_value = 1; uasm_slash_value = 1; opcode_modifier = 0; argument1 = 22; argument2 = 0; size1 = 32; size2 = 0; source_line = 849 };
        { entry_index = 2; opcode_bytes = [0x0F; 0x01]; flags = 0x002; slash_value = 1; uasm_slash_value = 1; opcode_modifier = 0; argument1 = 23; argument2 = 0; size1 = 64; size2 = 0; source_line = 850 };
        ];
      aliases =
        [
        ];
      source_line = 847 };
    { spelling = "LLDT";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x00]; flags = 0x000; slash_value = 2; uasm_slash_value = 2; opcode_modifier = 0; argument1 = 17; argument2 = 0; size1 = 16; size2 = 0; source_line = 852 };
        ];
      aliases =
        [
        ];
      source_line = 851 };
    { spelling = "SLDT";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x00]; flags = 0x001; slash_value = 0; uasm_slash_value = 0; opcode_modifier = 0; argument1 = 17; argument2 = 0; size1 = 16; size2 = 0; source_line = 854 };
        { entry_index = 1; opcode_bytes = [0x0F; 0x00]; flags = 0x002; slash_value = 0; uasm_slash_value = 0; opcode_modifier = 0; argument1 = 18; argument2 = 0; size1 = 32; size2 = 0; source_line = 855 };
        { entry_index = 2; opcode_bytes = [0x0F; 0x00]; flags = 0x002; slash_value = 0; uasm_slash_value = 0; opcode_modifier = 0; argument1 = 19; argument2 = 0; size1 = 64; size2 = 0; source_line = 856 };
        ];
      aliases =
        [
        ];
      source_line = 853 };
    { spelling = "LMSW";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x01]; flags = 0x000; slash_value = 6; uasm_slash_value = 6; opcode_modifier = 0; argument1 = 17; argument2 = 0; size1 = 16; size2 = 0; source_line = 858 };
        ];
      aliases =
        [
        ];
      source_line = 857 };
    { spelling = "SMSW";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x01]; flags = 0x001; slash_value = 4; uasm_slash_value = 4; opcode_modifier = 0; argument1 = 17; argument2 = 0; size1 = 16; size2 = 0; source_line = 860 };
        { entry_index = 1; opcode_bytes = [0x0F; 0x01]; flags = 0x002; slash_value = 4; uasm_slash_value = 4; opcode_modifier = 0; argument1 = 18; argument2 = 0; size1 = 32; size2 = 0; source_line = 861 };
        { entry_index = 2; opcode_bytes = [0x0F; 0x01]; flags = 0x002; slash_value = 4; uasm_slash_value = 4; opcode_modifier = 0; argument1 = 19; argument2 = 0; size1 = 64; size2 = 0; source_line = 862 };
        ];
      aliases =
        [
        ];
      source_line = 859 };
    { spelling = "LOCK";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xF0]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 864 };
        ];
      aliases =
        [
        ];
      source_line = 864 };
    { spelling = "LODSB";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xAC]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 865 };
        ];
      aliases =
        [
        ];
      source_line = 865 };
    { spelling = "LODSW";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xAD]; flags = 0x001; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 866 };
        ];
      aliases =
        [
        ];
      source_line = 866 };
    { spelling = "LODSD";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xAD]; flags = 0x002; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 867 };
        ];
      aliases =
        [
        ];
      source_line = 867 };
    { spelling = "LODSQ";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xAD]; flags = 0x042; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 868 };
        ];
      aliases =
        [
        ];
      source_line = 868 };
    { spelling = "LOOP";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xE2]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 1; argument1 = 1; argument2 = 0; size1 = 8; size2 = 0; source_line = 869 };
        ];
      aliases =
        [
        ];
      source_line = 869 };
    { spelling = "LOOPE";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xE1]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 1; argument1 = 1; argument2 = 0; size1 = 8; size2 = 0; source_line = 870 };
        ];
      aliases =
        [
        { spelling = "LOOPZ"; source_line = 870 };
        ];
      source_line = 870 };
    { spelling = "LOOPNE";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xE0]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 1; argument1 = 1; argument2 = 0; size1 = 8; size2 = 0; source_line = 871 };
        ];
      aliases =
        [
        { spelling = "LOOPNZ"; source_line = 871 };
        ];
      source_line = 871 };
    { spelling = "LSL";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x03]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 13; argument2 = 17; size1 = 16; size2 = 16; source_line = 873 };
        { entry_index = 1; opcode_bytes = [0x0F; 0x03]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 14; argument2 = 18; size1 = 32; size2 = 32; source_line = 874 };
        { entry_index = 2; opcode_bytes = [0x0F; 0x03]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 15; argument2 = 19; size1 = 64; size2 = 64; source_line = 875 };
        ];
      aliases =
        [
        ];
      source_line = 872 };
    { spelling = "LTR";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x00]; flags = 0x000; slash_value = 3; uasm_slash_value = 3; opcode_modifier = 0; argument1 = 17; argument2 = 0; size1 = 16; size2 = 0; source_line = 877 };
        ];
      aliases =
        [
        ];
      source_line = 876 };
    { spelling = "MOVSB";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xA4]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 878 };
        ];
      aliases =
        [
        ];
      source_line = 878 };
    { spelling = "MOVSW";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xA5]; flags = 0x001; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 879 };
        ];
      aliases =
        [
        ];
      source_line = 879 };
    { spelling = "MOVSD";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xA5]; flags = 0x002; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 880 };
        ];
      aliases =
        [
        ];
      source_line = 880 };
    { spelling = "MOVSQ";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xA5]; flags = 0x042; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 881 };
        ];
      aliases =
        [
        ];
      source_line = 881 };
    { spelling = "MOVSX";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0xBE]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 13; argument2 = 16; size1 = 16; size2 = 8; source_line = 883 };
        { entry_index = 1; opcode_bytes = [0x0F; 0xBE]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 14; argument2 = 16; size1 = 32; size2 = 8; source_line = 884 };
        { entry_index = 2; opcode_bytes = [0x0F; 0xBE]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 15; argument2 = 16; size1 = 64; size2 = 8; source_line = 885 };
        { entry_index = 3; opcode_bytes = [0x0F; 0xBF]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 14; argument2 = 17; size1 = 32; size2 = 16; source_line = 886 };
        { entry_index = 4; opcode_bytes = [0x0F; 0xBF]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 15; argument2 = 17; size1 = 64; size2 = 16; source_line = 887 };
        ];
      aliases =
        [
        ];
      source_line = 882 };
    { spelling = "MOVSXD";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x63]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 15; argument2 = 18; size1 = 64; size2 = 32; source_line = 889 };
        ];
      aliases =
        [
        ];
      source_line = 888 };
    { spelling = "MOVZX";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0xB6]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 13; argument2 = 16; size1 = 16; size2 = 8; source_line = 891 };
        { entry_index = 1; opcode_bytes = [0x0F; 0xB6]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 14; argument2 = 16; size1 = 32; size2 = 8; source_line = 892 };
        { entry_index = 2; opcode_bytes = [0x0F; 0xB6]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 15; argument2 = 16; size1 = 64; size2 = 8; source_line = 893 };
        { entry_index = 3; opcode_bytes = [0x0F; 0xB7]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 14; argument2 = 17; size1 = 32; size2 = 16; source_line = 894 };
        { entry_index = 4; opcode_bytes = [0x0F; 0xB7]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 15; argument2 = 17; size1 = 64; size2 = 16; source_line = 895 };
        ];
      aliases =
        [
        ];
      source_line = 890 };
    { spelling = "OUT";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xE6]; flags = 0x010; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 5; argument1 = 8; argument2 = 32; size1 = 8; size2 = 8; source_line = 897 };
        { entry_index = 1; opcode_bytes = [0xE6]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 5; argument1 = 4; argument2 = 32; size1 = 8; size2 = 8; source_line = 898 };
        { entry_index = 2; opcode_bytes = [0xE7]; flags = 0x011; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 5; argument1 = 8; argument2 = 33; size1 = 8; size2 = 16; source_line = 899 };
        { entry_index = 3; opcode_bytes = [0xE7]; flags = 0x001; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 5; argument1 = 4; argument2 = 33; size1 = 8; size2 = 16; source_line = 900 };
        { entry_index = 4; opcode_bytes = [0xE7]; flags = 0x012; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 5; argument1 = 8; argument2 = 34; size1 = 8; size2 = 32; source_line = 901 };
        { entry_index = 5; opcode_bytes = [0xE7]; flags = 0x002; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 5; argument1 = 4; argument2 = 34; size1 = 8; size2 = 32; source_line = 902 };
        { entry_index = 6; opcode_bytes = [0xEE]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 37; argument2 = 32; size1 = 16; size2 = 8; source_line = 903 };
        { entry_index = 7; opcode_bytes = [0xEF]; flags = 0x001; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 37; argument2 = 33; size1 = 16; size2 = 16; source_line = 904 };
        { entry_index = 8; opcode_bytes = [0xEF]; flags = 0x002; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 37; argument2 = 34; size1 = 16; size2 = 32; source_line = 905 };
        ];
      aliases =
        [
        ];
      source_line = 896 };
    { spelling = "OUTSB";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x6E]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 906 };
        ];
      aliases =
        [
        ];
      source_line = 906 };
    { spelling = "OUTSW";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x6F]; flags = 0x001; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 907 };
        ];
      aliases =
        [
        ];
      source_line = 907 };
    { spelling = "OUTSD";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x6F]; flags = 0x002; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 908 };
        ];
      aliases =
        [
        ];
      source_line = 908 };
    { spelling = "REP_INSB";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xF3; 0x6C]; flags = 0x020; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 910 };
        { entry_index = 1; opcode_bytes = [0xF3; 0x48; 0x6C]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 911 };
        ];
      aliases =
        [
        ];
      source_line = 909 };
    { spelling = "REP_INSW";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xF3; 0x6D]; flags = 0x001; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 912 };
        ];
      aliases =
        [
        ];
      source_line = 912 };
    { spelling = "REP_INSD";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xF3; 0x6D]; flags = 0x002; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 913 };
        ];
      aliases =
        [
        ];
      source_line = 913 };
    { spelling = "REP_MOVSB";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xF3; 0xA4]; flags = 0x020; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 915 };
        { entry_index = 1; opcode_bytes = [0xF3; 0x48; 0xA4]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 916 };
        ];
      aliases =
        [
        ];
      source_line = 914 };
    { spelling = "REP_MOVSW";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xF3; 0xA5]; flags = 0x001; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 917 };
        ];
      aliases =
        [
        ];
      source_line = 917 };
    { spelling = "REP_MOVSD";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xF3; 0xA5]; flags = 0x002; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 918 };
        ];
      aliases =
        [
        ];
      source_line = 918 };
    { spelling = "REP_MOVSQ";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xF3; 0x48; 0xA5]; flags = 0x002; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 919 };
        ];
      aliases =
        [
        ];
      source_line = 919 };
    { spelling = "REP_OUTSB";
      instructions =
        [
        { entry_index = 0; opcode_bytes = []; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 920 };
        { entry_index = 1; opcode_bytes = [0xF3; 0x6E]; flags = 0x020; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 921 };
        { entry_index = 2; opcode_bytes = [0xF3; 0x48; 0x6E]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 922 };
        ];
      aliases =
        [
        ];
      source_line = 920 };
    { spelling = "REP_OUTSW";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xF3; 0x6F]; flags = 0x001; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 923 };
        ];
      aliases =
        [
        ];
      source_line = 923 };
    { spelling = "REP_OUTSD";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xF3; 0x6F]; flags = 0x002; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 924 };
        ];
      aliases =
        [
        ];
      source_line = 924 };
    { spelling = "REP_LODSB";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xF2; 0xAC]; flags = 0x020; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 926 };
        { entry_index = 1; opcode_bytes = [0xF2; 0x48; 0xAC]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 927 };
        ];
      aliases =
        [
        ];
      source_line = 925 };
    { spelling = "REP_LODSW";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xF2; 0xAD]; flags = 0x001; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 928 };
        ];
      aliases =
        [
        ];
      source_line = 928 };
    { spelling = "REP_LODSD";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xF2; 0xAD]; flags = 0x002; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 929 };
        ];
      aliases =
        [
        ];
      source_line = 929 };
    { spelling = "REP_LODSQ";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xF2; 0x48; 0xAD]; flags = 0x002; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 930 };
        ];
      aliases =
        [
        ];
      source_line = 930 };
    { spelling = "REP_STOSB";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xF3; 0xAA]; flags = 0x020; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 932 };
        { entry_index = 1; opcode_bytes = [0xF3; 0x48; 0xAA]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 933 };
        ];
      aliases =
        [
        ];
      source_line = 931 };
    { spelling = "REP_STOSW";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xF3; 0xAB]; flags = 0x001; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 934 };
        ];
      aliases =
        [
        ];
      source_line = 934 };
    { spelling = "REP_STOSD";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xF3; 0xAB]; flags = 0x002; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 935 };
        ];
      aliases =
        [
        ];
      source_line = 935 };
    { spelling = "REP_STOSQ";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xF3; 0x48; 0xAB]; flags = 0x002; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 936 };
        ];
      aliases =
        [
        ];
      source_line = 936 };
    { spelling = "REPE_CMPSB";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xF3; 0xA6]; flags = 0x020; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 938 };
        { entry_index = 1; opcode_bytes = [0xF3; 0x48; 0xA6]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 939 };
        ];
      aliases =
        [
        ];
      source_line = 937 };
    { spelling = "REPE_CMPSW";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xF3; 0xA7]; flags = 0x001; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 940 };
        ];
      aliases =
        [
        ];
      source_line = 940 };
    { spelling = "REPE_CMPSD";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xF3; 0xA7]; flags = 0x002; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 941 };
        ];
      aliases =
        [
        ];
      source_line = 941 };
    { spelling = "REPE_CMPSQ";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xF3; 0x48; 0xA7]; flags = 0x002; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 942 };
        ];
      aliases =
        [
        ];
      source_line = 942 };
    { spelling = "REPE_SCASB";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xF3; 0xAE]; flags = 0x020; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 944 };
        { entry_index = 1; opcode_bytes = [0xF3; 0x48; 0xAE]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 945 };
        ];
      aliases =
        [
        ];
      source_line = 943 };
    { spelling = "REPE_SCASW";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xF3; 0xAF]; flags = 0x001; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 946 };
        ];
      aliases =
        [
        ];
      source_line = 946 };
    { spelling = "REPE_SCASD";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xF3; 0xAF]; flags = 0x002; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 947 };
        ];
      aliases =
        [
        ];
      source_line = 947 };
    { spelling = "REPE_SCASQ";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xF3; 0x48; 0xAF]; flags = 0x002; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 948 };
        ];
      aliases =
        [
        ];
      source_line = 948 };
    { spelling = "REPNE_CMPSB";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xF2; 0xA6]; flags = 0x020; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 950 };
        { entry_index = 1; opcode_bytes = [0xF2; 0x48; 0xA6]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 951 };
        ];
      aliases =
        [
        ];
      source_line = 949 };
    { spelling = "REPNE_CMPSW";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xF2; 0xA7]; flags = 0x001; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 952 };
        ];
      aliases =
        [
        ];
      source_line = 952 };
    { spelling = "REPNE_CMPSD";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xF2; 0xA7]; flags = 0x002; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 953 };
        ];
      aliases =
        [
        ];
      source_line = 953 };
    { spelling = "REPNE_CMPSQ";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xF2; 0x48; 0xA7]; flags = 0x002; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 954 };
        ];
      aliases =
        [
        ];
      source_line = 954 };
    { spelling = "REPNE_SCASB";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xF2; 0xAE]; flags = 0x020; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 956 };
        { entry_index = 1; opcode_bytes = [0xF2; 0x48; 0xAE]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 957 };
        ];
      aliases =
        [
        ];
      source_line = 955 };
    { spelling = "REPNE_SCASW";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xF2; 0xAF]; flags = 0x001; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 958 };
        ];
      aliases =
        [
        ];
      source_line = 958 };
    { spelling = "REPNE_SCASD";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xF2; 0xAF]; flags = 0x002; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 959 };
        ];
      aliases =
        [
        ];
      source_line = 959 };
    { spelling = "REPNE_SCASQ";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xF2; 0x48; 0xAF]; flags = 0x002; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 960 };
        ];
      aliases =
        [
        ];
      source_line = 960 };
    { spelling = "RET";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xC3]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 961 };
        ];
      aliases =
        [
        ];
      source_line = 961 };
    { spelling = "RET1";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xC2]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 6; argument1 = 5; argument2 = 0; size1 = 16; size2 = 0; source_line = 962 };
        ];
      aliases =
        [
        ];
      source_line = 962 };
    { spelling = "RETF";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xCB]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 963 };
        ];
      aliases =
        [
        ];
      source_line = 963 };
    { spelling = "RETF1";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xCA]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 6; argument1 = 5; argument2 = 0; size1 = 16; size2 = 0; source_line = 964 };
        ];
      aliases =
        [
        ];
      source_line = 964 };
    { spelling = "REX";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x48]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 965 };
        ];
      aliases =
        [
        ];
      source_line = 965 };
    { spelling = "REX2";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x40]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 966 };
        ];
      aliases =
        [
        ];
      source_line = 966 };
    { spelling = "RSM";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0xAA]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 967 };
        ];
      aliases =
        [
        ];
      source_line = 967 };
    { spelling = "SAHF";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x9E]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 968 };
        ];
      aliases =
        [
        ];
      source_line = 968 };
    { spelling = "SCASB";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xAE]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 969 };
        ];
      aliases =
        [
        ];
      source_line = 969 };
    { spelling = "SCASW";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xAF]; flags = 0x001; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 970 };
        ];
      aliases =
        [
        ];
      source_line = 970 };
    { spelling = "SCASD";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xAF]; flags = 0x002; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 971 };
        ];
      aliases =
        [
        ];
      source_line = 971 };
    { spelling = "SCASQ";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xAF]; flags = 0x042; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 972 };
        ];
      aliases =
        [
        ];
      source_line = 972 };
    { spelling = "SEGCS";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x2E]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 973 };
        ];
      aliases =
        [
        ];
      source_line = 973 };
    { spelling = "SEGSS";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x36]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 974 };
        ];
      aliases =
        [
        ];
      source_line = 974 };
    { spelling = "SEGDS";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x3E]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 975 };
        ];
      aliases =
        [
        ];
      source_line = 975 };
    { spelling = "SEGES";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x26]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 976 };
        ];
      aliases =
        [
        ];
      source_line = 976 };
    { spelling = "SEGFS";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x64]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 977 };
        ];
      aliases =
        [
        ];
      source_line = 977 };
    { spelling = "SEGGS";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x65]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 978 };
        ];
      aliases =
        [
        ];
      source_line = 978 };
    { spelling = "SETO";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x90]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 16; argument2 = 0; size1 = 8; size2 = 0; source_line = 979 };
        ];
      aliases =
        [
        ];
      source_line = 979 };
    { spelling = "SETNO";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x91]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 16; argument2 = 0; size1 = 8; size2 = 0; source_line = 980 };
        ];
      aliases =
        [
        ];
      source_line = 980 };
    { spelling = "SETB";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x92]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 16; argument2 = 0; size1 = 8; size2 = 0; source_line = 981 };
        ];
      aliases =
        [
        { spelling = "SETC"; source_line = 981 };
        { spelling = "SETNAE"; source_line = 981 };
        ];
      source_line = 981 };
    { spelling = "SETAE";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x93]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 16; argument2 = 0; size1 = 8; size2 = 0; source_line = 982 };
        ];
      aliases =
        [
        { spelling = "SETNC"; source_line = 982 };
        { spelling = "SETNB"; source_line = 982 };
        ];
      source_line = 982 };
    { spelling = "SETE";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x94]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 16; argument2 = 0; size1 = 8; size2 = 0; source_line = 983 };
        ];
      aliases =
        [
        { spelling = "SETZ"; source_line = 983 };
        ];
      source_line = 983 };
    { spelling = "SETNE";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x95]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 16; argument2 = 0; size1 = 8; size2 = 0; source_line = 984 };
        ];
      aliases =
        [
        { spelling = "SETNZ"; source_line = 984 };
        ];
      source_line = 984 };
    { spelling = "SETBE";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x96]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 16; argument2 = 0; size1 = 8; size2 = 0; source_line = 985 };
        ];
      aliases =
        [
        { spelling = "SETNA"; source_line = 985 };
        ];
      source_line = 985 };
    { spelling = "SETA";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x97]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 16; argument2 = 0; size1 = 8; size2 = 0; source_line = 986 };
        ];
      aliases =
        [
        { spelling = "SETNBE"; source_line = 986 };
        ];
      source_line = 986 };
    { spelling = "SETS";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x98]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 16; argument2 = 0; size1 = 8; size2 = 0; source_line = 987 };
        ];
      aliases =
        [
        ];
      source_line = 987 };
    { spelling = "SETNS";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x99]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 16; argument2 = 0; size1 = 8; size2 = 0; source_line = 988 };
        ];
      aliases =
        [
        ];
      source_line = 988 };
    { spelling = "SETP";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x9A]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 16; argument2 = 0; size1 = 8; size2 = 0; source_line = 989 };
        ];
      aliases =
        [
        { spelling = "SETPE"; source_line = 989 };
        ];
      source_line = 989 };
    { spelling = "SETNP";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x9B]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 16; argument2 = 0; size1 = 8; size2 = 0; source_line = 990 };
        ];
      aliases =
        [
        { spelling = "SETPO"; source_line = 990 };
        ];
      source_line = 990 };
    { spelling = "SETL";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x9C]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 16; argument2 = 0; size1 = 8; size2 = 0; source_line = 991 };
        ];
      aliases =
        [
        { spelling = "SETNGE"; source_line = 991 };
        ];
      source_line = 991 };
    { spelling = "SETGE";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x9D]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 16; argument2 = 0; size1 = 8; size2 = 0; source_line = 992 };
        ];
      aliases =
        [
        { spelling = "SETNL"; source_line = 992 };
        ];
      source_line = 992 };
    { spelling = "SETLE";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x9E]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 16; argument2 = 0; size1 = 8; size2 = 0; source_line = 993 };
        ];
      aliases =
        [
        { spelling = "SETNG"; source_line = 993 };
        ];
      source_line = 993 };
    { spelling = "SETG";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x9F]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 16; argument2 = 0; size1 = 8; size2 = 0; source_line = 994 };
        ];
      aliases =
        [
        { spelling = "SETNLE"; source_line = 994 };
        ];
      source_line = 994 };
    { spelling = "SHLD";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0xA5]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 17; argument2 = 13; size1 = 16; size2 = 16; source_line = 996 };
        { entry_index = 1; opcode_bytes = [0x0F; 0xA5]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 18; argument2 = 14; size1 = 32; size2 = 32; source_line = 997 };
        { entry_index = 2; opcode_bytes = [0x0F; 0xA5]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 19; argument2 = 15; size1 = 64; size2 = 64; source_line = 998 };
        ];
      aliases =
        [
        ];
      source_line = 995 };
    { spelling = "SHRD";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0xAD]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 17; argument2 = 13; size1 = 16; size2 = 16; source_line = 1000 };
        { entry_index = 1; opcode_bytes = [0x0F; 0xAD]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 18; argument2 = 14; size1 = 32; size2 = 32; source_line = 1001 };
        { entry_index = 2; opcode_bytes = [0x0F; 0xAD]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 19; argument2 = 15; size1 = 64; size2 = 64; source_line = 1002 };
        ];
      aliases =
        [
        ];
      source_line = 999 };
    { spelling = "STC";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xF9]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1003 };
        ];
      aliases =
        [
        ];
      source_line = 1003 };
    { spelling = "STD";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xFD]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1004 };
        ];
      aliases =
        [
        ];
      source_line = 1004 };
    { spelling = "STI";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xFB]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1005 };
        ];
      aliases =
        [
        ];
      source_line = 1005 };
    { spelling = "STOSB";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xAA]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1006 };
        ];
      aliases =
        [
        ];
      source_line = 1006 };
    { spelling = "STOSW";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xAB]; flags = 0x001; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1007 };
        ];
      aliases =
        [
        ];
      source_line = 1007 };
    { spelling = "STOSD";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xAB]; flags = 0x002; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1008 };
        ];
      aliases =
        [
        ];
      source_line = 1008 };
    { spelling = "STOSQ";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xAB]; flags = 0x042; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1009 };
        ];
      aliases =
        [
        ];
      source_line = 1009 };
    { spelling = "STR";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x00]; flags = 0x001; slash_value = 1; uasm_slash_value = 1; opcode_modifier = 0; argument1 = 17; argument2 = 0; size1 = 16; size2 = 0; source_line = 1011 };
        { entry_index = 1; opcode_bytes = [0x0F; 0x00]; flags = 0x002; slash_value = 1; uasm_slash_value = 1; opcode_modifier = 0; argument1 = 18; argument2 = 0; size1 = 32; size2 = 0; source_line = 1012 };
        { entry_index = 2; opcode_bytes = [0x0F; 0x00]; flags = 0x002; slash_value = 1; uasm_slash_value = 1; opcode_modifier = 0; argument1 = 19; argument2 = 0; size1 = 64; size2 = 0; source_line = 1013 };
        ];
      aliases =
        [
        ];
      source_line = 1010 };
    { spelling = "VERR";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x00]; flags = 0x001; slash_value = 4; uasm_slash_value = 4; opcode_modifier = 0; argument1 = 17; argument2 = 0; size1 = 16; size2 = 0; source_line = 1015 };
        { entry_index = 1; opcode_bytes = [0x0F; 0x00]; flags = 0x002; slash_value = 4; uasm_slash_value = 4; opcode_modifier = 0; argument1 = 18; argument2 = 0; size1 = 32; size2 = 0; source_line = 1016 };
        { entry_index = 2; opcode_bytes = [0x0F; 0x00]; flags = 0x002; slash_value = 4; uasm_slash_value = 4; opcode_modifier = 0; argument1 = 19; argument2 = 0; size1 = 64; size2 = 0; source_line = 1017 };
        ];
      aliases =
        [
        ];
      source_line = 1014 };
    { spelling = "VERW";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x00]; flags = 0x001; slash_value = 5; uasm_slash_value = 5; opcode_modifier = 0; argument1 = 17; argument2 = 0; size1 = 16; size2 = 0; source_line = 1019 };
        { entry_index = 1; opcode_bytes = [0x0F; 0x00]; flags = 0x002; slash_value = 5; uasm_slash_value = 5; opcode_modifier = 0; argument1 = 18; argument2 = 0; size1 = 32; size2 = 0; source_line = 1020 };
        { entry_index = 2; opcode_bytes = [0x0F; 0x00]; flags = 0x002; slash_value = 5; uasm_slash_value = 5; opcode_modifier = 0; argument1 = 19; argument2 = 0; size1 = 64; size2 = 0; source_line = 1021 };
        ];
      aliases =
        [
        ];
      source_line = 1018 };
    { spelling = "WAIT";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x9B]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1022 };
        ];
      aliases =
        [
        ];
      source_line = 1022 };
    { spelling = "FWAIT";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x9B]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1023 };
        ];
      aliases =
        [
        ];
      source_line = 1023 };
    { spelling = "XADD";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0xC0]; flags = 0x000; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 16; argument2 = 12; size1 = 8; size2 = 8; source_line = 1025 };
        { entry_index = 1; opcode_bytes = [0x0F; 0xC1]; flags = 0x001; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 17; argument2 = 13; size1 = 16; size2 = 16; source_line = 1026 };
        { entry_index = 2; opcode_bytes = [0x0F; 0xC1]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 18; argument2 = 14; size1 = 32; size2 = 32; source_line = 1027 };
        { entry_index = 3; opcode_bytes = [0x0F; 0xC1]; flags = 0x002; slash_value = 8; uasm_slash_value = 8; opcode_modifier = 0; argument1 = 19; argument2 = 15; size1 = 64; size2 = 64; source_line = 1028 };
        ];
      aliases =
        [
        ];
      source_line = 1024 };
    { spelling = "XLATB";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD7]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1029 };
        ];
      aliases =
        [
        ];
      source_line = 1029 };
    { spelling = "ROL";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD2]; flags = 0x000; slash_value = 0; uasm_slash_value = 0; opcode_modifier = 0; argument1 = 16; argument2 = 36; size1 = 8; size2 = 8; source_line = 1032 };
        { entry_index = 1; opcode_bytes = [0xD3]; flags = 0x001; slash_value = 0; uasm_slash_value = 0; opcode_modifier = 0; argument1 = 17; argument2 = 36; size1 = 16; size2 = 8; source_line = 1033 };
        { entry_index = 2; opcode_bytes = [0xD3]; flags = 0x002; slash_value = 0; uasm_slash_value = 0; opcode_modifier = 0; argument1 = 18; argument2 = 36; size1 = 32; size2 = 8; source_line = 1034 };
        { entry_index = 3; opcode_bytes = [0xD3]; flags = 0x002; slash_value = 0; uasm_slash_value = 0; opcode_modifier = 0; argument1 = 19; argument2 = 36; size1 = 64; size2 = 8; source_line = 1035 };
        { entry_index = 4; opcode_bytes = [0xC0]; flags = 0x010; slash_value = 0; uasm_slash_value = 0; opcode_modifier = 5; argument1 = 16; argument2 = 8; size1 = 8; size2 = 8; source_line = 1036 };
        { entry_index = 5; opcode_bytes = [0xC0]; flags = 0x000; slash_value = 0; uasm_slash_value = 0; opcode_modifier = 5; argument1 = 16; argument2 = 4; size1 = 8; size2 = 8; source_line = 1037 };
        { entry_index = 6; opcode_bytes = [0xC1]; flags = 0x011; slash_value = 0; uasm_slash_value = 0; opcode_modifier = 5; argument1 = 17; argument2 = 8; size1 = 16; size2 = 8; source_line = 1038 };
        { entry_index = 7; opcode_bytes = [0xC1]; flags = 0x001; slash_value = 0; uasm_slash_value = 0; opcode_modifier = 5; argument1 = 17; argument2 = 4; size1 = 16; size2 = 8; source_line = 1039 };
        { entry_index = 8; opcode_bytes = [0xC1]; flags = 0x012; slash_value = 0; uasm_slash_value = 0; opcode_modifier = 5; argument1 = 18; argument2 = 8; size1 = 32; size2 = 8; source_line = 1040 };
        { entry_index = 9; opcode_bytes = [0xC1]; flags = 0x002; slash_value = 0; uasm_slash_value = 0; opcode_modifier = 5; argument1 = 18; argument2 = 4; size1 = 32; size2 = 8; source_line = 1041 };
        { entry_index = 10; opcode_bytes = [0xC1]; flags = 0x012; slash_value = 0; uasm_slash_value = 0; opcode_modifier = 5; argument1 = 19; argument2 = 8; size1 = 64; size2 = 8; source_line = 1042 };
        { entry_index = 11; opcode_bytes = [0xC1]; flags = 0x002; slash_value = 0; uasm_slash_value = 0; opcode_modifier = 5; argument1 = 19; argument2 = 4; size1 = 64; size2 = 8; source_line = 1043 };
        ];
      aliases =
        [
        ];
      source_line = 1031 };
    { spelling = "ROL1";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD0]; flags = 0x000; slash_value = 0; uasm_slash_value = 0; opcode_modifier = 0; argument1 = 16; argument2 = 0; size1 = 8; size2 = 0; source_line = 1045 };
        { entry_index = 1; opcode_bytes = [0xD1]; flags = 0x001; slash_value = 0; uasm_slash_value = 0; opcode_modifier = 0; argument1 = 17; argument2 = 0; size1 = 16; size2 = 0; source_line = 1046 };
        { entry_index = 2; opcode_bytes = [0xD1]; flags = 0x002; slash_value = 0; uasm_slash_value = 0; opcode_modifier = 0; argument1 = 18; argument2 = 0; size1 = 32; size2 = 0; source_line = 1047 };
        { entry_index = 3; opcode_bytes = [0xD1]; flags = 0x002; slash_value = 0; uasm_slash_value = 0; opcode_modifier = 0; argument1 = 19; argument2 = 0; size1 = 64; size2 = 0; source_line = 1048 };
        ];
      aliases =
        [
        ];
      source_line = 1044 };
    { spelling = "ROR";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD2]; flags = 0x000; slash_value = 1; uasm_slash_value = 1; opcode_modifier = 0; argument1 = 16; argument2 = 36; size1 = 8; size2 = 8; source_line = 1050 };
        { entry_index = 1; opcode_bytes = [0xD3]; flags = 0x001; slash_value = 1; uasm_slash_value = 1; opcode_modifier = 0; argument1 = 17; argument2 = 36; size1 = 16; size2 = 8; source_line = 1051 };
        { entry_index = 2; opcode_bytes = [0xD3]; flags = 0x002; slash_value = 1; uasm_slash_value = 1; opcode_modifier = 0; argument1 = 18; argument2 = 36; size1 = 32; size2 = 8; source_line = 1052 };
        { entry_index = 3; opcode_bytes = [0xD3]; flags = 0x002; slash_value = 1; uasm_slash_value = 1; opcode_modifier = 0; argument1 = 19; argument2 = 36; size1 = 64; size2 = 8; source_line = 1053 };
        { entry_index = 4; opcode_bytes = [0xC0]; flags = 0x010; slash_value = 1; uasm_slash_value = 1; opcode_modifier = 5; argument1 = 16; argument2 = 8; size1 = 8; size2 = 8; source_line = 1054 };
        { entry_index = 5; opcode_bytes = [0xC0]; flags = 0x000; slash_value = 1; uasm_slash_value = 1; opcode_modifier = 5; argument1 = 16; argument2 = 4; size1 = 8; size2 = 8; source_line = 1055 };
        { entry_index = 6; opcode_bytes = [0xC1]; flags = 0x011; slash_value = 1; uasm_slash_value = 1; opcode_modifier = 5; argument1 = 17; argument2 = 8; size1 = 16; size2 = 8; source_line = 1056 };
        { entry_index = 7; opcode_bytes = [0xC1]; flags = 0x001; slash_value = 1; uasm_slash_value = 1; opcode_modifier = 5; argument1 = 17; argument2 = 4; size1 = 16; size2 = 8; source_line = 1057 };
        { entry_index = 8; opcode_bytes = [0xC1]; flags = 0x012; slash_value = 1; uasm_slash_value = 1; opcode_modifier = 5; argument1 = 18; argument2 = 8; size1 = 32; size2 = 8; source_line = 1058 };
        { entry_index = 9; opcode_bytes = [0xC1]; flags = 0x002; slash_value = 1; uasm_slash_value = 1; opcode_modifier = 5; argument1 = 18; argument2 = 4; size1 = 32; size2 = 8; source_line = 1059 };
        { entry_index = 10; opcode_bytes = [0xC1]; flags = 0x012; slash_value = 1; uasm_slash_value = 1; opcode_modifier = 5; argument1 = 19; argument2 = 8; size1 = 64; size2 = 8; source_line = 1060 };
        { entry_index = 11; opcode_bytes = [0xC1]; flags = 0x002; slash_value = 1; uasm_slash_value = 1; opcode_modifier = 5; argument1 = 19; argument2 = 4; size1 = 64; size2 = 8; source_line = 1061 };
        ];
      aliases =
        [
        ];
      source_line = 1049 };
    { spelling = "ROR1";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD0]; flags = 0x000; slash_value = 1; uasm_slash_value = 1; opcode_modifier = 0; argument1 = 16; argument2 = 0; size1 = 8; size2 = 0; source_line = 1063 };
        { entry_index = 1; opcode_bytes = [0xD1]; flags = 0x001; slash_value = 1; uasm_slash_value = 1; opcode_modifier = 0; argument1 = 17; argument2 = 0; size1 = 16; size2 = 0; source_line = 1064 };
        { entry_index = 2; opcode_bytes = [0xD1]; flags = 0x002; slash_value = 1; uasm_slash_value = 1; opcode_modifier = 0; argument1 = 18; argument2 = 0; size1 = 32; size2 = 0; source_line = 1065 };
        { entry_index = 3; opcode_bytes = [0xD1]; flags = 0x002; slash_value = 1; uasm_slash_value = 1; opcode_modifier = 0; argument1 = 19; argument2 = 0; size1 = 64; size2 = 0; source_line = 1066 };
        ];
      aliases =
        [
        ];
      source_line = 1062 };
    { spelling = "RCL";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD2]; flags = 0x000; slash_value = 2; uasm_slash_value = 2; opcode_modifier = 0; argument1 = 16; argument2 = 36; size1 = 8; size2 = 8; source_line = 1068 };
        { entry_index = 1; opcode_bytes = [0xD3]; flags = 0x001; slash_value = 2; uasm_slash_value = 2; opcode_modifier = 0; argument1 = 17; argument2 = 36; size1 = 16; size2 = 8; source_line = 1069 };
        { entry_index = 2; opcode_bytes = [0xD3]; flags = 0x002; slash_value = 2; uasm_slash_value = 2; opcode_modifier = 0; argument1 = 18; argument2 = 36; size1 = 32; size2 = 8; source_line = 1070 };
        { entry_index = 3; opcode_bytes = [0xD3]; flags = 0x002; slash_value = 2; uasm_slash_value = 2; opcode_modifier = 0; argument1 = 19; argument2 = 36; size1 = 64; size2 = 8; source_line = 1071 };
        { entry_index = 4; opcode_bytes = [0xC0]; flags = 0x010; slash_value = 2; uasm_slash_value = 2; opcode_modifier = 5; argument1 = 16; argument2 = 8; size1 = 8; size2 = 8; source_line = 1072 };
        { entry_index = 5; opcode_bytes = [0xC0]; flags = 0x000; slash_value = 2; uasm_slash_value = 2; opcode_modifier = 5; argument1 = 16; argument2 = 4; size1 = 8; size2 = 8; source_line = 1073 };
        { entry_index = 6; opcode_bytes = [0xC1]; flags = 0x011; slash_value = 2; uasm_slash_value = 2; opcode_modifier = 5; argument1 = 17; argument2 = 8; size1 = 16; size2 = 8; source_line = 1074 };
        { entry_index = 7; opcode_bytes = [0xC1]; flags = 0x001; slash_value = 2; uasm_slash_value = 2; opcode_modifier = 5; argument1 = 17; argument2 = 4; size1 = 16; size2 = 8; source_line = 1075 };
        { entry_index = 8; opcode_bytes = [0xC1]; flags = 0x012; slash_value = 2; uasm_slash_value = 2; opcode_modifier = 5; argument1 = 18; argument2 = 8; size1 = 32; size2 = 8; source_line = 1076 };
        { entry_index = 9; opcode_bytes = [0xC1]; flags = 0x002; slash_value = 2; uasm_slash_value = 2; opcode_modifier = 5; argument1 = 18; argument2 = 4; size1 = 32; size2 = 8; source_line = 1077 };
        { entry_index = 10; opcode_bytes = [0xC1]; flags = 0x012; slash_value = 2; uasm_slash_value = 2; opcode_modifier = 5; argument1 = 19; argument2 = 8; size1 = 64; size2 = 8; source_line = 1078 };
        { entry_index = 11; opcode_bytes = [0xC1]; flags = 0x002; slash_value = 2; uasm_slash_value = 2; opcode_modifier = 5; argument1 = 19; argument2 = 4; size1 = 64; size2 = 8; source_line = 1079 };
        ];
      aliases =
        [
        ];
      source_line = 1067 };
    { spelling = "RCL1";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD0]; flags = 0x000; slash_value = 2; uasm_slash_value = 2; opcode_modifier = 0; argument1 = 16; argument2 = 0; size1 = 8; size2 = 0; source_line = 1081 };
        { entry_index = 1; opcode_bytes = [0xD1]; flags = 0x001; slash_value = 2; uasm_slash_value = 2; opcode_modifier = 0; argument1 = 17; argument2 = 0; size1 = 16; size2 = 0; source_line = 1082 };
        { entry_index = 2; opcode_bytes = [0xD1]; flags = 0x002; slash_value = 2; uasm_slash_value = 2; opcode_modifier = 0; argument1 = 18; argument2 = 0; size1 = 32; size2 = 0; source_line = 1083 };
        { entry_index = 3; opcode_bytes = [0xD1]; flags = 0x002; slash_value = 2; uasm_slash_value = 2; opcode_modifier = 0; argument1 = 19; argument2 = 0; size1 = 64; size2 = 0; source_line = 1084 };
        ];
      aliases =
        [
        ];
      source_line = 1080 };
    { spelling = "RCR";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD2]; flags = 0x000; slash_value = 3; uasm_slash_value = 3; opcode_modifier = 0; argument1 = 16; argument2 = 36; size1 = 8; size2 = 8; source_line = 1086 };
        { entry_index = 1; opcode_bytes = [0xD3]; flags = 0x001; slash_value = 3; uasm_slash_value = 3; opcode_modifier = 0; argument1 = 17; argument2 = 36; size1 = 16; size2 = 8; source_line = 1087 };
        { entry_index = 2; opcode_bytes = [0xD3]; flags = 0x002; slash_value = 3; uasm_slash_value = 3; opcode_modifier = 0; argument1 = 18; argument2 = 36; size1 = 32; size2 = 8; source_line = 1088 };
        { entry_index = 3; opcode_bytes = [0xD3]; flags = 0x002; slash_value = 3; uasm_slash_value = 3; opcode_modifier = 0; argument1 = 19; argument2 = 36; size1 = 64; size2 = 8; source_line = 1089 };
        { entry_index = 4; opcode_bytes = [0xC0]; flags = 0x010; slash_value = 3; uasm_slash_value = 3; opcode_modifier = 5; argument1 = 16; argument2 = 8; size1 = 8; size2 = 8; source_line = 1090 };
        { entry_index = 5; opcode_bytes = [0xC0]; flags = 0x000; slash_value = 3; uasm_slash_value = 3; opcode_modifier = 5; argument1 = 16; argument2 = 4; size1 = 8; size2 = 8; source_line = 1091 };
        { entry_index = 6; opcode_bytes = [0xC1]; flags = 0x011; slash_value = 3; uasm_slash_value = 3; opcode_modifier = 5; argument1 = 17; argument2 = 8; size1 = 16; size2 = 8; source_line = 1092 };
        { entry_index = 7; opcode_bytes = [0xC1]; flags = 0x001; slash_value = 3; uasm_slash_value = 3; opcode_modifier = 5; argument1 = 17; argument2 = 4; size1 = 16; size2 = 8; source_line = 1093 };
        { entry_index = 8; opcode_bytes = [0xC1]; flags = 0x012; slash_value = 3; uasm_slash_value = 3; opcode_modifier = 5; argument1 = 18; argument2 = 8; size1 = 32; size2 = 8; source_line = 1094 };
        { entry_index = 9; opcode_bytes = [0xC1]; flags = 0x002; slash_value = 3; uasm_slash_value = 3; opcode_modifier = 5; argument1 = 18; argument2 = 4; size1 = 32; size2 = 8; source_line = 1095 };
        { entry_index = 10; opcode_bytes = [0xC1]; flags = 0x012; slash_value = 3; uasm_slash_value = 3; opcode_modifier = 5; argument1 = 19; argument2 = 8; size1 = 64; size2 = 8; source_line = 1096 };
        { entry_index = 11; opcode_bytes = [0xC1]; flags = 0x002; slash_value = 3; uasm_slash_value = 3; opcode_modifier = 5; argument1 = 19; argument2 = 4; size1 = 64; size2 = 8; source_line = 1097 };
        ];
      aliases =
        [
        ];
      source_line = 1085 };
    { spelling = "RCR1";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD0]; flags = 0x000; slash_value = 3; uasm_slash_value = 3; opcode_modifier = 0; argument1 = 16; argument2 = 0; size1 = 8; size2 = 0; source_line = 1099 };
        { entry_index = 1; opcode_bytes = [0xD1]; flags = 0x001; slash_value = 3; uasm_slash_value = 3; opcode_modifier = 0; argument1 = 17; argument2 = 0; size1 = 16; size2 = 0; source_line = 1100 };
        { entry_index = 2; opcode_bytes = [0xD1]; flags = 0x002; slash_value = 3; uasm_slash_value = 3; opcode_modifier = 0; argument1 = 18; argument2 = 0; size1 = 32; size2 = 0; source_line = 1101 };
        { entry_index = 3; opcode_bytes = [0xD1]; flags = 0x002; slash_value = 3; uasm_slash_value = 3; opcode_modifier = 0; argument1 = 19; argument2 = 0; size1 = 64; size2 = 0; source_line = 1102 };
        ];
      aliases =
        [
        ];
      source_line = 1098 };
    { spelling = "SHL";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD2]; flags = 0x000; slash_value = 4; uasm_slash_value = 4; opcode_modifier = 0; argument1 = 16; argument2 = 36; size1 = 8; size2 = 8; source_line = 1104 };
        { entry_index = 1; opcode_bytes = [0xD3]; flags = 0x001; slash_value = 4; uasm_slash_value = 4; opcode_modifier = 0; argument1 = 17; argument2 = 36; size1 = 16; size2 = 8; source_line = 1105 };
        { entry_index = 2; opcode_bytes = [0xD3]; flags = 0x002; slash_value = 4; uasm_slash_value = 4; opcode_modifier = 0; argument1 = 18; argument2 = 36; size1 = 32; size2 = 8; source_line = 1106 };
        { entry_index = 3; opcode_bytes = [0xD3]; flags = 0x002; slash_value = 4; uasm_slash_value = 4; opcode_modifier = 0; argument1 = 19; argument2 = 36; size1 = 64; size2 = 8; source_line = 1107 };
        { entry_index = 4; opcode_bytes = [0xC0]; flags = 0x010; slash_value = 4; uasm_slash_value = 4; opcode_modifier = 5; argument1 = 16; argument2 = 8; size1 = 8; size2 = 8; source_line = 1108 };
        { entry_index = 5; opcode_bytes = [0xC0]; flags = 0x000; slash_value = 4; uasm_slash_value = 4; opcode_modifier = 5; argument1 = 16; argument2 = 4; size1 = 8; size2 = 8; source_line = 1109 };
        { entry_index = 6; opcode_bytes = [0xC1]; flags = 0x011; slash_value = 4; uasm_slash_value = 4; opcode_modifier = 5; argument1 = 17; argument2 = 8; size1 = 16; size2 = 8; source_line = 1110 };
        { entry_index = 7; opcode_bytes = [0xC1]; flags = 0x001; slash_value = 4; uasm_slash_value = 4; opcode_modifier = 5; argument1 = 17; argument2 = 4; size1 = 16; size2 = 8; source_line = 1111 };
        { entry_index = 8; opcode_bytes = [0xC1]; flags = 0x012; slash_value = 4; uasm_slash_value = 4; opcode_modifier = 5; argument1 = 18; argument2 = 8; size1 = 32; size2 = 8; source_line = 1112 };
        { entry_index = 9; opcode_bytes = [0xC1]; flags = 0x002; slash_value = 4; uasm_slash_value = 4; opcode_modifier = 5; argument1 = 18; argument2 = 4; size1 = 32; size2 = 8; source_line = 1113 };
        { entry_index = 10; opcode_bytes = [0xC1]; flags = 0x012; slash_value = 4; uasm_slash_value = 4; opcode_modifier = 5; argument1 = 19; argument2 = 8; size1 = 64; size2 = 8; source_line = 1114 };
        { entry_index = 11; opcode_bytes = [0xC1]; flags = 0x002; slash_value = 4; uasm_slash_value = 4; opcode_modifier = 5; argument1 = 19; argument2 = 4; size1 = 64; size2 = 8; source_line = 1115 };
        ];
      aliases =
        [
        { spelling = "SAL"; source_line = 1115 };
        ];
      source_line = 1103 };
    { spelling = "SHL1";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD0]; flags = 0x000; slash_value = 4; uasm_slash_value = 4; opcode_modifier = 0; argument1 = 16; argument2 = 0; size1 = 8; size2 = 0; source_line = 1117 };
        { entry_index = 1; opcode_bytes = [0xD1]; flags = 0x001; slash_value = 4; uasm_slash_value = 4; opcode_modifier = 0; argument1 = 17; argument2 = 0; size1 = 16; size2 = 0; source_line = 1118 };
        { entry_index = 2; opcode_bytes = [0xD1]; flags = 0x002; slash_value = 4; uasm_slash_value = 4; opcode_modifier = 0; argument1 = 18; argument2 = 0; size1 = 32; size2 = 0; source_line = 1119 };
        { entry_index = 3; opcode_bytes = [0xD1]; flags = 0x002; slash_value = 4; uasm_slash_value = 4; opcode_modifier = 0; argument1 = 19; argument2 = 0; size1 = 64; size2 = 0; source_line = 1120 };
        ];
      aliases =
        [
        { spelling = "SAL1"; source_line = 1120 };
        ];
      source_line = 1116 };
    { spelling = "SHR";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD2]; flags = 0x000; slash_value = 5; uasm_slash_value = 5; opcode_modifier = 0; argument1 = 16; argument2 = 36; size1 = 8; size2 = 8; source_line = 1122 };
        { entry_index = 1; opcode_bytes = [0xD3]; flags = 0x001; slash_value = 5; uasm_slash_value = 5; opcode_modifier = 0; argument1 = 17; argument2 = 36; size1 = 16; size2 = 8; source_line = 1123 };
        { entry_index = 2; opcode_bytes = [0xD3]; flags = 0x002; slash_value = 5; uasm_slash_value = 5; opcode_modifier = 0; argument1 = 18; argument2 = 36; size1 = 32; size2 = 8; source_line = 1124 };
        { entry_index = 3; opcode_bytes = [0xD3]; flags = 0x002; slash_value = 5; uasm_slash_value = 5; opcode_modifier = 0; argument1 = 19; argument2 = 36; size1 = 64; size2 = 8; source_line = 1125 };
        { entry_index = 4; opcode_bytes = [0xC0]; flags = 0x010; slash_value = 5; uasm_slash_value = 5; opcode_modifier = 5; argument1 = 16; argument2 = 8; size1 = 8; size2 = 8; source_line = 1126 };
        { entry_index = 5; opcode_bytes = [0xC0]; flags = 0x000; slash_value = 5; uasm_slash_value = 5; opcode_modifier = 5; argument1 = 16; argument2 = 4; size1 = 8; size2 = 8; source_line = 1127 };
        { entry_index = 6; opcode_bytes = [0xC1]; flags = 0x011; slash_value = 5; uasm_slash_value = 5; opcode_modifier = 5; argument1 = 17; argument2 = 8; size1 = 16; size2 = 8; source_line = 1128 };
        { entry_index = 7; opcode_bytes = [0xC1]; flags = 0x001; slash_value = 5; uasm_slash_value = 5; opcode_modifier = 5; argument1 = 17; argument2 = 4; size1 = 16; size2 = 8; source_line = 1129 };
        { entry_index = 8; opcode_bytes = [0xC1]; flags = 0x012; slash_value = 5; uasm_slash_value = 5; opcode_modifier = 5; argument1 = 18; argument2 = 8; size1 = 32; size2 = 8; source_line = 1130 };
        { entry_index = 9; opcode_bytes = [0xC1]; flags = 0x002; slash_value = 5; uasm_slash_value = 5; opcode_modifier = 5; argument1 = 18; argument2 = 4; size1 = 32; size2 = 8; source_line = 1131 };
        { entry_index = 10; opcode_bytes = [0xC1]; flags = 0x012; slash_value = 5; uasm_slash_value = 5; opcode_modifier = 5; argument1 = 19; argument2 = 8; size1 = 64; size2 = 8; source_line = 1132 };
        { entry_index = 11; opcode_bytes = [0xC1]; flags = 0x002; slash_value = 5; uasm_slash_value = 5; opcode_modifier = 5; argument1 = 19; argument2 = 4; size1 = 64; size2 = 8; source_line = 1133 };
        ];
      aliases =
        [
        ];
      source_line = 1121 };
    { spelling = "SHR1";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD0]; flags = 0x000; slash_value = 5; uasm_slash_value = 5; opcode_modifier = 0; argument1 = 16; argument2 = 0; size1 = 8; size2 = 0; source_line = 1135 };
        { entry_index = 1; opcode_bytes = [0xD1]; flags = 0x001; slash_value = 5; uasm_slash_value = 5; opcode_modifier = 0; argument1 = 17; argument2 = 0; size1 = 16; size2 = 0; source_line = 1136 };
        { entry_index = 2; opcode_bytes = [0xD1]; flags = 0x002; slash_value = 5; uasm_slash_value = 5; opcode_modifier = 0; argument1 = 18; argument2 = 0; size1 = 32; size2 = 0; source_line = 1137 };
        { entry_index = 3; opcode_bytes = [0xD1]; flags = 0x002; slash_value = 5; uasm_slash_value = 5; opcode_modifier = 0; argument1 = 19; argument2 = 0; size1 = 64; size2 = 0; source_line = 1138 };
        ];
      aliases =
        [
        ];
      source_line = 1134 };
    { spelling = "SAR";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD2]; flags = 0x000; slash_value = 7; uasm_slash_value = 7; opcode_modifier = 0; argument1 = 16; argument2 = 36; size1 = 8; size2 = 8; source_line = 1140 };
        { entry_index = 1; opcode_bytes = [0xD3]; flags = 0x001; slash_value = 7; uasm_slash_value = 7; opcode_modifier = 0; argument1 = 17; argument2 = 36; size1 = 16; size2 = 8; source_line = 1141 };
        { entry_index = 2; opcode_bytes = [0xD3]; flags = 0x002; slash_value = 7; uasm_slash_value = 7; opcode_modifier = 0; argument1 = 18; argument2 = 36; size1 = 32; size2 = 8; source_line = 1142 };
        { entry_index = 3; opcode_bytes = [0xD3]; flags = 0x002; slash_value = 7; uasm_slash_value = 7; opcode_modifier = 0; argument1 = 19; argument2 = 36; size1 = 64; size2 = 8; source_line = 1143 };
        { entry_index = 4; opcode_bytes = [0xC0]; flags = 0x010; slash_value = 7; uasm_slash_value = 7; opcode_modifier = 5; argument1 = 16; argument2 = 8; size1 = 8; size2 = 8; source_line = 1144 };
        { entry_index = 5; opcode_bytes = [0xC0]; flags = 0x000; slash_value = 7; uasm_slash_value = 7; opcode_modifier = 5; argument1 = 16; argument2 = 4; size1 = 8; size2 = 8; source_line = 1145 };
        { entry_index = 6; opcode_bytes = [0xC1]; flags = 0x011; slash_value = 7; uasm_slash_value = 7; opcode_modifier = 5; argument1 = 17; argument2 = 8; size1 = 16; size2 = 8; source_line = 1146 };
        { entry_index = 7; opcode_bytes = [0xC1]; flags = 0x001; slash_value = 7; uasm_slash_value = 7; opcode_modifier = 5; argument1 = 17; argument2 = 4; size1 = 16; size2 = 8; source_line = 1147 };
        { entry_index = 8; opcode_bytes = [0xC1]; flags = 0x012; slash_value = 7; uasm_slash_value = 7; opcode_modifier = 5; argument1 = 18; argument2 = 8; size1 = 32; size2 = 8; source_line = 1148 };
        { entry_index = 9; opcode_bytes = [0xC1]; flags = 0x002; slash_value = 7; uasm_slash_value = 7; opcode_modifier = 5; argument1 = 18; argument2 = 4; size1 = 32; size2 = 8; source_line = 1149 };
        { entry_index = 10; opcode_bytes = [0xC1]; flags = 0x012; slash_value = 7; uasm_slash_value = 7; opcode_modifier = 5; argument1 = 19; argument2 = 8; size1 = 64; size2 = 8; source_line = 1150 };
        { entry_index = 11; opcode_bytes = [0xC1]; flags = 0x002; slash_value = 7; uasm_slash_value = 7; opcode_modifier = 5; argument1 = 19; argument2 = 4; size1 = 64; size2 = 8; source_line = 1151 };
        ];
      aliases =
        [
        ];
      source_line = 1139 };
    { spelling = "SAR1";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD0]; flags = 0x000; slash_value = 7; uasm_slash_value = 7; opcode_modifier = 0; argument1 = 16; argument2 = 0; size1 = 8; size2 = 0; source_line = 1153 };
        { entry_index = 1; opcode_bytes = [0xD1]; flags = 0x001; slash_value = 7; uasm_slash_value = 7; opcode_modifier = 0; argument1 = 17; argument2 = 0; size1 = 16; size2 = 0; source_line = 1154 };
        { entry_index = 2; opcode_bytes = [0xD1]; flags = 0x002; slash_value = 7; uasm_slash_value = 7; opcode_modifier = 0; argument1 = 18; argument2 = 0; size1 = 32; size2 = 0; source_line = 1155 };
        { entry_index = 3; opcode_bytes = [0xD1]; flags = 0x002; slash_value = 7; uasm_slash_value = 7; opcode_modifier = 0; argument1 = 19; argument2 = 0; size1 = 64; size2 = 0; source_line = 1156 };
        ];
      aliases =
        [
        ];
      source_line = 1152 };
    { spelling = "FILD";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xDF]; flags = 0x000; slash_value = 0; uasm_slash_value = 0; opcode_modifier = 0; argument1 = 21; argument2 = 0; size1 = 16; size2 = 0; source_line = 1159 };
        { entry_index = 1; opcode_bytes = [0xDB]; flags = 0x000; slash_value = 0; uasm_slash_value = 0; opcode_modifier = 0; argument1 = 22; argument2 = 0; size1 = 32; size2 = 0; source_line = 1160 };
        { entry_index = 2; opcode_bytes = [0xDF]; flags = 0x080; slash_value = 5; uasm_slash_value = 5; opcode_modifier = 0; argument1 = 23; argument2 = 0; size1 = 64; size2 = 0; source_line = 1161 };
        ];
      aliases =
        [
        ];
      source_line = 1158 };
    { spelling = "FISTP";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xDF]; flags = 0x080; slash_value = 7; uasm_slash_value = 7; opcode_modifier = 0; argument1 = 23; argument2 = 0; size1 = 64; size2 = 0; source_line = 1163 };
        ];
      aliases =
        [
        ];
      source_line = 1162 };
    { spelling = "FISTTP";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xDD]; flags = 0x080; slash_value = 1; uasm_slash_value = 1; opcode_modifier = 0; argument1 = 23; argument2 = 0; size1 = 64; size2 = 0; source_line = 1165 };
        ];
      aliases =
        [
        ];
      source_line = 1164 };
    { spelling = "FLD";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD9]; flags = 0x000; slash_value = 0; uasm_slash_value = 0; opcode_modifier = 0; argument1 = 22; argument2 = 0; size1 = 32; size2 = 0; source_line = 1167 };
        { entry_index = 1; opcode_bytes = [0xDD]; flags = 0x080; slash_value = 0; uasm_slash_value = 0; opcode_modifier = 0; argument1 = 23; argument2 = 0; size1 = 64; size2 = 0; source_line = 1168 };
        { entry_index = 2; opcode_bytes = [0xD9; 0xC0]; flags = 0x204; slash_value = 9; uasm_slash_value = 9; opcode_modifier = 0; argument1 = 47; argument2 = 0; size1 = 0; size2 = 0; source_line = 1169 };
        ];
      aliases =
        [
        ];
      source_line = 1166 };
    { spelling = "FSTP";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD9]; flags = 0x000; slash_value = 3; uasm_slash_value = 3; opcode_modifier = 0; argument1 = 22; argument2 = 0; size1 = 32; size2 = 0; source_line = 1171 };
        { entry_index = 1; opcode_bytes = [0xDD]; flags = 0x080; slash_value = 3; uasm_slash_value = 3; opcode_modifier = 0; argument1 = 23; argument2 = 0; size1 = 64; size2 = 0; source_line = 1172 };
        { entry_index = 2; opcode_bytes = [0xDD; 0xD8]; flags = 0x204; slash_value = 9; uasm_slash_value = 9; opcode_modifier = 0; argument1 = 47; argument2 = 0; size1 = 0; size2 = 0; source_line = 1173 };
        ];
      aliases =
        [
        ];
      source_line = 1170 };
    { spelling = "FST";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD9]; flags = 0x000; slash_value = 2; uasm_slash_value = 2; opcode_modifier = 0; argument1 = 22; argument2 = 0; size1 = 32; size2 = 0; source_line = 1175 };
        { entry_index = 1; opcode_bytes = [0xDD]; flags = 0x080; slash_value = 2; uasm_slash_value = 2; opcode_modifier = 0; argument1 = 23; argument2 = 0; size1 = 64; size2 = 0; source_line = 1176 };
        { entry_index = 2; opcode_bytes = [0xDD; 0xD0]; flags = 0x204; slash_value = 9; uasm_slash_value = 9; opcode_modifier = 0; argument1 = 47; argument2 = 0; size1 = 0; size2 = 0; source_line = 1177 };
        ];
      aliases =
        [
        ];
      source_line = 1174 };
    { spelling = "FRSTOR";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xDD]; flags = 0x000; slash_value = 4; uasm_slash_value = 4; opcode_modifier = 0; argument1 = 22; argument2 = 0; size1 = 32; size2 = 0; source_line = 1179 };
        { entry_index = 1; opcode_bytes = [0xDD]; flags = 0x000; slash_value = 4; uasm_slash_value = 4; opcode_modifier = 0; argument1 = 23; argument2 = 0; size1 = 64; size2 = 0; source_line = 1180 };
        ];
      aliases =
        [
        ];
      source_line = 1178 };
    { spelling = "FSAVE";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xDD]; flags = 0x000; slash_value = 6; uasm_slash_value = 6; opcode_modifier = 0; argument1 = 22; argument2 = 0; size1 = 32; size2 = 0; source_line = 1182 };
        { entry_index = 1; opcode_bytes = [0xDD]; flags = 0x000; slash_value = 6; uasm_slash_value = 6; opcode_modifier = 0; argument1 = 23; argument2 = 0; size1 = 64; size2 = 0; source_line = 1183 };
        ];
      aliases =
        [
        ];
      source_line = 1181 };
    { spelling = "FYL2X";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD9; 0xF1]; flags = 0x200; slash_value = 11; uasm_slash_value = 10; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1185 };
        ];
      aliases =
        [
        ];
      source_line = 1185 };
    { spelling = "FYL2XP1";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD9; 0xF9]; flags = 0x200; slash_value = 11; uasm_slash_value = 10; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1186 };
        ];
      aliases =
        [
        ];
      source_line = 1186 };
    { spelling = "F2XM1";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD9; 0xF0]; flags = 0x200; slash_value = 11; uasm_slash_value = 10; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1187 };
        ];
      aliases =
        [
        ];
      source_line = 1187 };
    { spelling = "FABS";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD9; 0xE1]; flags = 0x200; slash_value = 11; uasm_slash_value = 10; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1188 };
        ];
      aliases =
        [
        ];
      source_line = 1188 };
    { spelling = "FCHS";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD9; 0xE0]; flags = 0x200; slash_value = 11; uasm_slash_value = 10; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1189 };
        ];
      aliases =
        [
        ];
      source_line = 1189 };
    { spelling = "FSIN";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD9; 0xFE]; flags = 0x200; slash_value = 11; uasm_slash_value = 10; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1190 };
        ];
      aliases =
        [
        ];
      source_line = 1190 };
    { spelling = "FCOS";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD9; 0xFF]; flags = 0x200; slash_value = 11; uasm_slash_value = 10; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1191 };
        ];
      aliases =
        [
        ];
      source_line = 1191 };
    { spelling = "FPTAN";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD9; 0xF2]; flags = 0x200; slash_value = 11; uasm_slash_value = 10; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1192 };
        ];
      aliases =
        [
        ];
      source_line = 1192 };
    { spelling = "FPATAN";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD9; 0xF3]; flags = 0x200; slash_value = 11; uasm_slash_value = 10; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1193 };
        ];
      aliases =
        [
        ];
      source_line = 1193 };
    { spelling = "FSQRT";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD9; 0xFA]; flags = 0x200; slash_value = 11; uasm_slash_value = 10; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1194 };
        ];
      aliases =
        [
        ];
      source_line = 1194 };
    { spelling = "FMULP";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xDE; 0xC8]; flags = 0x204; slash_value = 9; uasm_slash_value = 9; opcode_modifier = 0; argument1 = 47; argument2 = 46; size1 = 0; size2 = 0; source_line = 1195 };
        ];
      aliases =
        [
        ];
      source_line = 1195 };
    { spelling = "FMUL";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD8]; flags = 0x000; slash_value = 1; uasm_slash_value = 1; opcode_modifier = 0; argument1 = 46; argument2 = 22; size1 = 0; size2 = 32; source_line = 1197 };
        { entry_index = 1; opcode_bytes = [0xDC]; flags = 0x080; slash_value = 1; uasm_slash_value = 1; opcode_modifier = 0; argument1 = 46; argument2 = 23; size1 = 0; size2 = 64; source_line = 1198 };
        { entry_index = 2; opcode_bytes = [0xD8; 0xC8]; flags = 0x204; slash_value = 9; uasm_slash_value = 9; opcode_modifier = 0; argument1 = 46; argument2 = 47; size1 = 0; size2 = 0; source_line = 1199 };
        { entry_index = 3; opcode_bytes = [0xDC; 0xC8]; flags = 0x204; slash_value = 9; uasm_slash_value = 9; opcode_modifier = 0; argument1 = 47; argument2 = 46; size1 = 0; size2 = 0; source_line = 1200 };
        ];
      aliases =
        [
        ];
      source_line = 1196 };
    { spelling = "FIMUL";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xDA]; flags = 0x000; slash_value = 1; uasm_slash_value = 1; opcode_modifier = 0; argument1 = 46; argument2 = 22; size1 = 0; size2 = 32; source_line = 1202 };
        { entry_index = 1; opcode_bytes = [0xDE]; flags = 0x000; slash_value = 1; uasm_slash_value = 1; opcode_modifier = 0; argument1 = 46; argument2 = 21; size1 = 0; size2 = 16; source_line = 1203 };
        ];
      aliases =
        [
        ];
      source_line = 1201 };
    { spelling = "FDIVP";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xDE; 0xF8]; flags = 0x204; slash_value = 9; uasm_slash_value = 9; opcode_modifier = 0; argument1 = 47; argument2 = 46; size1 = 0; size2 = 0; source_line = 1204 };
        ];
      aliases =
        [
        ];
      source_line = 1204 };
    { spelling = "FDIV";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD8]; flags = 0x000; slash_value = 6; uasm_slash_value = 6; opcode_modifier = 0; argument1 = 46; argument2 = 22; size1 = 0; size2 = 32; source_line = 1206 };
        { entry_index = 1; opcode_bytes = [0xDC]; flags = 0x080; slash_value = 6; uasm_slash_value = 6; opcode_modifier = 0; argument1 = 46; argument2 = 23; size1 = 0; size2 = 64; source_line = 1207 };
        { entry_index = 2; opcode_bytes = [0xD8; 0xF0]; flags = 0x204; slash_value = 9; uasm_slash_value = 9; opcode_modifier = 0; argument1 = 46; argument2 = 47; size1 = 0; size2 = 0; source_line = 1208 };
        { entry_index = 3; opcode_bytes = [0xDC; 0xF8]; flags = 0x204; slash_value = 9; uasm_slash_value = 9; opcode_modifier = 0; argument1 = 47; argument2 = 46; size1 = 0; size2 = 0; source_line = 1209 };
        ];
      aliases =
        [
        ];
      source_line = 1205 };
    { spelling = "FDIVRP";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xDE; 0xF0]; flags = 0x204; slash_value = 9; uasm_slash_value = 9; opcode_modifier = 0; argument1 = 47; argument2 = 46; size1 = 0; size2 = 0; source_line = 1210 };
        ];
      aliases =
        [
        ];
      source_line = 1210 };
    { spelling = "FDIVR";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD8]; flags = 0x000; slash_value = 7; uasm_slash_value = 7; opcode_modifier = 0; argument1 = 46; argument2 = 22; size1 = 0; size2 = 32; source_line = 1212 };
        { entry_index = 1; opcode_bytes = [0xDC]; flags = 0x080; slash_value = 7; uasm_slash_value = 7; opcode_modifier = 0; argument1 = 46; argument2 = 23; size1 = 0; size2 = 64; source_line = 1213 };
        { entry_index = 2; opcode_bytes = [0xD8; 0xF8]; flags = 0x204; slash_value = 9; uasm_slash_value = 9; opcode_modifier = 0; argument1 = 46; argument2 = 47; size1 = 0; size2 = 0; source_line = 1214 };
        { entry_index = 3; opcode_bytes = [0xDC; 0xF0]; flags = 0x204; slash_value = 9; uasm_slash_value = 9; opcode_modifier = 0; argument1 = 47; argument2 = 46; size1 = 0; size2 = 0; source_line = 1215 };
        ];
      aliases =
        [
        ];
      source_line = 1211 };
    { spelling = "FPREM";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD9; 0xF8]; flags = 0x200; slash_value = 11; uasm_slash_value = 10; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1216 };
        ];
      aliases =
        [
        ];
      source_line = 1216 };
    { spelling = "FADDP";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xDE; 0xC0]; flags = 0x204; slash_value = 9; uasm_slash_value = 9; opcode_modifier = 0; argument1 = 47; argument2 = 46; size1 = 0; size2 = 0; source_line = 1217 };
        ];
      aliases =
        [
        ];
      source_line = 1217 };
    { spelling = "FADD";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD8]; flags = 0x000; slash_value = 0; uasm_slash_value = 0; opcode_modifier = 0; argument1 = 46; argument2 = 22; size1 = 0; size2 = 32; source_line = 1219 };
        { entry_index = 1; opcode_bytes = [0xDC]; flags = 0x080; slash_value = 0; uasm_slash_value = 0; opcode_modifier = 0; argument1 = 46; argument2 = 23; size1 = 0; size2 = 64; source_line = 1220 };
        { entry_index = 2; opcode_bytes = [0xD8; 0xC0]; flags = 0x204; slash_value = 9; uasm_slash_value = 9; opcode_modifier = 0; argument1 = 46; argument2 = 47; size1 = 0; size2 = 0; source_line = 1221 };
        { entry_index = 3; opcode_bytes = [0xDC; 0xC0]; flags = 0x204; slash_value = 9; uasm_slash_value = 9; opcode_modifier = 0; argument1 = 47; argument2 = 46; size1 = 0; size2 = 0; source_line = 1222 };
        ];
      aliases =
        [
        ];
      source_line = 1218 };
    { spelling = "FSUBP";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xDE; 0xE8]; flags = 0x204; slash_value = 9; uasm_slash_value = 9; opcode_modifier = 0; argument1 = 47; argument2 = 46; size1 = 0; size2 = 0; source_line = 1223 };
        ];
      aliases =
        [
        ];
      source_line = 1223 };
    { spelling = "FSUB";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD8]; flags = 0x000; slash_value = 4; uasm_slash_value = 4; opcode_modifier = 0; argument1 = 46; argument2 = 22; size1 = 0; size2 = 32; source_line = 1225 };
        { entry_index = 1; opcode_bytes = [0xDC]; flags = 0x080; slash_value = 4; uasm_slash_value = 4; opcode_modifier = 0; argument1 = 46; argument2 = 23; size1 = 0; size2 = 64; source_line = 1226 };
        { entry_index = 2; opcode_bytes = [0xD8; 0xE0]; flags = 0x204; slash_value = 9; uasm_slash_value = 9; opcode_modifier = 0; argument1 = 46; argument2 = 47; size1 = 0; size2 = 0; source_line = 1227 };
        { entry_index = 3; opcode_bytes = [0xDC; 0xE8]; flags = 0x204; slash_value = 9; uasm_slash_value = 9; opcode_modifier = 0; argument1 = 47; argument2 = 46; size1 = 0; size2 = 0; source_line = 1228 };
        ];
      aliases =
        [
        ];
      source_line = 1224 };
    { spelling = "FSUBRP";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xDE; 0xE0]; flags = 0x204; slash_value = 9; uasm_slash_value = 9; opcode_modifier = 0; argument1 = 47; argument2 = 46; size1 = 0; size2 = 0; source_line = 1229 };
        ];
      aliases =
        [
        ];
      source_line = 1229 };
    { spelling = "FSUBR";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD8]; flags = 0x000; slash_value = 5; uasm_slash_value = 5; opcode_modifier = 0; argument1 = 46; argument2 = 22; size1 = 0; size2 = 32; source_line = 1231 };
        { entry_index = 1; opcode_bytes = [0xDC]; flags = 0x080; slash_value = 5; uasm_slash_value = 5; opcode_modifier = 0; argument1 = 46; argument2 = 23; size1 = 0; size2 = 64; source_line = 1232 };
        { entry_index = 2; opcode_bytes = [0xD8; 0xE8]; flags = 0x204; slash_value = 9; uasm_slash_value = 9; opcode_modifier = 0; argument1 = 46; argument2 = 47; size1 = 0; size2 = 0; source_line = 1233 };
        { entry_index = 3; opcode_bytes = [0xDC; 0xE0]; flags = 0x204; slash_value = 9; uasm_slash_value = 9; opcode_modifier = 0; argument1 = 47; argument2 = 46; size1 = 0; size2 = 0; source_line = 1234 };
        ];
      aliases =
        [
        ];
      source_line = 1230 };
    { spelling = "FCOMIP";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xDF; 0xF0]; flags = 0x204; slash_value = 9; uasm_slash_value = 9; opcode_modifier = 0; argument1 = 46; argument2 = 47; size1 = 0; size2 = 0; source_line = 1235 };
        ];
      aliases =
        [
        ];
      source_line = 1235 };
    { spelling = "FCOMI";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xDB; 0xF0]; flags = 0x204; slash_value = 9; uasm_slash_value = 9; opcode_modifier = 0; argument1 = 46; argument2 = 47; size1 = 0; size2 = 0; source_line = 1236 };
        ];
      aliases =
        [
        ];
      source_line = 1236 };
    { spelling = "FCLEX";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x9B; 0xDB; 0xE2]; flags = 0x200; slash_value = 11; uasm_slash_value = 10; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1237 };
        ];
      aliases =
        [
        ];
      source_line = 1237 };
    { spelling = "FNCLEX";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xDB; 0xE2]; flags = 0x200; slash_value = 11; uasm_slash_value = 10; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1238 };
        ];
      aliases =
        [
        ];
      source_line = 1238 };
    { spelling = "FSTSW";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xDF; 0xE0]; flags = 0x200; slash_value = 11; uasm_slash_value = 10; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1239 };
        ];
      aliases =
        [
        ];
      source_line = 1239 };
    { spelling = "FDECSTP";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD9; 0xF6]; flags = 0x200; slash_value = 11; uasm_slash_value = 10; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1240 };
        ];
      aliases =
        [
        ];
      source_line = 1240 };
    { spelling = "FINCSTP";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD9; 0xF7]; flags = 0x200; slash_value = 11; uasm_slash_value = 10; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1241 };
        ];
      aliases =
        [
        ];
      source_line = 1241 };
    { spelling = "FFREE";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xDD; 0xC0]; flags = 0x204; slash_value = 9; uasm_slash_value = 9; opcode_modifier = 0; argument1 = 47; argument2 = 0; size1 = 0; size2 = 0; source_line = 1242 };
        ];
      aliases =
        [
        ];
      source_line = 1242 };
    { spelling = "FRNDINT";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD9; 0xFC]; flags = 0x200; slash_value = 11; uasm_slash_value = 10; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1243 };
        ];
      aliases =
        [
        ];
      source_line = 1243 };
    { spelling = "FSCALE";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD9; 0xFD]; flags = 0x200; slash_value = 11; uasm_slash_value = 10; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1244 };
        ];
      aliases =
        [
        ];
      source_line = 1244 };
    { spelling = "FXTRACT";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD9; 0xF4]; flags = 0x200; slash_value = 11; uasm_slash_value = 10; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1245 };
        ];
      aliases =
        [
        ];
      source_line = 1245 };
    { spelling = "FLD1";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD9; 0xE8]; flags = 0x200; slash_value = 11; uasm_slash_value = 10; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1247 };
        ];
      aliases =
        [
        ];
      source_line = 1247 };
    { spelling = "FLDL2T";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD9; 0xE9]; flags = 0x200; slash_value = 11; uasm_slash_value = 10; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1248 };
        ];
      aliases =
        [
        ];
      source_line = 1248 };
    { spelling = "FLDL2E";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD9; 0xEA]; flags = 0x200; slash_value = 11; uasm_slash_value = 10; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1249 };
        ];
      aliases =
        [
        ];
      source_line = 1249 };
    { spelling = "FLDPI";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD9; 0xEB]; flags = 0x200; slash_value = 11; uasm_slash_value = 10; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1250 };
        ];
      aliases =
        [
        ];
      source_line = 1250 };
    { spelling = "FLDLG2";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD9; 0xEC]; flags = 0x200; slash_value = 11; uasm_slash_value = 10; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1251 };
        ];
      aliases =
        [
        ];
      source_line = 1251 };
    { spelling = "FLDLN2";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD9; 0xED]; flags = 0x200; slash_value = 11; uasm_slash_value = 10; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1252 };
        ];
      aliases =
        [
        ];
      source_line = 1252 };
    { spelling = "FLDZ";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD9; 0xEE]; flags = 0x200; slash_value = 11; uasm_slash_value = 10; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1253 };
        ];
      aliases =
        [
        ];
      source_line = 1253 };
    { spelling = "FXCH";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD9; 0xC8]; flags = 0x204; slash_value = 9; uasm_slash_value = 9; opcode_modifier = 0; argument1 = 47; argument2 = 0; size1 = 0; size2 = 0; source_line = 1255 };
        ];
      aliases =
        [
        ];
      source_line = 1255 };
    { spelling = "FTST";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD9; 0xE4]; flags = 0x200; slash_value = 11; uasm_slash_value = 10; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1256 };
        ];
      aliases =
        [
        ];
      source_line = 1256 };
    { spelling = "FXAM";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD9; 0xE5]; flags = 0x200; slash_value = 11; uasm_slash_value = 10; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1257 };
        ];
      aliases =
        [
        ];
      source_line = 1257 };
    { spelling = "FINIT";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x9B; 0xDB; 0xE3]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1258 };
        ];
      aliases =
        [
        ];
      source_line = 1258 };
    { spelling = "FNINIT";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xDB; 0xE3]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1259 };
        ];
      aliases =
        [
        ];
      source_line = 1259 };
    { spelling = "FSTCW";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD9]; flags = 0x000; slash_value = 7; uasm_slash_value = 7; opcode_modifier = 0; argument1 = 21; argument2 = 0; size1 = 16; size2 = 0; source_line = 1262 };
        ];
      aliases =
        [
        ];
      source_line = 1261 };
    { spelling = "FLDCW";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xD9]; flags = 0x000; slash_value = 5; uasm_slash_value = 5; opcode_modifier = 0; argument1 = 21; argument2 = 0; size1 = 16; size2 = 0; source_line = 1264 };
        ];
      aliases =
        [
        ];
      source_line = 1263 };
    { spelling = "FXSAVE";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0xAE]; flags = 0x002; slash_value = 0; uasm_slash_value = 0; opcode_modifier = 0; argument1 = 22; argument2 = 0; size1 = 32; size2 = 0; source_line = 1266 };
        { entry_index = 1; opcode_bytes = [0x0F; 0xAE]; flags = 0x002; slash_value = 0; uasm_slash_value = 0; opcode_modifier = 0; argument1 = 23; argument2 = 0; size1 = 64; size2 = 0; source_line = 1267 };
        ];
      aliases =
        [
        ];
      source_line = 1265 };
    { spelling = "FXRSTOR";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0xAE]; flags = 0x002; slash_value = 1; uasm_slash_value = 1; opcode_modifier = 0; argument1 = 22; argument2 = 0; size1 = 32; size2 = 0; source_line = 1269 };
        { entry_index = 1; opcode_bytes = [0x0F; 0xAE]; flags = 0x002; slash_value = 1; uasm_slash_value = 1; opcode_modifier = 0; argument1 = 23; argument2 = 0; size1 = 64; size2 = 0; source_line = 1270 };
        ];
      aliases =
        [
        ];
      source_line = 1268 };
    { spelling = "WBINVD";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x09]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1272 };
        ];
      aliases =
        [
        ];
      source_line = 1272 };
    { spelling = "CLFLUSH";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0xAE]; flags = 0x000; slash_value = 7; uasm_slash_value = 7; opcode_modifier = 0; argument1 = 16; argument2 = 0; size1 = 8; size2 = 0; source_line = 1273 };
        ];
      aliases =
        [
        ];
      source_line = 1273 };
    { spelling = "INVLPG";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x01]; flags = 0x000; slash_value = 7; uasm_slash_value = 7; opcode_modifier = 0; argument1 = 16; argument2 = 0; size1 = 8; size2 = 0; source_line = 1274 };
        ];
      aliases =
        [
        ];
      source_line = 1274 };
    { spelling = "CPUID";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0xA2]; flags = 0x042; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1275 };
        ];
      aliases =
        [
        ];
      source_line = 1275 };
    { spelling = "WRMSR";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x30]; flags = 0x042; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1276 };
        ];
      aliases =
        [
        ];
      source_line = 1276 };
    { spelling = "RDTSC";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x31]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1277 };
        ];
      aliases =
        [
        ];
      source_line = 1277 };
    { spelling = "RDMSR";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x32]; flags = 0x042; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1278 };
        ];
      aliases =
        [
        ];
      source_line = 1278 };
    { spelling = "PAUSE";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0xF3; 0x90]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1279 };
        ];
      aliases =
        [
        ];
      source_line = 1279 };
    { spelling = "MOV_CR0_EAX";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x22; 0xC0]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1281 };
        ];
      aliases =
        [
        ];
      source_line = 1281 };
    { spelling = "MOV_EAX_CR0";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x20; 0xC0]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1282 };
        ];
      aliases =
        [
        ];
      source_line = 1282 };
    { spelling = "MOV_CR2_EAX";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x22; 0xD0]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1283 };
        ];
      aliases =
        [
        ];
      source_line = 1283 };
    { spelling = "MOV_EAX_CR2";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x20; 0xD0]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1284 };
        ];
      aliases =
        [
        ];
      source_line = 1284 };
    { spelling = "MOV_CR3_EAX";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x22; 0xD8]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1285 };
        ];
      aliases =
        [
        ];
      source_line = 1285 };
    { spelling = "MOV_EAX_CR3";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x20; 0xD8]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1286 };
        ];
      aliases =
        [
        ];
      source_line = 1286 };
    { spelling = "MOV_CR4_EAX";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x22; 0xE0]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1287 };
        ];
      aliases =
        [
        ];
      source_line = 1287 };
    { spelling = "MOV_EAX_CR4";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x20; 0xE0]; flags = 0x000; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1288 };
        ];
      aliases =
        [
        ];
      source_line = 1288 };
    { spelling = "MOV_CR0_RAX";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x22; 0xC0]; flags = 0x042; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1290 };
        ];
      aliases =
        [
        ];
      source_line = 1290 };
    { spelling = "MOV_RAX_CR0";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x20; 0xC0]; flags = 0x042; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1291 };
        ];
      aliases =
        [
        ];
      source_line = 1291 };
    { spelling = "MOV_CR2_RAX";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x22; 0xD0]; flags = 0x042; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1292 };
        ];
      aliases =
        [
        ];
      source_line = 1292 };
    { spelling = "MOV_RAX_CR2";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x20; 0xD0]; flags = 0x042; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1293 };
        ];
      aliases =
        [
        ];
      source_line = 1293 };
    { spelling = "MOV_CR3_RAX";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x22; 0xD8]; flags = 0x042; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1294 };
        ];
      aliases =
        [
        ];
      source_line = 1294 };
    { spelling = "MOV_RAX_CR3";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x20; 0xD8]; flags = 0x042; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1295 };
        ];
      aliases =
        [
        ];
      source_line = 1295 };
    { spelling = "MOV_CR4_RAX";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x22; 0xE0]; flags = 0x042; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1296 };
        ];
      aliases =
        [
        ];
      source_line = 1296 };
    { spelling = "MOV_RAX_CR4";
      instructions =
        [
        { entry_index = 0; opcode_bytes = [0x0F; 0x20; 0xE0]; flags = 0x042; slash_value = 11; uasm_slash_value = 11; opcode_modifier = 0; argument1 = 0; argument2 = 0; size1 = 0; size2 = 0; source_line = 1297 };
        ];
      aliases =
        [
        ];
      source_line = 1297 };
  ]
