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
