(* This table is generated from the pinned TempleOS operator definitions.
   Run the operator table generator after an approved reference update. *)

[@@@ocamlformat "disable"]

let reference_commit = "c26482bb6ad3f80106d28504ec5db3c6a360732c"
let kernel_source_path = "Kernel/KernelA.HH"
let kernel_source_sha256 = "1b4b6d8b6aeeaedfd2b11536b84557d9d2efc05ff38200020cd7a4a94dcd7d41"
let compiler_source_path = "Compiler/CompilerA.HH"
let compiler_source_sha256 = "9eca54eff7d1c0803172e45e5483a57262e24f7b759a6a727c29beaf660967b2"
let cinit_source_path = "Compiler/CInit.HC"
let cinit_source_sha256 = "f187d11043dcceb8791409a3e6809ea26e9c3b4f182fe2cbe5c5e644e6938b19"
let lex_source_path = "Compiler/Lex.HC"
let lex_source_sha256 = "d35432fb7862588ae463524e3896d7de2f2e1db2ccb5f2ce6e11e18db27da864"

type named_constant = { name : string; value : int; source_line : int }

type sequence_kind = Token | Block_comment | Line_comment

type dual_sequence = {
  group : int;
  spelling : string;
  kind : sequence_kind;
  token_name : string option;
  token_id : int option;
  source_line : int;
}

type operator_origin = Dual_table of int | Shift_assignment | Dot_sequence | Current_position

type operator = {
  spelling : string;
  token_name : string option;
  token_id : int option;
  origin : operator_origin;
  source_line : int;
}

type association = Unspecified | Left | Right

type binary_operator = {
  spelling : string;
  token_name : string option;
  token_id : int;
  precedence_name : string;
  precedence_value : int;
  association : association;
  ic_name : string;
  ic_id : int;
  source_line : int;
}

let tokens =
  [
    { name = "TK_EOF"; value = 0x0; source_line = 2079 };
    { name = "TK_SUPERSCRIPT"; value = 0x1; source_line = 2080 };
    { name = "TK_SUBSCRIPT"; value = 0x2; source_line = 2081 };
    { name = "TK_NORMALSCRIPT"; value = 0x3; source_line = 2082 };
    { name = "TK_IDENT"; value = 0x100; source_line = 2083 };
    { name = "TK_STR"; value = 0x101; source_line = 2084 };
    { name = "TK_I64"; value = 0x102; source_line = 2085 };
    { name = "TK_CHAR_CONST"; value = 0x103; source_line = 2086 };
    { name = "TK_F64"; value = 0x104; source_line = 2087 };
    { name = "TK_PLUS_PLUS"; value = 0x105; source_line = 2088 };
    { name = "TK_MINUS_MINUS"; value = 0x106; source_line = 2089 };
    { name = "TK_DEREFERENCE"; value = 0x107; source_line = 2090 };
    { name = "TK_DBL_COLON"; value = 0x108; source_line = 2091 };
    { name = "TK_SHL"; value = 0x109; source_line = 2092 };
    { name = "TK_SHR"; value = 0x10A; source_line = 2093 };
    { name = "TK_EQU_EQU"; value = 0x10B; source_line = 2094 };
    { name = "TK_NOT_EQU"; value = 0x10C; source_line = 2095 };
    { name = "TK_LESS_EQU"; value = 0x10D; source_line = 2096 };
    { name = "TK_GREATER_EQU"; value = 0x10E; source_line = 2097 };
    { name = "TK_AND_AND"; value = 0x10F; source_line = 2098 };
    { name = "TK_OR_OR"; value = 0x110; source_line = 2099 };
    { name = "TK_XOR_XOR"; value = 0x111; source_line = 2100 };
    { name = "TK_SHL_EQU"; value = 0x112; source_line = 2101 };
    { name = "TK_SHR_EQU"; value = 0x113; source_line = 2102 };
    { name = "TK_MUL_EQU"; value = 0x114; source_line = 2103 };
    { name = "TK_DIV_EQU"; value = 0x115; source_line = 2104 };
    { name = "TK_AND_EQU"; value = 0x116; source_line = 2105 };
    { name = "TK_OR_EQU"; value = 0x117; source_line = 2106 };
    { name = "TK_XOR_EQU"; value = 0x118; source_line = 2107 };
    { name = "TK_ADD_EQU"; value = 0x119; source_line = 2108 };
    { name = "TK_SUB_EQU"; value = 0x11A; source_line = 2109 };
    { name = "TK_IF"; value = 0x11B; source_line = 2110 };
    { name = "TK_IFDEF"; value = 0x11C; source_line = 2111 };
    { name = "TK_IFNDEF"; value = 0x11D; source_line = 2112 };
    { name = "TK_IFAOT"; value = 0x11E; source_line = 2113 };
    { name = "TK_IFJIT"; value = 0x11F; source_line = 2114 };
    { name = "TK_ENDIF"; value = 0x120; source_line = 2115 };
    { name = "TK_ELSE"; value = 0x121; source_line = 2116 };
    { name = "TK_MOD_EQU"; value = 0x122; source_line = 2117 };
    { name = "TK_DOT_DOT"; value = 0x123; source_line = 2118 };
    { name = "TK_ELLIPSIS"; value = 0x124; source_line = 2119 };
    { name = "TK_INS_BIN"; value = 0x125; source_line = 2120 };
    { name = "TK_INS_BIN_SIZE"; value = 0x126; source_line = 2121 };
    { name = "TK_TKS_NUM"; value = 0x127; source_line = 2122 };
  ]

let association_flags =
  [
    { name = "ASSOCF_LEFT"; value = 0x1; source_line = 335 };
    { name = "ASSOCF_RIGHT"; value = 0x2; source_line = 336 };
    { name = "ASSOC_MASK"; value = 0x3; source_line = 337 };
  ]

let precedences =
  [
    { name = "PREC_NULL"; value = 0x0; source_line = 339 };
    { name = "PREC_TERM"; value = 0x4; source_line = 340 };
    { name = "PREC_UNARY_POST"; value = 0x8; source_line = 341 };
    { name = "PREC_UNARY_PRE"; value = 0xC; source_line = 342 };
    { name = "PREC_EXP"; value = 0x10; source_line = 343 };
    { name = "PREC_MUL"; value = 0x14; source_line = 344 };
    { name = "PREC_AND"; value = 0x18; source_line = 345 };
    { name = "PREC_XOR"; value = 0x1C; source_line = 346 };
    { name = "PREC_OR"; value = 0x20; source_line = 347 };
    { name = "PREC_ADD"; value = 0x24; source_line = 348 };
    { name = "PREC_CMP"; value = 0x28; source_line = 349 };
    { name = "PREC_CMP2"; value = 0x2C; source_line = 350 };
    { name = "PREC_AND_AND"; value = 0x30; source_line = 351 };
    { name = "PREC_XOR_XOR"; value = 0x34; source_line = 352 };
    { name = "PREC_OR_OR"; value = 0x38; source_line = 353 };
    { name = "PREC_ASSIGN"; value = 0x3C; source_line = 354 };
    { name = "PREC_MAX"; value = 0x40; source_line = 356 };
  ]

let dual_sequences =
  [
    { group = 1; spelling = "!="; kind = Token; token_name = Some "TK_NOT_EQU"; token_id = Some 0x10C; source_line = 259 };
    { group = 1; spelling = "&&"; kind = Token; token_name = Some "TK_AND_AND"; token_id = Some 0x10F; source_line = 260 };
    { group = 1; spelling = "*="; kind = Token; token_name = Some "TK_MUL_EQU"; token_id = Some 0x114; source_line = 261 };
    { group = 1; spelling = "++"; kind = Token; token_name = Some "TK_PLUS_PLUS"; token_id = Some 0x105; source_line = 262 };
    { group = 1; spelling = "->"; kind = Token; token_name = Some "TK_DEREFERENCE"; token_id = Some 0x107; source_line = 263 };
    { group = 1; spelling = "/*"; kind = Block_comment; token_name = None; token_id = None; source_line = 264 };
    { group = 1; spelling = "::"; kind = Token; token_name = Some "TK_DBL_COLON"; token_id = Some 0x108; source_line = 265 };
    { group = 1; spelling = "<="; kind = Token; token_name = Some "TK_LESS_EQU"; token_id = Some 0x10D; source_line = 266 };
    { group = 1; spelling = "=="; kind = Token; token_name = Some "TK_EQU_EQU"; token_id = Some 0x10B; source_line = 267 };
    { group = 1; spelling = ">="; kind = Token; token_name = Some "TK_GREATER_EQU"; token_id = Some 0x10E; source_line = 268 };
    { group = 1; spelling = "^="; kind = Token; token_name = Some "TK_XOR_EQU"; token_id = Some 0x118; source_line = 269 };
    { group = 1; spelling = "||"; kind = Token; token_name = Some "TK_OR_OR"; token_id = Some 0x110; source_line = 270 };
    { group = 1; spelling = "%="; kind = Token; token_name = Some "TK_MOD_EQU"; token_id = Some 0x122; source_line = 271 };
    { group = 2; spelling = "&="; kind = Token; token_name = Some "TK_AND_EQU"; token_id = Some 0x116; source_line = 274 };
    { group = 2; spelling = "+="; kind = Token; token_name = Some "TK_ADD_EQU"; token_id = Some 0x119; source_line = 275 };
    { group = 2; spelling = "--"; kind = Token; token_name = Some "TK_MINUS_MINUS"; token_id = Some 0x106; source_line = 276 };
    { group = 2; spelling = "//"; kind = Line_comment; token_name = None; token_id = None; source_line = 277 };
    { group = 2; spelling = "<<"; kind = Token; token_name = Some "TK_SHL"; token_id = Some 0x109; source_line = 278 };
    { group = 2; spelling = ">>"; kind = Token; token_name = Some "TK_SHR"; token_id = Some 0x10A; source_line = 279 };
    { group = 2; spelling = "^^"; kind = Token; token_name = Some "TK_XOR_XOR"; token_id = Some 0x111; source_line = 280 };
    { group = 2; spelling = "|="; kind = Token; token_name = Some "TK_OR_EQU"; token_id = Some 0x117; source_line = 281 };
    { group = 3; spelling = "-="; kind = Token; token_name = Some "TK_SUB_EQU"; token_id = Some 0x11A; source_line = 284 };
    { group = 3; spelling = "/="; kind = Token; token_name = Some "TK_DIV_EQU"; token_id = Some 0x115; source_line = 285 };
  ]

let operators =
  [
    { spelling = "<<="; token_name = Some "TK_SHL_EQU"; token_id = Some 0x112; origin = Shift_assignment; source_line = 1166 };
    { spelling = ">>="; token_name = Some "TK_SHR_EQU"; token_id = Some 0x113; origin = Shift_assignment; source_line = 1168 };
    { spelling = "..."; token_name = Some "TK_ELLIPSIS"; token_id = Some 0x124; origin = Dot_sequence; source_line = 1079 };
    { spelling = "++"; token_name = Some "TK_PLUS_PLUS"; token_id = Some 0x105; origin = Dual_table 1; source_line = 262 };
    { spelling = "--"; token_name = Some "TK_MINUS_MINUS"; token_id = Some 0x106; origin = Dual_table 2; source_line = 276 };
    { spelling = "->"; token_name = Some "TK_DEREFERENCE"; token_id = Some 0x107; origin = Dual_table 1; source_line = 263 };
    { spelling = "::"; token_name = Some "TK_DBL_COLON"; token_id = Some 0x108; origin = Dual_table 1; source_line = 265 };
    { spelling = "<<"; token_name = Some "TK_SHL"; token_id = Some 0x109; origin = Dual_table 2; source_line = 278 };
    { spelling = ">>"; token_name = Some "TK_SHR"; token_id = Some 0x10A; origin = Dual_table 2; source_line = 279 };
    { spelling = "=="; token_name = Some "TK_EQU_EQU"; token_id = Some 0x10B; origin = Dual_table 1; source_line = 267 };
    { spelling = "!="; token_name = Some "TK_NOT_EQU"; token_id = Some 0x10C; origin = Dual_table 1; source_line = 259 };
    { spelling = "<="; token_name = Some "TK_LESS_EQU"; token_id = Some 0x10D; origin = Dual_table 1; source_line = 266 };
    { spelling = ">="; token_name = Some "TK_GREATER_EQU"; token_id = Some 0x10E; origin = Dual_table 1; source_line = 268 };
    { spelling = "&&"; token_name = Some "TK_AND_AND"; token_id = Some 0x10F; origin = Dual_table 1; source_line = 260 };
    { spelling = "||"; token_name = Some "TK_OR_OR"; token_id = Some 0x110; origin = Dual_table 1; source_line = 270 };
    { spelling = "^^"; token_name = Some "TK_XOR_XOR"; token_id = Some 0x111; origin = Dual_table 2; source_line = 280 };
    { spelling = "*="; token_name = Some "TK_MUL_EQU"; token_id = Some 0x114; origin = Dual_table 1; source_line = 261 };
    { spelling = "/="; token_name = Some "TK_DIV_EQU"; token_id = Some 0x115; origin = Dual_table 3; source_line = 285 };
    { spelling = "%="; token_name = Some "TK_MOD_EQU"; token_id = Some 0x122; origin = Dual_table 1; source_line = 271 };
    { spelling = "&="; token_name = Some "TK_AND_EQU"; token_id = Some 0x116; origin = Dual_table 2; source_line = 274 };
    { spelling = "|="; token_name = Some "TK_OR_EQU"; token_id = Some 0x117; origin = Dual_table 2; source_line = 281 };
    { spelling = "^="; token_name = Some "TK_XOR_EQU"; token_id = Some 0x118; origin = Dual_table 1; source_line = 269 };
    { spelling = "+="; token_name = Some "TK_ADD_EQU"; token_id = Some 0x119; origin = Dual_table 2; source_line = 275 };
    { spelling = "-="; token_name = Some "TK_SUB_EQU"; token_id = Some 0x11A; origin = Dual_table 3; source_line = 284 };
    { spelling = ".."; token_name = Some "TK_DOT_DOT"; token_id = Some 0x123; origin = Dot_sequence; source_line = 1077 };
    { spelling = "$$"; token_name = None; token_id = None; origin = Current_position; source_line = 1100 };
  ]

let binary_operators =
  [
    { spelling = "`"; token_name = None; token_id = 0x60; precedence_name = "PREC_EXP"; precedence_value = 0x10; association = Right; ic_name = "IC_POWER"; ic_id = 0x2F; source_line = 288 };
    { spelling = "<<"; token_name = Some "TK_SHL"; token_id = 0x109; precedence_name = "PREC_EXP"; precedence_value = 0x10; association = Left; ic_name = "IC_SHL"; ic_id = 0x2B; source_line = 289 };
    { spelling = ">>"; token_name = Some "TK_SHR"; token_id = 0x10A; precedence_name = "PREC_EXP"; precedence_value = 0x10; association = Left; ic_name = "IC_SHR"; ic_id = 0x2C; source_line = 290 };
    { spelling = "*"; token_name = None; token_id = 0x2A; precedence_name = "PREC_MUL"; precedence_value = 0x14; association = Unspecified; ic_name = "IC_MUL"; ic_id = 0x30; source_line = 292 };
    { spelling = "/"; token_name = None; token_id = 0x2F; precedence_name = "PREC_MUL"; precedence_value = 0x14; association = Left; ic_name = "IC_DIV"; ic_id = 0x31; source_line = 293 };
    { spelling = "%"; token_name = None; token_id = 0x25; precedence_name = "PREC_MUL"; precedence_value = 0x14; association = Left; ic_name = "IC_MOD"; ic_id = 0x32; source_line = 294 };
    { spelling = "&"; token_name = None; token_id = 0x26; precedence_name = "PREC_AND"; precedence_value = 0x18; association = Unspecified; ic_name = "IC_AND"; ic_id = 0x33; source_line = 296 };
    { spelling = "^"; token_name = None; token_id = 0x5E; precedence_name = "PREC_XOR"; precedence_value = 0x1C; association = Unspecified; ic_name = "IC_XOR"; ic_id = 0x35; source_line = 298 };
    { spelling = "|"; token_name = None; token_id = 0x7C; precedence_name = "PREC_OR"; precedence_value = 0x20; association = Unspecified; ic_name = "IC_OR"; ic_id = 0x34; source_line = 300 };
    { spelling = "+"; token_name = None; token_id = 0x2B; precedence_name = "PREC_ADD"; precedence_value = 0x24; association = Unspecified; ic_name = "IC_ADD"; ic_id = 0x36; source_line = 302 };
    { spelling = "-"; token_name = None; token_id = 0x2D; precedence_name = "PREC_ADD"; precedence_value = 0x24; association = Left; ic_name = "IC_SUB"; ic_id = 0x37; source_line = 303 };
    { spelling = "<"; token_name = None; token_id = 0x3C; precedence_name = "PREC_CMP"; precedence_value = 0x28; association = Unspecified; ic_name = "IC_LESS"; ic_id = 0x3C; source_line = 305 };
    { spelling = ">"; token_name = None; token_id = 0x3E; precedence_name = "PREC_CMP"; precedence_value = 0x28; association = Unspecified; ic_name = "IC_GREATER"; ic_id = 0x3E; source_line = 306 };
    { spelling = "<="; token_name = Some "TK_LESS_EQU"; token_id = 0x10D; precedence_name = "PREC_CMP"; precedence_value = 0x28; association = Unspecified; ic_name = "IC_LESS_EQU"; ic_id = 0x3F; source_line = 307 };
    { spelling = ">="; token_name = Some "TK_GREATER_EQU"; token_id = 0x10E; precedence_name = "PREC_CMP"; precedence_value = 0x28; association = Unspecified; ic_name = "IC_GREATER_EQU"; ic_id = 0x3D; source_line = 308 };
    { spelling = "=="; token_name = Some "TK_EQU_EQU"; token_id = 0x10B; precedence_name = "PREC_CMP2"; precedence_value = 0x2C; association = Unspecified; ic_name = "IC_EQU_EQU"; ic_id = 0x3A; source_line = 310 };
    { spelling = "!="; token_name = Some "TK_NOT_EQU"; token_id = 0x10C; precedence_name = "PREC_CMP2"; precedence_value = 0x2C; association = Unspecified; ic_name = "IC_NOT_EQU"; ic_id = 0x3B; source_line = 311 };
    { spelling = "&&"; token_name = Some "TK_AND_AND"; token_id = 0x10F; precedence_name = "PREC_AND_AND"; precedence_value = 0x30; association = Unspecified; ic_name = "IC_AND_AND"; ic_id = 0x41; source_line = 313 };
    { spelling = "^^"; token_name = Some "TK_XOR_XOR"; token_id = 0x111; precedence_name = "PREC_XOR_XOR"; precedence_value = 0x34; association = Unspecified; ic_name = "IC_XOR_XOR"; ic_id = 0x43; source_line = 315 };
    { spelling = "||"; token_name = Some "TK_OR_OR"; token_id = 0x110; precedence_name = "PREC_OR_OR"; precedence_value = 0x38; association = Unspecified; ic_name = "IC_OR_OR"; ic_id = 0x42; source_line = 317 };
    { spelling = "="; token_name = None; token_id = 0x3D; precedence_name = "PREC_ASSIGN"; precedence_value = 0x3C; association = Right; ic_name = "IC_ASSIGN"; ic_id = 0x44; source_line = 319 };
    { spelling = "<<="; token_name = Some "TK_SHL_EQU"; token_id = 0x112; precedence_name = "PREC_ASSIGN"; precedence_value = 0x3C; association = Right; ic_name = "IC_SHL_EQU"; ic_id = 0x47; source_line = 320 };
    { spelling = ">>="; token_name = Some "TK_SHR_EQU"; token_id = 0x113; precedence_name = "PREC_ASSIGN"; precedence_value = 0x3C; association = Right; ic_name = "IC_SHR_EQU"; ic_id = 0x48; source_line = 321 };
    { spelling = "*="; token_name = Some "TK_MUL_EQU"; token_id = 0x114; precedence_name = "PREC_ASSIGN"; precedence_value = 0x3C; association = Right; ic_name = "IC_MUL_EQU"; ic_id = 0x49; source_line = 322 };
    { spelling = "/="; token_name = Some "TK_DIV_EQU"; token_id = 0x115; precedence_name = "PREC_ASSIGN"; precedence_value = 0x3C; association = Right; ic_name = "IC_DIV_EQU"; ic_id = 0x4A; source_line = 323 };
    { spelling = "%="; token_name = Some "TK_MOD_EQU"; token_id = 0x122; precedence_name = "PREC_ASSIGN"; precedence_value = 0x3C; association = Right; ic_name = "IC_MOD_EQU"; ic_id = 0x4B; source_line = 324 };
    { spelling = "&="; token_name = Some "TK_AND_EQU"; token_id = 0x116; precedence_name = "PREC_ASSIGN"; precedence_value = 0x3C; association = Right; ic_name = "IC_AND_EQU"; ic_id = 0x4C; source_line = 325 };
    { spelling = "|="; token_name = Some "TK_OR_EQU"; token_id = 0x117; precedence_name = "PREC_ASSIGN"; precedence_value = 0x3C; association = Right; ic_name = "IC_OR_EQU"; ic_id = 0x4D; source_line = 326 };
    { spelling = "^="; token_name = Some "TK_XOR_EQU"; token_id = 0x118; precedence_name = "PREC_ASSIGN"; precedence_value = 0x3C; association = Right; ic_name = "IC_XOR_EQU"; ic_id = 0x4E; source_line = 327 };
    { spelling = "+="; token_name = Some "TK_ADD_EQU"; token_id = 0x119; precedence_name = "PREC_ASSIGN"; precedence_value = 0x3C; association = Right; ic_name = "IC_ADD_EQU"; ic_id = 0x4F; source_line = 328 };
    { spelling = "-="; token_name = Some "TK_SUB_EQU"; token_id = 0x11A; precedence_name = "PREC_ASSIGN"; precedence_value = 0x3C; association = Right; ic_name = "IC_SUB_EQU"; ic_id = 0x50; source_line = 329 };
  ]
