(* Generated from the pinned TempleOS compiler-option definitions and consumers.
   Regenerate this file only as part of a reviewed reference or table update. *)

[@@@ocamlformat "disable"]

let reference_commit = "c26482bb6ad3f80106d28504ec5db3c6a360732c"

type source = { path : string; sha256 : string }

type source_reference = { path : string; line : int }

type option_entry = {
  name : string;
  bit_index : int;
  initially_enabled : bool;
  definition_line : int;
  source_comment : string option;
  consumers : source_reference list;
}

type gap = { first : int; last : int }

type api = {
  option_line : int;
  get_option_line : int;
  bequ_line : int;
  state_expression : string;
  set_returns_previous : bool;
}

let sources =
  [
    { path = "Compiler/BackFA.HC"; sha256 = "600aff7a73711571ac764f02bbbd6d8cb41e5651867e59dd496182866d93e924" };
    { path = "Compiler/BackLib.HC"; sha256 = "1bb302eec16d8231e08ea77a83a3e05d253c45202521510d4450c23a22518e7d" };
    { path = "Compiler/CExcept.HC"; sha256 = "4af48a26a34a79094cbdd211b5f586eae81e24d49e08cc74d5fdd0c935a49936" };
    { path = "Compiler/CMain.HC"; sha256 = "318717f411327cdb5b4b4f2d4836e0e9ec69995f80403f0eb2b6d527b2a43647" };
    { path = "Compiler/CMisc.HC"; sha256 = "78ebbe40817339bb61e79d2d9b5dbefab402d11397b3a7ad440219f70b1024ef" };
    { path = "Compiler/Lex.HC"; sha256 = "d35432fb7862588ae463524e3896d7de2f2e1db2ccb5f2ce6e11e18db27da864" };
    { path = "Compiler/LexLib.HC"; sha256 = "e3618d409597d8fffd9089bfd6c9fd31463a69c9b62ac6089a347b2e3b54e8b2" };
    { path = "Compiler/OptPass3.HC"; sha256 = "64c3b74d073b2d482751b6129d99e9f4b3b563216ec70f4a17a79aa663a92eae" };
    { path = "Compiler/OptPass6.HC"; sha256 = "7d00fd544e845423da6c354cd11e0956f45bf3b7bb839b518e01a09afd676253" };
    { path = "Compiler/OptPass789A.HC"; sha256 = "7cae9bc11a863d2066df27894449dbb28afb0d550b6b3fae1eeb6bb1362addcc" };
    { path = "Compiler/PrsLib.HC"; sha256 = "4c29376dea256cc6ca6cc5e2cb042816fb5ef6dc2aafe54f303143a013a555e1" };
    { path = "Compiler/PrsStmt.HC"; sha256 = "6bccf67abed7cc634d07e6b7b0201f51ce00094a039b61a406e75de725c342af" };
    { path = "Compiler/PrsVar.HC"; sha256 = "a4d090d96e13f2358aa9914699aefd58f6b785ba5e444d6e9e59ffafcc28bae8" };
    { path = "Kernel/KHashB.HC"; sha256 = "260e17f94bfddf8c5d3d5f95dbffd3b8e34bcfd0581167275ed4cf736bfab03d" };
    { path = "Kernel/KUtils.HC"; sha256 = "0372304b077110f40b13883af21aa5365b57ddfcb8572e0f75cb40af5a65131a" };
    { path = "Kernel/KernelA.HH"; sha256 = "1b4b6d8b6aeeaedfd2b11536b84557d9d2efc05ff38200020cd7a4a94dcd7d41" };
  ]

let options =
  [
    { name = "OPTf_ECHO"; bit_index = 0x0; initially_enabled = false; definition_line = 1546; source_comment = None; consumers = [
        { path = "Compiler/Lex.HC"; line = 257 };
        { path = "Compiler/CMisc.HC"; line = 65 };
      ] };
    { name = "OPTf_TRACE"; bit_index = 0x1; initially_enabled = false; definition_line = 1547; source_comment = None; consumers = [
        { path = "Compiler/CMisc.HC"; line = 60 };
        { path = "Compiler/CMain.HC"; line = 296 };
        { path = "Compiler/PrsLib.HC"; line = 309 };
        { path = "Compiler/PrsStmt.HC"; line = 182 };
        { path = "Compiler/PrsStmt.HC"; line = 186 };
      ] };
    { name = "OPTf_WARN_UNUSED_VAR"; bit_index = 0x10; initially_enabled = true; definition_line = 1548; source_comment = Some "Applied to funs, not stmts"; consumers = [
        { path = "Compiler/PrsStmt.HC"; line = 200 };
      ] };
    { name = "OPTf_WARN_PAREN"; bit_index = 0x11; initially_enabled = false; definition_line = 1549; source_comment = Some "Warn unnecessary parens"; consumers = [
        { path = "Compiler/CExcept.HC"; line = 112 };
      ] };
    { name = "OPTf_WARN_DUP_TYPES"; bit_index = 0x12; initially_enabled = false; definition_line = 1550; source_comment = Some "Warn dup local var type stmts"; consumers = [
        { path = "Compiler/LexLib.HC"; line = 123 };
      ] };
    { name = "OPTf_WARN_HEADER_MISMATCH"; bit_index = 0x13; initially_enabled = true; definition_line = 1551; source_comment = None; consumers = [
        { path = "Compiler/PrsStmt.HC"; line = 125 };
      ] };
    { name = "OPTf_EXTERNS_TO_IMPORTS"; bit_index = 0x20; initially_enabled = false; definition_line = 1552; source_comment = None; consumers = [
        { path = "Compiler/PrsStmt.HC"; line = 997 };
        { path = "Compiler/PrsStmt.HC"; line = 1042 };
      ] };
    { name = "OPTf_KEEP_PRIVATE"; bit_index = 0x21; initially_enabled = false; definition_line = 1553; source_comment = None; consumers = [
        { path = "Kernel/KHashB.HC"; line = 161 };
      ] };
    { name = "OPTf_NO_REG_VAR"; bit_index = 0x22; initially_enabled = false; definition_line = 1554; source_comment = Some "Applied to funs, not stmts"; consumers = [
        { path = "Compiler/OptPass3.HC"; line = 536 };
        { path = "Compiler/OptPass6.HC"; line = 103 };
      ] };
    { name = "OPTf_GLBLS_ON_DATA_HEAP"; bit_index = 0x23; initially_enabled = false; definition_line = 1555; source_comment = None; consumers = [
        { path = "Compiler/PrsStmt.HC"; line = 336 };
        { path = "Compiler/PrsStmt.HC"; line = 370 };
        { path = "Compiler/PrsVar.HC"; line = 59 };
        { path = "Compiler/PrsVar.HC"; line = 211 };
      ] };
    { name = "OPTf_NO_BUILTIN_CONST"; bit_index = 0x24; initially_enabled = false; definition_line = 1557; source_comment = Some "Applied to funs, not stmts"; consumers = [
        { path = "Compiler/BackLib.HC"; line = 429 };
      ] };
    { name = "OPTf_USE_IMM64"; bit_index = 0x25; initially_enabled = false; definition_line = 1558; source_comment = Some "Not completely implemented"; consumers = [
        { path = "Compiler/OptPass789A.HC"; line = 359 };
        { path = "Compiler/BackFA.HC"; line = 285 };
      ] };
  ]

let gaps =
  [
    { first = 0x2; last = 0xF };
    { first = 0x14; last = 0x1F };
  ]

let default_source_line = 37
let echo_mask_line = 1560

let api =
  { option_line = 1; get_option_line = 6; bequ_line = 88; state_expression = "Fs->last_cc->opts"; set_returns_previous = true }
