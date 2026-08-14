(* Generated from the pinned TempleOS member-list flag specification. *)

[@@@ocamlformat "disable"]

type source = { path : string; sha256 : string }
type source_reference = { path : string; line : int }
type flag_info = { source_name : string; bit_index : int; mask : int64; definition_line : int; consumers : source_reference list }
type behavior_info = { id : string; description : string; source : source_reference }

let reference_commit = "c26482bb6ad3f80106d28504ec5db3c6a360732c"

let sources =
  [
    { path = "Kernel/KernelA.HH"; sha256 = "1b4b6d8b6aeeaedfd2b11536b84557d9d2efc05ff38200020cd7a4a94dcd7d41" };
    { path = "Compiler/LexLib.HC"; sha256 = "e3618d409597d8fffd9089bfd6c9fd31463a69c9b62ac6089a347b2e3b54e8b2" };
    { path = "Compiler/PrsVar.HC"; sha256 = "a4d090d96e13f2358aa9914699aefd58f6b785ba5e444d6e9e59ffafcc28bae8" };
    { path = "Compiler/PrsStmt.HC"; sha256 = "6bccf67abed7cc634d07e6b7b0201f51ce00094a039b61a406e75de725c342af" };
    { path = "Compiler/PrsExp.HC"; sha256 = "e1746e64e943f26d6d96179379ec8684f24ae908614b965ea4e01f2d5398dd73" };
  ]

type t =
  | Default_available
  | Lastclass
  | String_default_available
  | Function_pointer
  | Variadic
  | No_unused_warning
  | Static

let all =
  [
    Default_available;
    Lastclass;
    String_default_available;
    Function_pointer;
    Variadic;
    No_unused_warning;
    Static;
  ]

let to_source_name = function
  | Default_available -> "MLF_DFT_AVAILABLE"
  | Lastclass -> "MLF_LASTCLASS"
  | String_default_available -> "MLF_STR_DFT_AVAILABLE"
  | Function_pointer -> "MLF_FUN"
  | Variadic -> "MLF_DOT_DOT_DOT"
  | No_unused_warning -> "MLF_NO_UNUSED_WARN"
  | Static -> "MLF_STATIC"

let of_source_name = function
  | "MLF_DFT_AVAILABLE" -> Some Default_available
  | "MLF_LASTCLASS" -> Some Lastclass
  | "MLF_STR_DFT_AVAILABLE" -> Some String_default_available
  | "MLF_FUN" -> Some Function_pointer
  | "MLF_DOT_DOT_DOT" -> Some Variadic
  | "MLF_NO_UNUSED_WARN" -> Some No_unused_warning
  | "MLF_STATIC" -> Some Static
  | _ -> None

let info = function
  | Default_available -> { source_name = "MLF_DFT_AVAILABLE"; bit_index = 0; mask = 0x1L; definition_line = 778; consumers = [ { path = "Compiler/PrsVar.HC"; line = 656 }; { path = "Compiler/LexLib.HC"; line = 164 }; { path = "Compiler/PrsExp.HC"; line = 455 } ] }
  | Lastclass -> { source_name = "MLF_LASTCLASS"; bit_index = 1; mask = 0x2L; definition_line = 779; consumers = [ { path = "Compiler/PrsVar.HC"; line = 632 }; { path = "Compiler/PrsExp.HC"; line = 458 }; { path = "Compiler/PrsExp.HC"; line = 460 } ] }
  | String_default_available -> { source_name = "MLF_STR_DFT_AVAILABLE"; bit_index = 2; mask = 0x4L; definition_line = 780; consumers = [ { path = "Compiler/PrsVar.HC"; line = 651 }; { path = "Compiler/LexLib.HC"; line = 168 }; { path = "Compiler/LexLib.HC"; line = 191 }; { path = "Compiler/LexLib.HC"; line = 230 }; { path = "Compiler/PrsExp.HC"; line = 460 } ] }
  | Function_pointer -> { source_name = "MLF_FUN"; bit_index = 3; mask = 0x8L; definition_line = 781; consumers = [ { path = "Compiler/PrsVar.HC"; line = 524 }; { path = "Compiler/LexLib.HC"; line = 193 }; { path = "Compiler/LexLib.HC"; line = 232 }; { path = "Compiler/PrsExp.HC"; line = 767 }; { path = "Compiler/PrsExp.HC"; line = 1009 } ] }
  | Variadic -> { source_name = "MLF_DOT_DOT_DOT"; bit_index = 4; mask = 0x10L; definition_line = 782; consumers = [ { path = "Compiler/PrsVar.HC"; line = 384 }; { path = "Compiler/PrsVar.HC"; line = 393 }; { path = "Compiler/PrsExp.HC"; line = 491 } ] }
  | No_unused_warning -> { source_name = "MLF_NO_UNUSED_WARN"; bit_index = 5; mask = 0x20L; definition_line = 783; consumers = [ { path = "Compiler/PrsVar.HC"; line = 344 }; { path = "Compiler/PrsStmt.HC"; line = 195 }; { path = "Compiler/PrsStmt.HC"; line = 797 } ] }
  | Static -> { source_name = "MLF_STATIC"; bit_index = 6; mask = 0x40L; definition_line = 784; consumers = [ { path = "Compiler/PrsVar.HC"; line = 493 }; { path = "Compiler/PrsExp.HC"; line = 776 } ] }

let to_bit_index flag = (info flag).bit_index
let to_mask flag = (info flag).mask
let is_set ~mask flag = Int64.logand mask (to_mask flag) <> 0L
let set ~mask flag = Int64.logor mask (to_mask flag)
let clear ~mask flag = Int64.logand mask (Int64.lognot (to_mask flag))

let behaviors =
  [
    { id = "anonymous-slot-no-warning"; description = "an unnamed argument slot suppresses its unused warning"; source = { path = "Compiler/PrsVar.HC"; line = 344 } };
    { id = "varargs-argc"; description = "the synthesized argc slot carries the variadic marker"; source = { path = "Compiler/PrsVar.HC"; line = 384 } };
    { id = "varargs-argv"; description = "the synthesized argv slot carries the variadic marker"; source = { path = "Compiler/PrsVar.HC"; line = 393 } };
    { id = "static-assignment"; description = "a static local member record receives MLF_STATIC"; source = { path = "Compiler/PrsVar.HC"; line = 493 } };
    { id = "callback-assignment"; description = "a returned callback record receives MLF_FUN"; source = { path = "Compiler/PrsVar.HC"; line = 524 } };
    { id = "lastclass-assignment"; description = "a lastclass default receives MLF_LASTCLASS"; source = { path = "Compiler/PrsVar.HC"; line = 632 } };
    { id = "string-default-assignment"; description = "a string-backed default receives its ownership marker"; source = { path = "Compiler/PrsVar.HC"; line = 651 } };
    { id = "default-assignment"; description = "every accepted argument default receives its availability marker"; source = { path = "Compiler/PrsVar.HC"; line = 656 } };
    { id = "default-header-compare"; description = "function header comparison checks default availability"; source = { path = "Compiler/LexLib.HC"; line = 164 } };
    { id = "string-default-header-compare"; description = "function header comparison distinguishes string defaults"; source = { path = "Compiler/LexLib.HC"; line = 168 } };
    { id = "string-default-delete"; description = "member-list deletion frees an owned string default"; source = { path = "Compiler/LexLib.HC"; line = 191 } };
    { id = "callback-delete"; description = "member-list deletion releases an owned callback record"; source = { path = "Compiler/LexLib.HC"; line = 193 } };
    { id = "string-default-size"; description = "member-list sizing includes an owned string default"; source = { path = "Compiler/LexLib.HC"; line = 230 } };
    { id = "callback-size"; description = "member-list sizing includes an owned callback record"; source = { path = "Compiler/LexLib.HC"; line = 232 } };
    { id = "unused-warning-consumer"; description = "function completion honors the unused-warning suppression bit"; source = { path = "Compiler/PrsStmt.HC"; line = 195 } };
    { id = "no-warn-directive"; description = "the no-warning statement marks a selected local"; source = { path = "Compiler/PrsStmt.HC"; line = 797 } };
    { id = "call-default-selection"; description = "call parsing selects an available omitted argument default"; source = { path = "Compiler/PrsExp.HC"; line = 455 } };
    { id = "call-lastclass-selection"; description = "call parsing substitutes the current last class"; source = { path = "Compiler/PrsExp.HC"; line = 458 } };
    { id = "call-reference-default"; description = "AOT calls materialize string and lastclass defaults as references"; source = { path = "Compiler/PrsExp.HC"; line = 460 } };
    { id = "varargs-call"; description = "call parsing recognizes the synthesized variadic slots"; source = { path = "Compiler/PrsExp.HC"; line = 491 } };
    { id = "callback-local-expression"; description = "a callback local supplies its stored function signature"; source = { path = "Compiler/PrsExp.HC"; line = 767 } };
    { id = "static-local-expression"; description = "a static local selects its static storage path"; source = { path = "Compiler/PrsExp.HC"; line = 776 } };
    { id = "callback-member-expression"; description = "a callback field supplies its stored function signature"; source = { path = "Compiler/PrsExp.HC"; line = 1009 } };
  ]

let behavior id = List.find_opt (fun item -> String.equal item.id id) behaviors
