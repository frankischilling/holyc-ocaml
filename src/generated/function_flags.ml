(* Generated from the pinned TempleOS function and parser flag definitions.
   Regenerate this file only after reviewing the source behavior. *)

[@@@ocamlformat "disable"]

let reference_commit = "c26482bb6ad3f80106d28504ec5db3c6a360732c"

type source = { path : string; sha256 : string }

type source_reference = { path : string; line : int }

let sources =
  [
    { path = "Kernel/KernelA.HH"; sha256 = "1b4b6d8b6aeeaedfd2b11536b84557d9d2efc05ff38200020cd7a4a94dcd7d41" };
    { path = "Compiler/CompilerA.HH"; sha256 = "9eca54eff7d1c0803172e45e5483a57262e24f7b759a6a727c29beaf660967b2" };
    { path = "Compiler/PrsStmt.HC"; sha256 = "6bccf67abed7cc634d07e6b7b0201f51ce00094a039b61a406e75de725c342af" };
    { path = "Compiler/PrsVar.HC"; sha256 = "a4d090d96e13f2358aa9914699aefd58f6b785ba5e444d6e9e59ffafcc28bae8" };
    { path = "Compiler/PrsExp.HC"; sha256 = "e1746e64e943f26d6d96179379ec8684f24ae908614b965ea4e01f2d5398dd73" };
    { path = "Compiler/OptPass3.HC"; sha256 = "64c3b74d073b2d482751b6129d99e9f4b3b563216ec70f4a17a79aa663a92eae" };
    { path = "Compiler/OptPass6.HC"; sha256 = "7d00fd544e845423da6c354cd11e0956f45bf3b7bb839b518e01a09afd676253" };
    { path = "Compiler/OptPass789A.HC"; sha256 = "7cae9bc11a863d2066df27894449dbb28afb0d550b6b3fae1eeb6bb1362addcc" };
    { path = "Kernel/FunSeg.HC"; sha256 = "e499763482dd544696cd9a1318cf366da50f9072440cdd8d4d926f4443819486" };
  ]

type flag_info = {
  source_name : string;
  bit_index : int;
  mask : int64;
  definition_line : int;
  consumers : source_reference list;
}

module Shared = struct
  type t =
    | Extern
    | Internal_type

  let all =
    [
      Extern;
      Internal_type;
    ]

  let to_source_name = function
    | Extern -> "Cf_EXTERN"
    | Internal_type -> "Cf_INTERNAL_TYPE"

  let of_source_name = function
    | "Cf_EXTERN" -> Some Extern
    | "Cf_INTERNAL_TYPE" -> Some Internal_type
    | _ -> None

  let info = function
    | Extern -> { source_name = "Cf_EXTERN"; bit_index = 0; mask = 0x1L; definition_line = 834; consumers = [ { path = "Compiler/PrsStmt.HC"; line = 11 }; { path = "Compiler/PrsStmt.HC"; line = 20 }; { path = "Compiler/PrsStmt.HC"; line = 36 }; { path = "Compiler/PrsStmt.HC"; line = 77 }; { path = "Compiler/PrsStmt.HC"; line = 105 }; { path = "Compiler/PrsStmt.HC"; line = 191 }; { path = "Compiler/PrsStmt.HC"; line = 248 }; { path = "Compiler/PrsStmt.HC"; line = 256 }; { path = "Compiler/PrsStmt.HC"; line = 861 }; { path = "Compiler/PrsStmt.HC"; line = 881 }; { path = "Compiler/PrsStmt.HC"; line = 1096 }; { path = "Compiler/PrsExp.HC"; line = 561 }; { path = "Compiler/PrsExp.HC"; line = 627 }; { path = "Kernel/FunSeg.HC"; line = 22 } ] }
    | Internal_type -> { source_name = "Cf_INTERNAL_TYPE"; bit_index = 1; mask = 0x2L; definition_line = 835; consumers = [ { path = "Compiler/PrsExp.HC"; line = 205 } ] }

  let to_bit_index flag = (info flag).bit_index
  let to_mask flag = (info flag).mask
  let is_set ~mask flag = Int64.logand mask (to_mask flag) <> 0L
  let set ~mask flag = Int64.logor mask (to_mask flag)
  let clear ~mask flag = Int64.logand mask (Int64.lognot (to_mask flag))
end

module Stored = struct
  type t =
    | Interrupt
    | Has_error_code
    | Argument_pop
    | No_argument_pop
    | Internal
    | Underscore_extern
    | Variadic
    | Ret1

  let all =
    [
      Interrupt;
      Has_error_code;
      Argument_pop;
      No_argument_pop;
      Internal;
      Underscore_extern;
      Variadic;
      Ret1;
    ]

  let to_source_name = function
    | Interrupt -> "Ff_INTERRUPT"
    | Has_error_code -> "Ff_HASERRCODE"
    | Argument_pop -> "Ff_ARGPOP"
    | No_argument_pop -> "Ff_NOARGPOP"
    | Internal -> "Ff_INTERNAL"
    | Underscore_extern -> "Ff__EXTERN"
    | Variadic -> "Ff_DOT_DOT_DOT"
    | Ret1 -> "Ff_RET1"

  let of_source_name = function
    | "Ff_INTERRUPT" -> Some Interrupt
    | "Ff_HASERRCODE" -> Some Has_error_code
    | "Ff_ARGPOP" -> Some Argument_pop
    | "Ff_NOARGPOP" -> Some No_argument_pop
    | "Ff_INTERNAL" -> Some Internal
    | "Ff__EXTERN" -> Some Underscore_extern
    | "Ff_DOT_DOT_DOT" -> Some Variadic
    | "Ff_RET1" -> Some Ret1
    | _ -> None

  let info = function
    | Interrupt -> { source_name = "Ff_INTERRUPT"; bit_index = 8; mask = 0x100L; definition_line = 851; consumers = [ { path = "Compiler/OptPass789A.HC"; line = 389 }; { path = "Compiler/OptPass789A.HC"; line = 405 }; { path = "Compiler/OptPass789A.HC"; line = 703 } ] }
    | Has_error_code -> { source_name = "Ff_HASERRCODE"; bit_index = 9; mask = 0x200L; definition_line = 852; consumers = [ { path = "Compiler/OptPass789A.HC"; line = 406 } ] }
    | Argument_pop -> { source_name = "Ff_ARGPOP"; bit_index = 10; mask = 0x400L; definition_line = 853; consumers = [ { path = "Compiler/PrsStmt.HC"; line = 870 }; { path = "Compiler/PrsExp.HC"; line = 572 }; { path = "Compiler/OptPass789A.HC"; line = 411 } ] }
    | No_argument_pop -> { source_name = "Ff_NOARGPOP"; bit_index = 11; mask = 0x800L; definition_line = 854; consumers = [ { path = "Compiler/PrsStmt.HC"; line = 870 }; { path = "Compiler/PrsExp.HC"; line = 573 }; { path = "Compiler/OptPass789A.HC"; line = 412 } ] }
    | Internal -> { source_name = "Ff_INTERNAL"; bit_index = 12; mask = 0x1000L; definition_line = 855; consumers = [ { path = "Compiler/PrsStmt.HC"; line = 247 }; { path = "Compiler/PrsExp.HC"; line = 415 }; { path = "Compiler/PrsExp.HC"; line = 551 }; { path = "Compiler/PrsExp.HC"; line = 555 }; { path = "Compiler/PrsExp.HC"; line = 626 }; { path = "Compiler/OptPass6.HC"; line = 45 }; { path = "Kernel/FunSeg.HC"; line = 23 } ] }
    | Underscore_extern -> { source_name = "Ff__EXTERN"; bit_index = 13; mask = 0x2000L; definition_line = 856; consumers = [ { path = "Compiler/PrsStmt.HC"; line = 258 } ] }
    | Variadic -> { source_name = "Ff_DOT_DOT_DOT"; bit_index = 14; mask = 0x4000L; definition_line = 857; consumers = [ { path = "Compiler/PrsStmt.HC"; line = 116 }; { path = "Compiler/PrsVar.HC"; line = 378 }; { path = "Compiler/OptPass3.HC"; line = 22 } ] }
    | Ret1 -> { source_name = "Ff_RET1"; bit_index = 15; mask = 0x8000L; definition_line = 858; consumers = [ { path = "Compiler/PrsStmt.HC"; line = 117 }; { path = "Compiler/PrsStmt.HC"; line = 869 }; { path = "Compiler/PrsExp.HC"; line = 572 }; { path = "Compiler/OptPass789A.HC"; line = 410 } ] }

  let to_bit_index flag = (info flag).bit_index
  let to_mask flag = (info flag).mask
  let is_set ~mask flag = Int64.logand mask (to_mask flag) <> 0L
  let set ~mask flag = Int64.logor mask (to_mask flag)
  let clear ~mask flag = Int64.logand mask (Int64.lognot (to_mask flag))
end

module Staging = struct
  type t =
    | Public
    | Assembly
    | Static
    | Underscore_name
    | Interrupt
    | Has_error_code
    | Argument_pop
    | No_argument_pop

  let all =
    [
      Public;
      Assembly;
      Static;
      Underscore_name;
      Interrupt;
      Has_error_code;
      Argument_pop;
      No_argument_pop;
    ]

  let to_source_name = function
    | Public -> "FSF_PUBLIC"
    | Assembly -> "FSF_ASM"
    | Static -> "FSF_STATIC"
    | Underscore_name -> "FSF__"
    | Interrupt -> "FSF_INTERRUPT"
    | Has_error_code -> "FSF_HASERRCODE"
    | Argument_pop -> "FSF_ARGPOP"
    | No_argument_pop -> "FSF_NOARGPOP"

  let of_source_name = function
    | "FSF_PUBLIC" -> Some Public
    | "FSF_ASM" -> Some Assembly
    | "FSF_STATIC" -> Some Static
    | "FSF__" -> Some Underscore_name
    | "FSF_INTERRUPT" -> Some Interrupt
    | "FSF_HASERRCODE" -> Some Has_error_code
    | "FSF_ARGPOP" -> Some Argument_pop
    | "FSF_NOARGPOP" -> Some No_argument_pop
    | _ -> None

  let info = function
    | Public -> { source_name = "FSF_PUBLIC"; bit_index = 0; mask = 0x1L; definition_line = 359; consumers = [ { path = "Compiler/PrsStmt.HC"; line = 37 }; { path = "Compiler/PrsStmt.HC"; line = 110 }; { path = "Compiler/PrsStmt.HC"; line = 388 }; { path = "Compiler/PrsStmt.HC"; line = 1084 } ] }
    | Assembly -> { source_name = "FSF_ASM"; bit_index = 1; mask = 0x2L; definition_line = 360; consumers = [ { path = "Compiler/PrsStmt.HC"; line = 918 }; { path = "Compiler/PrsStmt.HC"; line = 960 }; { path = "Compiler/PrsStmt.HC"; line = 1036 }; { path = "Compiler/PrsStmt.HC"; line = 1064 }; { path = "Compiler/PrsStmt.HC"; line = 1068 }; { path = "Compiler/PrsStmt.HC"; line = 1072 }; { path = "Compiler/PrsStmt.HC"; line = 1075 }; { path = "Compiler/PrsStmt.HC"; line = 1078 }; { path = "Compiler/PrsStmt.HC"; line = 1081 }; { path = "Compiler/PrsStmt.HC"; line = 1084 }; { path = "Compiler/PrsStmt.HC"; line = 1149 }; { path = "Compiler/PrsStmt.HC"; line = 1152 }; { path = "Compiler/PrsStmt.HC"; line = 1180 }; { path = "Compiler/PrsStmt.HC"; line = 1221 } ] }
    | Static -> { source_name = "FSF_STATIC"; bit_index = 2; mask = 0x4L; definition_line = 361; consumers = [ { path = "Compiler/PrsStmt.HC"; line = 1068 }; { path = "Compiler/PrsStmt.HC"; line = 1160 } ] }
    | Underscore_name -> { source_name = "FSF__"; bit_index = 3; mask = 0x8L; definition_line = 362; consumers = [ { path = "Compiler/PrsStmt.HC"; line = 251 }; { path = "Compiler/PrsStmt.HC"; line = 266 }; { path = "Compiler/PrsStmt.HC"; line = 1003 }; { path = "Compiler/PrsStmt.HC"; line = 1017 } ] }
    | Interrupt -> { source_name = "FSF_INTERRUPT"; bit_index = 8; mask = 0x100L; definition_line = 363; consumers = [ { path = "Compiler/PrsStmt.HC"; line = 1071 } ] }
    | Has_error_code -> { source_name = "FSF_HASERRCODE"; bit_index = 9; mask = 0x200L; definition_line = 364; consumers = [ { path = "Compiler/PrsStmt.HC"; line = 1075 } ] }
    | Argument_pop -> { source_name = "FSF_ARGPOP"; bit_index = 10; mask = 0x400L; definition_line = 365; consumers = [ { path = "Compiler/PrsStmt.HC"; line = 1078 } ] }
    | No_argument_pop -> { source_name = "FSF_NOARGPOP"; bit_index = 11; mask = 0x800L; definition_line = 366; consumers = [ { path = "Compiler/PrsStmt.HC"; line = 1071 }; { path = "Compiler/PrsStmt.HC"; line = 1081 } ] }

  let to_bit_index flag = (info flag).bit_index
  let to_mask flag = (info flag).mask
  let is_set ~mask flag = Int64.logand mask (to_mask flag) <> 0L
  let set ~mask flag = Int64.logor mask (to_mask flag)
  let clear ~mask flag = Int64.logand mask (Int64.lognot (to_mask flag))
end

module Group = struct
  type t =
    | Function_flags
    | Function_and_public_flags

  type info = {
    group : t;
    source_name : string;
    mask : int64;
    members : Staging.t list;
    source_terms : string list;
    definition_line : int;
    consumers : source_reference list;
  }

  let all =
    [
      Function_flags;
      Function_and_public_flags;
    ]

  let info = function
    | Function_flags -> { group = Function_flags; source_name = "FSG_FUN_FLAGS1"; mask = 0xf00L; members = [ Staging.Interrupt; Staging.Has_error_code; Staging.Argument_pop; Staging.No_argument_pop ]; source_terms = [ "FSF_INTERRUPT"; "FSF_HASERRCODE"; "FSF_ARGPOP"; "FSF_NOARGPOP"]; definition_line = 367; consumers = [ { path = "Compiler/PrsStmt.HC"; line = 106 } ] }
    | Function_and_public_flags -> { group = Function_and_public_flags; source_name = "FSG_FUN_FLAGS2"; mask = 0xf01L; members = [ Staging.Interrupt; Staging.Has_error_code; Staging.Argument_pop; Staging.No_argument_pop; Staging.Public ]; source_terms = [ "FSG_FUN_FLAGS1"; "FSF_PUBLIC"]; definition_line = 368; consumers = [ { path = "Compiler/PrsStmt.HC"; line = 1072 }; { path = "Compiler/PrsStmt.HC"; line = 1075 }; { path = "Compiler/PrsStmt.HC"; line = 1078 }; { path = "Compiler/PrsStmt.HC"; line = 1081 }; { path = "Compiler/PrsStmt.HC"; line = 1084 } ] }

  let to_source_name group = (info group).source_name
  let of_source_name = function
    | "FSG_FUN_FLAGS1" -> Some Function_flags
    | "FSG_FUN_FLAGS2" -> Some Function_and_public_flags
    | _ -> None

  let to_mask group = (info group).mask
end

type transition_operation =
  | Add_bits of int64
  | Replace_preserving of { keep_mask : int64; add_mask : int64 }

module Modifier = struct
  type t =
    | Static
    | Interrupt
    | Has_error_code
    | Argument_pop
    | No_argument_pop
    | Public
    | Underscore_name

  type info = {
    modifier : t;
    spelling : string;
    operation : transition_operation;
    sources : source_reference list;
  }

  let all =
    [
      Static;
      Interrupt;
      Has_error_code;
      Argument_pop;
      No_argument_pop;
      Public;
      Underscore_name;
    ]

  let info = function
    | Static -> { modifier = Static; spelling = "static"; operation = Replace_preserving { keep_mask = 0x2L; add_mask = 0x4L }; sources = [ { path = "Compiler/PrsStmt.HC"; line = 1067 } ] }
    | Interrupt -> { modifier = Interrupt; spelling = "interrupt"; operation = Replace_preserving { keep_mask = 0xf03L; add_mask = 0x900L }; sources = [ { path = "Compiler/PrsStmt.HC"; line = 1070 } ] }
    | Has_error_code -> { modifier = Has_error_code; spelling = "haserrcode"; operation = Replace_preserving { keep_mask = 0xf03L; add_mask = 0x200L }; sources = [ { path = "Compiler/PrsStmt.HC"; line = 1074 } ] }
    | Argument_pop -> { modifier = Argument_pop; spelling = "argpop"; operation = Replace_preserving { keep_mask = 0xf03L; add_mask = 0x400L }; sources = [ { path = "Compiler/PrsStmt.HC"; line = 1077 } ] }
    | No_argument_pop -> { modifier = No_argument_pop; spelling = "noargpop"; operation = Replace_preserving { keep_mask = 0xf03L; add_mask = 0x800L }; sources = [ { path = "Compiler/PrsStmt.HC"; line = 1080 } ] }
    | Public -> { modifier = Public; spelling = "public"; operation = Replace_preserving { keep_mask = 0xf03L; add_mask = 0x1L }; sources = [ { path = "Compiler/PrsStmt.HC"; line = 1083 } ] }
    | Underscore_name -> { modifier = Underscore_name; spelling = "leading underscore in _extern or _import"; operation = Add_bits 0x8L; sources = [ { path = "Compiler/PrsStmt.HC"; line = 1003 }; { path = "Compiler/PrsStmt.HC"; line = 1017 } ] }

  let to_spelling modifier = (info modifier).spelling
end

let apply_modifier ~mask modifier =
  match (Modifier.info modifier).operation with
  | Add_bits bits -> Int64.logor mask bits
  | Replace_preserving { keep_mask; add_mask } ->
      Int64.logor (Int64.logand mask keep_mask) add_mask

let stored_of_staging = function
  | Staging.Interrupt -> Some Stored.Interrupt
  | Staging.Has_error_code -> Some Stored.Has_error_code
  | Staging.Argument_pop -> Some Stored.Argument_pop
  | Staging.No_argument_pop -> Some Stored.No_argument_pop
  | Staging.Public | Staging.Assembly | Staging.Static | Staging.Underscore_name -> None

let stored_mask_of_staging mask =
  Int64.logand mask (Group.to_mask Group.Function_flags)

let public_requested mask = Staging.is_set ~mask Staging.Public
let assembly_mode mask = Staging.is_set ~mask Staging.Assembly

let derives_ret1 ~argument_count ~variadic =
  (not variadic)
  && Int64.compare argument_count 0L > 0
  && Int64.compare argument_count 4095L <= 0

let caller_expects_callee_pop ~stored_mask =
  (Stored.is_set ~mask:stored_mask Stored.Ret1
  || Stored.is_set ~mask:stored_mask Stored.Argument_pop)
  && not (Stored.is_set ~mask:stored_mask Stored.No_argument_pop)

let interrupt_discards_error_code ~stored_mask =
  Stored.is_set ~mask:stored_mask Stored.Interrupt
  && Stored.is_set ~mask:stored_mask Stored.Has_error_code

let is_internal ~stored_mask = Stored.is_set ~mask:stored_mask Stored.Internal

type behavior_sources = {
  symbol_flag_transfer : source_reference;
  public_type_transfer : source_reference;
  automatic_ret1 : source_reference;
  variadic_declaration : source_reference;
  variadic_optimizer : source_reference;
  caller_cleanup : source_reference;
  try_cleanup : source_reference;
  internal_dispatch : source_reference;
  internal_clobber : source_reference;
  symbol_lookup_exclusion : source_reference;
  interrupt_restore : source_reference;
  interrupt_return : source_reference;
  interrupt_error_code : source_reference;
  callee_cleanup : source_reference;
  interrupt_save : source_reference;
}

let behavior_sources =
  {
    symbol_flag_transfer = { path = "Compiler/PrsStmt.HC"; line = 106 };
    public_type_transfer = { path = "Compiler/PrsStmt.HC"; line = 110 };
    automatic_ret1 = { path = "Compiler/PrsStmt.HC"; line = 116 };
    variadic_declaration = { path = "Compiler/PrsVar.HC"; line = 378 };
    variadic_optimizer = { path = "Compiler/OptPass3.HC"; line = 22 };
    caller_cleanup = { path = "Compiler/PrsExp.HC"; line = 572 };
    try_cleanup = { path = "Compiler/PrsStmt.HC"; line = 869 };
    internal_dispatch = { path = "Compiler/PrsExp.HC"; line = 555 };
    internal_clobber = { path = "Compiler/OptPass6.HC"; line = 45 };
    symbol_lookup_exclusion = { path = "Kernel/FunSeg.HC"; line = 23 };
    interrupt_restore = { path = "Compiler/OptPass789A.HC"; line = 390 };
    interrupt_return = { path = "Compiler/OptPass789A.HC"; line = 405 };
    interrupt_error_code = { path = "Compiler/OptPass789A.HC"; line = 406 };
    callee_cleanup = { path = "Compiler/OptPass789A.HC"; line = 410 };
    interrupt_save = { path = "Compiler/OptPass789A.HC"; line = 704 };
  }
