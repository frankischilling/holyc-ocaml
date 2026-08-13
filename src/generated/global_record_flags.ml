(* Generated from the pinned TempleOS global-record flag specification. *)

[@@@ocamlformat "disable"]

type source = { path : string; sha256 : string }
type source_reference = { path : string; line : int }
type global_type = { index_name : string; mask_name : string; type_index : int; type_mask : int64; index_definition_line : int; mask_definition_line : int }
type flag_info = { bit_name : string option; mask_name : string; bit_index : int; mask : int64; definition_line : int; consumers : source_reference list }
type behavior_info = { id : string; description : string; source : source_reference }

let reference_commit = "c26482bb6ad3f80106d28504ec5db3c6a360732c"

let sources =
  [
    { path = "Kernel/KernelA.HH"; sha256 = "1b4b6d8b6aeeaedfd2b11536b84557d9d2efc05ff38200020cd7a4a94dcd7d41" };
    { path = "Compiler/PrsStmt.HC"; sha256 = "6bccf67abed7cc634d07e6b7b0201f51ce00094a039b61a406e75de725c342af" };
    { path = "Compiler/PrsExp.HC"; sha256 = "e1746e64e943f26d6d96179379ec8684f24ae908614b965ea4e01f2d5398dd73" };
    { path = "Kernel/KHashB.HC"; sha256 = "260e17f94bfddf8c5d3d5f95dbffd3b8e34bcfd0581167275ed4cf736bfab03d" };
    { path = "Compiler/CHash.HC"; sha256 = "56d44aa0e437d69740a1eb401726833895762be973cf370ca9b399284344c9a1" };
    { path = "Compiler/AsmResolve.HC"; sha256 = "0bd47938221100b8d4786979f84735f4fdd5aff8406cab75db77a05ee3226df9" };
    { path = "Doc/ScopingLinkage.DD"; sha256 = "abe01d285ce3a2d02a36c0009b75145802816db0fe8503f579efc18d4d1f5635" };
  ]

let global_type = { index_name = "HTt_GLBL_VAR"; mask_name = "HTT_GLBL_VAR"; type_index = 3; type_mask = 0x8L; index_definition_line = 659; mask_definition_line = 689 }

module Hash_flag = struct
  type t =
    | Private
    | Public
    | Export
    | Import
    | Immediate
    | Goto_label
    | Resolve
    | Unresolved
    | Local

  let all =
    [
      Private;
      Public;
      Export;
      Import;
      Immediate;
      Goto_label;
      Resolve;
      Unresolved;
      Local;
    ]

  let to_source_name = function
    | Private -> "HTF_PRIVATE"
    | Public -> "HTF_PUBLIC"
    | Export -> "HTF_EXPORT"
    | Import -> "HTF_IMPORT"
    | Immediate -> "HTF_IMM"
    | Goto_label -> "HTF_GOTO_LABEL"
    | Resolve -> "HTF_RESOLVE"
    | Unresolved -> "HTF_UNRESOLVED"
    | Local -> "HTF_LOCAL"

  let of_source_name = function
    | "HTF_PRIVATE" -> Some Private
    | "HTF_PUBLIC" -> Some Public
    | "HTF_EXPORT" -> Some Export
    | "HTF_IMPORT" -> Some Import
    | "HTF_IMM" -> Some Immediate
    | "HTF_GOTO_LABEL" -> Some Goto_label
    | "HTF_RESOLVE" -> Some Resolve
    | "HTF_UNRESOLVED" -> Some Unresolved
    | "HTF_LOCAL" -> Some Local
    | _ -> None

  let info = function
    | Private -> { bit_name = Some "HTf_PRIVATE"; mask_name = "HTF_PRIVATE"; bit_index = 23; mask = 0x800000L; definition_line = 705; consumers = [ { path = "Kernel/KHashB.HC"; line = 161 }; { path = "Compiler/CHash.HC"; line = 104 } ] }
    | Public -> { bit_name = Some "HTf_PUBLIC"; mask_name = "HTF_PUBLIC"; bit_index = 24; mask = 0x1000000L; definition_line = 706; consumers = [ { path = "Compiler/PrsStmt.HC"; line = 389 } ] }
    | Export -> { bit_name = Some "HTf_EXPORT"; mask_name = "HTF_EXPORT"; bit_index = 25; mask = 0x2000000L; definition_line = 707; consumers = [ { path = "Compiler/PrsStmt.HC"; line = 303 }; { path = "Compiler/PrsStmt.HC"; line = 362 }; { path = "Compiler/AsmResolve.HC"; line = 167 } ] }
    | Import -> { bit_name = Some "HTf_IMPORT"; mask_name = "HTF_IMPORT"; bit_index = 26; mask = 0x4000000L; definition_line = 708; consumers = [ { path = "Compiler/PrsStmt.HC"; line = 318 }; { path = "Compiler/CHash.HC"; line = 104 }; { path = "Compiler/AsmResolve.HC"; line = 137 } ] }
    | Immediate -> { bit_name = Some "HTf_IMM"; mask_name = "HTF_IMM"; bit_index = 27; mask = 0x8000000L; definition_line = 709; consumers = [  ] }
    | Goto_label -> { bit_name = Some "HTf_GOTO_LABEL"; mask_name = "HTF_GOTO_LABEL"; bit_index = 28; mask = 0x10000000L; definition_line = 710; consumers = [ { path = "Compiler/AsmResolve.HC"; line = 137 } ] }
    | Resolve -> { bit_name = Some "HTf_RESOLVED"; mask_name = "HTF_RESOLVE"; bit_index = 29; mask = 0x20000000L; definition_line = 711; consumers = [ { path = "Compiler/AsmResolve.HC"; line = 167 } ] }
    | Unresolved -> { bit_name = Some "HTf_UNRESOLVED"; mask_name = "HTF_UNRESOLVED"; bit_index = 30; mask = 0x40000000L; definition_line = 712; consumers = [ { path = "Compiler/PrsStmt.HC"; line = 331 } ] }
    | Local -> { bit_name = Some "HTf_LOCAL"; mask_name = "HTF_LOCAL"; bit_index = 31; mask = 0x80000000L; definition_line = 713; consumers = [  ] }

  let to_bit_index flag = (info flag).bit_index
  let to_mask flag = (info flag).mask
  let is_set ~mask flag = Int64.logand mask (to_mask flag) <> 0L
  let set ~mask flag = Int64.logor mask (to_mask flag)
  let clear ~mask flag = Int64.logand mask (Int64.lognot (to_mask flag))
end

module Global_flag = struct
  type t =
    | Function_pointer
    | Import
    | Extern
    | Data_heap
    | Alias
    | Array

  let all =
    [
      Function_pointer;
      Import;
      Extern;
      Data_heap;
      Alias;
      Array;
    ]

  let to_source_name = function
    | Function_pointer -> "GVF_FUN"
    | Import -> "GVF_IMPORT"
    | Extern -> "GVF_EXTERN"
    | Data_heap -> "GVF_DATA_HEAP"
    | Alias -> "GVF_ALIAS"
    | Array -> "GVF_ARRAY"

  let of_source_name = function
    | "GVF_FUN" -> Some Function_pointer
    | "GVF_IMPORT" -> Some Import
    | "GVF_EXTERN" -> Some Extern
    | "GVF_DATA_HEAP" -> Some Data_heap
    | "GVF_ALIAS" -> Some Alias
    | "GVF_ARRAY" -> Some Array
    | _ -> None

  let info = function
    | Function_pointer -> { bit_name = None; mask_name = "GVF_FUN"; bit_index = 0; mask = 0x1L; definition_line = 870; consumers = [ { path = "Compiler/PrsStmt.HC"; line = 405 }; { path = "Compiler/PrsExp.HC"; line = 899 } ] }
    | Import -> { bit_name = None; mask_name = "GVF_IMPORT"; bit_index = 1; mask = 0x2L; definition_line = 871; consumers = [ { path = "Compiler/PrsStmt.HC"; line = 400 }; { path = "Compiler/PrsExp.HC"; line = 881 } ] }
    | Extern -> { bit_name = None; mask_name = "GVF_EXTERN"; bit_index = 2; mask = 0x4L; definition_line = 872; consumers = [ { path = "Compiler/PrsStmt.HC"; line = 402 }; { path = "Kernel/KHashB.HC"; line = 25 }; { path = "Compiler/PrsExp.HC"; line = 891 } ] }
    | Data_heap -> { bit_name = None; mask_name = "GVF_DATA_HEAP"; bit_index = 3; mask = 0x8L; definition_line = 873; consumers = [ { path = "Compiler/PrsStmt.HC"; line = 340 }; { path = "Compiler/PrsStmt.HC"; line = 372 }; { path = "Compiler/PrsExp.HC"; line = 885 } ] }
    | Alias -> { bit_name = None; mask_name = "GVF_ALIAS"; bit_index = 4; mask = 0x10L; definition_line = 874; consumers = [ { path = "Compiler/PrsStmt.HC"; line = 310 }; { path = "Compiler/PrsStmt.HC"; line = 443 }; { path = "Kernel/KHashB.HC"; line = 78 } ] }
    | Array -> { bit_name = None; mask_name = "GVF_ARRAY"; bit_index = 5; mask = 0x20L; definition_line = 875; consumers = [ { path = "Compiler/PrsStmt.HC"; line = 408 }; { path = "Compiler/PrsExp.HC"; line = 873 } ] }

  let to_bit_index flag = (info flag).bit_index
  let to_mask flag = (info flag).mask
  let is_set ~mask flag = Int64.logand mask (to_mask flag) <> 0L
  let set ~mask flag = Int64.logor mask (to_mask flag)
  let clear ~mask flag = Int64.logand mask (Int64.lognot (to_mask flag))
end

let behaviors =
  [
    { id = "private-from-option"; description = "OPTf_KEEP_PRIVATE marks source-backed hash records private"; source = { path = "Kernel/KHashB.HC"; line = 161 } };
    { id = "public-from-modifier"; description = "the final parser staging mask publishes a global record"; source = { path = "Compiler/PrsStmt.HC"; line = 389 } };
    { id = "alternate-extern-export"; description = "an AOT alternate extern is an exported global record"; source = { path = "Compiler/PrsStmt.HC"; line = 303 } };
    { id = "alternate-extern-alias"; description = "an alternate extern does not own its bound storage"; source = { path = "Compiler/PrsStmt.HC"; line = 310 } };
    { id = "import-record"; description = "AOT imports carry both hash and global import state"; source = { path = "Compiler/PrsStmt.HC"; line = 318 } };
    { id = "jit-extern-unresolved"; description = "a plain JIT extern starts unresolved"; source = { path = "Compiler/PrsStmt.HC"; line = 331 } };
    { id = "aot-code-heap-export"; description = "an ordinary AOT code-heap global is exported"; source = { path = "Compiler/PrsStmt.HC"; line = 362 } };
    { id = "aot-data-heap"; description = "an AOT data-heap definition carries GVF_DATA_HEAP"; source = { path = "Compiler/PrsStmt.HC"; line = 340 } };
    { id = "jit-data-heap"; description = "a JIT data-heap definition carries GVF_DATA_HEAP"; source = { path = "Compiler/PrsStmt.HC"; line = 372 } };
    { id = "global-import-flag"; description = "ordinary and alternate imports carry GVF_IMPORT"; source = { path = "Compiler/PrsStmt.HC"; line = 400 } };
    { id = "global-extern-flag"; description = "only a plain extern carries GVF_EXTERN"; source = { path = "Compiler/PrsStmt.HC"; line = 402 } };
    { id = "function-pointer-flag"; description = "function-pointer globals carry GVF_FUN"; source = { path = "Compiler/PrsStmt.HC"; line = 405 } };
    { id = "array-flag"; description = "array globals carry GVF_ARRAY"; source = { path = "Compiler/PrsStmt.HC"; line = 408 } };
    { id = "alias-transfer"; description = "a superseded global record becomes an alias"; source = { path = "Compiler/PrsStmt.HC"; line = 443 } };
    { id = "extern-value-slot"; description = "HashVal returns an extern record's address slot"; source = { path = "Kernel/KHashB.HC"; line = 25 } };
    { id = "alias-does-not-own-data"; description = "deleting an alias record does not free its data address"; source = { path = "Kernel/KHashB.HC"; line = 78 } };
    { id = "map-omits-import-private"; description = "map output omits imported and private records"; source = { path = "Compiler/CHash.HC"; line = 104 } };
    { id = "aot-import-publication"; description = "AOT resolution emits used import records"; source = { path = "Compiler/AsmResolve.HC"; line = 137 } };
    { id = "aot-export-publication"; description = "AOT resolution emits an export for HTF_EXPORT"; source = { path = "Compiler/AsmResolve.HC"; line = 167 } };
    { id = "expression-array"; description = "global expression parsing preserves array dimensions"; source = { path = "Compiler/PrsExp.HC"; line = 873 } };
    { id = "expression-aot-import"; description = "AOT global access lowers imports separately"; source = { path = "Compiler/PrsExp.HC"; line = 881 } };
    { id = "expression-aot-data-heap"; description = "AOT global access lowers data-heap storage separately"; source = { path = "Compiler/PrsExp.HC"; line = 885 } };
    { id = "expression-jit-extern"; description = "JIT extern access dereferences the address slot"; source = { path = "Compiler/PrsExp.HC"; line = 891 } };
    { id = "expression-function-pointer"; description = "a GVF_FUN record is exposed as a callable expression"; source = { path = "Compiler/PrsExp.HC"; line = 899 } };
    { id = "documented-jit-extern"; description = "the linkage guide identifies JIT extern binding"; source = { path = "Doc/ScopingLinkage.DD"; line = 7 } };
  ]

let behavior id = List.find_opt (fun item -> String.equal item.id id) behaviors
