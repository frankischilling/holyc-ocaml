(* This table is generated from the pinned TempleOS type definitions.
   Run the primitive type generator after an approved reference update. *)

[@@@ocamlformat "disable"]

let reference_commit = "c26482bb6ad3f80106d28504ec5db3c6a360732c"
let kernel_source_path = "Kernel/KernelA.HH"
let kernel_source_sha256 = "1b4b6d8b6aeeaedfd2b11536b84557d9d2efc05ff38200020cd7a4a94dcd7d41"
let cinit_source_path = "Compiler/CInit.HC"
let cinit_source_sha256 = "f187d11043dcceb8791409a3e6809ea26e9c3b4f182fe2cbe5c5e644e6938b19"

type raw_type = {
  name : string;
  templeos_id : int;
  not_implemented : bool;
  fictitious : bool;
  source_line : int;
  source_comment : string option;
}

type raw_alias = {
  name : string;
  target_name : string;
  templeos_id : int;
  source_line : int;
  source_comment : string option;
}

type public_union = {
  storage_spelling : string;
  public_spelling : string;
  source_line : int;
}

type internal_type = {
  raw_name : string;
  byte_size : int;
  spelling : string;
  source_line : int;
}

let raw_types =
  [
    { name = "RT_I0"; templeos_id = 2; not_implemented = false; fictitious = false; source_line = 1564; source_comment = None };
    { name = "RT_U0"; templeos_id = 3; not_implemented = false; fictitious = false; source_line = 1565; source_comment = None };
    { name = "RT_I8"; templeos_id = 4; not_implemented = false; fictitious = false; source_line = 1566; source_comment = None };
    { name = "RT_U8"; templeos_id = 5; not_implemented = false; fictitious = false; source_line = 1567; source_comment = None };
    { name = "RT_I16"; templeos_id = 6; not_implemented = false; fictitious = false; source_line = 1568; source_comment = None };
    { name = "RT_U16"; templeos_id = 7; not_implemented = false; fictitious = false; source_line = 1569; source_comment = None };
    { name = "RT_I32"; templeos_id = 8; not_implemented = false; fictitious = false; source_line = 1570; source_comment = None };
    { name = "RT_U32"; templeos_id = 9; not_implemented = false; fictitious = false; source_line = 1571; source_comment = None };
    { name = "RT_I64"; templeos_id = 10; not_implemented = false; fictitious = false; source_line = 1572; source_comment = None };
    { name = "RT_U64"; templeos_id = 11; not_implemented = false; fictitious = false; source_line = 1574; source_comment = None };
    { name = "RT_F32"; templeos_id = 12; not_implemented = true; fictitious = false; source_line = 1575; source_comment = Some "Not implemented" };
    { name = "RT_UF32"; templeos_id = 13; not_implemented = true; fictitious = true; source_line = 1576; source_comment = Some "Not implemented, Fictitious" };
    { name = "RT_F64"; templeos_id = 14; not_implemented = false; fictitious = false; source_line = 1577; source_comment = None };
    { name = "RT_UF64"; templeos_id = 15; not_implemented = false; fictitious = true; source_line = 1578; source_comment = Some "Fictitious" };
  ]

let pointer_alias =
  { name = "RT_PTR"; target_name = "RT_I64"; templeos_id = 10; source_line = 1573; source_comment = Some "Signed to allow negative err codes. $LK,\"DOCM_CANCEL\",A=\"MN:DOCM_CANCEL\"$" }

let raw_types_count = 16
let unsigned_flag = 1
let raw_group_mask = 255

let public_unions =
  [
    { storage_spelling = "U16i"; public_spelling = "U16"; source_line = 67 };
    { storage_spelling = "I16i"; public_spelling = "I16"; source_line = 73 };
    { storage_spelling = "U32i"; public_spelling = "U32"; source_line = 79 };
    { storage_spelling = "I32i"; public_spelling = "I32"; source_line = 87 };
    { storage_spelling = "U64i"; public_spelling = "U64"; source_line = 95 };
    { storage_spelling = "I64i"; public_spelling = "I64"; source_line = 105 };
  ]

let internal_types =
  [
    { raw_name = "RT_I0"; byte_size = 0; spelling = "I0i"; source_line = 7 };
    { raw_name = "RT_I0"; byte_size = 0; spelling = "I0"; source_line = 7 };
    { raw_name = "RT_U0"; byte_size = 0; spelling = "U0i"; source_line = 7 };
    { raw_name = "RT_U0"; byte_size = 0; spelling = "U0"; source_line = 7 };
    { raw_name = "RT_I8"; byte_size = 1; spelling = "I8i"; source_line = 8 };
    { raw_name = "RT_I8"; byte_size = 1; spelling = "I8"; source_line = 8 };
    { raw_name = "RT_I8"; byte_size = 1; spelling = "Bool"; source_line = 8 };
    { raw_name = "RT_U8"; byte_size = 1; spelling = "U8i"; source_line = 9 };
    { raw_name = "RT_U8"; byte_size = 1; spelling = "U8"; source_line = 9 };
    { raw_name = "RT_I16"; byte_size = 2; spelling = "I16i"; source_line = 10 };
    { raw_name = "RT_U16"; byte_size = 2; spelling = "U16i"; source_line = 10 };
    { raw_name = "RT_I32"; byte_size = 4; spelling = "I32i"; source_line = 11 };
    { raw_name = "RT_U32"; byte_size = 4; spelling = "U32i"; source_line = 11 };
    { raw_name = "RT_I64"; byte_size = 8; spelling = "I64i"; source_line = 12 };
    { raw_name = "RT_U64"; byte_size = 8; spelling = "U64i"; source_line = 12 };
    { raw_name = "RT_F64"; byte_size = 8; spelling = "F64i"; source_line = 13 };
    { raw_name = "RT_F64"; byte_size = 8; spelling = "F64"; source_line = 13 };
  ]
