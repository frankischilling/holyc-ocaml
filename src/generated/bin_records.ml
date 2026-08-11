(* Generated from the pinned TempleOS BIN definitions and consumers. *)

[@@@ocamlformat "disable"]

type source = { path : string; sha256 : string }
type source_reference = { path : string; line : int }

type width = Width_0 | Width_8 | Width_16 | Width_32 | Width_64

let width_bytes = function
  | Width_0 -> 0
  | Width_8 -> 1
  | Width_16 -> 2
  | Width_32 -> 4
  | Width_64 -> 8

type source_status = Source_active | Source_fictitious | Source_not_implemented | Source_not_really_used
type category = Terminator | Import | Export | Absolute_addresses | Code_heap | Data_heap | Main
type leading_value = No_leading_value | Patch_offset | Export_value | Entry_count | Main_offset
type name_mode = No_name_field | Required_name | First_name_then_inherited | Optional_export_name | Empty_name
type payload = No_payload | U32_offsets | I32_size_then_u32_offsets | I64_size_then_u32_offsets
type relocation_kind = Relative | Immediate
type relocation = { kind : relocation_kind; width : width; displacement_bias : int }
type pass1_action = Pass1_stop | Resolve_import | Register_relative_export | Register_immediate_export | Apply_module_base_u32 | Allocate_code | Allocate_zeroed_code | Allocate_data | Allocate_zeroed_data | Pass1_ignore
type pass2_action = Pass2_stop | Execute_main | Skip_u32_offsets | Skip_i32_size_and_u32_offsets | Skip_i64_size_and_u32_offsets | Pass2_ignore

type header_field = { name : string; source_type : string; width_bytes : int; offset : int; definition_line : int }

type adjustment_operation = Add | Subtract

let reference_commit = "c26482bb6ad3f80106d28504ec5db3c6a360732c"

let sources =
  [
    { path = "Kernel/KernelA.HH"; sha256 = "1b4b6d8b6aeeaedfd2b11536b84557d9d2efc05ff38200020cd7a4a94dcd7d41" };
    { path = "Kernel/KLoad.HC"; sha256 = "da0c11feab995197faf94593bf5eaebb7f110ce2aaefa1ef2583b10002c6f88b" };
    { path = "Kernel/KStart16.HC"; sha256 = "db71f3cef755745bb23ff6888cdf958e6e63046f2e68a9d96b7c9c0ec0685a85" };
    { path = "Kernel/KStart32.HC"; sha256 = "8b913008c3f10876353889562da44c26fadcb3b5e6618f48d2ee56da39b8adc2" };
    { path = "Compiler/CMain.HC"; sha256 = "318717f411327cdb5b4b4f2d4836e0e9ec69995f80403f0eb2b6d527b2a43647" };
    { path = "Compiler/AsmResolve.HC"; sha256 = "0bd47938221100b8d4786979f84735f4fdd5aff8406cab75db77a05ee3226df9" };
    { path = "Compiler/Asm.HC"; sha256 = "00f2b2fa8b32eff2ceb3c338b796b1608e2b71b3134f3782cb61b0d460fbfec0" };
    { path = "Compiler/PrsVar.HC"; sha256 = "a4d090d96e13f2358aa9914699aefd58f6b785ba5e444d6e9e59ffafcc28bae8" };
    { path = "Compiler/BackFA.HC"; sha256 = "600aff7a73711571ac764f02bbbd6d8cb41e5651867e59dd496182866d93e924" };
    { path = "Compiler/BackLib.HC"; sha256 = "1bb302eec16d8231e08ea77a83a3e05d253c45202521510d4450c23a22518e7d" };
    { path = "Compiler/BackC.HC"; sha256 = "207ca307204ffa8a29870dae38f9663bd3b2021d78caf35ece0f68380feb6139" };
    { path = "Compiler/OptPass3.HC"; sha256 = "64c3b74d073b2d482751b6129d99e9f4b3b563216ec70f4a17a79aa663a92eae" };
    { path = "Compiler/OptPass789A.HC"; sha256 = "7cae9bc11a863d2066df27894449dbb28afb0d550b6b3fae1eeb6bb1362addcc" };
  ]

let signature_spelling = "'TOSB'"
let signature_value = 0x42534f54l
let signature_definition_line = 383
let header_size = 32
let immediate_not_relative_mask = 1
let immediate_not_relative_definition_line = 416

let header_fields =
  [
    { name = "jmp"; source_type = "U16"; width_bytes = 2; offset = 0; definition_line = 386 };
    { name = "module_align_bits"; source_type = "U8"; width_bytes = 1; offset = 2; definition_line = 387 };
    { name = "reserved"; source_type = "U8"; width_bytes = 1; offset = 3; definition_line = 388 };
    { name = "bin_signature"; source_type = "U32"; width_bytes = 4; offset = 4; definition_line = 389 };
    { name = "org"; source_type = "I64"; width_bytes = 8; offset = 8; definition_line = 390 };
    { name = "patch_table_offset"; source_type = "I64"; width_bytes = 8; offset = 16; definition_line = 391 };
    { name = "file_size"; source_type = "I64"; width_bytes = 8; offset = 24; definition_line = 392 };
  ]

let reserved_codes = [ 1; 12; 13; 14; 15 ]

module Entry = struct
  type t =
    | End
    | Rel_i0
    | Imm_u0
    | Rel_i8
    | Imm_u8
    | Rel_i16
    | Imm_u16
    | Rel_i32
    | Imm_u32
    | Rel_i64
    | Imm_i64
    | Rel32_export
    | Imm32_export
    | Rel64_export
    | Imm64_export
    | Abs_addr
    | Code_heap
    | Zeroed_code_heap
    | Data_heap
    | Zeroed_data_heap
    | Main

  type info = { entry : t; source_name : string; code : int; status : source_status; category : category; leading_value : leading_value; name_mode : name_mode; payload : payload; relocation : relocation option; pass1 : pass1_action; pass2 : pass2_action; definition_line : int; consumers : source_reference list }

  let all =
    [
      End;
      Rel_i0;
      Imm_u0;
      Rel_i8;
      Imm_u8;
      Rel_i16;
      Imm_u16;
      Rel_i32;
      Imm_u32;
      Rel_i64;
      Imm_i64;
      Rel32_export;
      Imm32_export;
      Rel64_export;
      Imm64_export;
      Abs_addr;
      Code_heap;
      Zeroed_code_heap;
      Data_heap;
      Zeroed_data_heap;
      Main;
    ]

  let info = function
    | End ->
        { entry = End; source_name = "IET_END"; code = 0; status = Source_active; category = Terminator; leading_value = No_leading_value; name_mode = No_name_field; payload = No_payload; relocation = None; pass1 = Pass1_stop; pass2 = Pass2_stop; definition_line = 404; consumers = [{ path = "Compiler/CMain.HC"; line = 534 }]; }
    | Rel_i0 ->
        { entry = Rel_i0; source_name = "IET_REL_I0"; code = 2; status = Source_fictitious; category = Import; leading_value = Patch_offset; name_mode = First_name_then_inherited; payload = No_payload; relocation = Some { kind = Relative; width = Width_0; displacement_bias = 0 }; pass1 = Resolve_import; pass2 = Pass2_ignore; definition_line = 406; consumers = [{ path = "Compiler/CMain.HC"; line = 225 }; { path = "Compiler/CMain.HC"; line = 254 }; { path = "Compiler/CMain.HC"; line = 335 }; { path = "Compiler/CMain.HC"; line = 354 }; { path = "Compiler/CMain.HC"; line = 402 }; { path = "Compiler/OptPass3.HC"; line = 278 }; { path = "Kernel/KLoad.HC"; line = 91 }]; }
    | Imm_u0 ->
        { entry = Imm_u0; source_name = "IET_IMM_U0"; code = 3; status = Source_fictitious; category = Import; leading_value = Patch_offset; name_mode = First_name_then_inherited; payload = No_payload; relocation = Some { kind = Immediate; width = Width_0; displacement_bias = 0 }; pass1 = Resolve_import; pass2 = Pass2_ignore; definition_line = 407; consumers = [{ path = "Compiler/CMain.HC"; line = 255 }; { path = "Compiler/CMain.HC"; line = 355 }; { path = "Compiler/CMain.HC"; line = 397 }]; }
    | Rel_i8 ->
        { entry = Rel_i8; source_name = "IET_REL_I8"; code = 4; status = Source_active; category = Import; leading_value = Patch_offset; name_mode = First_name_then_inherited; payload = No_payload; relocation = Some { kind = Relative; width = Width_8; displacement_bias = 1 }; pass1 = Resolve_import; pass2 = Pass2_ignore; definition_line = 408; consumers = [{ path = "Compiler/Asm.HC"; line = 279 }; { path = "Compiler/AsmResolve.HC"; line = 21 }; { path = "Compiler/CMain.HC"; line = 257 }; { path = "Compiler/CMain.HC"; line = 357 }; { path = "Compiler/CMain.HC"; line = 404 }; { path = "Kernel/KLoad.HC"; line = 41 }]; }
    | Imm_u8 ->
        { entry = Imm_u8; source_name = "IET_IMM_U8"; code = 5; status = Source_active; category = Import; leading_value = Patch_offset; name_mode = First_name_then_inherited; payload = No_payload; relocation = Some { kind = Immediate; width = Width_8; displacement_bias = 0 }; pass1 = Resolve_import; pass2 = Pass2_ignore; definition_line = 409; consumers = [{ path = "Compiler/AsmResolve.HC"; line = 27 }; { path = "Compiler/CMain.HC"; line = 262 }; { path = "Compiler/CMain.HC"; line = 362 }; { path = "Compiler/CMain.HC"; line = 398 }; { path = "Kernel/KLoad.HC"; line = 42 }]; }
    | Rel_i16 ->
        { entry = Rel_i16; source_name = "IET_REL_I16"; code = 6; status = Source_active; category = Import; leading_value = Patch_offset; name_mode = First_name_then_inherited; payload = No_payload; relocation = Some { kind = Relative; width = Width_16; displacement_bias = 2 }; pass1 = Resolve_import; pass2 = Pass2_ignore; definition_line = 410; consumers = [{ path = "Compiler/Asm.HC"; line = 306 }; { path = "Compiler/AsmResolve.HC"; line = 22 }; { path = "Compiler/AsmResolve.HC"; line = 32 }; { path = "Compiler/CMain.HC"; line = 265 }; { path = "Compiler/CMain.HC"; line = 365 }; { path = "Compiler/CMain.HC"; line = 405 }; { path = "Kernel/KLoad.HC"; line = 43 }]; }
    | Imm_u16 ->
        { entry = Imm_u16; source_name = "IET_IMM_U16"; code = 7; status = Source_active; category = Import; leading_value = Patch_offset; name_mode = First_name_then_inherited; payload = No_payload; relocation = Some { kind = Immediate; width = Width_16; displacement_bias = 0 }; pass1 = Resolve_import; pass2 = Pass2_ignore; definition_line = 411; consumers = [{ path = "Compiler/CMain.HC"; line = 270 }; { path = "Compiler/CMain.HC"; line = 370 }; { path = "Compiler/CMain.HC"; line = 399 }; { path = "Kernel/KLoad.HC"; line = 44 }]; }
    | Rel_i32 ->
        { entry = Rel_i32; source_name = "IET_REL_I32"; code = 8; status = Source_active; category = Import; leading_value = Patch_offset; name_mode = First_name_then_inherited; payload = No_payload; relocation = Some { kind = Relative; width = Width_32; displacement_bias = 4 }; pass1 = Resolve_import; pass2 = Pass2_ignore; definition_line = 412; consumers = [{ path = "Compiler/Asm.HC"; line = 333 }; { path = "Compiler/AsmResolve.HC"; line = 34 }; { path = "Compiler/BackFA.HC"; line = 301 }; { path = "Compiler/CMain.HC"; line = 273 }; { path = "Compiler/CMain.HC"; line = 373 }; { path = "Compiler/CMain.HC"; line = 406 }; { path = "Compiler/OptPass789A.HC"; line = 377 }; { path = "Kernel/KLoad.HC"; line = 45 }]; }
    | Imm_u32 ->
        { entry = Imm_u32; source_name = "IET_IMM_U32"; code = 9; status = Source_active; category = Import; leading_value = Patch_offset; name_mode = First_name_then_inherited; payload = No_payload; relocation = Some { kind = Immediate; width = Width_32; displacement_bias = 0 }; pass1 = Resolve_import; pass2 = Pass2_ignore; definition_line = 413; consumers = [{ path = "Compiler/CMain.HC"; line = 278 }; { path = "Compiler/CMain.HC"; line = 378 }; { path = "Compiler/CMain.HC"; line = 400 }; { path = "Compiler/OptPass789A.HC"; line = 130 }; { path = "Kernel/KLoad.HC"; line = 46 }]; }
    | Rel_i64 ->
        { entry = Rel_i64; source_name = "IET_REL_I64"; code = 10; status = Source_active; category = Import; leading_value = Patch_offset; name_mode = First_name_then_inherited; payload = No_payload; relocation = Some { kind = Relative; width = Width_64; displacement_bias = 8 }; pass1 = Resolve_import; pass2 = Pass2_ignore; definition_line = 414; consumers = [{ path = "Compiler/Asm.HC"; line = 359 }; { path = "Compiler/AsmResolve.HC"; line = 36 }; { path = "Compiler/CMain.HC"; line = 281 }; { path = "Compiler/CMain.HC"; line = 381 }; { path = "Compiler/CMain.HC"; line = 407 }; { path = "Kernel/KLoad.HC"; line = 47 }]; }
    | Imm_i64 ->
        { entry = Imm_i64; source_name = "IET_IMM_I64"; code = 11; status = Source_active; category = Import; leading_value = Patch_offset; name_mode = First_name_then_inherited; payload = No_payload; relocation = Some { kind = Immediate; width = Width_64; displacement_bias = 0 }; pass1 = Resolve_import; pass2 = Pass2_ignore; definition_line = 415; consumers = [{ path = "Compiler/BackFA.HC"; line = 290 }; { path = "Compiler/CMain.HC"; line = 225 }; { path = "Compiler/CMain.HC"; line = 284 }; { path = "Compiler/CMain.HC"; line = 335 }; { path = "Compiler/CMain.HC"; line = 384 }; { path = "Compiler/CMain.HC"; line = 401 }; { path = "Compiler/OptPass3.HC"; line = 278 }; { path = "Compiler/OptPass789A.HC"; line = 365 }; { path = "Kernel/KLoad.HC"; line = 48 }; { path = "Kernel/KLoad.HC"; line = 91 }]; }
    | Rel32_export ->
        { entry = Rel32_export; source_name = "IET_REL32_EXPORT"; code = 16; status = Source_active; category = Export; leading_value = Export_value; name_mode = Required_name; payload = No_payload; relocation = None; pass1 = Register_relative_export; pass2 = Pass2_ignore; definition_line = 418; consumers = [{ path = "Compiler/AsmResolve.HC"; line = 169 }; { path = "Compiler/CMain.HC"; line = 208 }; { path = "Compiler/CMain.HC"; line = 393 }; { path = "Compiler/CMain.HC"; line = 496 }; { path = "Kernel/KLoad.HC"; line = 77 }]; }
    | Imm32_export ->
        { entry = Imm32_export; source_name = "IET_IMM32_EXPORT"; code = 17; status = Source_active; category = Export; leading_value = Export_value; name_mode = Required_name; payload = No_payload; relocation = None; pass1 = Register_immediate_export; pass2 = Pass2_ignore; definition_line = 419; consumers = [{ path = "Compiler/CMain.HC"; line = 209 }; { path = "Compiler/CMain.HC"; line = 216 }; { path = "Compiler/CMain.HC"; line = 394 }; { path = "Kernel/KLoad.HC"; line = 78 }; { path = "Kernel/KLoad.HC"; line = 84 }]; }
    | Rel64_export ->
        { entry = Rel64_export; source_name = "IET_REL64_EXPORT"; code = 18; status = Source_not_implemented; category = Export; leading_value = Export_value; name_mode = Required_name; payload = No_payload; relocation = None; pass1 = Register_relative_export; pass2 = Pass2_ignore; definition_line = 420; consumers = [{ path = "Compiler/CMain.HC"; line = 210 }; { path = "Compiler/CMain.HC"; line = 395 }; { path = "Kernel/KLoad.HC"; line = 79 }]; }
    | Imm64_export ->
        { entry = Imm64_export; source_name = "IET_IMM64_EXPORT"; code = 19; status = Source_not_implemented; category = Export; leading_value = Export_value; name_mode = Required_name; payload = No_payload; relocation = None; pass1 = Register_immediate_export; pass2 = Pass2_ignore; definition_line = 421; consumers = [{ path = "Compiler/CMain.HC"; line = 211 }; { path = "Compiler/CMain.HC"; line = 216 }; { path = "Compiler/CMain.HC"; line = 396 }; { path = "Compiler/CMain.HC"; line = 496 }; { path = "Kernel/KLoad.HC"; line = 80 }; { path = "Kernel/KLoad.HC"; line = 84 }]; }
    | Abs_addr ->
        { entry = Abs_addr; source_name = "IET_ABS_ADDR"; code = 20; status = Source_active; category = Absolute_addresses; leading_value = Entry_count; name_mode = Empty_name; payload = U32_offsets; relocation = None; pass1 = Apply_module_base_u32; pass2 = Skip_u32_offsets; definition_line = 422; consumers = [{ path = "Compiler/CMain.HC"; line = 448 }; { path = "Kernel/KLoad.HC"; line = 95 }; { path = "Kernel/KLoad.HC"; line = 166 }]; }
    | Code_heap ->
        { entry = Code_heap; source_name = "IET_CODE_HEAP"; code = 21; status = Source_not_really_used; category = Code_heap; leading_value = Entry_count; name_mode = Optional_export_name; payload = I32_size_then_u32_offsets; relocation = None; pass1 = Allocate_code; pass2 = Skip_i32_size_and_u32_offsets; definition_line = 423; consumers = [{ path = "Kernel/KLoad.HC"; line = 108 }; { path = "Kernel/KLoad.HC"; line = 169 }]; }
    | Zeroed_code_heap ->
        { entry = Zeroed_code_heap; source_name = "IET_ZEROED_CODE_HEAP"; code = 22; status = Source_not_really_used; category = Code_heap; leading_value = Entry_count; name_mode = Optional_export_name; payload = I32_size_then_u32_offsets; relocation = None; pass1 = Allocate_zeroed_code; pass2 = Skip_i32_size_and_u32_offsets; definition_line = 424; consumers = [{ path = "Kernel/KLoad.HC"; line = 111 }; { path = "Kernel/KLoad.HC"; line = 170 }]; }
    | Data_heap ->
        { entry = Data_heap; source_name = "IET_DATA_HEAP"; code = 23; status = Source_active; category = Data_heap; leading_value = Entry_count; name_mode = Optional_export_name; payload = I64_size_then_u32_offsets; relocation = None; pass1 = Allocate_data; pass2 = Skip_i64_size_and_u32_offsets; definition_line = 425; consumers = [{ path = "Compiler/CMain.HC"; line = 470 }; { path = "Kernel/KLoad.HC"; line = 130 }; { path = "Kernel/KLoad.HC"; line = 173 }]; }
    | Zeroed_data_heap ->
        { entry = Zeroed_data_heap; source_name = "IET_ZEROED_DATA_HEAP"; code = 24; status = Source_not_really_used; category = Data_heap; leading_value = Entry_count; name_mode = Optional_export_name; payload = I64_size_then_u32_offsets; relocation = None; pass1 = Allocate_zeroed_data; pass2 = Skip_i64_size_and_u32_offsets; definition_line = 426; consumers = [{ path = "Kernel/KLoad.HC"; line = 133 }; { path = "Kernel/KLoad.HC"; line = 174 }]; }
    | Main ->
        { entry = Main; source_name = "IET_MAIN"; code = 25; status = Source_active; category = Main; leading_value = Main_offset; name_mode = Empty_name; payload = No_payload; relocation = None; pass1 = Pass1_ignore; pass2 = Execute_main; definition_line = 427; consumers = [{ path = "Compiler/CMain.HC"; line = 87 }; { path = "Compiler/PrsVar.HC"; line = 83 }; { path = "Compiler/PrsVar.HC"; line = 234 }; { path = "Kernel/KLoad.HC"; line = 163 }]; }

  let to_source_name entry = (info entry).source_name
  let to_code entry = (info entry).code

  let of_code = function
    | 0 -> Some End
    | 2 -> Some Rel_i0
    | 3 -> Some Imm_u0
    | 4 -> Some Rel_i8
    | 5 -> Some Imm_u8
    | 6 -> Some Rel_i16
    | 7 -> Some Imm_u16
    | 8 -> Some Rel_i32
    | 9 -> Some Imm_u32
    | 10 -> Some Rel_i64
    | 11 -> Some Imm_i64
    | 16 -> Some Rel32_export
    | 17 -> Some Imm32_export
    | 18 -> Some Rel64_export
    | 19 -> Some Imm64_export
    | 20 -> Some Abs_addr
    | 21 -> Some Code_heap
    | 22 -> Some Zeroed_code_heap
    | 23 -> Some Data_heap
    | 24 -> Some Zeroed_data_heap
    | 25 -> Some Main
    | _ -> None

  let of_source_name = function
    | "IET_END" -> Some End
    | "IET_REL_I0" -> Some Rel_i0
    | "IET_IMM_U0" -> Some Imm_u0
    | "IET_REL_I8" -> Some Rel_i8
    | "IET_IMM_U8" -> Some Imm_u8
    | "IET_REL_I16" -> Some Rel_i16
    | "IET_IMM_U16" -> Some Imm_u16
    | "IET_REL_I32" -> Some Rel_i32
    | "IET_IMM_U32" -> Some Imm_u32
    | "IET_REL_I64" -> Some Rel_i64
    | "IET_IMM_I64" -> Some Imm_i64
    | "IET_REL32_EXPORT" -> Some Rel32_export
    | "IET_IMM32_EXPORT" -> Some Imm32_export
    | "IET_REL64_EXPORT" -> Some Rel64_export
    | "IET_IMM64_EXPORT" -> Some Imm64_export
    | "IET_ABS_ADDR" -> Some Abs_addr
    | "IET_CODE_HEAP" -> Some Code_heap
    | "IET_ZEROED_CODE_HEAP" -> Some Zeroed_code_heap
    | "IET_DATA_HEAP" -> Some Data_heap
    | "IET_ZEROED_DATA_HEAP" -> Some Zeroed_data_heap
    | "IET_MAIN" -> Some Main
    | _ -> None

  type decoded = Entry of t | Reserved of int | Unknown of int

  let decode code =
    match of_code code with
    | Some entry -> Entry entry
    | None ->
        if List.mem code reserved_codes then Reserved code else Unknown code
end

module Adjustment = struct
  type t =
    | Add_u8
    | Sub_u8
    | Add_u16
    | Sub_u16
    | Add_u32
    | Sub_u32
    | Add_u64
    | Sub_u64

  type info = { adjustment : t; source_name : string; code : int; width : width; operation : adjustment_operation; definition_line : int; consumers : source_reference list }

  let all =
    [
      Add_u8;
      Sub_u8;
      Add_u16;
      Sub_u16;
      Add_u32;
      Sub_u32;
      Add_u64;
      Sub_u64;
    ]

  let info = function
    | Add_u8 ->
        { adjustment = Add_u8; source_name = "AAT_ADD_U8"; code = 0; width = Width_8; operation = Add; definition_line = 1979; consumers = [{ path = "Compiler/Asm.HC"; line = 291 }; { path = "Compiler/CMain.HC"; line = 188 }; { path = "Compiler/CMain.HC"; line = 317 }]; }
    | Sub_u8 ->
        { adjustment = Sub_u8; source_name = "AAT_SUB_U8"; code = 1; width = Width_8; operation = Subtract; definition_line = 1980; consumers = [{ path = "Compiler/Asm.HC"; line = 299 }; { path = "Compiler/CMain.HC"; line = 189 }; { path = "Compiler/CMain.HC"; line = 318 }]; }
    | Add_u16 ->
        { adjustment = Add_u16; source_name = "AAT_ADD_U16"; code = 2; width = Width_16; operation = Add; definition_line = 1981; consumers = [{ path = "Compiler/Asm.HC"; line = 318 }; { path = "Compiler/CMain.HC"; line = 190 }; { path = "Compiler/CMain.HC"; line = 319 }]; }
    | Sub_u16 ->
        { adjustment = Sub_u16; source_name = "AAT_SUB_U16"; code = 3; width = Width_16; operation = Subtract; definition_line = 1982; consumers = [{ path = "Compiler/Asm.HC"; line = 326 }; { path = "Compiler/CMain.HC"; line = 191 }; { path = "Compiler/CMain.HC"; line = 320 }]; }
    | Add_u32 ->
        { adjustment = Add_u32; source_name = "AAT_ADD_U32"; code = 4; width = Width_32; operation = Add; definition_line = 1983; consumers = [{ path = "Compiler/Asm.HC"; line = 345 }; { path = "Compiler/BackC.HC"; line = 734 }; { path = "Compiler/BackC.HC"; line = 747 }; { path = "Compiler/BackLib.HC"; line = 680 }; { path = "Compiler/CMain.HC"; line = 192 }; { path = "Compiler/CMain.HC"; line = 321 }; { path = "Compiler/OptPass789A.HC"; line = 1126 }]; }
    | Sub_u32 ->
        { adjustment = Sub_u32; source_name = "AAT_SUB_U32"; code = 5; width = Width_32; operation = Subtract; definition_line = 1984; consumers = [{ path = "Compiler/Asm.HC"; line = 353 }; { path = "Compiler/CMain.HC"; line = 193 }; { path = "Compiler/CMain.HC"; line = 322 }]; }
    | Add_u64 ->
        { adjustment = Add_u64; source_name = "AAT_ADD_U64"; code = 6; width = Width_64; operation = Add; definition_line = 1985; consumers = [{ path = "Compiler/Asm.HC"; line = 369 }; { path = "Compiler/CMain.HC"; line = 194 }; { path = "Compiler/CMain.HC"; line = 323 }; { path = "Compiler/OptPass789A.HC"; line = 106 }; { path = "Compiler/OptPass789A.HC"; line = 143 }; { path = "Compiler/OptPass789A.HC"; line = 353 }; { path = "Compiler/PrsVar.HC"; line = 44 }]; }
    | Sub_u64 ->
        { adjustment = Sub_u64; source_name = "AAT_SUB_U64"; code = 7; width = Width_64; operation = Subtract; definition_line = 1986; consumers = [{ path = "Compiler/Asm.HC"; line = 377 }; { path = "Compiler/CMain.HC"; line = 195 }; { path = "Compiler/CMain.HC"; line = 324 }]; }

  let to_source_name adjustment = (info adjustment).source_name
  let to_code adjustment = (info adjustment).code

  let of_code = function
    | 0 -> Some Add_u8
    | 1 -> Some Sub_u8
    | 2 -> Some Add_u16
    | 3 -> Some Sub_u16
    | 4 -> Some Add_u32
    | 5 -> Some Sub_u32
    | 6 -> Some Add_u64
    | 7 -> Some Sub_u64
    | _ -> None

  let of_source_name = function
    | "AAT_ADD_U8" -> Some Add_u8
    | "AAT_SUB_U8" -> Some Sub_u8
    | "AAT_ADD_U16" -> Some Add_u16
    | "AAT_SUB_U16" -> Some Sub_u16
    | "AAT_ADD_U32" -> Some Add_u32
    | "AAT_SUB_U32" -> Some Sub_u32
    | "AAT_ADD_U64" -> Some Add_u64
    | "AAT_SUB_U64" -> Some Sub_u64
    | _ -> None
end

type behavior_sources = { header_layout : source_reference; header_write : source_reference; module_validation : source_reference; module_base : source_reference; import_grouping : source_reference; import_patches : source_reference; export_registration : source_reference; absolute_patch : source_reference; code_heap_patch : source_reference; data_heap_patch : source_reference; main_execution : source_reference; pass_order : source_reference; patch_termination : source_reference; boot_patch_table : source_reference; boot_absolute_patch : source_reference; jit_adjustments : source_reference; aot_adjustments : source_reference }

let behavior_sources =
  {
    header_layout = { path = "Kernel/KernelA.HH"; line = 384 };
    header_write = { path = "Compiler/CMain.HC"; line = 540 };
    module_validation = { path = "Kernel/KLoad.HC"; line = 195 };
    module_base = { path = "Kernel/KLoad.HC"; line = 224 };
    import_grouping = { path = "Kernel/KLoad.HC"; line = 14 };
    import_patches = { path = "Kernel/KLoad.HC"; line = 41 };
    export_registration = { path = "Kernel/KLoad.HC"; line = 84 };
    absolute_patch = { path = "Kernel/KLoad.HC"; line = 102 };
    code_heap_patch = { path = "Kernel/KLoad.HC"; line = 109 };
    data_heap_patch = { path = "Kernel/KLoad.HC"; line = 131 };
    main_execution = { path = "Kernel/KLoad.HC"; line = 163 };
    pass_order = { path = "Kernel/KLoad.HC"; line = 232 };
    patch_termination = { path = "Compiler/CMain.HC"; line = 534 };
    boot_patch_table = { path = "Kernel/KStart16.HC"; line = 182 };
    boot_absolute_patch = { path = "Kernel/KStart32.HC"; line = 96 };
    jit_adjustments = { path = "Compiler/CMain.HC"; line = 174 };
    aot_adjustments = { path = "Compiler/CMain.HC"; line = 302 };
  }
