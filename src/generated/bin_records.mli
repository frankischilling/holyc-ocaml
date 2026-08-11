(* Generated interface for the pinned TempleOS BIN specification. *)

[@@@ocamlformat "disable"]

type source = { path : string; sha256 : string }
type source_reference = { path : string; line : int }
type width = Width_0 | Width_8 | Width_16 | Width_32 | Width_64
val width_bytes : width -> int
type source_status = Source_active | Source_fictitious | Source_not_implemented | Source_not_really_used
type category = Terminator | Import | Export | Absolute_addresses | Code_heap | Data_heap | Main
type leading_value = No_leading_value | Patch_offset | Export_value | Entry_count | Main_offset
type name_mode = No_name_field | Required_name | First_name_then_inherited | Optional_export_name | Empty_name
type payload = No_payload | U32_offsets | I32_size_then_u32_offsets | I64_size_then_u32_offsets
type relocation_kind = Relative | Immediate
type relocation = { kind : relocation_kind; width : width; displacement_bias : int }
type pass1_action = Pass1_stop | Resolve_import | Register_relative_export | Register_immediate_export | Apply_module_base_u32 | Allocate_code | Allocate_zeroed_code | Allocate_data | Allocate_zeroed_data | Pass1_ignore
type pass2_action = Pass2_stop | Execute_main | Skip_u32_offsets | Skip_i32_size_and_u32_offsets | Skip_i64_size_and_u32_offsets | Pass2_ignore
type header_field = private { name : string; source_type : string; width_bytes : int; offset : int; definition_line : int }
type adjustment_operation = Add | Subtract
val reference_commit : string
val sources : source list
val signature_spelling : string
val signature_value : int32
val signature_definition_line : int
val header_size : int
val header_fields : header_field list
val immediate_not_relative_mask : int
val immediate_not_relative_definition_line : int
val reserved_codes : int list

module Entry : sig
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

  type info = private { entry : t; source_name : string; code : int; status : source_status; category : category; leading_value : leading_value; name_mode : name_mode; payload : payload; relocation : relocation option; pass1 : pass1_action; pass2 : pass2_action; definition_line : int; consumers : source_reference list }
  type decoded = Entry of t | Reserved of int | Unknown of int
  val all : t list
  val info : t -> info
  val to_source_name : t -> string
  val to_code : t -> int
  val of_code : int -> t option
  val of_source_name : string -> t option
  val decode : int -> decoded
end

module Adjustment : sig
  type t =
    | Add_u8
    | Sub_u8
    | Add_u16
    | Sub_u16
    | Add_u32
    | Sub_u32
    | Add_u64
    | Sub_u64

  type info = private { adjustment : t; source_name : string; code : int; width : width; operation : adjustment_operation; definition_line : int; consumers : source_reference list }
  val all : t list
  val info : t -> info
  val to_source_name : t -> string
  val to_code : t -> int
  val of_code : int -> t option
  val of_source_name : string -> t option
end

type behavior_sources = private { header_layout : source_reference; header_write : source_reference; module_validation : source_reference; module_base : source_reference; import_grouping : source_reference; import_patches : source_reference; export_registration : source_reference; absolute_patch : source_reference; code_heap_patch : source_reference; data_heap_patch : source_reference; main_execution : source_reference; pass_order : source_reference; patch_termination : source_reference; boot_patch_table : source_reference; boot_absolute_patch : source_reference; jit_adjustments : source_reference; aot_adjustments : source_reference }
val behavior_sources : behavior_sources
