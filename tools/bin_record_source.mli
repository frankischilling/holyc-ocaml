type source_reference = { path : string; line : int }
type width = Width_0 | Width_8 | Width_16 | Width_32 | Width_64

type source_status =
  | Source_active
  | Source_fictitious
  | Source_not_implemented
  | Source_not_really_used

type category =
  | Terminator
  | Import
  | Export
  | Absolute_addresses
  | Code_heap
  | Data_heap
  | Main

type leading_value =
  | No_leading_value
  | Patch_offset
  | Export_value
  | Entry_count
  | Main_offset

type name_mode =
  | No_name_field
  | Required_name
  | First_name_then_inherited
  | Optional_export_name
  | Empty_name

type payload =
  | No_payload
  | U32_offsets
  | I32_size_then_u32_offsets
  | I64_size_then_u32_offsets

type relocation_kind = Relative | Immediate

type relocation = {
  kind : relocation_kind;
  width : width;
  displacement_bias : int;
}

type pass1_action =
  | Pass1_stop
  | Resolve_import
  | Register_relative_export
  | Register_immediate_export
  | Apply_module_base_u32
  | Allocate_code
  | Allocate_zeroed_code
  | Allocate_data
  | Allocate_zeroed_data
  | Pass1_ignore

type pass2_action =
  | Pass2_stop
  | Execute_main
  | Skip_u32_offsets
  | Skip_i32_size_and_u32_offsets
  | Skip_i64_size_and_u32_offsets
  | Pass2_ignore

type entry = {
  name : string;
  code : int;
  status : source_status;
  category : category;
  leading_value : leading_value;
  name_mode : name_mode;
  payload : payload;
  relocation : relocation option;
  pass1 : pass1_action;
  pass2 : pass2_action;
  definition_line : int;
  consumers : source_reference list;
}

type header_field = {
  name : string;
  source_type : string;
  width_bytes : int;
  offset : int;
  definition_line : int;
}

type adjustment_operation = Add | Subtract

type adjustment = {
  name : string;
  code : int;
  width : width;
  operation : adjustment_operation;
  definition_line : int;
  consumers : source_reference list;
}

type behavior = {
  header_layout : source_reference;
  header_write : source_reference;
  module_validation : source_reference;
  module_base : source_reference;
  import_grouping : source_reference;
  import_patches : source_reference;
  export_registration : source_reference;
  absolute_patch : source_reference;
  code_heap_patch : source_reference;
  data_heap_patch : source_reference;
  main_execution : source_reference;
  pass_order : source_reference;
  patch_termination : source_reference;
  boot_patch_table : source_reference;
  boot_absolute_patch : source_reference;
  jit_adjustments : source_reference;
  aot_adjustments : source_reference;
}

type tables = {
  signature_spelling : string;
  signature_value : int32;
  signature_line : int;
  header_size : int;
  header_fields : header_field list;
  immediate_not_relative_mask : int;
  immediate_not_relative_line : int;
  entries : entry list;
  reserved_codes : int list;
  adjustments : adjustment list;
  behavior : behavior;
}

type error = { path : string option; line : int option; message : string }

val required_source_paths : string list
val error_to_string : error -> string
val verify_sha256 : expected:string -> string -> (unit, error) result
val width_bytes : width -> int
val parse : sources:(string * string) list -> (tables, error) result
