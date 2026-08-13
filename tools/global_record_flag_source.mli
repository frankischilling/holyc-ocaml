type source_reference = { path : string; line : int }

type flag_entry = {
  bit_name : string option;
  mask_name : string;
  bit_index : int;
  mask : int64;
  definition_line : int;
  consumers : source_reference list;
}

type global_type = {
  index_name : string;
  mask_name : string;
  type_index : int;
  type_mask : int64;
  index_definition_line : int;
  mask_definition_line : int;
}

type behavior_entry = {
  id : string;
  description : string;
  source : source_reference;
}

type tables = {
  global_type : global_type;
  hash_flags : flag_entry list;
  global_flags : flag_entry list;
  behaviors : behavior_entry list;
}

type error = { path : string option; line : int option; message : string }

val error_to_string : error -> string
val verify_sha256 : expected:string -> string -> (unit, error) result

val parse :
  kernel_source:string ->
  prs_stmt_source:string ->
  prs_exp_source:string ->
  khash_source:string ->
  chash_source:string ->
  asm_resolve_source:string ->
  scoping_source:string ->
  (tables, error) result
