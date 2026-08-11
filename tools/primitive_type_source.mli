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

type tables = {
  raw_types : raw_type list;
  pointer_alias : raw_alias;
  raw_types_count : int;
  unsigned_flag : int;
  raw_group_mask : int;
  public_unions : public_union list;
  internal_types : internal_type list;
}

type error = { line : int option; message : string }

val parse :
  kernel_source:string -> cinit_source:string -> (tables, error) result

val verify_sha256 : expected:string -> string -> (unit, error) result
val error_to_string : error -> string
