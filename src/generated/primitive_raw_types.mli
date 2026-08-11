type raw_type = private {
  name : string;
  templeos_id : int;
  not_implemented : bool;
  fictitious : bool;
  source_line : int;
  source_comment : string option;
}

type raw_alias = private {
  name : string;
  target_name : string;
  templeos_id : int;
  source_line : int;
  source_comment : string option;
}

type public_union = private {
  storage_spelling : string;
  public_spelling : string;
  source_line : int;
}

type internal_type = private {
  raw_name : string;
  byte_size : int;
  spelling : string;
  source_line : int;
}

val reference_commit : string
val kernel_source_path : string
val kernel_source_sha256 : string
val cinit_source_path : string
val cinit_source_sha256 : string
val raw_types : raw_type list
val pointer_alias : raw_alias
val raw_types_count : int
val unsigned_flag : int
val raw_group_mask : int
val public_unions : public_union list
val internal_types : internal_type list
