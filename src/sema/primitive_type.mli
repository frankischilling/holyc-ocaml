type t = I0 | I8 | I16 | I32 | I64 | U0 | U8 | U16 | U32 | U64 | F64 | Bool
type category = Integer | Floating | Boolean
type signedness = Signed | Unsigned | Not_applicable
type declaration_form = Internal_type | Public_union

type info = private {
  primitive : t;
  spelling : string;
  storage_spelling : string;
  raw_name : string;
  raw_id : int;
  byte_size : int;
  category : category;
  signedness : signedness;
  raw_is_unsigned : bool;
  declaration_form : declaration_form;
  raw_source_line : int;
  storage_source_line : int;
  declaration_source_line : int;
}

type pointer_representation = private {
  raw_name : string;
  target_raw_name : string;
  raw_id : int;
  source_line : int;
}

val reference_commit : string
val raw_source_path : string
val raw_source_sha256 : string
val internal_type_source_path : string
val internal_type_source_sha256 : string
val all : t list
val compare : t -> t -> int
val equal : t -> t -> bool
val to_string : t -> string
val of_spelling : string -> t option
val of_storage_spelling : string -> t option
val info : t -> info
val is_zero_sized : t -> bool
val pointer_representation : pointer_representation
