val reference_commit : string

type source = { path : string; sha256 : string }
type source_reference = { path : string; line : int }

type option_entry = {
  name : string;
  bit_index : int;
  initially_enabled : bool;
  definition_line : int;
  source_comment : string option;
  consumers : source_reference list;
}

type gap = { first : int; last : int }

type api = {
  option_line : int;
  get_option_line : int;
  bequ_line : int;
  state_expression : string;
  set_returns_previous : bool;
}

val sources : source list
val options : option_entry list
val gaps : gap list
val default_source_line : int
val echo_mask_line : int
val api : api
