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

type tables = {
  options : option_entry list;
  gaps : gap list;
  default_source_line : int;
  echo_mask_line : int;
  api : api;
}

type error = { path : string option; line : int option; message : string }

val parse :
  kernel_source:string ->
  lex_source:string ->
  cmisc_source:string ->
  bequ_source:string ->
  consumers:(string * string) list ->
  (tables, error) result

val verify_sha256 : expected:string -> string -> (unit, error) result
val error_to_string : error -> string
