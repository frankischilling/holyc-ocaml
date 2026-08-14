type source_reference = { path : string; line : int }

type flag_entry = {
  source_name : string;
  bit_index : int;
  mask : int64;
  definition_line : int;
  consumers : source_reference list;
}

type behavior_entry = {
  id : string;
  description : string;
  source : source_reference;
}

type tables = { flags : flag_entry list; behaviors : behavior_entry list }
type error = { path : string option; line : int option; message : string }

val error_to_string : error -> string
val verify_sha256 : expected:string -> string -> (unit, error) result

val parse :
  kernel_source:string ->
  lex_lib_source:string ->
  prs_var_source:string ->
  prs_stmt_source:string ->
  prs_exp_source:string ->
  (tables, error) result
