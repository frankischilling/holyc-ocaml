type argument_count = Zero | One | Two | Variable
type structural_type = Null | Dereference | Assignment | Comparison

type entry = {
  source_name : string;
  constructor_name : string;
  display_name : string;
  code : int;
  argument_count : argument_count;
  result_count : int;
  structural_type : structural_type;
  pops_float : bool;
  prevents_constant_folding : bool;
  definition_line : int;
  metadata_line : int;
}

type tables = { entries : entry list; count : int; count_line : int }
type error = { path : string option; line : int option; message : string }

val parse :
  compiler_source:string -> cinit_source:string -> (tables, error) result

val verify_sha256 : expected:string -> string -> (unit, error) result
val error_to_string : error -> string
