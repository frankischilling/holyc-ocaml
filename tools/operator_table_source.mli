type named_constant = { name : string; value : int; source_line : int }
type sequence_kind = Token of named_constant | Block_comment | Line_comment

type dual_sequence = {
  group : int;
  spelling : string;
  kind : sequence_kind;
  source_line : int;
}

type operator_origin =
  | Dual_table of int
  | Shift_assignment
  | Dot_sequence
  | Current_position

type operator = {
  spelling : string;
  token_name : string option;
  token_id : int option;
  origin : operator_origin;
  source_line : int;
}

type association = Unspecified | Left | Right

type binary_operator = {
  spelling : string;
  token_name : string option;
  token_id : int;
  precedence_name : string;
  precedence_value : int;
  association : association;
  ic_name : string;
  ic_id : int;
  source_line : int;
}

type tables = {
  tokens : named_constant list;
  association_flags : named_constant list;
  precedences : named_constant list;
  dual_sequences : dual_sequence list;
  operators : operator list;
  binary_operators : binary_operator list;
}

type error = { line : int option; message : string }

val parse :
  kernel_source:string ->
  compiler_source:string ->
  cinit_source:string ->
  lex_source:string ->
  (tables, error) result

val verify_sha256 : expected:string -> string -> (unit, error) result
val error_to_string : error -> string
