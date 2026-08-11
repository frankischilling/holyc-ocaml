type named_constant = private { name : string; value : int; source_line : int }
type sequence_kind = Token | Block_comment | Line_comment

type dual_sequence = private {
  group : int;
  spelling : string;
  kind : sequence_kind;
  token_name : string option;
  token_id : int option;
  source_line : int;
}

type operator_origin =
  | Dual_table of int
  | Shift_assignment
  | Dot_sequence
  | Current_position

type operator = private {
  spelling : string;
  token_name : string option;
  token_id : int option;
  origin : operator_origin;
  source_line : int;
}

type association = Unspecified | Left | Right

type binary_operator = private {
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

val reference_commit : string
val kernel_source_path : string
val kernel_source_sha256 : string
val compiler_source_path : string
val compiler_source_sha256 : string
val cinit_source_path : string
val cinit_source_sha256 : string
val lex_source_path : string
val lex_source_sha256 : string
val tokens : named_constant list
val association_flags : named_constant list
val precedences : named_constant list
val dual_sequences : dual_sequence list
val operators : operator list
val binary_operators : binary_operator list
