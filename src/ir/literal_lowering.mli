type literal =
  | Integer of int64
  | Character of int64
  | Float_bits of int64
  | String_bytes of string

type description = {
  instruction_id : Instruction_sequence.Instruction_id.t;
  value_id : Instruction_sequence.Value_id.t;
  literal : literal;
  span : Common.Span.t option;
}

type identity = {
  instruction_id : Instruction_sequence.Instruction_id.t;
  value_id : Instruction_sequence.Value_id.t;
}

type t
type expression_result = Lowered of t | Not_literal

val reference_commit : string

val lower : description -> (t, Instruction_sequence.error list) result
(** Lower one source literal through the checked instruction-sequence boundary.
*)

val lower_expression :
  instruction_id:Instruction_sequence.Instruction_id.t ->
  value_id:Instruction_sequence.Value_id.t ->
  ?unary_identities:identity list ->
  Frontend.Ast.expression ->
  (expression_result, Instruction_sequence.error list) result
(** Lower a parsed literal through transparent parentheses and unary plus. Unary
    minus, logical not, bitwise complement, and dereference consume one
    caller-owned [unary_identities] entry for each emitted instruction. Entries
    run from the innermost operator to the outermost operator. Other expressions
    return [Not_literal] without constructing IR. *)

val sequence : t -> Instruction_sequence.t
val result_type : t -> Sema.Type.t

val human : t -> string
(** Render the versioned, deterministic literal-lowering form. *)
