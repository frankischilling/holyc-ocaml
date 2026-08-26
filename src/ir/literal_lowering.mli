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
    minus, logical not, bitwise complement, dereference, and address-of consume
    one caller-owned [unary_identities] entry for each emitted instruction.
    Address-of cancels an immediately preceding dereference before identities
    are counted. Entries run from the innermost emitted operator to the
    outermost operator. Other expressions return [Not_literal] without
    constructing IR. *)

val lower_typed_result :
  instruction_id:Instruction_sequence.Instruction_id.t ->
  value_id:Instruction_sequence.Value_id.t ->
  Sema.Function_call_expression_result.expression_result ->
  (expression_result, Instruction_sequence.error list) result
(** Lower one literal, optionally nested in parentheses, from the shared typed
    semantic result view. The semantic payload, checked result type, and
    literal source location are copied without consulting a frontend AST node.
    Parentheses add no instruction or identity. Other semantic expression
    shapes return [Not_literal]. *)

val sequence : t -> Instruction_sequence.t
val result_type : t -> Sema.Type.t

val human : t -> string
(** Render the versioned, deterministic literal-lowering form. *)
