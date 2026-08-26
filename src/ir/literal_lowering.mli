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

type t

val reference_commit : string

val lower : description -> (t, Instruction_sequence.error list) result
(** Lower one source literal through the checked instruction-sequence boundary.
*)

val sequence : t -> Instruction_sequence.t
val result_type : t -> Sema.Type.t

val human : t -> string
(** Render the versioned, deterministic literal-lowering form. *)
