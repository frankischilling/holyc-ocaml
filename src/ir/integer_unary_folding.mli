module Block_id = Instruction_sequence.Block_id
module Instruction_id = Instruction_sequence.Instruction_id
module Value_id = Instruction_sequence.Value_id

type validation_stage = Graph_rebuild | X87_reverification

type error = private {
  stage : validation_stage;
  code : string;
  message : string;
  block_id : int option;
  instruction_id : int option;
  span : Common.Span.t option;
}

type rewrite = private {
  block_id : Block_id.t;
  removed_instruction_id : Instruction_id.t;
  removed_value_id : Value_id.t;
  retained_instruction_id : Instruction_id.t;
  retained_value_id : Value_id.t;
  opcode : Opcode.t;
  input_bits : int64;
  folded_bits : int64;
}

type t

val reference_commit : string

val fold : X87_stack.t -> (t, error list) result
(** Fold the source-audited, zero-flag integer unary subset and publish a new
    result only after rebuilding the graph and rerunning x87 verification. *)

val x87 : t -> X87_stack.t
val rewrites : t -> rewrite list

val human : t -> string
(** Render the versioned, source-ordered rewrite trace. *)
