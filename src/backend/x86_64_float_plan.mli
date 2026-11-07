(** Source-backed admission for the materialized scalar F64 subset. This is
    deliberately not an optimizer: unproved extended-precision links reject. *)
type load = Bits | Zero | One

type operation =
  | Negate
  | Add
  | Subtract
  | Multiply
  | Divide
  | Copy_left
  | Copy_right

type orientation = Left_first | Right_first | Bit_copy

type entry = private {
  instruction_id : Ir.Instruction_sequence.Instruction_id.t;
  result_id : Ir.Instruction_sequence.Value_id.t;
  operands : Ir.Instruction_sequence.Value_id.t list;
  loads : load list;
  operation : operation;
  live_before : Ir.Instruction_sequence.Value_id.t list;
  live_after : Ir.Instruction_sequence.Value_id.t list;
  peak_depth : int;
  orientation : orientation;
  live_steps : Ir.Instruction_sequence.Value_id.t list list;
}

type error = { message : string; span : Common.Span.t option }
type t

val is_f64 : Sema.Type.t -> bool
val prepare : Ir.Block_graph.t -> (t, error list) result

val find :
  t ->
  graph:Ir.Block_graph.t ->
  Ir.Instruction_sequence.description ->
  entry option

val peak_depth : t -> int
