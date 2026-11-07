(** Source-backed admission for scalar F64 and authenticated pure linked
    regions. Unproved normalization and extended-precision links reject. *)
type load = Bits | Zero | One

type operation =
  | Negate
  | Add
  | Subtract
  | Multiply
  | Divide
  | Copy_left
  | Copy_right
  | Alias_left
  | Alias_right

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

type source = {
  runtime : Ir.Runtime_call_context.t;
  body : Ir.Function_body.t;
  frame : Sema.Function_frame_layout.function_layout;
}

val prepare : ?source:source -> Ir.Block_graph.t -> (t, error list) result

val find :
  t ->
  graph:Ir.Block_graph.t ->
  Ir.Instruction_sequence.description ->
  entry option

val peak_depth : t -> int

val validated_entries : t -> graph:Ir.Block_graph.t -> entry list option
(** Validate the complete frozen graph and original source region evidence once,
    then return the entries for immediate synchronous emission. *)
