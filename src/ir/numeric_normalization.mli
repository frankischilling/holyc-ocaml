(** Bounded replay of pinned TempleOS conversion normalization. Canonical IR is
    never rewritten. Receipts authorize only the recorded physical events. *)
type scalar_class = Integer64 | Float64

type phase =
  | Mixed_operands
  | Identity_replaced
  | Identity_normalized
  | Return_classified
  | Operand_migrated

type state = private {
  phase : phase;
  input_class : scalar_class;
  input_to_float : bool;
  raw_class : scalar_class;
  class2 : scalar_class;
  stacked_class : scalar_class;
  producer_to_float : bool;
  producer_to_integer : bool;
  return_operand_to_float : bool;
  arithmetic_operand_to_float : bool;
}

type event =
  | Load_signed_integer
  | Store_binary64
  | Copy_binary64
  | Load_binary64
  | Store_integer_truncate

type position = Argument1 | Argument2 | Instruction | Result

type step = private {
  owner : Instruction_sequence.Instruction_id.t;
  position : position;
  event : event;
}

type recipe = Deleted_float_identity | Materialized_copy_barrier
type t

val prepare :
  runtime:Runtime_call_context.t ->
  body:Function_body.t ->
  frame:Sema.Function_frame_layout.function_layout ->
  Block_graph.t ->
  t list

val valid : t -> graph:Block_graph.t -> bool
val instruction : t -> Instruction_sequence.Instruction_id.t
val input : t -> Instruction_sequence.Value_id.t
val result : t -> Instruction_sequence.Value_id.t
val recipe : t -> recipe
val states : t -> state list
val events : t -> event list
val steps : t -> step list
val eliminated : t -> Instruction_sequence.Instruction_id.t list
