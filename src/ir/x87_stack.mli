module Block_id = Instruction_sequence.Block_id
module Instruction_id = Instruction_sequence.Instruction_id

type conversion_direction = To_f64 | To_int

type slot_kind =
  | Operand_2_conversion of conversion_direction
  | Operand_1_conversion of conversion_direction
  | Operation
  | Result_conversion of conversion_direction

type slot_trace = {
  index : int;
  kind : slot_kind;
  suppress_push : bool;
  suppress_pop : bool;
  before_depth : int;
  after_depth : int;
}

type instruction_trace = {
  block_id : Block_id.t;
  instruction_id : Instruction_id.t;
  before_depth : int;
  after_depth : int;
  opcode_pops_float : bool;
  use_f64 : bool;
  use_int : bool;
  push_comparison : bool;
  pop_comparison : bool;
  slots : slot_trace list;
}

type error = {
  code : string;
  message : string;
  block_id : int option;
  instruction_id : int option;
  span : Common.Span.t option;
}

type t

val max_depth : int
val pops_float : Opcode.t -> bool
(** Return the pinned [fpop] fact from [Compiler/CInit.HC]. This fact does not
    include operand or result conversions. *)

val verify : Block_graph.t -> (t, error list) result
(** Check x87 depth along every graph edge. Calls preserve depth apart from
    their explicit conversions, exits require an empty stack, and every merge
    must receive one depth. *)

val graph : t -> Block_graph.t
val trace : t -> instruction_trace list
val human : t -> string
(** Render the source-ordered stack trace used by tests and diagnostics. *)

val slot_kind_name : slot_kind -> string
