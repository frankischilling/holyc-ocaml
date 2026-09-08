type terminal = {
  value : Ir.Instruction_sequence.Value_id.t;
  destination_type : Sema.Type.t;
}

type failure = {
  instruction : Ir.Instruction_sequence.description;
  reason : string;
}

val check_graph :
  globals:Ir.Integer_globals.t ->
  frame:Sema.Function_frame_layout.function_layout option ->
  compiler_options:int64 ->
  terminal:terminal option ->
  Ir.Block_graph.t ->
  (unit, failure) result
(** Restrict original initializer and callee graphs to narrow updates whose raw
    reference behavior is invariant under the audited native choices. This
    analysis grants no execution authority and tracks no mutable cell values.
    Ordinary narrow parameter entry seeds the bounded-write proof; subsequent
    unbounded assignments or direct updates still disqualify that location.
    [terminal] is the exact declaration leaf sink, never a callee return. *)
