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
  function_body:Ir.Function_body.t option ->
  compiler_options:int64 ->
  terminal:terminal option ->
  Ir.Block_graph.t ->
  (unit, failure) result
(** Restrict original initializer and callee graphs to narrow updates whose raw
    reference behavior is invariant under audited native choices or matches a
    proven register-selected implementation. This analysis grants no execution
    authority and tracks no mutable cell values. Ordinary narrow parameter entry
    seeds the bounded-write proof; subsequent unbounded assignments or direct
    updates still disqualify that location, except when the exact bound body
    proves a supported register update. [terminal] is the exact declaration leaf
    sink, never a callee return. *)
