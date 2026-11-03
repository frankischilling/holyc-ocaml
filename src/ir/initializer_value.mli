val is_void : Sema.Function_call_expression_result.expression_result -> bool

val discards :
  scalar:Sema.Initializer_type.scalar option ->
  Sema.Function_call_expression_result.expression_result ->
  bool

val pointer_destination :
  Sema.Initializer_type.scalar option -> Sema.Type.t option

val stores_pointer :
  scalar:Sema.Initializer_type.scalar option ->
  Sema.Function_call_expression_result.expression_result ->
  bool

val discards_reference :
  scalar:Sema.Initializer_type.scalar option ->
  Sema.Function_call_expression_result.expression_result ->
  bool
(** A zero-byte live destination may consume an actually evaluated reference
    without copying its address or granting a scalar word. Expression lowering,
    execution ownership and final-size settlement remain required. *)
