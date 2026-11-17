val is_void : Sema.Function_call_expression_result.expression_result -> bool

type numeric_conversion = Same_class | To_f64 | To_integer

val floating_destination :
  Sema.Initializer_type.scalar option -> Sema.Type.t option

val expression_is_f64 :
  Sema.Function_call_expression_result.expression_result -> bool

val same_f64 :
  scalar:Sema.Initializer_type.scalar option ->
  Sema.Function_call_expression_result.expression_result ->
  bool

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

val has_integer_word :
  Sema.Function_call_expression_result.expression_result -> bool

val native_numeric_conversion :
  scalar:Sema.Initializer_type.scalar option ->
  Sema.Function_call_expression_result.expression_result ->
  numeric_conversion option

val native_numeric :
  scalar:Sema.Initializer_type.scalar option ->
  Sema.Function_call_expression_result.expression_result ->
  bool
(** Admit a numeric scalar leaf whose original expression or destination is F64.
    Pointer and callback ownership keep their separate preparation paths. *)

val convert_native_word : numeric_conversion -> int64 -> int64
(** Apply PrsVar's numeric conversion after its expression Call returns a full
    word, before copying the destination's bytes. This pure conversion grants no
    execution or publication authority and does not model a load-time
    assignment. *)
