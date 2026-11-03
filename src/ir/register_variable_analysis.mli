type selection = Register_candidate | Stack | Unknown
type t

val analyze :
  frame:Sema.Function_frame_layout.function_layout ->
  body:Function_body.t ->
  t option
(** Analyze a bound two-parameter function that updates an 8-, 16- or 32-bit
    integer parameter using a shift, compound arithmetic or bitwise update by
    the other parameter, then returns it directly. Also analyze a function that
    initializes one narrow local from an integer literal or a full-word
    parameter and returns it directly, optionally after a shift, addition,
    subtraction or multiplication by an integer literal. Parameter-initialized
    locals are only proved for direct returns. Division is proved only for
    parameter operands. Local compound addition or subtraction requires a
    nonnegative signed-32-bit literal. Both require default compiler options.
    Escapes, explicit hardware registers, other control flow and all other
    shapes remain unproved. This witness does not alter storage or execution. *)

val selection :
  t ->
  frame:Sema.Function_frame_layout.function_layout ->
  body:Function_body.t ->
  symbol:Sema.Symbol.t ->
  selection option
(** Recheck the original frame, body and instruction identities. Foreign or
    modified inputs return [None], not a reconstructed allocation decision. *)
