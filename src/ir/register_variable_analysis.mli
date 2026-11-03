type selection = Register_candidate | Stack | Unknown
type t

val analyze :
  frame:Sema.Function_frame_layout.function_layout ->
  body:Function_body.t ->
  t option
(** Analyze a bound two-parameter function that shifts an 8-, 16- or 32-bit
    integer parameter by the other parameter and returns it directly, or a
    function that initializes one narrow local from an integer literal and
    returns it directly, optionally after shifting it by an integer literal.
    Both require default compiler options. Escapes, explicit hardware registers,
    other control flow and all other shapes remain unproved. This witness does
    not alter storage or execution. *)

val selection :
  t ->
  frame:Sema.Function_frame_layout.function_layout ->
  body:Function_body.t ->
  symbol:Sema.Symbol.t ->
  selection option
(** Recheck the original frame, body and instruction identities. Foreign or
    modified inputs return [None], not a reconstructed allocation decision. *)
