type selection = Register_candidate | Stack | Unknown
type t

val analyze :
  frame:Sema.Function_frame_layout.function_layout ->
  body:Function_body.t ->
  t option
(** Analyze only a bound two-parameter function whose entry block updates an
    I8/U8/I16/U16/I32/U32 parameter by the other parameter and directly returns
    it, or initializes one narrow local from an integer literal, shifts it left
    by an integer literal (right shifts are also supported for byte locals), and
    directly returns it. Both require default compiler options. Escapes,
    explicit registers, other control flow and all other shapes remain unproved.
    This witness does not alter storage or execution. *)

val selection :
  t ->
  frame:Sema.Function_frame_layout.function_layout ->
  body:Function_body.t ->
  symbol:Sema.Symbol.t ->
  selection option
(** Recheck the original frame, body and instruction identities. Foreign or
    modified inputs return [None], not a reconstructed allocation decision. *)
