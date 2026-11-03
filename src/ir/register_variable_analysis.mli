type selection = Register_candidate | Stack | Unknown
type t

val analyze :
  frame:Sema.Function_frame_layout.function_layout ->
  body:Function_body.t ->
  t option
(** Analyze only a bound two-parameter function whose entry block updates an
    I8/U8 parameter by the other parameter and directly returns the updated
    parameter under the default compiler options. Escapes, explicit registers,
    other options, other control flow and all other shapes remain unproved. This
    witness does not alter storage or execution. *)

val selection :
  t ->
  frame:Sema.Function_frame_layout.function_layout ->
  body:Function_body.t ->
  symbol:Sema.Symbol.t ->
  selection option
(** Recheck the original frame, body and instruction identities. Foreign or
    modified inputs return [None], not a reconstructed allocation decision. *)
