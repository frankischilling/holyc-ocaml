type selection = Register_candidate | Stack | Unknown
type t

val analyze :
  frame:Sema.Function_frame_layout.function_layout ->
  body:Function_body.t ->
  t option
(** Analyze source-proven narrow register variables under default compiler
    options. Supported shapes include a two-parameter function that updates a
    narrow parameter, and a function that initializes one narrow local from a
    literal or a signed or unsigned integer parameter before returning it. The
    local may receive a supported literal or parameter update. Addition and
    subtraction by a local literal are limited to nonnegative signed-32-bit
    operands. Remainder is not proved for the two-parameter update of a narrow
    parameter. Explicit hardware registers, escapes, extra locals and other
    control flow remain unproved. The witness itself does not alter execution.
*)

val selection :
  t ->
  frame:Sema.Function_frame_layout.function_layout ->
  body:Function_body.t ->
  symbol:Sema.Symbol.t ->
  selection option
(** Recheck the original frame, body and instruction identities. Foreign or
    modified inputs return [None], not a reconstructed allocation decision. *)
