type t

val analyze :
  frame:Sema.Function_frame_layout.function_layout ->
  body:Function_body.t ->
  t option
(** Prove a literal update of a narrow global in a zero-argument, zero-local
    function using its checked source and exact instruction graph. The update
    may be returned directly or added to another literal or a signed/unsigned
    64-bit global value. Addition and subtraction literals are limited to
    nonnegative signed-32-bit values. *)

val matches :
  t ->
  frame:Sema.Function_frame_layout.function_layout ->
  body:Function_body.t ->
  instruction:Instruction_sequence.description ->
  symbol:Sema.Symbol.t ->
  bool
(** Recheck the original owner and graph before using the proof. *)
