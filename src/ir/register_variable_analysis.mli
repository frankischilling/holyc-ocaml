type selection = Register_candidate | Stack | Unknown
type t

val analyze :
  frame:Sema.Function_frame_layout.function_layout ->
  body:Function_body.t ->
  t option
(** Analyze source-proven narrow register variables under default compiler
    options. Supported shapes include a function with two to six scalar
    parameters that updates a narrow parameter, and a function that initializes
    one narrow local from a literal or a signed or unsigned integer parameter
    before returning it. The local may receive a supported literal or parameter
    update, including a directly returned literal compound update. A single
    narrow parameter also supports a directly returned literal compound update.
    A narrow parameter or literal-initialized local may be incremented or
    decremented before return, or returned with a prefix or postfix update.
    Addition and subtraction by a literal in these shapes are limited to
    nonnegative signed-32-bit operands. A bounded function may also apply
    shifts, addition, subtraction, multiplication, division or bitwise updates
    using one or two unchanged parameters to two scalar parameters, at least one
    narrow, and return their sum. Three to five scalar parameters, at least one
    narrow, may each receive a supported update using an unchanged parameter and
    return their sum in any order. Their widths and signedness may differ.
    Remainder is not proved for these parameter updates. Distinct R12 through
    R15 requests are covered in these call-free shapes. Other explicit
    registers, conflicting requests, escapes, extra locals and other control
    flow remain unproved. The witness itself does not alter execution. *)

val selection :
  t ->
  frame:Sema.Function_frame_layout.function_layout ->
  body:Function_body.t ->
  symbol:Sema.Symbol.t ->
  selection option
(** Recheck the original frame, body and instruction identities. Foreign or
    modified inputs return [None], not a reconstructed allocation decision. *)
