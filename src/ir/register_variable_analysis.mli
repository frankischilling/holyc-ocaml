type selection = Register_candidate | Stack | Unknown
type t

val analyze :
  frame:Sema.Function_frame_layout.function_layout ->
  body:Function_body.t ->
  t option
(** Analyze source-proven narrow register variables under their original
    compiler-option snapshots. Supported shapes include a function with two to
    six scalar parameters that updates a narrow parameter, and a function that
    initializes one narrow local from a literal or a signed or unsigned integer
    parameter before returning it. The local may receive a supported literal or
    parameter update, including a directly returned literal compound update. A
    single narrow parameter also supports a directly returned literal compound
    update. A narrow parameter or literal-initialized local may be incremented
    or decremented before return, or returned with a prefix or postfix update.
    Addition and subtraction by a literal in these shapes are limited to
    nonnegative signed-32-bit operands. A bounded function may also apply
    shifts, addition, subtraction, multiplication, division, remainder or
    bitwise updates using one or two unchanged parameters to two scalar
    parameters, at least one narrow, and return their sum or either updated
    parameter. Three to five scalar parameters, at least one narrow, may each
    receive a supported update using an unchanged parameter and return one
    updated parameter or a sum of two to six updated parameters. Their widths
    and signedness may differ. Two to five updates may also revisit a target,
    provided every operand parameter remains unchanged. Distinct RSI, RDI and
    R10 through R15 requests are covered in these shapes. Other explicit
    registers, conflicting requests, escapes and extra locals remain unproved. A
    separate whole-function proof covers branches and loops over two to 32
    integer frame members when the automatic candidates fit the unreserved
    variable banks, with statement-only compound updates whose operands are
    direct member reads or supported immediate values. Add/subtract/multiply
    immediates are nonnegative signed-32-bit values; immediate shifts use
    normalized counts below 64. Discarded prefix/postfix increment and decrement
    are also supported. Each update must match its exact typed source operator,
    operand bindings and source position. Automatic locals require one original
    literal or parameter initializer in the entry block; reassignments remain
    unproved. Original named definitions with provided fixed arguments and
    integer or U0 results may be called, including recursive calls and the named
    JIT address-slot path. Callbacks, defaults and variadic calls remain
    unproved. The proof accepts up to 64 blocks and 512 instructions, no address
    escapes, no Boolean parameters and default options apart from the four
    warning bits and NO_REG_VAR. Warning settings do not change allocation.
    NO_REG_VAR disables automatic selection, including plain reg requests, but
    leaves supported explicit register requests selected. The option mask must
    match the original definition snapshot. Other control flow remains unproved.
    The witness itself does not alter execution. *)

val selection :
  t ->
  frame:Sema.Function_frame_layout.function_layout ->
  body:Function_body.t ->
  symbol:Sema.Symbol.t ->
  selection option
(** Recheck the original frame, body and instruction identities. Foreign or
    modified inputs return [None], not a reconstructed allocation decision. *)
