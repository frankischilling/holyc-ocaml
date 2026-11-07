type selection = Register_candidate | Stack | Unknown
type t

val analyze :
  frame:Sema.Function_frame_layout.function_layout ->
  body:Function_body.t ->
  t option
(** Analyze integer register selection using the original definition, frame,
    typed occurrences and completed-body compiler options. Specialized proofs
    cover direct returns of supported literal compound updates and prefix or
    postfix updates. The whole-function proof covers one to 32 integer frame
    members across straight-line code, branches, loops and original named calls.

    Automatic locals require one original entry-block initializer. Initializer
    expressions use parameters, literals and locals initialized before each read
    in the entry block; later assignments and compound operands may read any
    checked integer frame member. Source-owned arithmetic, bitwise, unary,
    division/remainder, shift and comparison trees are supported. Compound and
    unary update results must be discarded in the whole-function proof. Plain
    assignment results may be consumed only for proven register destinations;
    stack and unknown-pressure results remain unproved. Literal-only
    add/subtract/multiply updates remain limited to nonnegative signed-32-bit
    operands. Each operation retains its original typed operator, ordered
    operands and source position. Calls require original named definitions,
    provided fixed arguments and integer or U0 results; callbacks, defaults and
    variadic calls remain outside this proof.

    Distinct RSI, RDI and R10 through R15 requests are supported. Automatic
    candidates must fit the unreserved variable banks unless forced requests
    have guaranteed capacity and remaining ordinary members are I64/U64. Those
    ordinary pressured members retain [Unknown] selection. Other explicit
    registers, duplicate requests, address escapes and Boolean frame members
    remain unproved. Limits are 64 blocks, 512 instructions and 32 expression
    levels. Calls, casts, short-circuit expressions, power and unmatched
    optimizer rewrites cannot acquire an expression-tree proof.

    Only the four warning bits and NO_REG_VAR may differ from default options.
    Warnings do not change allocation. NO_REG_VAR disables automatic and plain
    reg selection, while supported explicit requests remain selected. Options
    must match the original definition snapshot. The witness does not itself
    change execution or claim physical register placement. *)

val selection :
  t ->
  frame:Sema.Function_frame_layout.function_layout ->
  body:Function_body.t ->
  symbol:Sema.Symbol.t ->
  selection option
(** Recheck the original frame, body and instruction identities. Foreign or
    modified inputs return [None], not a reconstructed allocation decision. *)

val physical_register :
  t ->
  frame:Sema.Function_frame_layout.function_layout ->
  body:Function_body.t ->
  symbol:Sema.Symbol.t ->
  int option
(** Return an original source-owned member's explicit R12 through R15 request
    only after [selection] revalidates [Register_candidate]. Automatic and plain
    [reg] candidates, stack or unknown selections, and other registers return
    [None]. Register numbers, canonical spellings and source origins are sealed
    independently of the shared semantic request record; changing those inputs
    invalidates the witness. This query authorizes only this bounded physical
    placement, not a general register allocation decision. *)
