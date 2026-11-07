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

    A stricter proof runs first for complete straight-line integer functions:
    named parameters, initialized locals, discarded writes/updates and one final
    return. It authenticates every original AST root and typed occurrence
    against unique canonical IR accesses. Each occurrence scores twice;
    candidates are ordered by the original member list and TempleOS's unstable
    QSort2b before zero-score trimming. The exact six-slot threshold, two
    non-pointer slots, all explicit reservations and plain-reg seeds decide both
    winners and spills. Used winners are [Register_candidate], used losers are
    [Stack]. Branches, calls, casts, nested writes and chained comparisons
    retain the conservative proof below rather than receiving speculative
    scores.

    Automatic locals require one original entry-block initializer. Initializer
    expressions use parameters, literals and locals initialized before each read
    in the entry block; later assignments and compound operands may read any
    checked integer frame member. Source-owned arithmetic, bitwise, unary,
    division/remainder, shift and comparison trees are supported. Compound and
    unary update results must be discarded in the whole-function proof. Plain
    assignment results may be consumed for proven register or stack
    destinations: registers preserve the RHS word; stack results reload the
    stored width. Unknown-pressure assignment results remain unproved.
    Literal-only add/subtract/multiply updates remain limited to nonnegative
    signed-32-bit operands. Each operation retains its original typed operator,
    ordered operands and source position. Calls require original named
    definitions, provided fixed arguments and integer or U0 results; callbacks,
    defaults and variadic calls remain outside this proof.

    Distinct RSI, RDI and R10 through R15 requests are supported. Outside the
    exact straight-line subset, automatic candidates must fit the unreserved
    variable banks unless forced requests have guaranteed capacity and remaining
    ordinary members are I64/U64. Those ordinary pressured members retain
    [Unknown] selection. Other explicit registers, duplicate requests, address
    escapes and Boolean frame members remain unproved. Every original signature
    parameter must have its own named frame location. Unnamed formals are
    rejected because their original _anon_ members can still reserve or compete
    for registers, but are not represented by this frame API. Limits are 64
    blocks, 512 instructions and 32 expression levels. Calls, casts,
    short-circuit expressions, power and unmatched optimizer rewrites cannot
    acquire an expression-tree proof.

    Only the four warning bits and NO_REG_VAR may differ from default options.
    Warnings do not change allocation. NO_REG_VAR disables automatic and plain
    reg selection, while supported explicit requests remain selected. Options
    must match the original definition snapshot. The independently sealed whole
    source body must remain original; missing receipts and try/catch reject both
    exact and fallback proofs. Every register request must still agree with its
    original AST qualifier. The witness does not itself change execution or
    claim physical register placement. *)

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
(** Return the exact scored winner's RSI, RDI or R12 through R15 placement, or
    an original source-owned member's supported explicit request, only after
    [selection] revalidates [Register_candidate]. R10/R11 consume their actual
    allocation positions and retain private full-width homes. Exact spills have
    no physical register. The original source/IR bijection, member order and
    complete allocation map are rechecked, not inferred from fallback
    selections.

    Outside that strict subset, also admit the sole possible automatic
    candidate, or the sole plain [reg] candidate when at most six frame members
    could be automatic candidates. Count every unspecified and plain [reg]
    member, including unused and unknown members. The sole forced candidate's
    original I64_MAX/2 priority proves it precedes ordinary bounded scores; no
    score or sort-tie order is guessed.

    Explicit requests reserve slots even when unused. Automatic placement uses
    the first unreserved member of RSI, RDI, R10, R11, R14, R15, and returns
    [None] if that first slot is R10/R11, if capacity is exhausted, or if the
    candidate upper bound could activate the R12/R13 bank. NO_REG_VAR disables
    automatic and plain [reg] placement. Stack and unknown selections remain
    unproved, regardless of their requests.

    Every original formal must be represented by an authenticated named frame
    member before any physical grant, including an explicit request. Every
    member contributing to these facts must retain semantic ownership and its
    original request identities. Request contents, source origins, frame types
    and instruction data are sealed by independent primitive snapshots; changes
    invalidate the witness. This query authorizes only this bounded physical
    placement, not a general register allocation decision. *)
