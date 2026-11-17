type selection = Register_candidate | Stack | Unknown
type t

type explicit_conflict = private {
  register : int;
  previous : Sema.Symbol.t;
  replacement : Sema.Symbol.t;
  origin : Sema.Symbol.origin;
}

val analyze :
  frame:Sema.Function_frame_layout.function_layout ->
  body:Function_body.t ->
  t option
(** Analyze scalar register selection using the original definition, frame,
    typed occurrences and completed-body compiler options. Specialized proofs
    cover direct returns of supported literal compound updates and prefix or
    postfix updates. The whole-function proof covers complete integer frame
    members across straight-line code, branches, loops and original named calls.

    Mixed integer/F64 functions require complete canonical re-lowering of the
    original source body: named parameters, entry-initialized locals,
    expressions, returns, branches and loops. Every source root, typed child,
    instruction, bit payload and control edge must agree. Floating copies,
    assignment, unary minus and +, -, *, / are admitted. The six scalar F64
    comparisons retain their exact numeric children and internal-I64 result,
    with canonical USE_F64 and conversion flags. Floating comparison chains
    remain excluded. Scalar numeric conversions are reconstructed with their
    exact producer flags; floating updates, pointers and unsupported source
    forms remain outside this mixed proof. Named calls through
    [analyze_with_runtime] additionally require original exact-owner target
    classifications, fixed provided arguments, definition targets and canonical
    provided-argument conversions. Their complete call sequences and control
    flow are independently re-lowered and primitive-compared, never trusted from
    graph identity alone. Arithmetic and x87 links still require their separate
    execution proof.

    Ordinary F64 members are always [Stack] and do not compete. Plain [reg] F64
    members count as forced candidates even when unused. Explicit requests
    reserve their slot before type filtering and NO_REG_VAR, including unused
    F64 members. Outside the exact phase-qualified subsets below, mixed
    candidates must fit the source variable banks. Canonical hosted instructions
    alone are not source optimizer access-survival evidence.

    A stricter proof runs first for complete straight-line integer/F64
    functions: named parameters, initialized locals, discarded writes/integer
    updates and one final scalar return. F64 trees admit direct reads, literals,
    parentheses, unary +/-, +, -, *, / and discarded plain assignments without
    conversions. It authenticates every original AST root and typed occurrence
    against unique canonical IR accesses, comparing floating payloads by raw
    bits. Each occurrence scores twice; candidates retain original member order
    through TempleOS's unstable QSort2b before zero-score trimming. Ordinary F64
    members are excluded before sorting, while ordinary zero-score integers
    remain. The exact six-slot threshold, two non-pointer slots, explicit
    reservations and plain-reg seeds decide both winners and spills. Used
    winners are [Register_candidate], used losers are [Stack]; unused forced F64
    candidates also receive their proved winner/spill selection.

    The whole-function exact path reconstructs the complete original statement
    tree and canonical graph, then separately proves access survival through
    OptPass012. It admits if/else, while/do/for, multiple returns, fixed named
    calls and recursion with original runtime receipts, scalar numeric
    conversions, the six scalar F64 comparisons, entry initializers and
    discarded direct integer updates. Comparisons add no member score, and do
    not grant ordered x87 links or equality materialization authority. Each
    branch arm, loop component and return is scored once, including
    constant-dead arms, zero-trip bodies and statements after return. The ledger
    retains original member order, unused candidates and reservations. Every
    direct address/consumer pair proves score 2 and net LEA balance 0. Graph
    pairing uses all blocks; the current canonical lowerer preserves them even
    when unreachable. No dynamic iteration count or reachable-only graph can
    replace the ledger. Casts, nested writes, short circuit, chained comparisons
    and decrement conditions retain conservative proofs. Scores do not authorize
    floating arithmetic, conversions or x87 behavior without execution proofs.

    The owned-member exact path additionally admits single-indirection integer
    pointer members, Bool's signed-I8 storage and declarations without
    initializers. It authenticates complete original declarator coverage, source
    type shapes, frame extents and member order independently of initialization.
    Balanced pointer reads score twice; address formation scores once and
    retains its LEA balance, including [*(&x)]. Addressed automatic/plain-reg
    members spill. Pointer survivors count toward the six-member threshold but
    cannot enter automatic R12/R13 homes. Their selected registers contain
    private checked descriptor pointers, not language-visible host addresses.
    Explicit addressed requests remain unproved, as do arrays, callbacks and
    wider pointer domains.

    Outside that owned-member path, automatic locals require one original
    entry-block initializer. Initializer expressions use parameters, literals
    and locals initialized before each read in the entry block; later
    assignments and compound operands may read any checked integer frame member.
    Source-owned arithmetic, bitwise, unary, division/remainder, shift and
    comparison trees are supported. Compound and unary update results must be
    discarded in the whole-function proof. Plain assignment results may be
    consumed for proven register or stack destinations: registers preserve the
    RHS word; stack results reload the stored width. Unknown-pressure assignment
    results remain unproved. Literal-only add/subtract/multiply updates remain
    limited to nonnegative signed-32-bit operands. Each operation retains its
    original typed operator, ordered operands and source position. Calls require
    original named definitions, provided fixed arguments and integer or U0
    results; callbacks, defaults and variadic calls remain outside this proof.

    RSI, RDI and R10 through R15 requests are supported. Only the complete
    owned-member path admits repeated requests: each later member replaces the
    prior owner, which remains stack-backed and excluded from automatic
    candidates. A source-zero final owner in a duplicate group is [Unknown] and
    still reserves the slot; this does not grant a readable physical home or
    claim the original unused parameter load/save bytes. Conflicts are retained
    as pure facts; analysis never emits a warning.

    Outside that path, only distinct requests are supported. Outside the exact
    subsets, automatic candidates must fit the unreserved variable banks unless
    forced requests have guaranteed capacity and remaining ordinary members are
    I64/U64. Those ordinary pressured members retain [Unknown] selection. Other
    explicit registers and unproved duplicates remain unsupported; address
    formation and Boolean frame members require the owned-member path. Every
    original signature parameter must have its own authenticated frame location,
    including each anonymous [_anon_] member. First-visible name lookup does not
    remove later members or their register reservations. Members, blocks and
    instructions use a size-derived cumulative traversal allowance, shared by
    exact and conservative attempts, rather than separate count caps. Recursive
    visitors retain a 256-level host-stack safeguard. Execution callers use the
    result-returning APIs below so exhaustion rejects explicitly rather than
    silently changing narrow register semantics. Calls, casts, short-circuit
    expressions, power and unmatched optimizer rewrites cannot acquire an
    expression-tree proof.

    The four warning bits, ECHO, TRACE, KEEP_PRIVATE and NO_REG_VAR may differ
    from default options. Warning/output/symbol-retention options do not change
    allocation. Unknown bits and other unclassified options reject. NO_REG_VAR
    disables automatic and plain reg selection, while supported explicit
    requests remain selected. Options must match the original definition
    snapshot. The independently sealed whole source body must remain original;
    missing receipts and try/catch reject both exact and fallback proofs. Every
    register request must still agree with its original AST qualifier. The
    witness does not itself change execution or claim physical register
    placement. *)

val analyze_with_runtime :
  runtime:Runtime_call_context.t ->
  frame:Sema.Function_frame_layout.function_layout ->
  body:Function_body.t ->
  t option
(** As [analyze], with original call target receipts for independently
    re-lowered scalar functions containing named calls. Exact whole-function
    call scores require this context, including integer-only functions. It is
    retained and revalidated on every query; foreign or changed contexts cannot
    grant call allocation evidence. *)

val selection :
  t ->
  frame:Sema.Function_frame_layout.function_layout ->
  body:Function_body.t ->
  symbol:Sema.Symbol.t ->
  selection option
(** Recheck the original frame, body and instruction identities. Foreign or
    modified inputs return [None], not a reconstructed allocation decision. *)

val shadowed_explicit_request :
  t ->
  frame:Sema.Function_frame_layout.function_layout ->
  body:Function_body.t ->
  symbol:Sema.Symbol.t ->
  int option
(** Revalidate a stack-backed explicit member displaced by a later original
    member requesting the same supported bank. Requires complete ordered source
    membership, distinct nonoverlapping frame extents and zero address balance.
    A plain stack selection or matching request number alone grants no evidence.
    Shadowed members never acquire an automatic or physical register home. *)

val explicit_conflicts :
  t ->
  frame:Sema.Function_frame_layout.function_layout ->
  body:Function_body.t ->
  explicit_conflict list option
(** Revalidate retained OptPass3 conflicts in original traversal order. Each
    subsequent request yields one fact, with its displaced and replacement
    owners and original explicit request origin. This query has no diagnostic
    side effect; the compilation driver owns once-per-function emission. *)

val unused_explicit_reservation :
  t ->
  frame:Sema.Function_frame_layout.function_layout ->
  body:Function_body.t ->
  symbol:Sema.Symbol.t ->
  int option
(** Revalidate a supported explicit reservation whose selection is [Unknown].
    Independently reconstruct the complete original source and prove zero reads,
    writes and address formations in its member ledger. This reserves the
    requested bank slot without granting a physical home or readable storage;
    absence of hosted accesses alone is not evidence. *)

val physical_register :
  t ->
  frame:Sema.Function_frame_layout.function_layout ->
  body:Function_body.t ->
  symbol:Sema.Symbol.t ->
  int option
(** Return the exact scored winner's RSI, RDI or R10 through R15 placement, or
    an original source-owned member's supported explicit request, only after
    [selection] revalidates [Register_candidate]. Exact spills have no physical
    register. The original source/IR bijection, member order and complete
    allocation map are rechecked, not inferred from fallback selections.

    Outside both exact subsets, also admit the sole possible automatic
    candidate, or the sole plain [reg] candidate when at most six frame members
    could be automatic candidates. Count every unspecified and plain [reg]
    integer member and every plain [reg] scalar member, including unused and
    unknown members. Ordinary F64 members are excluded. The sole forced
    candidate's original I64_MAX/2 priority proves it precedes ordinary bounded
    scores; no score or sort-tie order is guessed.

    Explicit requests reserve slots even when unused. Automatic placement uses
    the first unreserved member of RSI, RDI, R10, R11, R14, R15, and returns
    [None] if capacity is exhausted, or if the candidate upper bound could
    activate the R12/R13 bank. NO_REG_VAR disables automatic and plain [reg]
    placement. Stack and unknown selections remain unproved, regardless of their
    requests.

    Every original formal must be represented by an authenticated frame member
    (including anonymous formals) before any physical grant, including an
    explicit request. Every member contributing to these facts must retain
    semantic ownership and its original request identities. Request contents,
    source origins, frame types and instruction data are sealed by independent
    primitive snapshots; changes invalidate the witness. This query authorizes
    only this bounded physical placement, not a general register allocation
    decision. *)

val authenticates_mixed_scalar_source :
  runtime:Runtime_call_context.t ->
  frame:Sema.Function_frame_layout.function_layout ->
  body:Function_body.t ->
  bool
(** Reconstruct the bounded scalar source body and compare all original graph
    records, including conversion flags, ordered operands and result classes.
    This authenticates source shape, not optimizer scores or x87 links. *)

type proof_error = Proof_limit_exceeded

val analyze_result :
  frame:Sema.Function_frame_layout.function_layout ->
  body:Function_body.t ->
  (t option, proof_error) result

val analyze_with_runtime_result :
  runtime:Runtime_call_context.t ->
  frame:Sema.Function_frame_layout.function_layout ->
  body:Function_body.t ->
  (t option, proof_error) result
(** Execution callers must reject [Error Proof_limit_exceeded], never interpret
    it as stack selection: a missing register can change narrow value semantics.
    [Ok None] retains the existing unsupported-source contract. A function with
    no frame members returns [Ok None] without an allocation-expression walk:
    there is no register/stack choice, even for independently supported deep
    source expressions. *)

val analyze_with_work_limit :
  max_work:int ->
  frame:Sema.Function_frame_layout.function_layout ->
  body:Function_body.t ->
  (t option, proof_error) result
(** As [analyze_result], with a stricter per-attempt work allowance. This can
    reduce, but never expand, the size-derived hosted limit. *)
