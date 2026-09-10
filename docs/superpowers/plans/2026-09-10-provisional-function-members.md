# Provisional function members implementation plan

> Use superpowers:subagent-driven-development for the independent parser producer
> and semantic consumer work, followed by independent review. Root owns all Dune
> runs, integration, runtime admission and publication.

**Goal:** Preserve original provisional parameter/member state through nested
source execution and admit calls using the native header phase.

**Architecture:** Named function parameter lists publish private member-head
receipts after type lookahead and before default parsing. Default and parameter
completion attach their original children to that member. Ellipsis publishes its
flag before the following lookahead and its synthetic members afterward. A checked
semantic record retains immutable phase snapshots, native argument-count state
and original ancestry independently of completed function signatures. Runtime
admission must consume those snapshots without synthesizing ordinary source ASTs.

**Tech stack:** Existing OCaml frontend, semantic records, retained task ledger and
bounded IR runtime. Keep package names and CLI schemas unchanged.

**Spec:** `C:/Users/imike/Desktop/holyc-ocaml.txt`, latest provisional-header gate;
TempleOS `c26482bb6ad3f80106d28504ec5db3c6a360732c`, PrsVar.HC:519-530,618-657,
PrsDotDotDot:373-405, PrsStmt.HC:62-138 and PrsExp.HC:430-569.

## Constraints

- Fresh and reused native arg_cnt start at zero: ClassMemberLstDel resets the
  reused count after PrsFunJoin saves its old value for comparison. Nested reuse
  can change that same native record before the outer header resumes. Member
  count, active argument count and saved comparison count are separate fields.
- A member exists before parsing/evaluating its default. Default availability
  follows successful preparation. No saved value is reevaluated during reads.
- Original type/name/register/pointer/callback/default children and publication
  lineage remain mandatory. Failed or foreign callbacks cannot allocate locals,
  advance another ledger, install replacement metadata or consume saved evidence.
- Parameter completion is source evidence, not header completion. Ordinary
  completed signatures and legacy caller APIs keep their existing contracts.
- Only branch consumers short-circuit. The acceptance source uses
  `if(0&&F()) {}`, never the eager value expression `0&&F();`.
- Keep every #635 criterion and the full native/BIN/loader/bootstrap goal active.

## Parser producer

Files: `src/frontend/parser.ml`, `src/frontend/parser.mli`, a focused parser test
module, `test/dune`, and `test/test_main.ml`.

- [x] Add failing phase-order fixtures before changing production parsing:
  `I64 F(I64 n=#exe {}40)#exe {}{return n;}` must show n before default input,
  then its exact default/completion before closing-parenthesis lookahead.
- [x] Add `function_parameter_publication` with original head children, index,
  predecessor and live callback activity; publish `Function_parameter_declared`
  after PrsType lookahead and before default parsing.
- [x] Link each existing completed default receipt to its member; add
  `completed_function_parameter` and `Function_parameter_completed`, retaining
  the exact final Ast.function_parameter and publication ancestry.
- [x] Add ellipsis start/member-completion receipts around its native lookahead,
  sharing the original variadic marker with the eventual header.
- [x] Retain ordered member completions and ellipsis receipt on completed headers.
  Cover prototypes, nested directives, defaults, recursive callback children,
  malformed delimiters, rejection, exceptions and later task recovery.

## Semantic phase records and ledger

Files: new `src/sema/provisional_function.ml/.mli`, existing source activation,
compiler-record and task-declaration adapters; focused semantic tests.

- [x] Start a source record only from the exact namespace publication and active
  parser function declaration.
- [ ] Add mutable native shared-record state with checked count/member lineage,
  independently of each immutable source transcript.
- [x] Observe each original head/default/completion/ellipsis event in sequence;
  produce immutable source snapshots. Validate table, namespace,
  environment, command, predecessor and original-child identity before mutation.
- [ ] Capture the checked native and source snapshots for selected reads.
- [x] Complete the source record only with its exact completed header. Test unavailable
  defaults during preparation, rejected/repeated/foreign events, original callback
  signatures and distinct source versions across nested same-name declarations.
- [x] Preserve the transcript through source activation and task ledger replay.
  Existing completed-header consumers must still pass unchanged behavior tests.

## Runtime integration

- [ ] Add the source-derived gate as a failing runtime regression in both outer
  modes: `#exe {I64 F(I64 n=40)#exe {if(0&&F()) {}}{return n;}StreamPrint("42;");}`.
- [ ] Connect checked provisional snapshots to call selection and argument
  binding using the native current count and actual partial members. Preserve
  exact defaults, flags, ABI cleanup, return type and mutable extern lineage.
- [ ] Cover reached UndefinedExtern, argument effects, saved calls, joined
  headers, variadics, malformed arguments, recovery and exact resource limits.
  Do not bypass source-reference checks just because a call is in a dead branch.

## Verification and publication

- [ ] Run focused and full tests, quoted format/generated/all/install aliases,
  reference checksums, provenance scenarios and exact corpus comparisons.
- [ ] Obtain independent review and update current documentation and the desktop
  prompt. Publish coherent tested commits to draft #636, verify normal committed
  built/installed consumers and hosted checks, and leave exact remaining scope.

Runtime acceptance remains unimplemented until the actual source gate passes.
Parser/semantic prerequisites are not a substitute for that requirement.
