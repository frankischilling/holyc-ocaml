# Persistent arrays implementation plan

> **For agentic workers:** Use superpowers:executing-plans task by task. Root
> serializes builds and executable use; independent agents have explicit file
> ownership and must stop editing before verification.

**Goal:** Execute persistent I64/U64/U8 arrays, including fixed declaration
initializers and direct byte-string copies, through the checked source pipeline.

**Architecture:** Preserve real declaration objects and typed VM cells. Join
checked shapes with ordered initializer-source leaves and per-leaf destinations;
extend existing indexing, preparation and execution-region validation.

**Tech stack:** OCaml, Dune, Alcotest, PowerShell, pinned TempleOS source.

**Spec:** `docs/superpowers/specs/2026-09-08-persistent-arrays-design.md` and
Section 60 of `C:/Users/imike/Desktop/holyc-ocaml.txt` / issue #627.

## Global constraints

- Reference `c26482bb6ad3f80106d28504ec5db3c6a360732c`; preserve public packaging.
- Root uses branch `feat/627-persistent-arrays`; human Git identity is mandatory.
- Build through opam; quote Dune aliases. No concurrent Dune/executable use or
  compiled edits during verification. No new dependency is needed.
- One checked object per declaration; exact owners, symbols, definition frames,
  compiler options, publication positions and initializer subtrees remain intact.
- Full compiler requirements remain active beyond this issue.

## Task 1: Checked persistent shapes and reached element operations

Files: new `test/test_integer_persistent_arrays.ml`, registration in `test/dune`
and `test/test_main.ml`; new `src/sema/global_array_layout.ml/.mli` and
`src/driver/global_array_layout.ml/.mli`; shared checked storage-shape helper;
`integer_source`, `integer_program`, `integer_globals`, `integer_statics`,
`global_address_lowering`, `expression_lowering`, `integer_interpreter`.

Interfaces: global layout consumes exact Global_dimension_binding and AST
dimension inputs, yielding per-record checked dimensions. Persistent storage
exposes object element count and suffix byte strides, with base indexes measured
in cells. Existing scalar accessors continue to operate on scalar objects.

- [x] Add all eight source gates using the existing report helper. The first
  four explicitly store 40 and 2 into separate global/static U8/I64 elements and
  assert numeric 42, exact word class and empty capture. The braced and string
  gates remain part of this issue, not a deferred success criterion.
- [x] Run `opam exec -- dune exec test/test_main.exe -- test
  'source persistent arrays'`; inspect seven HCRUN0001 failures and one
  HCSEMA0052 failure before production edits.
- [x] Add checked global extent producers using Aggregate_layout's closed
  evaluator and Global_dimension_binding's before-owner source provenance.
  Reject nonpositive or unresolved extents and checked product overflow.
- [x] Extend objects with count/strides/extent and cumulative cell bases. Static
  padding applies once per complete allocation. Preserve immutable image
  defaults without allocating large arrays before runtime limits.
- [x] Extend global/static addresses and indexed expression/VM validation with
  exact rank, stride, pointee and object count. Reuse automatic pointer bounds.
- [x] Run the first four gates, then add U64/rank-two/rank-three, calls, recursion,
  mixed scalar/array neighbors, fresh images and exact/one-below quotas. Cover
  U8[9] global=9/static=16 and reject access at byte 9 despite static padding.

## Task 2: Ordered checked initializer leaves and stores

Files: shared initializer-source representation, global/local type producers,
global initializer binding, top-level expression binding/tree, function call
resolution/results, frame/global address lowering, integer initializer
preparation, program lowering and global execution-region validation.

Interfaces: one source initializer manifest per declaration; abstract ordered
leaves retain exact source expressions and paths. Global groups contain all
leaves once; static inputs retain an exact leaf. Checked destinations join a
leaf to an element/byte offset in its original storage object.

- [x] Preserve the existing braced-global and static-braced failing tests; add
  nested and flat fixed-rank forms plus exact malformed-leaf controls.
- [x] Mint source evidence in the original declaration producer and retain it
  through bindings. Verify complete source-expression correspondence when
  creating semantic leaves; do not rely solely on matching paths/origins.
- [x] Extend one global statement group with ordered roots. Replace empty-path
  and scalar-singleton assumptions with complete owner/leaf validation. Extend
  static initializer inputs and consume a complete batch per declaration.
- [x] Join source traversal to checked destination offsets. Reject missing,
  extra, reordered, duplicate and foreign leaves and invalid initializer arity.
- [x] Classify and prepare each numeric leaf separately. Publish AOT constants
  before scheduled load work; preserve JIT declaration order and unknown reads.
  Keep narrowed stored bits separate from evaluated initializer/RHS bits.
- [x] Lower scheduled element stores into exact closed regions, retaining leaf
  subtree membership for calls and declaration phase/owner on faults. Cover
  later constants read by earlier scheduled leaves, uncalled static effects,
  joined callees, transitive guards and earlier output surviving later faults.
- [x] Run focused numeric initializer groups and the earlier shape groups.

## Task 3: Direct owned string copies and complete acceptance

Files: initializer source/layout, immutable persistent images, preparation and
literal handling, persistent array tests, `test/test_integer_program_cli.ml`,
new `examples/integer-persistent-arrays.hc`, CLI argument/dependency registration.

Interfaces: a direct string is one source leaf with a checked copy destination
and retained bytes. Copying creates mutable destination bytes; literal ownership
and its distinct resource budget remain explicit.

- [x] Keep both direct string gate failures, then add equal-size, truncated,
  concatenated, empty and row-string cases. Assert independent destinations and
  successful truncation followed by an explicitly failed unterminated scan.
- [x] Copy only the declared determined prefix of terminator-inclusive owned
  bytes. Reject oversized copies and unsupported multidimensional root-string
  interpretation; do not treat grouped/braced scalar strings as direct copies.
- [x] Audit inferred/zero extents and native count/brace behavior. Retain
  source-grounded diagnostics for forms not connected by this checked path.
- [x] Add one maintained CLI fixture exercising array initialization, mutation
  and a passed alias; measure counts on the implementation and assert exact/
  one-below budgets, deterministic dumps and unchanged v1/v2 reports.
- [x] Run all eight gates and expanded ownership/lifetime/initialization/resource
  cases in both modes, followed by existing persistent-byte/automatic-array tests.

## Task 4: Review, documentation and protected integration

Files: README, changelog, new persistent-array guide, storage/IR/testing/
compatibility/reference-source-map docs, traceability, plan, external prompt;
issue #627, parent #396 and linked PR/project state.

- [x] Document exact source evidence, supported initializer grammar, source
  identities, per-leaf phases, string-copy semantics and measured resource counts.
  Update the external prompt for the zero-versus-negative inference discrepancy
  and per-leaf AOT image visibility while preserving all 70 fenced fixtures.
- [x] Request independent production/test review and resolve concrete findings
  with tests. Freeze compiled files; run full suite/CLI, quoted @fmt,
  @generated-check, @all, @install, 82 checksums, 11 provenance scenarios and
  exact lexer/parser JSON plus normalized parser text comparisons.
- [ ] Commit and push with human identity; open the linked draft PR. Rebuild
  committed identity and rerun maintained fixtures. Require OCaml 5.1/5.3,
  dependency-review, Lexer and Parser checks on final source before normal merge.
- [ ] Verify identical source/merge trees, rebuilt merged identity and fixtures,
  successful post-merge CI and clean synchronized main. Complete issue/project
  evidence, update the prompt checkpoint, and continue the full compiler goal.

## Local verification evidence

All eight baseline gates were reproduced before implementation. Array, leaf,
publication, scalar-brace, scalar-manifest and signed-zero controls each had
their relevant failing run inspected before the fix. Final focused review
coverage passes 43 tests in 0.152 seconds; the full Dune test alias passes
1,925 tests in 38.365 seconds plus CLI checks. Quoted formatting, generated,
build and install aliases pass, as do 82 pinned checksums and all 11
incremental provenance scenarios. Lexer JSON matches the previous 528/528
result exactly; parser JSON and normalized text match the committed
25/528 standalone and 126/528 prelude baseline. Independent review found
no remaining blocker after the scalar fallback and floating payload fixes.

The post-commit and protected integration gates above are recorded in issue
#627 and its linked PR after source identity exists; they are not claims
about this pre-commit plan snapshot. Full compiler work remains active.
