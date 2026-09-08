# Stateful #exe implementation plan

> Use superpowers:executing-plans for serial implementation and verification.
> Independent reviews are read-only; root owns edits and executable use.

**Goal:** Execute stateful #exe commands through the existing semantic IR and
inject completed StreamPrint text into the live parser.

**Architecture:** Incremental parser checkpoints feed an opaque task compiler.
Independently checked commands link exact prior publications to stable runtime
objects. Grammar, compilation, runtime effects and stream generation retain
separate ownership and timing.

**Spec:** `docs/superpowers/specs/2026-09-08-stateful-exe-design.md`.

## Global constraints

- Work on `feat/635-stateful-exe` in the established checkout.
- Keep reference `c26482bb6ad3f80106d28504ec5db3c6a360732c`, dependencies,
  package names and supported OCaml versions unchanged.
- Serialize builds and executable use; freeze compiled files during checks.
- Preserve all #633 and earlier execution, ownership and resource gates.
- Full numeric/runtime, optimizer, native/BIN/loader and bootstrap work remains.

## 1. Source RED and parser checkpoints

Files: `test/test_stateful_exe.ml`, `test/dune`, `test/test_main.ml`,
`src/frontend/parser.ml/.mli`, `preprocessor.ml/.mli`, `lexer_frame.ml/.mli`.

- [x] Add all eight literal I64 42/empty-capture issue expectations to the real
  run API; add native timing and cross-frame token controls.
- [x] Run `opam exec -- dune exec test/test_main.exe -- test 'stateful exe'`;
  preserve the missing-execution failures before production edits.
- [x] Extract a shared parser command reader used by full-module and stream-block
  parsing. Preserve statement grammar, source locations and lookahead.
- [x] Add executor entry/command/resume/finish callbacks. The frontend callback
  carries task symbol/definition environments and diagnostics, never IR objects.
- [x] Add a generated lexer-frame kind with directive origin and shared byte/
  nesting charges. Test joined tokens, nested frames, mode restoration and faults
  through real parsed ASTs, independently of the later VM connection.

## 2. Checked task publication and cross-command links

Files: new task state/catalog modules in `src/driver` and `src/sema`,
`session.ml/.mli`, declaration/function collection and resolution adapters,
`integer_source.ml/.mli`, outer binding/call classification and address lowering.

- [ ] Retain one task namespace, exact declaration/header/body/frame/storage
  owners, publication epochs and a pending-command predecessor token.
- [ ] Add checked incremental declaration views and seeded publication joins;
  test an old direct call and an extern join after later declarations.
- [ ] Populate existing outer environments from the catalog. Add exact retained
  direct-function metadata, argument binding and call-target evidence.
- [ ] Add catalog-backed global/static address proofs; test foreign bindings,
  shadowed names, shape/type changes and stale command plans before effects.
- [x] Split `Integer_program` parsing from AST lowering so parsed commands use
  the ordinary pipeline without reparsing strings or wrapping a fake function.
- [ ] Retain old sealed bodies and contexts when publishing a new command;
  only new initializer/preparation effects enter the command delta.

## 3. Persistent VM and generation service

Files: `src/ir/integer_interpreter.ml/.mli`, integer storage/initialization,
runtime call and output modules; driver task executor and report integration.

- [ ] Add opaque task state and checked command execution while retaining
  disposable fresh-state behavior for existing entry points.
- [ ] Bind command-local storage/literal positions to stable per-owner regions;
  verify old statics and mutated literals survive subsequent allocations.
- [ ] Preserve unknown cells, once-only initialization/preparation and consumed
  command history, including writes/publications preceding reached faults.
- [ ] Share cumulative instruction/preparation/work and live-memory limits;
  invalidate current/suspended activation frames without invalidating task cells.
- [ ] Add explicit StreamPrint service authority and a distinct active-block
  buffer using the existing checked formatter; keep ordinary capture separate.
- [ ] Connect parser callbacks to prepare/publish/resume/finish task execution.
  Preserve temporary JIT lookup, pending statements, unfinished functions and
  outer context restoration for generated source.
- [ ] Pass the original eight run gates and additional native timing, nested
  buffer, partial-declaration and task/AOT separation controls.

## 4. Maintained fixture and complete verification

Files: an `examples/stateful-exe.hc` fixture, API/CLI tests, public docs,
`reference/traceability.toml`, changelog and external prompt.

- [ ] Measure and assert exact/one-below cumulative execution, preparation,
  memory, depth, generation and work limits; verify faults and fresh replay.
- [ ] Add CLI v1/v2 and deterministic dump/source-origin assertions for the
  original gates and combined fixture.
- [ ] Update support docs, traceability and prompt with verified behavior and
  explicit native/hosted boundaries; resolve independent review findings.
- [ ] Run `opam exec -- dune runtest -j 1` and
  `opam exec -- dune build '@fmt' '@generated-check' '@all' '@install'`.
- [ ] Verify pinned checksums, eleven incremental provenance scenarios and
  complete lexer/parser corpus reports; inspect every changed result.
- [ ] Commit/push useful checkpoints, open a draft PR, rebuild the final source
  and verify embedded identities, original gates and exact resource controls.
- [ ] Obtain five final-source checks and merge normally; verify merged tree,
  execution, post-merge CI, branch cleanup, issue/project/parent/prompt state.

## Baseline review evidence

Three read-only reviews identified native lookahead ordering, temporary task
JIT mode, unfinished function headers, shifting flattened static/literal indices,
physical proof ownership and prefix retyping hazards. The design incorporates
these before refactoring. The sixteen original CLI reports retain HCPP0008 at
merged `2c966212f7749185ead749b8dcf3cdb793c13325`.

## Frontend checkpoint

Twenty-five maintained parser groups pass. They cover ordinary task command
grammar, nested and included inputs, joined tokens with both source segments,
temporary JIT mode, hidden/restored outer locals, fatal failures and warning
order, shared definition/generation limits, pending command resume, exact
selected callee receipts and early global visibility. A generated global and
function compile through `compile_integer_ast` and run the shared IR to 42 in
both modes without reparsing. This test supplies text through a parser executor;
it does not claim StreamPrint execution.

Independent review reproduced and resolved late recovery execution, opener
lookup in the wrong environment, colliding environment-local definition IDs,
callee rebinding during lookahead and lost opener warnings. Earlier selection
receipts retain exact AST occurrence, environment and entry objects, including
absence and local shadowing.

The fourteen real #exe runtime groups remain RED. Before connecting parser
execution, add semantic publication hooks for provisional function records,
completed headers and each global declarator; extend selection evidence to
sizeof/offset/defined queries. Core task storage can be implemented independently
through the existing outer-binding pipeline, as described below.

Checkpoint verification: 2,074 of 2,088 tests passed in 39.577 seconds; only
the fourteen pending runtime groups failed, all at missing #exe execution.
CLI checks passed. Formatting, generated sources, build/install, 82 audited
checksums and all eleven incremental provenance scenarios passed. Lexer results
remain 528/528 with no errors; complete parser JSON and normalized text match
the committed AOT baseline (25 standalone, 126 with prelude). The reference
remains `c26482bb6ad3f80106d28504ec5db3c6a360732c`; no new native capture is claimed.

## Retained global execution checkpoint

The task API now compiles command deltas against immutable outer-global views
and runs them with fixed-size retained VM allocations. Scalar/array mutations,
unknown cells, pending selection across shadowing, new function bodies reading
old globals, call/formatter array arguments and once-only command receipts are
implemented. Twenty-five task groups cover these paths and cumulative execution,
preparation, memory, output and work limits. Preparation charges occur during
compilation, including pending commands and reached faults, and honor configured
limits above the previous per-unit default.

The phase checkboxes above remain open where they also require retained
functions/statics/literal owners, extern joins or parser publication authority.
This checkpoint does not claim #exe execution. Next connect retained function
headers and bodies to existing call/argument evidence, keeping each body's
original frame, globals, literal arena and callee context. Then add provisional
publication hooks and StreamPrint generation through the parser callbacks.

Checkpoint verification: 2,099 of 2,113 tests passed in 45.997 seconds, with only
the fourteen pending #exe runtime groups failing. CLI checks, formatting,
generated files, build/install, all 82 checksums and eleven provenance scenarios
passed. Complete lexer JSON and parser JSON/normalized text match the earlier
baselines. Independent review reproduced body-outer lowering, array argument,
preparation-budget and statement-wrapper receipt defects before their fixes.
The API and remaining boundaries are documented in `docs/integer-task.md`.
