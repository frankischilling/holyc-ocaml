# Scalar Static Locals Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox syntax for tracking.

**Goal:** Execute persistent scalar static locals with checked constant initial images through the public compiler and CLI.

**Architecture:** Add opaque static slots to the existing persistent image, retaining separate global and static ownership. Reuse scalar address/update execution and the current bounded constant initializer evaluator.

**Tech Stack:** OCaml 5.1/5.3, Dune, Alcotest, PowerShell, existing dependencies only.

**Spec:** `docs/superpowers/specs/2026-09-07-static-locals-design.md`.

## Global constraints

- Exact semantic symbol, frame, location, initializer and option ownership; no public raw image setters.
- Eight-byte public I64/U64 statics, JIT unknown fill and AOT zero fill.
- Definition-time constant preparation including unused/unreachable declarations; no first-call guard.
- Nonconstant static initialization remains an explicit unsupported phase boundary.
- Preserve full compiler scope and normal protected integration under the configured human Git identity.

## Task 1: Persistent counter through the existing pipeline

Files: `src/ir/integer_statics.ml/.mli`, `src/ir/integer_globals.ml/.mli`, `src/ir/global_address_lowering.ml/.mli`, `src/ir/expression_lowering.ml`, `src/ir/integer_interpreter.ml`, `src/ir/global_initialization.ml`, `src/driver/integer_initializers.ml/.mli`, `src/driver/integer_program.ml`, `src/holyc_lib.mli`, `test/test_integer_statics.ml`, `test/test_main.ml`, `test/dune`.

Interfaces: retain existing global slot getters. Add abstract `static_slot` and `storage_slot`, read-only storage getters, and internal `with_statics ~span ~frames ~functions ~records` plus image publication. Extend `Global_address_lowering.prepare` with optional checked frame. Keep global initializer `item`/`root` compatibility; expose static items separately.

- [x] Add source tests using `G.run ~mode "I64 Next(){static I64 n=40;return ++n;}Next();Next();" |> F.expect 42L`, AOT zero-fill, and persistent recursion. Run `opam exec -- dune exec test/test_main.exe -- test 'source integer statics'`; expect the existing HCRUN0003 failure.
- [x] Add checked static storage and common storage-slot getters. Allocate static indices after globals and preserve exact frame/location/initializer/options. Reject unsupported shapes at context creation, even when unused.
- [x] Lower static identifiers with canonical symbol-backed address opcodes after the existing frame validation. Consume static initializer roots without emitting per-call stores. Require matching static context before excluding statics from invocation frames.
- [x] Generalize the existing initializer collect loop over global/static owners; share classification, guard and evaluator. Reject nonconstant static graphs, publish constant bits internally, and include static preparation steps in the matching initialization context.
- [x] Run the focused counter/lifetime tests green and inspect generated address/update operations.

## Task 2: Ownership, bounds and public executable proof

Files: the files above; `examples/integer-statics.hc`, `test/test_integer_program_cli.ml`, `README.md`, `CHANGELOG.md`, `docs/integer-statics.md`, compatibility/testing/IR/source-map/traceability documentation.

Interfaces: public `compile_integer_program`, `run_integer_program`, existing execution limits, immutable globals/preparation introspection and `holyc run --mode=jit|aot --target=ir`.

- [x] Add and run regression cases for same-name statics in separate functions, nested/recursive calls, fresh image replay, all updates, signedness/wrapping, JIT unknown reads, AOT zero fill, pure `sizeof(parameter)`, and fault/owner spans.
- [x] Add unused/unreachable constant faults and nonconstant initializer rejection. Exercise exact preparation budgets and combined global/static byte limits; statics consume no active-frame bytes.
- [x] Add foreign frame/context and malformed canonical-address regression checks using the existing graph test helpers. Verify static initializer evidence remains immutable and globals-only APIs remain compatible.
- [x] Add the CLI counter fixture and verify value, counts, budgets and dumps in both modes. Update executable-gate documentation and traceability with the pinned source contract and remaining phase-aware work.
- [x] Run focused and full Alcotest suites, `dune build '@fmt' '@generated-check' '@all' '@install'`, pinned checksums and lexer/parser corpus comparisons. Record exact outputs.
- [ ] Obtain independent source/code review, resolve findings, commit/push, open a draft PR linked to #607, and wait for all five CI checks at its exact head.
- [ ] Verify review threads and branch protection, ready the PR, merge normally with the exact head guard, verify merge tree/version/proofs, update issue/project/epic and desktop specification, and leave clean synchronized main.

## Local verification

The three executable source regressions failed before implementation (XRUUZX0W).
The private cross-table storage-ID regression failed before the join guard
(EGKQYU0B) and passed after it (4FXKKQS5). The final 11 focused groups pass
(IDZOB3QG), including both-mode nondefault option images, recursion bounds,
static fault spans and fresh replay. The full suite passes 1,741 tests
(4MEZRWKS, 77.560s), plus CLI checks. Format/generated/all/install checks,
82 pinned checksums, exact lexer manifest and exact AOT parser baseline pass.

Independent source/design/code review found no remaining blockers. The static
counter returns 42 in 21 runtime steps plus 4 preparation steps in both modes,
with exact eight-byte persistent, one-byte frame and depth-one limits. Earlier
fixtures retain 42/40+3, 42/46+3, 42/50, 42/29 and 42/476; eval retains 42/5.
The remaining protected integration is tracked in #607. The full compiler goal
and phase-aware nonconstant static initialization remain open.
