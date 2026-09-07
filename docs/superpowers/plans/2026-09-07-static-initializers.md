# Static Initializers Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox syntax for tracking.

**Goal:** Execute nonconstant scalar static initializers at their source declaration phase through public run and CLI.

**Architecture:** Extend the current initializer region checker and program lowerer with static owners while preserving the global APIs. Reuse existing persistent words, scalar operations, constant preparation and calls; grant initialization authority only within checked static regions and enforce JIT callee publication transitively in VM preflight.

**Tech Stack:** OCaml 5.1/5.3, Dune, Alcotest, PowerShell, existing dependencies.

**Spec:** `docs/superpowers/specs/2026-09-07-static-initializers-design.md`.

## Global constraints

- Exact frame/location/symbol/type/root/options and entry identity; no fabricated global records or public raw image setters.
- JIT compile-initializer and AOT load-initializer phases; no first-call initialization or parameter capture.
- Definition-time processing of unused/unreachable declarations in semantic source order.
- Preserve whole-program preflight versus native JIT fault-order limitations and remaining full compiler requirements.
- Normal protected integration under the configured human identity; all five exact-head CI checks required.

## Task 1: Connect the four source gates

Files: `src/ir/global_initialization.ml/.mli`, `src/ir/global_address_lowering.ml/.mli`, `src/ir/expression_lowering.ml/.mli`, `src/ir/integer_program_lowering.ml/.mli`, `src/ir/integer_interpreter.ml`, `src/driver/integer_initializers.ml/.mli`, `src/driver/integer_program.ml`, `test/test_integer_static_initializers.ml`, `test/test_main.ml`, `test/dune`.

Interfaces: add `static_region_description`/`static_region` and opaque common `storage_region`; `create ?static_descriptions` checks both kinds, `find_storage` and common bounds/phase/frame/symbol getters serve the VM. Add `Initialize_static of Integer_globals.static_slot`, checked static destination/store lowering and `lower_with_storage_initializers` returning entry/global/static descriptions. Existing global-only lowering remains a rejecting compatibility projection. Preparation accepts optional checked function targets and exposes static classification read-only.

- [x] Add source regressions using `G.run ~mode "I64 G=0;I64 Never(){static I64 n=(G=7);return n;}G;" |> F.expect 7L`, early/late snapshots (1/7), and `Seed` counter (42). Run `opam exec -- dune exec test/test_main.exe -- test 'source static initializers'`; require the existing HCRUN0006 failures.
- [x] Generalize the region checker over distinct typed owners while retaining global API behavior. Validate semantic owner order, exact static root/slot, canonical destination/store/end, closed operands/calls, nonoverlap and exact entry identity.
- [x] Add static destination/store and source statement lowering through the current planners. Give recursive direct-call lowering an explicit frame context; static initializers must select exact function targets. Compose pending static regions at function item indices alongside existing module statements and globals.
- [x] Schedule nonconstant static values after shared arithmetic guards, reject actual frame instructions and unsupported AOT option phases, and leave constant image accounting unchanged. Reuse already classified function targets for value preparation.
- [x] Grant static authority to address producers and storage consumers only inside matching checked regions; retain body ownership. Check actual supplied callee graphs transitively against JIT definition availability before effects. Use common region lookup for fault provenance and discarded initializer values.
- [x] Run the four gates green in both modes and inspect canonical static region IR and phases.

## Task 2: Phase, ownership and executable verification

Files: the files above; `examples/integer-static-initializers.hc`, `test/test_integer_program_cli.ml`, relevant static/global/program/IR/testing/compatibility/source-map docs, `reference/traceability.toml`, README and CHANGELOG.

- [x] Add meaningful phase regressions: unused/false-branch/after-return effects and faults; same-frame static reads and updates; earlier recursive callees; JIT self/transitive publication rejection versus AOT containing-function availability; actual parameter/automatic reads; nondefault option rejection; multiple declarations and ordinary statement order.
- [x] Exercise raw-IR preflight: missing/foreign/duplicate/reordered/cross-block static regions, wrong destination/root/type, foreign frame, pointer reuse outside a region, changed callee bodies, complete static-only replay evidence and fresh execution images. Preserve existing global-only APIs and checks.
- [x] Assert exact runtime/preparation/global/frame/depth bounds and operator/active-function/initializer-phase provenance. Add a checked-in nonconstant counter fixture through CLI and deterministic dumps. Preserve earlier executable values/counts and document remaining stateful/native requirements.
- [x] Run focused/full suites, `dune build '@fmt' '@generated-check' '@all' '@install'`, reference checksums and exact lexer/parser corpus comparisons. Record actual results and resolve independent review findings.
- [ ] Commit/push and open a linked draft PR; wait for all five checks at the exact source head, verify review threads/protection, and merge normally with the head guard.
- [ ] Verify merge/source tree equality, exact rebuilt version and executable proofs, then update issue/project/epic and desktop specification and leave synchronized clean main.

Local verification: four original gates failed with HCRUN0006 (XTCCIH4K), then passed (J74PHQ06). All 24 static groups passed (1TVOEJ3Y); the final full suite passed 1,754 tests in 72.003s (SD8YWY1U), plus CLI and combined fmt/generated/all/install checks with native exit zero. All 82 reference checksums, exact lexer manifest and exact AOT parser corpus passed. Independent review resolved transitive caller provenance and the entry-region API comment; no remaining implementation blocker. Working-version executable proofs retain the new 7/1/7/42 gates, static 42/32+0, constant static 42/21+4, and all earlier fixtures.
