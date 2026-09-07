# Scalar Pointer Aliases Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox syntax for tracking.

**Goal:** Execute checked scalar pointer aliases across storage and direct calls through public run and CLI.

**Architecture:** Extend the shared expression planner with checked dynamic lvalues and the VM with internal runtime references. Preserve public integer results, canonical address metadata, immutable preflight evidence and existing scalar accounting.

**Tech Stack:** OCaml 5.1/5.3, Dune, Alcotest, PowerShell; existing dependencies.

**Spec:** `docs/superpowers/specs/2026-09-07-pointer-aliases-design.md`.

## Global constraints

- Exact checked operand/type/binding/object evidence; runtime references identify live invocation storage.
- One-level I64/U64 pointer locals/fixed parameters and existing scalar pointees; explicit broader-domain boundaries.
- Existing scalar values/counts, modes, phases, limits, preflight and public result signatures remain valid.
- Pinned reference c26482bb6ad3f80106d28504ec5db3c6a360732c; normal protected Git integration under configured human identity.

## Task 1: Connect pointer source gates

Files: `src/ir/expression_lowering.ml`, `src/ir/integer_interpreter.ml/.mli`, `test/test_integer_pointers.ml`, `test/test_main.ml`, `test/dune`.

Interfaces: internal `runtime_value = Runtime_word of word | Runtime_pointer of address`; address holds a storage instance, index and pointee type. Add pointer operand/prepared value and stored-type variants while preserving numeric operands for arithmetic. Add checked direct/indirect assignment addresses in the planner. Public execute_program and scalar results retain their signatures.

- [x] Add seven source regressions from #611 using `G.run ~mode source |> F.expect 42L`. Run `opam exec -- dune exec test/test_main.exe -- test 'source integer pointers'`; verify HCRUN0003 before production changes.
- [x] Extend checked frame-value recognition to exact one-level pointer values. Add pointer-compatible initializer/assignment validation; scalar update validation stays word-only. Plan direct &lvalue through Storage_address then IC_ADDR; &*p forwards p through IC_ADDR. Dynamic lvalues visit their pointer and alias its result before RHS lowering; preserve typed prefix checks and source spans.
- [x] Add internal runtime values and storage instances to VM frame slots, values and saved callers. IC_ADDR produces references from canonical checked addresses; dynamic load/store/update resolve the retained instance. Frame_base/offset metadata still ticks. Invalidate returned frames and reject unknown/dead/type-mismatched references with instruction provenance.
- [x] Extend frame validation, pointer-object canonical addresses, pointer loads/stores, function-local END_EXP and fixed argument pushes. Match pointer types and reverse push positions; retain scalar argument conversions and integer-only returns. Reject raw integer execute_function arguments for pointer parameters.
- [x] Run all seven gates green in both modes. Inspect IC_ADDR, pointer argument pushes, dynamic stores and untouched old scalar instruction counts.

## Task 2: Ownership and integration verification

Files: above; `examples/integer-pointers.hc`, `test/test_integer_program_cli.ml`, pointer/program/function/global/static/IR/testing/compatibility/source-map docs, README, CHANGELOG, `reference/traceability.toml`.

- [x] Add caller-observed writeback, no-read address creation, reassignment/copy/aliasing, U64 high bits, reverse mixed argument order, nested and recursive calls, same frame offsets in distinct activations, branches/loops and explicitly forwarded static references. Preserve initializer phase/owner/callee faults.
- [x] Add malformed-IR pointer/integer/type/owner joins, invalid pointer pushes, wrong pointee types, raw pointer argument rejection, forbidden pointer returns/images/arithmetic/casts and reached unknown pointer/pointee loads. Verify exact step/frame/depth/global budgets and fresh execution images.
- [x] Add the caller-writeback CLI fixture, exact counts/bounds and deterministic dumps in both modes. Rebuild exact-version proofs for all earlier fixtures. Document pointer lifetime policy and remaining complete compiler requirements.
- [x] Run focused/full tests, CLI, `dune build '@fmt' '@generated-check' '@all' '@install'`, 82 pinned checksums, exact lexer manifest and AOT parser baseline. Resolve independent review findings.
- [ ] Commit/push, open linked draft PR, verify all five exact-head checks and review threads, and merge normally with the source-head guard.
- [ ] Verify source/merge tree equality, rebuilt merge version and executable proofs. Update issue/project/epic/desktop section 52 and leave clean synchronized main with the full goal active.

Review clarification: IC_ADDR checks static ownership at its own consumer instruction, so canonical metadata cannot be borrowed beyond an initializer region. Materialized references remain transferable. Dynamic lvalues use a distinct Indirect_address node with pointer-to-result type checking, preserving normal Alias type equality. Test pointer parameter reassignment separately from pointee mutation.

Local verification: seven original tests failed with HCRUN0003 before implementation
(9OOHZWFJ), then passed in both modes (86TZ0VIG). The expanded nineteen groups
passed (3UESHDKZ); final full run KR4CHF98 passed all 1,773 tests in 71.151s with
formatting/generated/build/install. The first full run exposed two obsolete
I64 pointer rejection tests; those now check the unsupported I32 domain, and
raw owned-local pointer execution has a positive case. The CLI verifies 43 runtime
steps, zero preparation, 16 active bytes and depth 2; lower bounds fail. All 82
checksums and exact lexer/parser baselines pass. Earlier executable values and
counts are preserved. Independent production review found no remaining blocker.
