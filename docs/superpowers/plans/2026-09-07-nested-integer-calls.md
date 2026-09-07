# Nested integer calls implementation plan

> **For agentic workers:** Use superpowers:executing-plans to implement this plan task by task. Keep implementation sequential in the clean feature branch; obtain independent review before integration.

**Goal:** Execute the #599 source fixture through `run --target=ir` and observe 42 from calls inside a return, mutable loop assignment and top-level arithmetic, then cover the other supported expression contexts.

**Architecture:** Add an optional call callback to the existing expression worklist. It returns a checked instruction sequence for an exact typed call at caller-seeded identities. The expression emitter validates contiguous identities, the final call-end producer and its checked type, then composes the fragment and any retained result conversion. The program builder supplies the existing classified direct-call composer as that callback; arguments, initializer values and returns use the same path. Preserve existing entrypoints when no callback is supplied.

**Tech stack:** OCaml, Dune, Alcotest, existing immutable semantic/IR APIs and bounded integer interpreter. No new dependencies.

**Spec:** [Issue #599](https://github.com/frankischilling/holyc-ocaml/issues/599) and desktop specification sections 35–36, 41, 46–47. Reference commit `c26482bb6ad3f80106d28504ec5db3c6a360732c`.

## Constraints

- Preserve exact semantic ownership, source provenance, graph/x87 verification, deterministic identities and failure atomicity.
- Arguments execute right to left and retain formal positions. Calls share instruction, active frame-byte and call-depth bounds with independent storage.
- Reuse the current VM. Change it only if source regressions expose a real missing continuation or call-protocol behavior.
- Keep unsupported defaults, variadic/indirect/external execution, broader storage and native domains explicit. Bounded eval remains unchanged.
- Quote Dune alias targets in PowerShell: `dune build '@fmt' '@generated-check' '@all' '@install'`.

## Work

- [x] Add `examples/integer-nested-calls.hc`, `test/test_integer_nested_calls.ml` and registration; extend `test/test_integer_program_cli.ml`/`test/dune`. Run the source regression and observe the current HCRUN0003 return-call failure.
- [x] Extend `src/ir/expression_lowering.ml/.mli` with the optional call callback, a call plan node and checked fragment emission. Preserve conversion flags only on the final call result.
- [x] Thread the callback through `src/ir/direct_call_lowering.ml/.mli` and `src/ir/return_lowering.ml/.mli`, including initializer lowering in the expression module. Replace the root-call fallback in `src/ir/integer_program_lowering.ml` with the shared expression path.
- [x] Run source tests for nested arguments, initialized locals, returns, arithmetic, condition side effects, early returns, loops, signedness, comparison sharing, faults, bounds and retained unsupported forms. Add malformed-fragment checks for the new callback boundary.
- [x] Update source-function docs, compatibility, IR guide, testing inventory, source map, changelog and traceability. Run focused tests, quoted format/generated/build/install aliases, full tests, reference verifier and corpus comparisons.
- [ ] Obtain review, resolve findings, commit with configured human identity, verify post-commit executable provenance, push a draft PR, pass CI and merge under existing protection. Recheck synchronized main and project/issue state.

The complete compiler mission remains open after this gate: optimizer compatibility, general storage/runtime services, stateful compilation/#exe, native backends, loader acceptance and bootstrap.

Verification so far: the original source regression failed with HCRUN0003 (DOHOLZH3), then exposed unsupported public call-result arithmetic at top level (M6KIVP7W). Program-context word validation was corrected without changing graph-only execution. All 1,700 tests passed (VIK00WD2); quoted format/generated/build/install checks passed. Independent read-only review found no code blockers. The pinned reference and all 82 audited checksums pass; the lexer accepts 528/528 files and the AOT parser JSON exactly matches the reviewed baseline (25 standalone, 126 with prelude, 402 known failures). Integration remains pending.
