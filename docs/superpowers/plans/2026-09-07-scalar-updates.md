# Scalar updates implementation plan

> For agentic workers: use superpowers:executing-plans and independent review to implement the connected tasks in order.

**Goal:** Execute #605's scalar compound assignments and prefix/postfix updates through the real source pipeline in JIT and AOT modes.

**Architecture:** Retain checked postfix children and reuse canonical frame/global address lowering. Emit original update ICs and preflight them as location operations. Execute one bounded read/modify/write at the IC, retaining old/new result rules and initializer barriers.

**Tech Stack:** OCaml 5.1+, Dune, Alcotest, generated IC metadata, checked integer VM.

**Spec:** [design](../specs/2026-09-07-scalar-updates-design.md), [#605](https://github.com/frankischilling/holyc-ocaml/issues/605), desktop compiler specification.

## Global constraints

Preserve exact symbols, source origins, public types, source evaluation order, JIT/AOT address intent, whole-program preflight and all resource bounds. Keep the full compiler mission open. Work on `feat/605-scalar-updates` in this checkout. Keep unsupported storage and optimizer forms explicit; do not introduce arbitrary host addresses or native execution.

## 1. Source regressions and semantic/lowering connection

Files: `test/test_integer_updates.ml`, `test/dune`, `test/test_main.ml`, `examples/integer-updates.hc`; `src/sema/function_call_expression_result.ml/.mli`, `src/ir/expression_lowering.ml`.

- [x] Add both-mode source tests: initialized `Total+=n` accumulator -> 42; `I64 Next(){return Total++;}` -> old values; `for(;n<42;n++)` -> 42. Run `opam exec -- dune exec test/test_main.exe -- test 'source integer updates'` and observe HCRUN0003 before implementation.
- [x] Add exact `operand_result` retention to `type_postfix`, using `make_result ~operand_result:operand`. Keep existing lvalue validation and source/result types.
- [x] Add checked scalar update planning. Reuse `prepare_assignment_address` for the lvalue; emit `Storage_address`, then RHS visits for compound forms and existing unary/binary plan nodes for the update. Reject incompatible categories, result/destination types and unsupported locations before emitting any sequence.
- [x] Test all ten compound opcodes, prefix/postfix decrement/increment, alias RHS `G+=(G=2)` -> 4 and `G+=Set()` where Set changes G before returning; preserve original update IC names in deterministic dumps.

## 2. Verified bounded execution

Files: `src/ir/integer_interpreter.ml`, `test/test_integer_updates.ml`.

- [x] Extend location preflight with compound and increment/decrement operation kinds. Accept exact destination/result types and supported RHS words; reject forged addresses, flags, payloads and foreign globals at zero steps. Keep graph-only execution without storage contexts unsupported.
- [x] Prepare update operations with location, arithmetic operation, optional RHS, old/new result choice and destination word type. On execution, read the current slot after RHS evaluation, compute via existing `binary_bits`, store only on success, then publish the selected word.
- [x] Verify all operators against independently calculated values, I64/U64 division and shifts, wrapping boundaries, read-before-write diagnostics, division faults with operator spans, caller argument order, shadowing, comparison and short-circuit contexts.
- [x] Verify exact step/global/frame/depth limits and repeated execution with unchanged initial images. Exercise malformed ICs and foreign contexts through public VM APIs.

## 3. Initializer integration, documentation and protected merge

Files: `src/driver/integer_initializers.ml`, `test/test_integer_updates.ml`, CLI tests, public interface/docs, `reference/traceability.toml`.

- [x] Test nonconstant `I64 H=G++;` and `I64 H=(G+=2);` plus calls containing updates; preserve owner/phase and ordinary final value. Verify updates never materialize as pure constants.
- [x] Extend the existing initializer guard with `Ic_shl_equ`, `Ic_shr_equ`, `Ic_div_equ`, `Ic_mod_equ`. Test known constant RHS and transitive callees reject with HCRUN0006 while dynamic nonzero divisors execute.
- [x] Document source evidence and raw hosted arithmetic/optimizer boundaries. Add CLI fixture/dump coverage.
- [x] Run focused tests, quoted formatting/generated/build/install checks, full tests, 82 pinned checksums and exact lexer/parser baselines sequentially with respect to Dune.
- [ ] Obtain independent review and resolve findings. Commit with configured identity, rebuild exact source revision, verify fixtures, push draft PR linked to #605, pass all five CI checks, and merge normally against the tested head.
- [ ] Verify identical merged tree and exact merge binary, synchronize main, update issue/project/epic/desktop evidence and retain the full compiler goal.

## Local verification

The three source gates failed with HCRUN0003 before implementation (UVD2EYBD).
The initializer compound-divisor guard was observed failing before its extension
(N22JVCYB), and compound fault names failed before the diagnostic fix (E3M0259V).
All ten focused groups pass (NHUAXJPR); the full suite passes all 1,730 tests
(YOGWNX3J), including the source CLI checks. Quoted formatting/generated/all/install
checks, all 82 pinned checksums and exact lexer/parser baselines pass.
Independent source and implementation reviews found no remaining blockers;
the diagnostic and citation findings were fixed. The working source accumulator
returns 42 in 40 runtime steps plus 3 preparation steps in both modes; earlier
initializer/global/function/nested fixtures retain 42/46+3, 42/50, 42/29 and
42/476, and eval retains 42/5. Protected integration is tracked in #605.
