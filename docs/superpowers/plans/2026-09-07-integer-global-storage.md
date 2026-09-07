# Shared integer global storage implementation plan

> For agentic workers: use superpowers:executing-plans to implement and verify these connected tasks in order.

**Goal:** Execute the #601 global accumulator source through the public run path in JIT and AOT modes, returning 42.

**Architecture:** A checked `Ir.Integer_globals` context retains ordinary scalar declarations and their logical address intent. Expression lowering reuses the existing frame load/store emitter with a checked global-address alternative. The interpreter preflights those addresses and stores global words independently of active local frames and block-local temporary values.

**Tech stack:** OCaml 5.1+, Dune, Alcotest, existing generated IC registry and semantic passes.

**Spec:** Issue [#601](https://github.com/frankischilling/holyc-ocaml/issues/601), desktop compiler specification sections 41–48, pinned TempleOS `c26482bb6ad3f80106d28504ec5db3c6a360732c`.

**Constraints:** Preserve exact checked identities, source order, mode/options, failure atomicity, graph/x87 validation and existing public APIs. Use a positive global-byte bound separate from active frame bytes. No new native-execution claim. Declaration initializers, aliases, external/import/data-heap storage, static locals and non-word objects remain explicit boundaries for this gate; they remain full-compiler requirements. Continue on the focused feature branch in the existing checkout.

## 1. Source regression

Files: `examples/integer-globals.hc`, `test/test_integer_globals.ml`, `test/dune`, `test/test_main.ml`.

- [x] Add `I64 Total; I64 AddTo(I64 n){Total=Total+n;return Total;} Total=0; AddTo(20); AddTo(22); Total;` as the checked-in fixture and an actual library execution test in both modes.
- [x] Run `opam exec -- dune exec test/test_main.exe -- test 'source integer globals'` and confirm `HCRUN0001` at the declaration boundary before production edits.

## 2. Checked declarations and addresses

Files: new `src/ir/integer_globals.ml/.mli`, `src/ir/global_address_lowering.ml/.mli`; `src/driver/integer_source.ml/.mli`.

- [x] Retain `Global_record_classification.t` from the existing semantic preparation. `Integer_globals.create` consumes it and returns an opaque checked context or located diagnostics. Retain each exact classified record, symbol, public I64/U64 type, source span, slot index, address opcode and initial word policy. Validate ordinary definition, no alias, scalar object, no initializer and code-heap storage, including unused objects. Count eight bytes per slot with checked arithmetic.
- [x] `Global_address_lowering.prepare ~globals result` validates function-body and top-level module-bound identifiers against the context's exact source and canonical symbol, item/declarator positions, type, rank, category and origin. `lower_prepared` emits one canonical `IC_IMM_I64` (JIT) or `IC_ABS_ADDR` (AOT) with a logical symbol payload and checked pointer type, using the caller's identity cursors.

## 3. Shared source composition

Files: `src/ir/expression_lowering.ml/.mli`, `direct_call_lowering.ml/.mli`, `return_lowering.ml/.mli`, `integer_program_lowering.ml/.mli`; `src/driver/integer_program.ml/.mli`; `src/holyc_lib.ml/.mli`.

- [x] Thread optional `?globals` through the expression, argument, initializer-value, return and program lowerers. Select a checked frame or global address according to the retained binding, including transparent parentheses; emit the existing dereference and assignment instructions. Never read the old destination during a simple assignment.
- [x] Accept global declarations in the source driver only through `Integer_globals.create`; retain the context in the compiled program and expose it for checked execution and deterministic dumps. Keep all unsupported declarations and initializers located and explicit.

## 4. Preflight, storage lifetime and public bounds

Files: `src/ir/integer_interpreter.ml/.mli`, `src/driver/integer_program.ml/.mli`, `src/holyc_lib.mli`, `bin/holyc.ml`, `test/test_integer_globals.ml`, `test/test_integer_program_cli.ml`.

- [x] Extend program preflight to recognize exact symbol-backed global addresses and public word loads/stores while preserving graph-only contracts. Reject foreign symbols, wrong opcode/type/flags, forged integer pointers and mismatched contexts before execution.
- [x] Allocate one global word array per program execution after checking `max_global_bytes`; never save, reset or replace it on call/return/block transfer. AOT code-heap words start at zero. JIT words begin unknown and reached reads produce a located hosted diagnostic; assignments adopt the checked destination class and preserve bits.
- [x] Add optional `max_global_bytes` (default 1,048,576) to program APIs and `--global-byte-limit` to run with human/JSON reporting. Validate positive bounds before compilation and exact allocation before execution.
- [x] Exercise the fixture; repeated/nested/recursive calls; loops; local shadowing; separate objects; local initializer, argument and return contexts; right-to-left side effects; I64/U64 crossings; uninitialized/fault contexts; exact byte/step/frame/depth bounds; repeated-run isolation; unsupported declarations; deterministic dumps; malformed IR and foreign contexts.

## 5. Review and integration

Files: README, CHANGELOG, compatibility/testing/IR/source-map documentation, new `docs/integer-globals.md`, public interfaces and `reference/traceability.toml`.

- [x] Run focused regressions, then `opam exec -- dune runtest` and `opam exec -- dune build '@fmt' '@generated-check' '@all' '@install'` sequentially. Check the pinned reference and exact lexer/parser corpus baseline.
- [x] Review code independently; resolve findings. Update the docs with actual execution evidence, limits and next declaration-initializer connection.
- [ ] Commit using the configured human identity, incrementally rebuild and verify the actual source revision and both source fixtures. Push a draft PR linked to #601; wait for all five checks and merge normally against the tested head. Verify the merged tree, build identity and source result; update issue/project/epic/prompt evidence and synchronize main.

Local verification: 1,708 tests passed (run BCMUX7X8), quoted formatting/generated/build/install aliases passed, all 82 pinned reference files verified, and the fresh lexer/parser corpora match their checked baselines. Independent review found no remaining code blockers after the graph-only API guard and malformed/foreign-context regressions were added. Integration remains tracked in issue #601.
