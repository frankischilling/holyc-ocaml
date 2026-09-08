# U8 numeric signatures implementation plan

> Use superpowers:executing-plans for serial implementation and verification.

**Goal:** Execute existing checked U8 numeric parameter and return producers.

**Architecture:** Extend exact frame and function-return admission. Preserve
byte storage, eight-byte parameter ABI allocation and full returned register
bits. Add ordinary narrowed parameter entry to the existing initializer proof.

**Tech stack:** OCaml, Dune, Alcotest, existing PowerShell Git workflow.

**Spec:** `docs/superpowers/specs/2026-09-08-u8-signatures-design.md`.

## Constraints

- Reference `c26482bb6ad3f80106d28504ec5db3c6a360732c`; no pin changes.
- Keep existing package names, dependencies and supported OCaml versions.
- Serialize Dune and executable use; compiled files stay frozen during checks.
- Use `feat/631-u8-signatures` in the established checkout. Preserve unrelated work.
- Require five final-source checks and a normal protected merge.

## 1. Parameter and return execution

Files: `src/ir/integer_interpreter.ml/.mli`,
`test/test_integer_byte_signatures.ml`, `test/dune`, `test/test_main.ml`.

- [x] Add the eight Section 62 gates and standalone entry tests. Assert
  `I64 F(U8 n){return n;}F(554);` gives I64 42 and
  `U8 F(){return 554;}F();` gives U64 554.
- [x] Run `opam exec -- dune exec test/test_main.exe -- test 'source byte signatures'`
  and inspect the expected admission failures before editing production code.
- [x] Add `function_return_word_type` using the existing byte-aware scalar
  value classifier; use it only in checked return classification and
  IC_RETURN_VAL validation. Keep `return_word_type` unchanged.
- [x] Validate parameter allocated/slot bytes as eight while keeping object
  bytes one; initialize Stored_byte arguments as runtime U64 `(bits & 255)`.
  Nested calls already use the shared byte-aware coercion.
- [x] Verify mixed signatures, full negative/high-bit returns, updates,
  right-to-left evaluation, aliases/neighbor bounds, recursion, missing returns
  and exact typed call/cleanup/return preflight rejection.

## 2. Native initializer parameter entry

Files: `src/driver/integer_update_initializers.ml/.mli`, same focused tests.

- [x] Retain the initial gate's HCRUN0006 failure and add positive readonly
  parameters with 554/-1 entry plus transitive calls. Add unsafe later-write,
  update, explicit-register and wide call-result range controls.
- [x] Build an entry map from exact ordinary U8 Named_parameter locations;
  seed each bounded-write scan with that map. Keep byte call ends unknown,
  every-write conjunction, direct-update exclusions and the existing fixed point.
- [x] Rerun focused signatures and surrounding update/initializer/storage tests;
  inspect original owner/span diagnostics and noreg/memory positive controls.

## 3. Maintained CLI, fixture and review

Files: `examples/integer-byte-signatures.hc`, `test/test_integer_program_cli.ml`,
`test/dune`, focused API tests and public support/traceability documentation.

- [x] Add a fixture using byte entry, wide byte returns, local/persistent data,
  scheduled initialization, nested output calls and an owned literal.
- [x] Measure actual resource counters; assert exact and one-below runtime,
  preparation, persistent, active frame, depth, literal, output and work limits.
- [x] Add all eight original CLI gates, reporting v1/v2 and deterministic dumps.
- [x] Request independent source/integration review while completing docs;
  reproduce findings before fixes and rerun affected tests.

## 4. Verification and protected integration

- [x] Run `opam exec -- dune runtest -j 1` and
  `opam exec -- dune build '@fmt' '@generated-check' '@all' '@install'`.
- [x] Verify reference checksums, incremental build provenance and exact lexer/
  parser corpora against the preceding successful/committed reports.
- [ ] Review the final diff, commit/push under configured human identity and
  create a draft PR. Rebuild the committed executable and rerun exact gates.
- [ ] Obtain all five final-source checks, complete review, and merge normally.
- [ ] Rebuild merged main; verify source/merge tree identity, gates, quotas,
  post-merge CI, issue/project/parent/prompt status and clean synchronized Git.

## Local verification checkpoint

The first maintained run reproduced 13 expected failures in 20 groups. After
the executor/proof changes, all 20 passed; expanded coverage passes 26 groups
in 0.130 seconds. Final verification passed 1,994 tests in 38.878 seconds plus
CLI checks. The first full run exposed two new test harness mistakes (missing
runtime-output context on replay and an incorrect return dump substring),
both corrected before the passing full run. No production change was needed
for those failures.

The maintained fixture returns I64 42/capture 3432 in both modes with 110
runtime instructions, seven preparation units, twelve persistent bytes, 32
active frame bytes, depth two, three literal bytes, two output bytes and eight
work units. All exact and one-below API/CLI assertions pass. Formatting,
generated-source, build/install, 82 checksums and eleven provenance scenarios
pass. Lexer JSON matches the preceding result exactly; parser JSON and
normalized text match the committed AOT baseline (528 lexer successes,
25 standalone and 126 prelude parser successes).

Independent native/source and integration reviews found no production blocker.
Suggested initializer alias and unchanged-body reconstruction controls now pass.
Source/merge CI tracking is recorded in #631 and its PR.
