# Persistent scalar U8 storage implementation plan

**Goal:** Execute all six #625 gates through the public source/API/CLI path in
both modes, preserving the full compiler requirements.

**Architecture:** Extend the existing immutable global/static storage metadata
and the VM's owned scalar cells. Share public scalar widths and initial-image
narrowing between the two persistent producers. Keep byte object extent separate
from static allocation padding and retain all exact owner/initializer evidence.

**Tech stack:** OCaml 5.1 or later, Dune, existing checked semantic/IR modules.

**Spec:** Issue #625 and Section 59 of the external requirements prompt.
Reference: `c26482bb6ad3f80106d28504ec5db3c6a360732c`.

## Constraints and design

Work on `feat/625-persistent-bytes` in the established primary checkout. Root
serializes all Dune and executable use and freezes compiled files during checks.
Use the Git identity helper for every Git/GitHub operation. Keep names, package
interfaces, CLI v1/v2 and graph-only eval's existing domains.

Globals charge their declared scalar width (1 or 8); static scalars charge the
source's eight-byte-padded allocation. Their accessible extent remains 1 or 8.
The hosted quota counts per-object requests, excluding AOT inter-object layout
gaps and host allocator/bookkeeping overhead. Sum sizes with checked arithmetic;
retain a separate conservative host slot-count bound. Unused declarations count.
Static cells have no invocation-frame charge and initialize at declaration time.

## Task 1: Connect persistent byte storage

Files: new `test/test_integer_persistent_bytes.ml`, test registration,
`src/ir/integer_scalar_storage.ml/.mli`, `integer_globals.ml/.mli`,
`integer_statics.ml`, `integer_interpreter.ml`.

- [x] Add the six source gates with exact I64/U64 final classes and captures:

  ```ocaml
  let report = Test_integer_output.run ~mode "U8 G=298;G;" in
  let result = (Test_integer_functions.checked
    (integer_program_report_outcome report)).value in
  let word = Option.get (Ir_integer_interpreter.final_value result) in
  Alcotest.(check int64) "final bits" 42L word.bits;
  Alcotest.(check bool) "final class" true (word.type_ = Ir_integer_interpreter.U64)
  ```

  The actual test helper reads the report and asserts final bits/class and bytes
  directly. Run `dune exec test/test_main.exe -- test 'source persistent bytes'
  --show-errors`; require HCRUN0001 failures before production changes.
- [x] Add a shared public-scalar width and image narrowing helper:

  ```ocaml
  val public_byte_size : Sema.Type.t -> int option
  val narrow_bits : Sema.Type.t -> int64 -> int64
  ```

  Accept only zero-pointer-depth public I64/U64/U8 storage. For valid U8 storage,
  `Int64.logand bits 255L` prepares its initial image; other bits are unchanged.
- [x] Admit checked U8 global/static declarations. Global collection accumulates
  declared widths with overflow checks. Static validation compares element and
  declared size to its scalar width, requires alignment 8 and no frame slot;
  combined storage adds eight bytes per admitted static with checked arithmetic.
  Update global dump byte counts and narrow constant images in both producers.
- [x] Initial persistent VM words use the existing byte-capable scalar value
  classification, preserving U64 for U8 cells. Existing reached stores, loads,
  aliases and output scans keep their current checked cell and extent logic.
- [x] Run the six groups and inspect every failure before expanding admission.

## Task 2: Verify persistent lifetime, initialization and resources

Files: persistent byte tests, `test/test_integer_program_cli.ml`, `test/dune`,
new `examples/integer-persistent-bytes.hc`, public comments.

- [x] Add byte narrowing at 128/255/256/298/-1/unsigned limits for constant,
  scheduled and ordinary stores, independent RHS assignment values, zero images
  and reached unknown JIT reads. Compare U8 initializer reads into byte/word
  destinations without weakening unsupported primitive classes.
- [x] Cover global/static aliases, mixed byte/word cells, recursion, joined
  definitions, fresh executions, static initialization in uncalled functions,
  prior output on faults and exact initializer phase/owner/step evidence.
- [x] Reach declaration and frame provenance guards with malformed public
  contexts; retain valid controls. Exercise scalar bounds through indexed U8*
  aliases and output scans so adjacent cells and static padding stay inaccessible.
- [x] Lock quota semantics with exact/one-below globals, padded statics, mixed
  images and unused declarations; verify frame/depth/preparation accounting.
  Preserve explicit persistent array, pointer-valued storage and narrow-update
  boundaries. Test deterministic dumps and initial narrowed payloads.
- [x] Add maintained CLI fixture and both-mode v1/v2 checks, measured exact
  limits and one-below failures. Keep all previous fixture arguments stable.

## Task 3: Review and integrate

- [x] Document source evidence, width/padding policy, measured resource counts,
  initial-image versus register semantics and remaining full-compiler work in a
  guide, README/changelog, IR/testing/compatibility/source map and traceability.
  Update the external prompt for any newly demonstrated missing requirement.
- [x] Request independent production/test review. Resolve findings with focused
  regressions. Root freezes compiled files for full suite/CLI, quoted `@fmt`,
  `@generated-check`, `@all`, `@install`, 82 reference checksums, all 11
  provenance scenarios and exact lexer/parser JSON plus normalized parser text.
- [ ] Commit, push and open a linked draft PR. Verify committed executable
  identity and fixtures; require all five final-source CI checks before normal
  protected merge. Verify same merged tree, refreshed executable, post-merge CI
  and clean synchronized main. Update #625, #396, project and external checkpoint
  while preserving all prior fenced fixtures and the active full compiler goal.

## Current verification evidence

Six initial groups failed HCRUN0001 in 0.063s, then passed in 0.042s.
The U8-valued global initializer regression failed separately, then all eight
groups passed in 0.144s. Fifteen expanded groups passed in 0.163s.
Independent review requested stronger guard assertions and word-destination RHS
tests; all 34 automatic/persistent byte groups passed in 0.262s after those
changes. The first full suite exposed superseded U8 rejection cases in the
older automatic-byte test; these now retain U16 rejection, while new positive
tests cover admitted scalar bytes. CLI checks passed at 42/77+7 and exact limits.

Global/static collection now checks Sys.max_array_length as well as arithmetic
slot counts. The preserved source quota excludes AOT inter-object padding and
host bookkeeping; a static's one-byte declared extent never includes its padded
eight-byte allocation. Full verification and integration evidence is in #625.

After updating the superseded rejection cases, all 1,885 tests passed in
38.547 seconds, plus CLI checks. No production failure remained.

Independent final review found no code/documentation blocker. Formatting,
generated/build/install, 82 pinned checksums and all 11 provenance scenarios
passed. Lexer JSON matches the preceding capture exactly (528/528, zero errors).
Parser JSON and normalized text match the committed AOT baseline (25/528
standalone, 126/528 with the prelude). The external prompt's 62 fenced fixtures
are preserved alongside added initializer-result and quota-policy requirements.
