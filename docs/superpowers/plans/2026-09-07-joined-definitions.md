# Joined function definitions implementation plan

**Goal:** Execute the four #623 source gates in both modes while retaining exact
definition/frame ownership and selected call declarations.

**Architecture:** Keep `Function_body.symbol` as the definition's own symbol.
Attach opaque checked definition evidence through `with_definition`, deriving
the callable symbol from the exact classified declaration. VM callee lookup and
initializer traversal consume that callable identity; local/frame/static checks
continue using the definition owner.

**Tech stack:** OCaml 5.1 or later, Dune, existing checked semantic and IR modules.

**Spec:** Issue #623 and Section 58 of the external requirements prompt. Pinned
TempleOS reference: `c26482bb6ad3f80106d28504ec5db3c6a360732c`.

## Constraints

Preserve public raw-body construction, package names, CLI v1/v2, source call
snapshots and all existing resource/initializer guards. Do not alter symbol
equality. Prototype declarations add no executable frame or instructions.
Earlier selected ordinary extern calls and ambiguous multiple callable bodies
retain explicit boundaries. Hosted providers keep their selected declaration
behavior. Renamed parameters do not block joins; optional native header warning
parity remains a distinct requirement.

## Task 1: Connect checked definition evidence

Files: `src/ir/function_body.ml/.mli`, `src/driver/integer_program.ml`,
`src/driver/integer_initializers.ml`, `src/ir/integer_interpreter.ml`,
`test/test_integer_joined_definitions.ml`, `test/test_main.ml`, `test/dune`.

Review refinement: preserve physical declaration membership through
`function_call_resolution`, `function_call_conversion_policy` and
`function_call_expression_result`; retain `function_frame_layout`'s exact input
header. A replayed declaration resolution around the original header reproduced
the missing provenance guard. Matching symbol/scope/item metadata alone is
insufficient. Tests also rebuild a frame with a different physical header while
keeping the original source declaration, to reach the frame-header guard.

- [x] Add four public report tests, using the existing output report helpers:

  ```ocaml
  let source = "extern I64 Id(I64 value);I64 Id(I64 n){return n;}Id(42);"
  let check mode =
    Test_integer_output.run ~mode source |> Test_integer_output.expect ""
  ```

  Include joined U0, nested calls and the PutChars snapshot fixture from #623.
  Run the focused group before changing production; require the observed
  HCRUN0001 admission failures.
- [x] Add the checked body adaptor and read-only consumers:

  ```ocaml
  val with_definition :
    records:Sema.Function_record_classification.t ->
    sources:Sema.Function_call_expression_result.t ->
    frames:Sema.Function_frame_layout.t ->
    definition:Sema.Function_record_classification.classified_declaration ->
    frame:Sema.Function_frame_layout.function_layout ->
    t -> (t, error list) result
  val callable_symbol : t -> Sema.Symbol.t
  val definition_declaration : t -> Sema.Function_resolution.resolved_declaration option
  val definition_matches_frame : t -> Sema.Function_frame_layout.function_layout -> bool
  ```

  Validate physical declaration membership, Definition kind, exact typed/header/
  frame symbols and scopes, item index, parameter/local members, return type,
  flags and compiler options. Derive the callable identity from that declaration.
  Raw `create` retains its current symbol and no alternate-identity input.
- [x] Replace the driver rejection with the checked adaptor after body creation.
  Keep runtime-context caller ownership on the definition symbol. Use the
  callable accessor for callee summaries, duplicate checks and transitive
  initializer lookup. Retain exact associated-frame checks in VM preflight and
  compare selected definition snapshots when dispatching bound source bodies.
- [x] Run focused tests, then extend cases for repeated prototypes, recursion,
  pointer/U64/U0 behavior, statics/fresh images, initializer effects/fault phases,
  declaration snapshots, JIT shadowing and AOT import/multiple-body boundaries.

## Task 2: Verify ownership and public reports

Files: joined-definition tests, `test/test_integer_program_cli.ml`, `test/dune`,
`examples/integer-joined-definitions.hc`, public interface comments.

- [x] Reconstruct malformed associations through public APIs. Reject foreign
  declaration/frame/body batches, same-number symbols, incorrect declaration
  chains, prototype-owned parameters, changed return/flags/options, missing
  binding evidence and duplicate callable identities at the intended guard.
- [x] Preserve raw body/frame rejection for bare symbol substitution. Retain
  exact initializer-subtree checks and definition-position JIT publication.
- [x] Compare prototyped execution to the equivalent unprototyped source for
  steps/preparation and exact frame/depth limits. Add maintained source/CLI
  fixtures for both modes and captured provider/source snapshot behavior.
- [x] Freeze compiled files before serialized full tests and build checks.

## Task 3: Review and integrate

Files: README, changelog, joined-definition guide, program/IR/testing/
compatibility/source-map docs and traceability.

- [x] Document the measured behavior, checked API, pinned evidence and remaining
  boundaries. Update the external prompt if implementation uncovers a missing
  requirement. Preserve all existing fenced fixtures.
- [x] Run focused/full/CLI, quoted `@fmt`, `@generated-check`, `@all`, `@install`,
  reference checksums, all provenance scenarios and exact lexer/parser corpus
  comparisons. Resolve independent review findings with focused regressions.
- [ ] Commit and push through the Git identity helper, open a linked draft PR,
  rebuild to verify committed identity and fixtures, and require all five final
  source CI checks before normal protected merge. Verify merged tree/version,
  post-merge CI and synchronized main; update issue/project/prompt evidence.

## Verification evidence

Initial four gates failed HCRUN0001 before implementation. Independent review
reproduced replayed declaration resolution passing the first adaptor; exact
declaration membership and frame-header provenance fixed it. An independent
frame-header negative test retains the original valid declaration.

The additive `holyc-ir-function-binding-v1` program dump records callable and
definition IDs plus actual definition item index. Its missing-marker assertion
failed before implementation; final tests check exact values, deterministic
replay and unchanged unjoined dumps.

All 1,870 tests passed in 42.782 seconds with CLI checks. The final strengthened
14-group suite passed in 0.072 seconds. Independent production/test review found
no remaining blocker. Integration verification is recorded in #623.

Formatting, generated-source, build/install, 82 pinned checksums and all 11
incremental provenance scenarios passed. Lexer JSON matches the preceding
reviewed capture exactly (528/528, zero errors). Parser JSON matches the
committed AOT baseline exactly, including normalized text: 25/528 standalone
and 126/528 with the prelude. All 56 external-prompt fenced fixtures were
preserved when adding the independently tested producer-provenance requirement.
