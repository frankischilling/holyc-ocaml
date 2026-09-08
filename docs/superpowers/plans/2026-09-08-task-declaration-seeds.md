# Task declaration seeds implementation plan

> Use superpowers:executing-plans for serial implementation and verification.
> Independent reviews are read-only; root owns edits and executable use.

**Goal:** Carry parser-assigned semantic symbols into completed task commands
without recollecting their declarations or moving them into new module scopes.

**Architecture:** A semantic namespace allocates opaque publication tokens.
A driver ledger consumes private parser events, retains the original source
objects and seals a command-local collection view. The existing integer pipeline
uses that checked view when compiling a parsed task command. Runtime publication
remains separate from semantic symbol allocation.

**Tech stack:** Existing OCaml, Dune, Alcotest, checked Sema and integer IR.

**Spec:** `docs/superpowers/specs/2026-09-08-stateful-exe-design.md`, especially
published identities, native declaration timing and exact source ownership.

## Constraints

- Stay on `feat/635-stateful-exe`; preserve the reference
  `c26482bb6ad3f80106d28504ec5db3c6a360732c`, packages and dependencies.
- Serialize executable use and preserve all earlier integer task controls.
- A semantic symbol is not executable/storage admission. Failed or unfinished
  declarations cannot become retained runtime bindings through this change.
- Final source association uses physical AST children and parser witnesses;
  matching names, spans or numeric IDs is insufficient.
- Cross-command extern reconciliation, partial type/storage/header admission,
  query selections, pending/predecessor authority and StreamPrint remain required
  parts of the complete #635 plan. This work supplies their identity seam.

## 1. Allocate retained semantic publications

Files: `src/sema/declaration_collection.ml/.mli`,
`test/test_declaration_collection.ml`, `test/test_integer_task.ml`.

- [x] Add real task RED tests: separate command declarations must share the same
  module scope, and parsing errors after global/function publication must retain
  the assigned semantic symbol while leaving runtime lookup unavailable.
- [x] Run `opam exec -- dune exec test/test_main.exe -- test 'integer task'` and
  retain the expected scope/publication failures.
- [x] Add opaque `namespace` and `publication` types to Declaration_collection.
  `create_namespace ~table ()` owns a fresh module scope;
  `publish namespace ~name ~kind ~origin` allocates one symbol;
  `view namespace (publication * declaration) list` validates exact ownership,
  declaration kind/name/origin, unique publications and command-local order.
  It allocates no symbols and leaves earlier views unchanged.
- [x] Verify copied/foreign publication rejection, retained symbol identity,
  intervening shadows, duplicate and reordered view rejection without mutation.

## 2. Seal parser source associations

Files: new `src/driver/task_declarations.ml/.mli`, `src/holyc_lib.ml`,
new `test/test_task_declarations.ml`, `test/test_main.ml`, `test/dune`.

- [x] `Task_declarations.create session` owns one semantic namespace and the
  session's source and parser-symbol environments. `observe` consumes exact
  private declaration events. Replayed, foreign and out-of-order completions
  fail without assigning another symbol.
- [x] Keep original provisional witnesses and completed ASTs. Header completion
  associates both exact parser entry snapshots with the same semantic symbol.
  Preserve saved alias/join selection witnesses for the later resolution seam.
- [x] `seal ledger ast` matches final globals, prototypes and definitions with
  their exact publication and completed children, then creates a command-local
  declaration view. Reject substitutions, unfinished sources and overlap before
  claiming those publications. Preserve the existing integer-program rejection
  of aggregate declarations; ordinary aggregate collection remains unchanged.
- [x] `collection ~table ~ast command` exposes the view only for its exact source
  AST and owning table. Read-only `symbol_for` maps an exact parser entry to its
  assigned symbol; it grants no runtime authority.
- [x] Verify real Parser.parse event streams, nested interleaving and shadows,
  multiple globals, singleton globals, prototypes, body completion, copied AST
  substitutions, source/table/environment mismatches, replay and incomplete input.

## 3. Compile task commands from their retained declarations

Files: `src/driver/integer_source.ml/.mli`, `integer_program.ml/.mli`,
`integer_task.ml/.mli`, `test/test_integer_task.ml`.

- [x] Thread an optional opaque Task_declarations command through the task-only
  compilation path to Integer_source.prepare_unit. Reuse its checked collection;
  keep ordinary source and callback-free AST APIs on their established path.
- [x] Integer_task owns a declaration ledger. Its run path supplies the parser
  observer, seals the completed AST and compiles it with those assigned symbols.
  Parse failures retain reached semantic publications but do not admit runtime
  storage or executables. Existing command replay and whole-graph preflight
  remain in force.
- [x] Pass the original RED tests and existing retained-global/function tests.
  Add controls showing no duplicate symbols at command completion and failed
  declarations remain unavailable to runtime lookup after a later valid command.

## 4. Review and verify

- [x] Obtain a read-only source-ownership review and resolve concrete findings.
- [x] Format, run the full suite serially, then build formatting/generated/all/
  install targets. Only the existing fourteen real #exe groups may remain RED.
- [x] Verify 82 checksums, eleven provenance scenarios and exact full corpora.
- [x] Update support docs, the main plan and external prompt without changing
  its 102 fenced examples. Commit/push, rebuild the committed revision and verify
  retained task controls plus the original CLI/resource report identities.
- [x] Inspect hosted checks and keep PR #636 draft, issue #635 In Progress and
  all full-compiler requirements active until the complete goal is achieved.

## Local verification

The real task scope/publication controls failed before production changes.
The reconstructed-source control also failed first. Independent source review
reproduced reversed original declarators; the publication ordinal fix passes that
control while retaining nested-command gaps. No further blocker remained in the
reviewed declaration association. Whole-command membership and predecessor
admission are explicitly outside this completed portion.

All 101 focused controls pass: 47 task, 39 streaming-parser, eight declaration
ledger and seven semantic collection groups. Full verification ran 2,162 tests
in 51.275 seconds, passing 2,148 and failing the fourteen pending #exe groups.
A separate real #exe run confirms all fourteen remain RED. CLI, formatting,
generated files and build/install checks pass. All 82 audited checksums and
eleven incremental provenance scenarios pass. Complete lexer JSON and parser
JSON/normalized text match the existing baselines: 528/528 tokenizes,
25 standalone parses and 126 with the prelude.

Pushed source `4a2955c14c1c2f6d8dfeb261ce4917e772c4ea1d` was rebuilt and passed
120 focused groups. Sixteen pending #exe and 34 prior integer/resource reports
retain exact source/reference identities. CI 34256649351 failed only the
fourteen #exe groups on OCaml 5.1/5.3; Corpus 34256649331 and dependency review
34256649338 passed. The external prompt preserves all 102 fenced examples.
Issue #635 stays In Progress with seven pending criteria, and PR #636 is draft.
The complete #635 acceptance list remains open.
