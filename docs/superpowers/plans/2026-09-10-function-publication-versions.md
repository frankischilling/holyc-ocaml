# Function header and executable publication versions

> Use superpowers:subagent-driven-development for semantic changes and independent
> review. The root worker owns all Dune runs and runtime integration.

**Goal:** Execute nested same-name function replacement with the original
header selections, mutable extern targets, captured direct bodies and hidden
record lineage preserved.

**Architecture:** Keep each resolved declaration's source/body site intact and
retain its current lookup header separately. Completion consumes the original
pending source but succeeds the latest node of that exact record lineage,
including a hidden lineage. Task snapshots preserve source lookup order while
executable publications update that lineage's target. Compiled direct calls
retain an immutable executable selection; unresolved calls consult the current
published target. Call/header metadata never substitutes the body source.

**Tech stack:** OCaml 5.1+, Dune 3.12+, existing Sema, Driver and IR modules.

**Spec:** [Completed function headers](../../completed-function-headers.md),
TempleOS `c26482bb6ad3f80106d28504ec5db3c6a360732c`, and #635. Full compiler,
native backend, BIN/loader and bootstrap requirements remain active.

## Constraints

- Preserve public package names, original source children and task authority.
- Never publish a body before its original parser resume boundary.
- Do not accept arbitrary resolved predecessors or bind hidden completion by name.
- Preserve current inner defaults/header and earlier direct executable captures.
- Retain existing argument, frame, storage, output and preparation limits.
- Keep ordinary AOT image linking separate from its JIT directive task.
- Incompatible native header/ABI behavior remains explicit until implemented.

## Semantic publication

Files: `src/sema/function_resolution.ml/.mli`,
`src/sema/function_record_classification.ml/.mli`,
`test/test_pending_function_resolution.ml`.

- [x] Reproduce tests for exact outer pending completion after a nested joined
  body and after a resolved same-name shadow.
- [x] Retain current header independently of the original source/body site.
  Resolve completion against an explicit pool of exact record heads, separate
  from ordinary visible-name joins. Check pending witness, typed-object identity,
  namespace/table/mode, joined ancestry, current head and single completion.
- [x] Inherit current header/record flags when publishing an outer body;
  classification still validates original source modifiers and binding.
- [x] Verify copied/current/predecessor substitutions and repeated completion
  reject without consuming the genuine pending source.

## Driver, catalog and execution

Files: `src/driver/function_resolution.ml/.mli`,
`src/driver/integer_source.ml`, `src/driver/task_declarations.ml/.mli`,
`src/sema/outer_environment.ml`, `src/ir/integer_globals.ml/.mli`,
`src/ir/integer_interpreter.ml` and existing call metadata consumers.

- [x] Reproduce the 42/7/101 source matrix in both outer modes and a hidden
  lineage case. Add source fixtures to `test/test_pending_function_header.ml`.
- [x] Carry exact record heads from task snapshots into completion. Preserve
  hidden source records without moving them ahead of a newer lookup shadow.
- [x] Use the current header for new argument/default binding and the original
  source/body for frame layout and executable validation.
- [x] Resolve pending calls through the latest body of their exact lineage;
  retain the executable captured by already resolved direct calls. Apply the
  same target selection to initializer source inspection.
- [x] Cover saved defaults, reached calls between publications, failure recovery,
  argument effects, providers, variadics and exact/one-below quotas.

## Verification and publication

- [x] Run focused tests and full tests, quoted format/generated/all/install
  aliases, pinned checksums, provenance fixtures and complete corpus comparisons.
- [x] Obtain independent review and resolve demonstrated findings.
- [ ] Update user documentation and desktop prompt with exact source evidence,
  unsupported boundaries and executable results. Commit and push to #636.
- [ ] Rebuild normally, verify built/installed consumers against committed Git
  HEAD, inspect hosted checks and confirm clean synchronized state. Leave #635
  and the full compiler objective open until all requirements are satisfied.

Local validation passed 2,623 compiler tests, fourteen authority tests, both CLI
suites, 82 pinned checksums, 104 provenance scenarios and complete corpus
comparisons. Independent review findings were reproduced before fixes:
historical classification reconstruction, recursive initializer targets and
static initializer target precedence. Committed consumer identities and hosted
check results belong in the publication checkpoint after push.
