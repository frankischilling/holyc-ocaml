# Outer JIT source activation implementation plan

> **For agentic workers:** Use superpowers:executing-plans to implement this plan task by task.

**Goal:** Execute the original outer JIT source and its `#exe` blocks in one retained task, activating source effects in their original order at the first directive.

**Architecture:** The source ledger records original declaration, reference and command witnesses before activation. A single-use activation authority permits deferred initializer/default execution only while consuming the exact next recorded event. The public execution report projects the VM's cumulative result and exposes individual compiled units; ordinary sources without directives retain the existing single-unit path.

**Tech stack:** OCaml, Dune, Alcotest; no new dependencies.

**Spec:** `docs/superpowers/specs/2026-09-08-stateful-exe-design.md`.

## Constraints

- Preserve original AST nodes, publication identities, read selections and command lookahead timing.
- Source promotion alone must not admit declarations or upgrade earlier references.
- Do not reparse a source prefix, synthesize declaration ASTs, or execute ordinary inputs twice.
- Keep AOT task isolation and cumulative quotas. Preserve output and task progress after failure.
- This plan covers outer activation for the existing integer runtime. It does not complete native code generation, F64/string defaults, effectful dimensions, or the remaining compiler requirements.

## Task 1: Ordered deferred execution

Files: create `src/sema/source_activation.ml` and `.mli`; modify `src/driver/task_declarations.ml`, `src/driver/integer_task.ml`, initializer/default authorities and `src/ir/integer_interpreter.ml`; test `test/test_source_promotion.ml` and `test/test_stateful_exe.ml`.

- [x] Add acceptance tests for `I64 N=20; I64 Next(){return ++N;}; I64 Saved(I64 n=Next()){return n;}; N=0; #exe {StreamPrint("%d;",Saved()+Saved());}` yielding 42, and partial initializer/default activation.
- [x] Run the stateful tests and confirm the missing JIT executor diagnostic.
- [x] Journal successfully observed original declaration/reference/command events. At activation, consume the immutable journal once, revoking event authority after each callback and on failure.
- [x] Bind each deferred read to the admission of its originally selected publication at that event; preserve absent, local and unfinished function selections.
- [x] Extend initializer/default guards to accept only live callbacks or the exact active deferred receipt, owned by the same namespace. Keep ordinary stale/replay/foreign rejection tests passing.
- [x] Compile and execute only `Command_resumed` ASTs during activation. A pending statement remains pending until its original resume callback.
- [x] Test skipped events, replay, foreign tasks, activation after parse closure, effect order and partial storage.

## Task 2: Public source execution

Files: `src/driver/integer_source_execution.ml` and `.mli`, `src/driver/integer_program.ml` and `.mli`, `src/ir/integer_interpreter.ml` and `.mli`.

- [x] Lazily adopt the original JIT ledger on the first directive, consume deferred events, then use live task callbacks for all following outer source events.
- [x] Install missing hosted providers through a detached parser view while the original root is suspended. Existing source selections retain their identities.
- [x] Project a VM-owned cumulative successful result after the root sequence is accepted. Report task units separately; never present the final command as the whole program.
- [x] Preserve the existing single-unit outcome getter for isolated programs and add an explicit stateful compilation inspection result.
- [x] Verify all stateful acceptance cases, no-directive counts, AOT isolation, prior output on failure, and cumulative quotas.

## Task 3: Verification and publication

Files: README, task execution guide, reference source map and user prompt checkpoint.

- [x] Run `opam exec -- dune build '@fmt' '@generated-check' '@all' '@install'` then `opam exec -- dune runtest --force --no-buffer` sequentially.
- [x] Review the activation authority, original read binding, error paths and report ownership; fix any findings and repeat affected checks.
- [ ] Verify installed CLI, pinned corpus/checksums and exact revision metadata as required by the repository workflow.
- [ ] Commit and push with configured human identity, update PR #636 and issue #635 with exact evidence, and preserve the prompt's full outstanding scope.

## Follow-up discovered during verification

The existing promotion preflight charges all previously observed closed dimension
work before deferred commands execute. A two-step preparation limit on
`extern U0 Print(U8 *fmt,...);Print("A");I64 Values[1+1];#exe {}` therefore rejects
with HCIRVM0001 and empty output. Preserve the separate inert-promotion contract,
but add ordered dimension charging for source activation so earlier output
survives this later resource failure. The follow-up is implemented and tracked
in `2026-09-09-deferred-dimension-charging.md`; #635 remains unfinished.
