# Source integer-function execution

**Goal:** Execute the original V1 Add fixture through `holyc run --target=ir`, including a real local initializer, checked function frame, direct call, caller continuation and an observed final value of 42. Tracked by [#597](https://github.com/frankischilling/holyc-ocaml/issues/597), after merged #595/#596.

**Architecture:** Extend the existing semantic root traversal and integer program driver. Retain scalar initializers with their exact checked local declarations. Reuse frame-address, expression, direct-call and return lowering in the existing structured graph builder. Compile a top-level entry graph plus checked named bodies; the existing integer interpreter preflights all bodies and executes an explicit stack of independent function invocations. Preserve the existing graph-only and bounded eval entry points.

**Public behavior:** `run` keeps stream termination and exit status separate from the final reached top-level expression value. Human and JSON output report the value, bits, type, implementation/reference revisions, mode and resource limits. Calls consume the same total instruction budget as their caller. Frame bytes and call depth have explicit positive limits.

**Initial domain:** Fixed scalar I64/U64 parameters and automatic locals, scalar initializer expressions, supported ordinary statements and structured conditions, integer returns and checked direct executable calls. Retain explicit rejection of unsupported call/expression shapes, defaults, variadic or indirect calls, external execution, non-scalar storage and native targets. This completes V1 only after the original source fixture actually runs; broader nested-expression calls and V2 coverage may follow according to the supported lowering boundary.

## Work

- [x] Add the checked-in original fixture and a failing CLI integration assertion for its actual 42 result in JIT and AOT modes.
- [x] Retain scalar initializer roots through call resolution, conversion policy and expression typing, including exact declaration and source evidence.
- [x] Extend checked frame-address/expression lowering for initializer stores without manufacturing an identifier occurrence or reading the old local value.
- [x] Retain function typing, frames and classified call targets in the existing source preparation pipeline while preserving bounded eval preparation.
- [x] Compose supported named bodies and the top-level stream with shared graph/return/call components and complete source-root consumption checks.
- [x] Preflight canonical direct-call protocols and all function definitions; execute caller continuations, independent storage and shared step/frame/depth limits in the existing interpreter.
- [x] Expose the complete compiled program through the public library and `run`, including final value and contextual diagnostics.
- [x] Exercise changed/zero/negative arguments, local initializer order and mutations, returns, repeated calls, limits, unsupported source and deterministic mode-specific replay.
- [x] Update compatibility, source maps, traceability and documentation; pass formatting, generated checks, builds, full tests, pinned-reference checks and corpus comparison.
- [ ] Obtain review, resolve findings, push a draft PR, pass required CI and merge under existing branch protection.

## Evidence

Pinned TempleOS: `c26482bb6ad3f80106d28504ec5db3c6a360732c`. `Compiler/PrsVar.HC:592-619` allocates a local slot, restores its source expression and parses the initializer assignment. `PrsStmt.HC:151-169,1114-1117` provides the named-body/shared-leave and return-class boundaries. `PrsLib.HC:107-111` and `PrsExp.HC:438-586` distinguish source-order argument parsing from right-to-left COC execution and supply the argument-push/call/cleanup/end protocol. Supplied variadic trees run in reverse order, followed by the hidden count and reversed fixed trees; formal parameter binding remains in declaration order. Reuse the existing audited frame, direct-call and return references. Source evidence and hosted interpreter execution are distinct from native TempleOS captures, ABI delivery and bootstrap.

Local verification: 27 focused cases and all 1,691 tests pass. Formatting, generated checks, full build/install and the 82 pinned reference checks pass. The lexer accepts 528/528 files; the AOT parser report exactly matches the reviewed JSON baseline (25 standalone, 126 with prelude, 402 known failures). Read-only review found no remaining code blockers after the argument-order correction; two stale documentation exclusions were corrected.
