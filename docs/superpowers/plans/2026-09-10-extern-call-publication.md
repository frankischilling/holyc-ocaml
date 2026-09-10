# Extern call publication

Continue #635 from 2d51f9112f06a92de560e209ae4a605bc81cf1f9 against
TempleOS c26482bb6ad3f80106d28504ec5db3c6a360732c.

PrsStmt.HC:62-190 reuses an unresolved function record, initially points its
exe_addr to UndefinedExtern, and publishes compiled code before clearing extern.
PrsExp.HC:545-582 emits indirect JIT calls through that slot and symbolic AOT
extern calls. CExcept.HC:98-102 throws UndefExt when an unresolved call is reached.
An already compiled call retains its header/ABI while the joined executable
becomes available. A later JIT definition with a new identity cannot replace it.

- [x] Retain exact predecessor declaration objects across local and retained
      joins in Function_resolution; expose a strict ancestry predicate.
- [x] Reproduce forward calls, mutually recursive functions, retained unresolved
      bodies followed by publication, and reached unresolved calls.
- [x] Retain entry statement item ownership in Runtime_call_context. Prepare
      extern call protocols from their original signatures and resolve only
      exact joined published definitions at invocation. Preserve original
      provider behavior until a compatible source definition is published.
- [x] Verify source-order JIT publication, AOT linked availability, saved
      defaults, variadic bodies, original callees after shadowing, signature
      boundaries, resources, failure recovery and hostile copied evidence.
- [ ] Review, run full/build/install/provenance/reference/corpus checks, commit,
      verify committed consumers, push, and update draft PR #636 and the prompt.

The complete compiler/native/BIN/loader/bootstrap goal remains active. This
connects source-backed extern call publication; machine-address reads, native
imports and full ABI/header-mismatch behavior remain subsequent requirements.

## Local evidence

Initial forward and retained calls failed HCIRVM0014 before implementation
(2AM6JBHC); the ancestry API's tests failed to build before its addition.
Ten extern groups now cover saved defaults, variadic and pointer arguments,
mutual recursion, reached effects and recovery, provider replacement, captured
ABI mismatch, resource limits, exact source ownership, initializer traversal,
and preparation fragments. An independently reconstructed legacy AST cannot
acquire the source-selected function lineage.

Review identified initializer traversal selecting future JIT provider overrides
and potentially missing current-image targets through retained bodies. Source
inspection now observes the same publication boundary; global and static
provider timing regressions pass. Fragment production callers supply no local
definitions and inspect only admitted task functions. The final read-only
review found no remaining production blocker.

All 2,580 compiler tests pass (G4993RUH, 58.453 seconds), plus fourteen authority
tests. Both CLI suites pass; obsolete CLI expectations were updated to retain
earlier output on reached HCIRVM0030 and to use the prelinked AOT source body.
The stateful extern fixture returns 42 with empty ordinary output, 50/6 runtime
and preparation instructions in JIT and 52/6 in AOT. Exact and one-below
instruction limits pass the maintained CLI checks.

All 104 provenance scenarios and 82 reference checksums pass. Complete corpus
reports remain identical to the pinned baselines: 528 lexical files, 25
standalone parser acceptances and 126 with the prelude. Format, generated-source,
build and install checks pass. Commit, exact committed consumer verification,
push, and hosted/prompt checkpoints follow this local record; #635 remains open.
