# Source variadic execution

Continue #635 from 22fee0114ff1d0fe563b5de14c9d757a15c871e0 against
TempleOS c26482bb6ad3f80106d28504ec5db3c6a360732c.

PrsVar.HC:373-409 binds an I64 argc followed by an I64 argv array whose
127-element extent is explicitly arbitrary. PrsExp.HC:499-582 supplies the
actual tail, its count and cleanup bytes. The hosted integer VM must use that
actual count for storage and quotas while preserving checked source metadata.

- [x] Reproduce ordinary, recursive and retained source variadic calls in JIT
      and AOT, including `I64 F(...){return argc+argv[0];}F(41);`.
- [x] Admit only exact checked synthetic frame bindings. Prepare argv as a
      dynamic array address, with owned integer tail cells appended to each
      invocation's storage. Keep the declared placeholder out of allocations.
- [x] Specialize checked call protocols to actual fixed/count/tail arguments;
      retain original callee identities and invalidate tail pointers on return.
- [x] Verify zero tails, mutation, pointer forwarding, recursion, retained
      bodies, implicit calls, initializers, actual bounds, and exact/one-below
      frame and instruction limits. Preserve unsupported type boundaries.
- [ ] Review, run full/build/install/reference/corpus/provenance checks, commit,
      verify committed consumers, push and update the draft PR and prompt.

The integer runtime continues to require integer variadic tail values; machine
address bits and floating point tails need their own runtime representations.
All seven #635 criteria and the full compiler/native/BIN/loader/bootstrap goal
remain active beyond this milestone.

Local verification: the two initial source/retained regressions failed before
implementation (9ORA7WPK). Nine new variadic groups and surrounding suites pass:
113 focused tests in 0.669 seconds (TW7ELKZZ). Independent review identified and
verified fixes for allocation before invocation quotas and combined standalone
cell capacity. Final review found no remaining blocker. Previously unsupported
nested calls and I64i/I64 scalar pointer views are now positive regressions;
unsigned-pointer incompatibility remains a negative regression.

The final full run passes 2,566 compiler tests in 45.969 seconds (15KY1I1Y),
fourteen standalone authority tests and both CLI suites. Format, generated
sources, build and install pass. All 104 provenance scenarios and 82 reference
checksums pass. Complete corpus JSON matches the pinned baselines: 528 lexical
files, 25 standalone parser acceptances and 126 with the prelude. The maintained
CLI fixture returns 42 with empty capture at 106/6 JIT and 108/6 AOT runtime/
preparation steps; exact limits pass and either one-below limit fails. Committed
consumer identity checks and publication follow the source commit.
