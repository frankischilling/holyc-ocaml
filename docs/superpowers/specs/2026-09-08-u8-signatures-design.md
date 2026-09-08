# U8 numeric parameters and returns

Issue #631 continues #396 after merged #629/#630. The eight original sources
in Section 62 fail on both source `132418a97038ed66377874de4e6be5ff0e0df460`
and merge `461c5ecab0d6977cf8d862025af786b8e26ac911`, in JIT and AOT.
Seven fail at executor admission (HCIRVM0011); the scheduled initializer
fails byte-read invariance (HCRUN0006). Each must ultimately produce 42.

## Existing architecture and chosen change

Frame analysis already retains U8 object size one, parameter allocation eight
and positive RBP offsets. Call and return lowering already retain exact public
U8 target types. Extend the executor's existing frame and checked signature
joins; preserve the original graphs, runtime call authority and storage owners.

A separate function-return classifier admits U8 as a runtime U64 word.
The general word-only classifier stays unchanged because it also determines
storage widths and unrelated expression admission. Reusing it globally would
silently change byte storage into word storage. Rewriting the frontend into
I64 signatures would erase the exact source types and is unnecessary.

Parameters expose one initialized byte cell within each eight-byte ABI slot.
Standalone and nested execution narrow incoming bits with `bits & 255` at
entry. Charge all eight bytes; references cannot reach padding or neighbors.
Returns retain all register bits, even when their public checked type is U8.
A later byte store or byte parameter entry narrows those bits independently.

The pinned source is `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
OptPass789A.HC:710-717 loads register-allocated parameters using their raw
memory class; BackLib.HC:528-530 selects MOVZX for U8. The return and call-end
emitters at OptPass789A.HC:779-782,1026-1030 transport full I64 RAX bits.
PrsExp.HC:468-483 and BackLib.HC:312-347 do not add a universal byte argument
cast; entry loads supply the narrowing. No new native capture is claimed.

## Initializer compatibility

Seed ordinary exact U8 named parameters in each whole-graph bounded-write
scan. A parameter qualifies only after the existing fixed point establishes
all direct assignment RHS values as byte-bounded and excludes direct updates.
Keep unknown cycles, explicit registers and escaping explicit byte addresses
outside the proof. A U8 call result remains unknown: its declaration alone
cannot establish a byte range. Continue checking every transitive callee,
original scheduled region, preparation barrier and JIT/AOT phase.

This conservative proof may reject native-safe programs requiring richer
control-flow or interprocedural range analysis. It must never admit a wide
parameter assignment merely because the parameter began narrowed.

## Verification and integration

Use maintained API/CLI tests for all eight original sources, entry narrowing,
full returns, mixed signatures, aliases, neighbors, recursion, argument order,
discard/continuation, missing returns and malformed exact IR joins. Include
initializer positives and rejection controls after unsafe writes and returned
wide values. A maintained example exercises output, literal/persistent/frame
storage, preparation, calls and all exact/one-below limits it uses.

Run serial focused tests, full tests and CLI, quoted formatting/generated/
build/install aliases, 82 checksums, 11 provenance cases and exact corpus
comparisons. Keep the package names, OCaml compatibility and dependencies.
Use the configured human Git identity, independent review, all five final-source
CI checks and normal protected merge. Verify rebuilt source/merge identities
and identical trees, then post-merge CI and tracking. The full compiler mission
remains active beyond this feature.
