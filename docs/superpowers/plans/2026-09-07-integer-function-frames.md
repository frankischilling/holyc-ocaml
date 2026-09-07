# Integer function-frame execution

**Goal:** Execute checked I64/U64 parameter and automatic-local storage in the existing bounded integer interpreter. This is the storage dependency of V1, tracked by [#595](https://github.com/frankischilling/holyc-ocaml/issues/595).

**Architecture:** Keep the current semantic binding/frame evidence, expression worklist, instruction sequence, named-function body, graph verifier and interpreter. Add an optional checked frame to expression lowering. A named-function execution entry point validates its matching frame and argument bits, preflights the complete graph, and allocates independent slot storage. Canonical RBP-plus-displacement addresses identify checked slots; temporary SSA values remain block-local. No host addresses are involved.

**Tech stack:** OCaml >= 5.1, Dune, Alcotest, pinned TempleOS source.

**Specification:** Issue #595 and desktop mission V1. The final required source remains `I64 Add(I64 a,I64 b){ I64 c=a+b; return c; } (Add(20,22));`. Retention of initializer roots, full body composition and direct-call continuation remain necessary after this increment.

## Implementation

- [x] Add failing tests for actual typed parameter reads, local assignment/read and a returned 42 using the current frame and named-body constructors.
- [x] Expose an immutable prepared address from `frame_address_lowering` so expression planning validates exact binding evidence before allocating instruction/value identities.
- [x] Extend `expression_lowering` with optional frame loads and simple assignments. The assignment target produces its address without reading the previous value; keep source order, operator spans, result type and failure atomicity.
- [x] Extend `integer_interpreter` with a named-function entry point requiring explicit step and frame-byte limits. Match symbol/scope/member identities and parameter order against the checked frame before allocation.
- [x] Preflight canonical frame addresses, loads and stores. Accept only scalar I64/U64 slots and exact type relationships. Keep the existing graph-only interpreter boundary unchanged.
- [x] Execute loads/stores against per-invocation slot storage. Block transfers clear temporary values but retain slots. Reject uninitialized reads with reached-instruction context and charge every instruction.
- [x] Test signed/unsigned bits, assignment results, repeated independent invocations, branch/loop persistence, unreachable validation, foreign frames, noncanonical addresses, unsupported types, identity exhaustion and exact resource limits.
- [x] Update traceability, compatibility and source documentation; run format/generated/build/install, full tests, pinned-reference verification and corpus comparison as applicable.
- [x] Obtain code review and resolve the public/internal type, return signedness and array-boundary findings; confirm the follow-up review has no remaining blockers.
- [ ] Push a buildable draft PR, pass required CI and integrate under branch protection.

## Source anchors and limits

Reference: `c26482bb6ad3f80106d28504ec5db3c6a360732c`. Reuse the audited frame-layout/address sources, plus `PrsStmt.HC:114-124,143-169`, `PrsExp.HC:762-805`, `OptPass012.HC:866-896`, and `BackC.HC:159-189` for parameter offsets, address construction and assignment.

Globals, static locals, arrays, callbacks, aggregates, floating values, numerical casts and arbitrary pointer arithmetic remain unsupported by this execution entry point. Uninitialized local reads are a hosted diagnostic, not a claim that TempleOS initializes those slots. This increment neither claims a native ABI nor completes full source V1 execution.
