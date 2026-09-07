# Scalar pointer aliases

Issue #611 advances V3 from merged 22fe66b5a510d60ab3824328604a05c8d48a5850.
The full compiler goal and desktop section 52 remain authoritative. Implement
all seven audited source gates through the existing run pipeline in both modes.
They return 42 through local/global/static aliases, caller writeback, address
creation before initialization and updates after aliasing RHS calls.

## Representation and alternatives

The public word/result/termination APIs remain integer-valued. Internally the VM
stores `Runtime_word of word | Runtime_pointer of address` in its existing value
maps, frame slots and call argument scopes. An address retains a storage instance,
slot index and exact pointee type. Each invocation receives a distinct live
storage instance; pointers into saved caller frames keep that instance. Returning
invalidates the departing frame. Persistent words use one instance per execution.
This extends existing storage and calls without a second evaluator.

Canonical RBP/offset/add and symbol addresses retain their existing metadata
classification and instruction counts. IC_ADDR materializes a checked runtime
reference from a canonical address, or forwards an already checked runtime
pointer for &*p. Pointer-valued loads, stores and argument pushes preserve that
reference. Dereference operations resolve its original object at execution time.
Using integer-encoded handles would require an additional address registry and
could confuse word arithmetic with references; relabeling slot indexes would
lose caller/recursive invocation identity. Direct internal references are chosen.

## Source lowering and preflight

The shared expression planner accepts one-level I64/U64 pointer object loads,
initializers and assignments, and fixed pointer arguments. Address-taking uses
existing exact frame/global/static producers without loading the pointee. A
checked dereference supplies a dynamic lvalue address for scalar stores/updates.
Evaluate that address before the RHS and read the destination at the update.
Keep original IC_ADDR/IC_DEREF/IC_ASSIGN/update opcodes and their barriers.

Preflight distinguishes integer operands, pointer values, and canonical address
metadata. Pointer assignments and fixed parameters require the retained pointer
type; word-to-word conversions retain the existing behavior. Arguments are
pushed right-to-left, so validation selects parameters in reverse order while
runtime argument arrays retain source order. Pointer-object frame addresses
may have depth two internally; arbitrary source double pointers are unsupported.
Only canonical addresses of checked slots can produce a runtime reference.
Consumers validate live storage and exact pointee type. Direct static-symbol
access keeps its owner/region restrictions; explicit pointers obtained by the
owner can be passed to a callee. No region authority enters callee preflight.

Pointer slots occupy eight bytes and share existing active-frame limits. Calls
share step/depth limits. Unknown pointer or pointee loads retain an execution
diagnostic and source/callee/initializer provenance. Taking &n itself performs
no read. Raw execute_function integer arguments cannot fabricate pointer
parameters. Pointer returns/escapes, null/integer address conversions, pointer
arithmetic/casts, pointer initial images, deeper pointers and other pointee
shapes remain explicit boundaries. Pointer expression ends are allowed inside
functions, including pointer initializers; top-level final results remain words.

## Evidence

Reference c26482bb6ad3f80106d28504ec5db3c6a360732c: PrsExp.HC:151-161,
201-208,776-784,800-802,877-896; OptPass789A.HC:461-464;
OptPass3.HC:519-543; BackLib.HC:693-707; BackC.HC:159-204;
BackA.HC:555-566. The source audit is recorded in #611; no new native capture.

Require all source gates, pointer copies/reassignments, signed/unsigned values,
argument order, nested calls, recursion with same offsets but distinct objects,
loops/branches, same-object aliasing, static pointer forwarding, initialization
phases, unknown loads, exact budgets and malformed-IR type/owner joins. Preserve
all previous executable values/counts, global-only APIs and fresh replay.
Full tests, CLI, formatting/generated/build/install, 82 checksums, exact corpus,
independent review and all five exact-head CI checks precede protected merging.
General memory/runtime, stateful #exe, optimizer parity, native/BIN/loader and
bootstrap remain required beyond this increment.

Review clarification: IC_ADDR checks static ownership at its own consumer instruction, so canonical metadata cannot be borrowed beyond an initializer region. Materialized references remain transferable. Dynamic lvalues use a distinct Indirect_address node with pointer-to-result type checking, preserving normal Alias type equality. Test pointer parameter reassignment separately from pointee mutation.
