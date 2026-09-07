# Scalar static locals and constant initial images

Issue: #607. The executable gate is `I64 Next(){static I64 n=40;return ++n;}Next();Next();`, which must return 42 in JIT and AOT modes. This advances V3; nonconstant static initialization, general memory, stateful compilation, native execution, and bootstrap remain required by the full compiler goal.

## Source contract

At reference commit `c26482bb6ad3f80106d28504ec5db3c6a360732c`, `PrsVar.HC:534-588` allocates static storage while parsing the function body. JIT uses code-heap allocation with no guaranteed zero fill; AOT reserves a zero image. `PrsExp.HC:776-784` uses `IC_IMM_I64` or `IC_ABS_ADDR`, never an RBP slot. `PrsVar.HC:215-244` prepares the initializer through the common initializer path, whose constant/nonconstant decision is in `PrsVar.HC:53-112`.

Consequently, declarations in unused functions, false branches, and after returns still receive storage and constant preparation. There is no first-invocation guard. Static allocation ignores the globals-on-data-heap option, although future nonconstant AOT deferral must honor that option. `PrsStmt.HC:160-184` parses a body before publishing its code: future compile-time calls cannot assume the containing function is already executable. Actual parameter or automatic-local reads in a static initializer cannot borrow a future invocation frame. Pure metadata queries such as `sizeof(parameter)` may remain constant.

## Storage and interfaces

Extend `Integer_globals.t`, the immutable persistent image context, with distinct opaque static slots. Keep `slots`, `find`, and the mandatory `slot_record` global API unchanged. A common read-only storage-slot view gives the interpreter index, exact symbol, scalar type, address opcode, initial bits, and preparation steps for globals and statics. Static slots retain the exact function frame, location, initializer root, and compiler options. No fabricated global record and no public raw image setter is introduced.

Create static slots from checked typed functions and frames in source order, after ordinary global indices. Validate function/scope/item identity, local binding identity, scalar public I64/U64 shape, eight-byte allocation, absent frame slot, exact initializer owner and type, compilation mode, and declaration options. All statics, including unused ones, count against `max_global_bytes`. Invocation frames contain only parameters and automatic locals; accepting a static requires its exact persistent context and owner options.

Alternatives considered: a second VM memory subsystem would duplicate address and update handling; treating statics as globals would erase declaration ownership. The shared image with distinct slot kinds preserves existing consumer paths and evidence.

## Lowering and initialization

Reuse frame-bound identifier validation before producing a canonical static address through the persistent address lowerer. The existing load, store, compound-update and prefix/postfix instructions then apply unchanged. Function statement lowering consumes each retained static initializer but omits its per-call store.

Extend the existing bounded initializer preparation driver with a distinct static item. Both kinds use the same original-opcode barrier classification, arithmetic guard, VM constant evaluator, diagnostics and aggregate preparation budget. Static RHS graphs may use their declaring frame for semantic lowering, but any remaining nonconstant graph is rejected with an explicit phase boundary. Constant bits and executed steps are published only through the internal checked pipeline. Global scheduled regions retain their current API. Preparation accounting includes static images and requires the matching immutable image/entry context on replay.

## Verification and next connection

VM address preflight retains the active exact frame and rejects another
function's static symbol or any static address in the entry graph. Preparation
merges globals and statics by semantic item/declarator order, independently of
storage indices. A static-only constant image still requires matching replay
evidence and reports preparation steps. The existing frame-aware preparation
harness takes four steps for a literal; the global harness takes three.
The constructor checks retained roots; initializer completeness is proved by
the later source-driver join consuming every AST initializer.

Tests cover persistent counters, recursion, nested calls, same-name ownership, fresh runs, unsigned arithmetic, uninitialized JIT/AOT behavior, unreachable and unused declarations, constant faults, preparation/storage/frame limits, nonconstant rejection, exact frame/context rejection, address opcodes, IR dumps, and the CLI fixture. Run focused tests red before implementation, then green, the full suite, format/generated/build/install checks, pinned checksums and corpus checks. Obtain independent review, all five CI checks at the exact PR head, and a normal protected merge.

The next connection is phase-aware nonconstant static initialization using the existing initializer scheduling path. Keep compile-time JIT, AOT load-time, declaration ordering, function publication and data-heap-option semantics explicit; do not replace them with first-call initialization.
