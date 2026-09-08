# Scalar static locals

[Scalar U8 statics](integer-persistent-bytes.md) now use the same declaration
initialization and persistent lifetime as word statics. Each scalar allocation
charges eight padded bytes while a U8 reference has one accessible byte.

[Scalar pointer aliases](integer-pointers.md) can refer to existing static words
and be explicitly passed to callees. Materializing the reference requires the
declaring frame or its exact initializer region. Pointer-valued statics remain
outside this hosted storage domain.

`holyc run --target=ir examples/integer-statics.hc` calls `Next` twice and
returns 42 in 21 runtime instructions plus 4 preparation instructions in JIT
and AOT modes. Its `static I64 n=40` owns one persistent
eight-byte word. Repeated and recursive calls share that word; each separate
hosted execution starts from a fresh initial image. Static words support the
same loads, assignments, compound updates and prefix/postfix operations as
the existing scalar global executor.

## Definition-time preparation

Constant initializers prepare while compiling the program, including
declarations in unused functions, false branches and after a return. No store
or initialization guard runs at the declaration on each invocation. A constant
fault in an unused function therefore fails preparation. [Nonconstant initializers](integer-static-initializers.md) schedule persistent
reads, assignments and direct calls at their declaration phase. Actual reads of
containing parameters or automatic locals report HCRUN0006.
Pure metadata queries such as `sizeof(parameter)` are accepted.

The existing initializer driver classifies original value opcodes, applies the
same optimizer guard, and uses the bounded integer VM to prepare constants.
Global and static work is merged by semantic source item/declarator order,
independently of storage indices. Both share `--initializer-step-limit` and
the reported `compiled_initializer_steps`. A literal static initializer takes
four preparation instructions: literal, expression end, function-tail jump and
return. A literal global initializer takes three, ending with the stream marker.
These are hosted harness counts, not native TempleOS instruction counts.

`--global-byte-limit` bounds the combined global and static image, including
unused functions. `--frame-byte-limit` counts active parameters and automatic
locals; statics have no RBP slots. The counter runs with eight persistent bytes,
a frame limit of one byte, call depth one, and four preparation steps. Runtime
steps are bounded separately. AOT statics without initializers start at zero;
JIT statics retain the hosted unknown-fill boundary and reached reads report
HCIRVM0012 unless a preceding write supplies a value.

## Ownership and evidence

The immutable image retains exact static frame, location, symbol, scalar type,
initializer and compiler-option evidence. Canonical static addresses use JIT
`IC_IMM_I64` or AOT `IC_ABS_ADDR` with a symbol payload. Source lowering and VM
preflight require the declaring frame or its checked static initializer region.
Another function cannot use that static address, even if its exact symbol exists
in the image. Region authority never enters callees or escapes its bounds.
`execute_function`, which has no persistent image argument, still rejects
static frames. Static constant images require the exact checked initialization
context on direct program replay, even with no globals or scheduled regions.

The public `Ir_integer_globals.slots`, `find` and `slot_record` APIs still
describe globals. `statics` and the common read-only `storage_slots` view expose
static metadata separately. `byte_size` includes both. Initializer preparation
likewise keeps global `items`/`root` and exposes separate `static_items`.
Neither public API exposes a raw initial-bits setter. The private constructor
checks retained roots against their frames; the later source-driver join proves
completeness by consuming every AST initializer and requiring its exact root.
An absent root alone is not proof that the source declaration is uninitialized.
The image join also rejects numeric symbol-ID collisions across slot kinds.

At pinned reference commit `c26482bb6ad3f80106d28504ec5db3c6a360732c`,
`Compiler/PrsVar.HC:534-588` allocates static code-heap storage during body
parsing, using optional JIT fill and a zero AOT image. Allocation ignores the
globals-on-data-heap option. `Compiler/PrsExp.HC:776-784` selects static address
opcodes. `Compiler/PrsVar.HC:215-244,53-112` shares initializer preparation and
the constant/nonconstant decision. `Compiler/PrsStmt.HC:160-184` parses the body
before publishing its compiled entry.

## Remaining connections

Nonconstant AOT statics with globals-on-data-heap still require their separate
compile-time phase. The normal JIT/AOT declaration paths are documented in
[static initializers](integer-static-initializers.md). Parameter reads cannot
capture a future invocation.

Pointers, arrays, narrow/floating/aggregate storage, general runtime output,
stateful compilation, optimizer parity (#574/#585), native execution, BIN/loader
acceptance and bootstrap remain unfinished. This increment has hosted execution
tests and pinned-source evidence, without a new native TempleOS capture.
