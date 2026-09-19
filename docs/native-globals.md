# Native scalar globals

`holyc run --target=host-jit examples/native-scalar-globals.hc` returns I64 42
in JIT and AOT preprocessing modes. Generated entry code and direct functions
share ordinary I8/U8/I16/U16/I32/U32/I64/U64 globals, including closed scalar
declaration initializers. `examples/native-scalar-initializers.hc` combines
prepared globals and a saved parameter default and returns 42 in both modes.
Loads, assignments, compound operations and prefix/postfix updates execute as
machine instructions. Calls, recursion, local shadowing, constant parameter
defaults and function-local gotos retain their existing checked semantics.

Declared widths determine stored bytes. Loads sign- or zero-extend those bytes;
assignment and compound expression results retain full register bits. Prefix
updates normalize narrow results and postfix updates return the previous value,
as in the checked interpreter. A right-hand call can change the destination
before a compound operation reads its old value.

## Initialization and ownership

Globals without an initializer start at zero in AOT mode. In JIT mode they start
unknown, and a reached read or update
before assignment reports `HCIRVM0012`. The JIT rule is the existing hosted
policy, not a claim about TempleOS allocator contents. Plain assignment sets
the initialization flag after storing. Skipped assignments do not initialize.
Every execution, including repeated executions of one image, receives fresh
storage. Checked faults unwind named calls and release the arena.

Closed integer initializers prepare at their original parser leaf, including
unused declarations. They use the same checked constant-preparation engine and
work budget as scalar defaults. The compiled unit reuses each prepared value
without reevaluating its expression. Declared widths normalize stored bytes;
each execution restores these bytes and their initialized flags.

`Native_global_initializers` binds successful preparation to the original
ordered leaves, symbols, types, initialization, entry and call bundle. Missing,
duplicate, reordered or foreign evidence rejects. Ordinary batch preparation
and caller-supplied initial bits cannot supply this certificate. Preparation
faults occur before native entry and retain earlier declaration work.

`X86_64_global_storage` seals the original initialization/entry bundle, exact
global symbol objects, declared types, slots and address opcodes. JIT uses the
checked symbolic `IC_IMM_I64` path; AOT uses `IC_ABS_ADDR`. Integer payloads and
foreign equal-spelling symbols cannot authorize arena accesses. Complete
preflight includes unused functions and unreachable storage instructions.

Storage-bearing images reserve R9 as the arena base, alongside R10's step
budget and R11's private runtime context. Generated code reads the arena pointer
from the context but cannot write that field. Entry programs without named
functions use the same storage-capable bridge. The older empty-storage context
and machine-image path retain their contracts.

The arena is separate RW, non-executable memory. Code still transitions from RW
to RX and Windows registers the checked unwind table before entry. The bridge
releases both mappings before boxing status. The exported initial image is a
copy. These are private hosted implementation details, not TempleOS addresses,
a complete HolyC ABI, or a module relocation format.

## Limits and reports

`--global-byte-limit` / `max_global_bytes` defaults to 1,048,576 declared bytes,
with a positive configuration range capped at 16,777,216. Unused objects count.
The compiler checks this bound before allocating the private image; execution
checks it again before native entry. One initialization byte per object is
charged separately in a private arena capped at 33,554,432 bytes. The private
packed layout does not expose padding or raw pointers.

Native v2 reports add `native.image.global_bytes` and `global_arena_bytes`.
Code, IR, block, spill, semantic frame, call-depth and active-stack limits remain
independent. Storage bookkeeping adds machine instructions, not extra IR steps.
Initializer work counts toward `--initializer-step-limit`, shared with defaults.
Global payloads count toward the global-byte quota, not `--default-byte-limit`.
The native API tests compare exact runtime meters with fresh isolated checked
interpreter execution; public source tests independently check values.

Effectful or call-dependent initializers, static locals, arrays, pointers, aggregates,
aliases, extern/import/data-heap storage and retained task storage remain
unsupported. Defaults still prepare closed numeric expressions in a separate
empty fragment at their original declaration boundary; admitting globals does
not authorize a default to read them. Full memory/runtime support, object/BIN
output, loader acceptance and bootstrap remain open.
Initializers must precede executable top-level statements. String-backed values,
floating values and unresolved initializer shift-optimizer behavior also reject.

## Source evidence and verification

The reference is `c26482bb6ad3f80106d28504ec5db3c6a360732c`:
`Compiler/PrsExp.HC:867-902` selects global addresses;
`Compiler/PrsStmt.HC:285-435` allocates and publishes global storage;
`Compiler/BackLib.HC:281-309,453-572` selects declared-width movements;
`Compiler/BackC.HC:159-204` separates assignment storage and result registers.
`Compiler/PrsVar.HC:1-115,206-212` distinguishes immediate initializer preparation
from AOT scheduling, converts its result and copies the declared width.

`test/test_native_expression.ml` checks literal arena instruction encodings.
`test/test_native_program.ml` checks exact ownership, corrupted address
producers, unsupported storage and immutable exports without native execution.
`test/native_initializer_authority` checks original leaf ownership, exact and
one-below preparation quotas, both ABI encodings and failure before entry.
`test/native/test_native_global_execution.ml` covers all widths, shared calls,
recursive faults, fresh images, updates and exact resource boundaries.
`test/native/test_native_global_cli.ml` exercises the maintained source fixture,
reports, both modes, unknown reads and rejected neighbors. These tests provide
hosted interpreter/native evidence; they add no TempleOS oracle capture.
