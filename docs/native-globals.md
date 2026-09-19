# Native scalar globals

`holyc run --target=host-jit examples/native-scalar-globals.hc` returns I64 42
in JIT and AOT preprocessing modes. Generated entry code and direct functions
share ordinary I8/U8/I16/U16/I32/U32/I64/U64 globals declared without initializers.
Loads, assignments, compound operations and prefix/postfix updates execute as
machine instructions. Calls, recursion, local shadowing, constant parameter
defaults and function-local gotos retain their existing checked semantics.

Declared widths determine stored bytes. Loads sign- or zero-extend those bytes;
assignment and compound expression results retain full register bits. Prefix
updates normalize narrow results and postfix updates return the previous value,
as in the checked interpreter. A right-hand call can change the destination
before a compound operation reads its old value.

## Initialization and ownership

AOT globals start at zero. JIT globals start unknown and a reached read or update
before assignment reports `HCIRVM0012`. The JIT rule is the existing hosted
policy, not a claim about TempleOS allocator contents. Plain assignment sets
the initialization flag after storing. Skipped assignments do not initialize.
Every execution, including repeated executions of one image, receives fresh
storage. Checked faults unwind named calls and release the arena.

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
The native API tests compare exact runtime meters with fresh isolated checked
interpreter execution; public source tests independently check values.

Declaration initializers, static locals, arrays, pointers, aggregates,
aliases, extern/import/data-heap storage and retained task storage remain
unsupported. Defaults still prepare closed numeric expressions in a separate
empty fragment at their original declaration boundary; admitting globals does
not authorize a default to read them. Full memory/runtime support, object/BIN
output, loader acceptance and bootstrap remain open.

## Source evidence and verification

The reference is `c26482bb6ad3f80106d28504ec5db3c6a360732c`:
`Compiler/PrsExp.HC:867-902` selects global addresses;
`Compiler/PrsStmt.HC:285-435` allocates and publishes global storage;
`Compiler/BackLib.HC:281-309,453-572` selects declared-width movements;
`Compiler/BackC.HC:159-204` separates assignment storage and result registers.

`test/test_native_expression.ml` checks literal arena instruction encodings.
`test/test_native_program.ml` checks exact ownership, corrupted address
producers, unsupported storage and immutable exports without native execution.
`test/native/test_native_global_execution.ml` covers all widths, shared calls,
recursive faults, fresh images, updates and exact resource boundaries.
`test/native/test_native_global_cli.ml` exercises the maintained source fixture,
reports, both modes, unknown reads and rejected neighbors. These tests provide
hosted interpreter/native evidence; they add no TempleOS oracle capture.
