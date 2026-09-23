# Native persistent arrays and strings

`holyc run --target=host-jit examples/native-persistent-arrays.hc` returns I64 42
in JIT and AOT source modes. The example combines an initialized I16 matrix, a
U8 array copied from a string, a function-static array, a mutable string object
and a fixed pointer parameter receiving a partial row. Native execution records
101 runtime instructions, 21 initializer steps and four dimension preparation
steps. Preparation does not run again when the generated function is called.
The ordinary interpreter reports 23 initializer steps: its two static numeric
leaves each include a frame-exit jump that the native closed-expression
preparation path does not generate. Both paths retain their own exact budgets.

Global and static arrays support I8/U8/I16/U16/I32/U32/I64/U64 elements. Reads,
assignments and compound or prefix/postfix updates preserve the declared width
and the expression result rules of [native arrays](native-arrays.md). The original
dimensions and strides determine each byte offset. Bounds apply to the complete
declared object, including when a partial-row alias crosses a row boundary.
Static allocation padding and neighboring objects are outside that extent.

## Initial state and source preparation

Persistent cells without an initializer start unknown in JIT mode and zeroed in
AOT mode. A successful write marks only its destination cell initialized. Reads
and updates of an unknown cell report `HCIRVM0012`. This unknown-state check is
hosted policy; it does not describe the contents of a TempleOS allocation.

Numeric initializer leaves and byte-string copies retain their original parser
owner, declaration, order and destination. Static preparation runs as each leaf
is consumed, before the parser accepts a later comma or closing delimiter. An
earlier leaf's work remains in the failure report when a later leaf, quota or
delimiter check fails. An incomplete declaration cannot publish an image.

The live and completed array layouts use the same dimension and brace-elision
rules. A string at a byte-array rank copies that rank's fixed count, as
`PrsVarInit2` does. At the final rank this fills the row; at an outer rank it can
leave part of the complete object untouched. Those cells retain their original
initialization state. The source-owned bytes include one terminator; a copy
that would read beyond them rejects. A shorter destination does not gain an
extra terminator or synthesized fill. Numeric leaves narrow only when written
to their declared destination cells.

The native preparation certificate binds the completed leaves to their exact
roots, declarations, entry and function bundle. Missing, duplicate, reordered or
foreign evidence rejects. Caller-provided bits and a new equal-looking syntax
tree cannot supply that certificate. The completed compilation imports the
prepared payloads without evaluating the leaves again.

Prepared JIT publications retain the complete lowerer's original receipt and
source-root order. Native image preparation requires every publication to occur
before the first instruction of the entry block. A publication after earlier
entry work cannot be moved into the initial image. Scheduled initialization
regions remain outside this path.

Function-static arrays share one object across calls and recursion. Distinct
function owners retain separate objects even when their local names match.
Each native execution allocates a fresh arena from the sealed image, so writes
from a previous successful or faulting execution do not persist into the next.

## Mutable literal objects

Each original `IC_STR_CONST` producer owns a mutable U8 byte region with its
trailing NUL. Embedded NULs remain part of the byte sequence; they do not shorten
its extent. Adjacent literal tokens follow the parser's existing concatenation.
Two producers with equal text have separate regions. Reaching the same producer
again within one execution reuses that producer's object, including through a
recursive call.

Literal references can be saved in automatic U8 pointer locals and passed to
fixed U8 pointer parameters. Indexed references retain their original extent
and element offset. The terminator is an ordinary writable byte within that
extent; the following address is one-past and cannot be read or written. Pointer
returns, persistent pointer variables, unrelated pointee conversions and general
pointer arithmetic remain separate compiler work.

Literal reference records live in the arena. They do not consume one automatic
frame record per literal byte. An element or one-past offset selects a stable
32-byte record, so reusing a literal producer cannot retarget an earlier alias.
Literal bytes are fully initialized and do not need per-element unknown flags.

## Limits and reports

`--global-byte-limit` / `max_global_bytes` counts declared global storage and
the existing padded static allocations. `--literal-byte-limit` /
`max_literal_bytes` counts literal bytes, including terminators. Each defaults
to 1,048,576 and accepts positive values through 16,777,216. Unused declarations
and unreachable original literal producers still consume their storage quota.

Private metadata is separate from both data counts. Scalar objects retain their
one-byte initialization flags. Arrays retain the object-prefix position and add
one eight-byte initialization slot per element. Literal regions add one 32-byte
reference record per byte plus one for one-past. The complete arena is capped at
33,554,432 bytes; layouts check this bound before expanding flags or records.
Array references materialized by a function still count their canonical tables
against that function's physical-frame quota.

The public executor checks the logical data limits and the exact sum of global,
literal and metadata bytes. The C bridge repeats those checks before allocating
the execution arena, which is writable and non-executable. Exported image bytes
are copies. Generated code retains the existing RW-to-RX transition, unwind
registration and fault cleanup.

Native JSON reports expose `native.image.global_bytes`, `literal_bytes`,
`arena_metadata_bytes` and the retained total `global_arena_bytes`. Preparation
work remains in `compiled_initializer_steps`; dimension work remains in
`dimension_preparation_work`. Initializer array payloads consume the global data
quota, rather than the separate saved-parameter-default byte quota.

## Evidence and remaining boundaries

The pinned TempleOS revision is `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
`Compiler/PrsVar.HC` supplies fixed-array initializer recursion, byte copies and
static allocation; `PrsExp.HC:1055-1098` supplies index strides and offset work.
Literal ownership and lengths follow `PrsExp.HC:692-704`,
`LexLib.HC:248-275` and `OptPass789A.HC:1098-1106`. The backend consumes the
shared checked representation and adds bounded hosted references.

Tests exercise both source modes and both host ABI images, independent expected
values, checked interpreter comparisons, source/API/CLI failure work, owner
mismatches, mutable aliases, embedded NULs, recursive sharing and fresh images.
They also cover exact and one-below limits, inaccessible padding and malformed
bridge inputs before native entry. These are hosted tests; they do not add a
TempleOS execution capture.

Automatic array initializers, runtime-dependent extents, pointer or aggregate
elements, effectful initializer scheduling and retained task storage remain
outside this native path. A native `Print` provider is separate runtime work,
so the interpreter's broader `integer-persistent-arrays.hc` example is not a
native acceptance claim. Full ABI behavior, object/BIN emission, actual loader
acceptance and bootstrap remain open compiler requirements.
