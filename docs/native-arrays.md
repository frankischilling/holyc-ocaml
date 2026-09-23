# Native automatic integer arrays

`run --target=host-jit` executes automatic arrays of I8, U8, I16, U16, I32,
U32, I64 and U64 in both source modes. Indexed reads, assignments, compound
assignments and prefix/postfix updates use each element's declared width.
Array decay and addresses of elements can be saved in automatic pointer locals
and passed to fixed pointer parameters.
Shared call validation admits all eight element widths while retaining the
original array rank, child expressions and pointee type. A decayed partial row
still carries the full root object's extent through the call.

```text
holyc run --target=host-jit --mode=jit examples/integer-arrays.hc
holyc run --target=host-jit --mode=aot examples/integer-arrays.hc
holyc run --target=host-jit examples/native-array-aliases.hc
```

These examples return I64 42. The separate `native-array-layout.hc` fixture
checks `sizeof`: its `I16 values[3][7]` occupies 42 object bytes. The original
frame layout aligns the complete object to eight bytes and places neighboring
locals without overlap. Object size, aligned semantic-frame size and private
native storage remain separate quantities.

## Evaluation and bounds

The base and index expressions run once, before the RHS. Compound assignment
retains that destination but reads its old value after RHS effects. For example,
`a[i++]+=Set(&a[0])` uses the original index even when the call changes the
selected element. Assignment and compound results retain the full computed
word; prefix updates return the normalized new element and postfix updates
return the old value.

Indexing follows the original dimension strides. Bounds apply to the full
declared object, so `a[0][3]` can name the same element as `a[1][0]` in an
`I64 a[2][3]`. An intermediate offset may leave the object and a later index
may bring it back. Each scale and addition still checks signed address overflow
at its own IR instruction. A high-bit U64 index is outside the hosted signed
address domain and faults before any host address calculation.

Final address materialization requires an aligned offset from zero through the
object's byte extent, inclusive. A one-past reference can be copied or passed to
a function; reading or writing through it faults. Negative indexing from an
interior or one-past reference can reach earlier elements because the reference
retains the full original extent. A scalar reference retains only its own scalar
extent and cannot reach a neighboring local.

Scale and address-addition overflow report `HCIRVM0020`. Final bounds failures
report `HCIRVM0019`. The compiled image records which instructions can raise
each fault, and status decoding rejects unrelated sites or a fault with no
consumed instruction. API reports retain the image, source span, function,
instruction and exact reached step count after an execution fault.

## Saved references and initialization

Every materialized reference selects a canonical record for one original
object element or its one-past address. The record carries the root data
address, root initialization-flag address, logical byte offset and full object
extent. It never changes to identify another element. Repeating `&a[i]` in a
loop therefore cannot retarget a previously saved alias when `i` changes.

Pointer loads, assignment results and call arguments preserve the selected
record. Arguments evaluate right to left: in `Use(p=&a[1],p)`, the right
argument retains the earlier value of `p` after the left argument rebinds it.
An indexed base similarly survives a pointer rebind in its index or RHS.
Canonical tables belong to the activation that owns the materialized object
reference. Recursive activations have separate tables; the accepted pointer
flow cannot return or persist a pointer to an expired activation. See
[native pointers](native-pointers.md) for the lifetime restriction.

Taking an address does not read the element. Each automatic array element has
its own initialization flag, initially clear in both source modes. A successful
write initializes only that element. Reads and compound updates require it to
be initialized and otherwise report `HCIRVM0012`. Every invocation gets fresh
automatic storage and flags, including repeated execution of the same image.

## Preparation and resource limits

Dimensions prepare once after expression lookahead and before closing-bracket
validation, including declarations in unused functions. The existing source
ledger retains each original owner, expression, predecessor and completion.
Calls reuse the prepared extents. `max_dimension_work` and the CLI's
`--dimension-work-limit` bound this work independently from initializer steps.
The positive default is 100,000. API `Native_program.dimension_work`, human
output and JSON `dimension_preparation_work` retain reached work even when a
later expression, delimiter, layout or compilation check fails.

The native frame validator requires the original function/body/frame join,
closed source dimensions, scalar integer elements, the complete object size,
source alignment and nonoverlapping frame ranges. It reserves one eight-byte
initialization flag per element. A materialized reference table reserves one
32-byte record per element plus one record for one-past. Both allocations are
checked against the physical-frame quota before metadata expansion. The number
of records depends on the compiled objects and address sites, never the number
of loop iterations.

Private flags, reference records, spills and call staging count toward native
frame and active-stack limits. Semantic-frame accounting still charges the
original aligned object layout. Descriptor work belongs to its original IR
instruction and adds no interpreter steps. Code, IR, block, call-depth,
preparation and persistent-arena limits remain independent.

Runtime-dependent extents, zero-sized arrays, automatic array initializers,
persistent arrays, pointer/aggregate elements and reference escapes remain
outside this path. General pointer arithmetic, the complete HolyC ABI,
floating-point code generation, object/BIN output, actual loader acceptance and
bootstrap remain separate compiler work.

## Source and tests

At pinned reference `c26482bb6ad3f80106d28504ec5db3c6a360732c`,
`Compiler/PrsVar.HC:247-281` prepares dimensions and products, and
`PrsVar.HC:590-606` allocates and aligns automatic storage.
`PrsExp.HC:1055-1098` supplies the stride, multiplication and addition sequence;
`PrsExp.HC:151-161,201-208` supplies address cancellation and lvalue handling.
`PrsExp.HC:470-489,538-554` collects arguments in source order and appends their
code contexts in reverse order for evaluation.
`BackA.HC:555-566` reads a compound destination after evaluating the RHS.
The native backend consumes the shared lowerer's original checked output.

Compile-only tests cover all eight integer widths, both host ABI images,
foreign or reconstructed metadata, physical-frame limits and unsupported
neighbors. Native API tests compare independent expected results with fresh
source and checked-batch interpreter runs. They cover indexed updates, saved
loop aliases, argument rebinding, flat offsets, one-past references, overflow,
per-element initialization, recursion and repeated images. Preparation tests
retain unused declarations, malformed closing delimiters and exact/one-below
work limits. CLI tests exercise both source modes and human/JSON reports.
The checks run under `dune runtest` and the explicit `@native-tests` alias.
The reference representation and bounded faults are hosted policy; no new
TempleOS execution capture is claimed.
