# Native automatic array preparation

Issue #681 is in progress. Native compilation now prepares closed automatic
integer array dimensions and allocates their checked frame extents. `sizeof`
can read the resulting size. Element addressing, indexing and array decay still
reject before native entry; the array alias acceptance programs remain open.

```text
holyc run --target=host-jit --mode=jit examples/native-array-layout.hc
holyc run --target=host-jit --mode=aot examples/native-array-layout.hc
```

The example returns I64 42 in both modes. Its `I16 values[3][7]` has 42 object
bytes. The original frame layout aligns the complete object to eight bytes,
then places the neighboring scalar without overlap. Object size, allocated
frame bytes and compiler-private state remain separate quantities.

Dimensions prepare once after expression lookahead and before closing-bracket
validation, including declarations in unused functions. The existing source
ledger retains each original owner, expression, predecessor and completion.
Calls reuse the prepared extents. `max_dimension_work` and the CLI's
`--dimension-work-limit` bound this work independently from initializer steps.
The positive default is 100,000. API `Native_program.dimension_work`, human
output and JSON `dimension_preparation_work` retain reached work even when a
later expression, delimiter, layout or compilation check fails.

The native frame validator requires the original function/body/frame join,
closed source-backed dimensions, scalar integer elements, the complete object
size, source alignment and nonoverlapping frame ranges. It reserves one qword
initialization flag per element and checks the total physical frame quota
before constructing that metadata. Every activation charges the full aligned
semantic frame and starts with fresh flags. Runtime-dependent dimensions,
pointer elements, zero-sized arrays, automatic initializers and persistent
arrays remain outside this path.

The next step is checked indexed addresses. An address-producing instruction
inside a loop must preserve every previously copied alias when its next index
changes. Intermediate flat offsets, final one-past bounds, per-element reads
and writes, overflow faults and passing array references need their own runtime
representation before admission. Scalar descriptors alone do not supply it.

## Source and tests

At pinned reference `c26482bb6ad3f80106d28504ec5db3c6a360732c`,
`Compiler/PrsVar.HC:247-281` prepares dimensions and products;
`PrsVar.HC:590-606` allocates and aligns automatic storage. The shared frame
layout and declaration ledger already implement these rules. Native admission
now consumes their checked output.

Compile-only tests cover all eight integer widths and both host ABI images,
physical-frame limits and unsupported neighbors. Native API tests compare
independent results with the interpreter, exercise recursion, repeated images,
unused declarations, malformed closing delimiters and exact/one-below work and
frame limits. The native CLI suite checks both modes and human/JSON reports.
These are hosted tests, not a new TempleOS oracle capture.
