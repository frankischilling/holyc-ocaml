# Owned swap calls

Issue #773 connects the original numeric SwapI64, SwapU32, SwapU16 and SwapU8
declarations to IR, retained source tasks and hosted native execution. Each
call exchanges two live owned scalar cells and completes U0. A reached outer
swap clears the final expression value; it does not return numeric zero.

```text
holyc run --mode=jit --format=json examples/swap-internals.hc
holyc run --target=host-jit --mode=aot --format=json examples/swap-internals.hc
holyc run --mode=aot --format=json examples/stream-swap-internals.hc
```

The ordinary example exercises all four widths, a Bool/byte pair and a literal
pair. It captures `42:42:42:42:128:BA;` and returns I64 42. IR JIT execution
includes the original binding targets and declaration work: 274 runtime steps
and thirteen preparation steps. AOT and closed native execution use 256 runtime
steps and no preparation. Both use 41 formatting-work units. The retained
example prepares a default through an actual void swap,
checks its effects and reuses the saved result after later writes. Both IR
modes return I64 42 without captured output, using 118 runtime instructions,
12 preparation instructions and seven formatting-work units.

## Original calls and storage

The numeric targets are 0xA5, 0xA6, 0xA7 and 0xA8. Their exact canonical
signatures are respectively `U0(U8*,U8*)`, `U0(U16*,U16*)`,
`U0(U32*,U32*)` and `U0(I64*,I64*)`. Both arguments retain their original
producers, source types, origins and roles. The right pointer expression runs
before the left. Original call phases and immutable producer records authorize
execution; foreign contexts, copied or changed producers and altered call
shapes reject.
Renamed numeric declarations work. Ordinary same-name definitions execute
their own bodies.

Each pointer must retain a live owned integer/Bool cell with backing width
matching the selected operation. The formal pointer type does not replace the
original pointee type, width, extent, offset, initialization or lifetime. The
call may borrow scalar locals, caller objects, array cells, persistent cells
or literal bytes. Aliases keep the original storage; swapping a cell with
itself preserves its value. Signed and unsigned cells of equal width exchange
the same bits, then loads use each destination's original type. Bool retains
its signed one-byte backing.

After argument effects, execution checks and reads the left cell, then the
right cell, before writing either. Bounds precede initialization for each
read. One-past dereferences fault with HCIRVM0019; unknown pointers or cells
fault with HCIRVM0012. A failing left read prevents the right read. Reached
argument effects, output and attempted work remain visible. Successful calls
store the old left bits into the right cell, then the old right bits into the
left cell. Other cells remain unchanged. These exchanges are non-atomic.

Each operation consumes one IC tick and adds no callee activation. Native
code uses the existing checked byte, word, dword and qword loads and stores,
with two bounded private slots for the first address and value. Original
storage, code, frame, call, output and preparation limits remain checked.
Retained defaults execute the original mutation once. Native closed
internal-call preparation reports HCRUN0006; native retained frontend
execution reports HCPP0008 under #704.

## Source and verification

The reference is `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
`Kernel/KernelB.HH:110-117` supplies the four signatures and
`Compiler/CompilerA.HH:216-219` their numeric targets.
`Compiler/PrsExp.HC:440-586` supplies argument and internal call phases.
`Compiler/OptPass789A.HC:887-892` dispatches to `BackC.HC:490-537`, whose
`ICSwap` loads both operands before storing the second and then the first.
`OptPass012.HC:1231-1234` and `OptPass3.HC:455-458` distinguish runtime
handling from constant folding.

Compiler consumers include `BackA.HC:8-10,173-175`, `Asm.HC:157-158`,
`BackFA.HC:483-486`, `BackC.HC:283`, `BackB.HC:479` and
`PrsStmt.HC:705`. Tests exercise the compiler's three-word operand-swap
pattern. `Kernel/QSort.HC:21,46` supplies further callers whose callback and
general pointer prerequisites remain open.

Tests check 287 independent value cases across all nine integer/Bool storage
types, every bit at each width, aliases, arrays, literals, persistent cells,
recursive borrowing and effectful arguments. Seven source groups and six
native groups cover real void completion, signatures, original/foreign/
changed/copied authority, read-order faults, both ABIs, fresh images and
exact/one-below runtime, preparation, code and frame limits. Maintained CLI
fixtures check source values, capture, work, faults and once-only defaults.

This path requires complete matching-width scalar cells. Partial-object
reinterpretation, escaping/deeper/raw pointers and general pointer conversions
remain under #687/#699; aggregates under #700; optimizer and broader
preparation work under #696/#697/#685; broader runtime and concurrency under
#693/#695; native retained publication under #704; and full ABI, artifacts,
BIN/loader and bootstrap acceptance under #702/#682. Expectations come from
the pinned source and hosted execution. No new TempleOS oracle capture is
included.
