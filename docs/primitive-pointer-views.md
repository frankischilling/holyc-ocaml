# Primitive pointer views in the interpreter

The interpreter accepts explicit postfix casts between owned, one-level
Bool, I8, U8, I16, U16, I32, U32, I64 and U64 pointers. A cast changes the
pointee width and signedness while retaining the original object, its extent,
current byte offset and lifetime. Array values decay through the existing
checked array-address path before the cast.

```c
I64 F() {
  U64 n=0;
  U8 *bytes=(&n)(U8*);
  bytes[1]=42;
  return n; // 10752
}
F();
```

`holyc run --target=ir` runs this source in both JIT and AOT mode. Views work
on parameters, automatic and static objects, globals, ordinary arrays, owned
byte literals and a source function's `argv` storage. Copies and calls retain
the same object. Indexing and pointer arithmetic scale by the view's width;
bounds checks still use the original byte extent. A wider view can begin at
an unaligned byte offset when its complete access window fits the object.
One-past views can be copied and compared without reading storage.

The maintained `examples/primitive-pointer-views.hc` prints `AB` and returns
42 in 233 runtime instructions, with no constant preparation. It succeeds
with 56 active frame bytes, call depth two, two output bytes and eight units
of output work; lowering any of these required limits fails at the reached
operation.

Reads and writes use little-endian bytes. A read applies the view's width and
signedness, and a store changes only the bytes covered by that view. Direct
reads of the original object, compound updates, prefix/postfix updates,
pointed bit operations, Swap and ModU64 observe those changes. Swap reads
both operands before writing the second and then the first, including
overlapping views.

Initialization belongs to the original bytes. Writing one byte into an
uninitialized U64 makes that byte readable; reading the U64 still reports
HCIRVM0012. Once all eight bytes have been written, the original word is
readable. A full ordinary store initializes the whole original cell. Partial
initialization and successful writes survive later task commands and reached
faults. AOT globals retain their existing zero-initialization behavior.

Print, its interpreter callback entries, stream formatting and StrLen can
scan an explicit U8 view of a wider or signed object. A terminator stops the
scan before later unknown bytes. Existing output, work, step, frame and
object limits still apply. A cast consumes an ordinary IR instruction and
does not allocate another object or widen the original extent.

The checked cast requires an existing data reference. Ordinary assignments
and parameters still use their existing pointer compatibility checks.
Integers and callback cells cannot gain data ownership through this path.
Function addresses keep their separate executable ownership. Returned
frames remain invalid, and bounds and initialization diagnostics identify
the reached instruction.

All source locations refer to TempleOS commit
`c26482bb6ad3f80106d28504ec5db3c6a360732c`:

- `Compiler/PrsExp.HC:970,1017-1055` changes the preceding operand class,
  clears array classification and appends IC_HOLYC_TYPECAST with the saved
  parenthesis state.
- `Compiler/OptPass012.HC:87-110` owns immediate folding and ungrouped class
  transfer; `Compiler/OptPass789A.HC:445-448` emits a retained cast as ICMov.
- `Compiler/BackLib.HC:693-707` selects the pointed-to load class, and
  `Compiler/BackC.HC:159-204` stores at the pointed-to width while retaining
  the assignment result.

The hosted interpreter keeps explicit cast instructions and checks object
ownership, initialization and extents. These guards are execution policy;
they are not a claim about TempleOS invalid-memory behavior or a new native
oracle capture. Maintained tests cover every admitted source/view class
pair, unaligned and overlapping windows, aliases, initialization, task
retention, providers, malformed IR and exact resource limits.

Native pointer casts remain unfinished. Its current canonical descriptor
tables and initialization flags assume the original element width. Native
preflight rejects these cast graphs before entry. Completing that path
requires a reference representation that retains original storage geometry
alongside the view and supports byte initialization without losing aliases.
Integer/null address conversions, pointer returns and escapes, deeper
indirection, F64 and aggregate views, exported ABI integration and the full
guest memory model remain requirements of the compiler.

The native task target can retain earlier successful declaration commands
before rejecting a later cast graph. It currently requires JIT source mode.
The CLI tests check both that retained progress and isolated native rejection.
