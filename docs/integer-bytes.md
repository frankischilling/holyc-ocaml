# U8 storage and indexed byte access

[Persistent scalar bytes](integer-persistent-bytes.md) in #625 extend this
automatic-byte connection to globals and static locals. The automatic-only
scope below records #615; [persistent arrays](integer-persistent-arrays.md) and
[byte updates](integer-byte-updates.md) supply the later connections.

Issue [#615](https://github.com/frankischilling/holyc-ocaml/issues/615) extends
[automatic arrays](integer-arrays.md) and [pointer aliases](integer-pointers.md)
to one-byte automatic objects. The source fixture is
[examples/integer-bytes.hc](../examples/integer-bytes.hc):

```c
I64 Sum(U8 *p) { return p[0] + p[1]; }
I64 F()
{
    U8 a[3];
    a[0] = 40;
    a[1] = 2;
    a[2] = 0;
    return Sum(a);
}
F();
```

The fixture returns I64 42 in both modes, using 69 runtime instructions, zero
preparation instructions, 16 active frame bytes and call depth two. Run each
mode through the normal program path:

```text
holyc run --target=ir --mode=jit examples/integer-bytes.hc
holyc run --target=ir --mode=aot examples/integer-bytes.hc
```

At merged baseline `6bc38f436fa9296ec9d438c7bf1e55fc2604b98d`, both commands
reject the first `Sum` definition with HCRUN0003 and no successful stdout.
The source, public API and CLI regressions exercise the implemented connection.
The full local suite passes 1,811 tests, including nineteen byte groups.
Formatting, generated checks, build/install, 82 reference checksums and all
11 incremental provenance scenarios pass. The lexer retains 528/528 acceptance;
the parser report matches its baseline after Windows newline normalization.
Final-commit CI and integration evidence are recorded in #615 and PR #616.
No new native TempleOS capture is claimed.

## Values, types and storage

The byte path admits ordinary automatic U8 scalars and positive-sized arrays,
plain assignments, indexed reads, element addresses, and exact U8* automatic
locals and fixed pointer parameters. Numeric parameters and function returns
remain I64/U64, as do the public result tags and admitted numeric index types.

U8 memory stores keep the low eight bits, and loads zero-extend the stored byte.
Stores of 256, 298 and -1 therefore read back as 0, 42 and 255. Values of 128
and 255 remain positive after a load. Each element has an independent
initialization state; taking its address does not read it.

An ordinary assignment's expression result retains its computed RHS payload.
`(a=298)` produces 298 while a later U8 load of `a` produces 42. Arithmetic
intermediates keep their full 64-bit payload until a store narrows it.
The exact checked `Type.t` retained in the VM determines raw-class rank;
operation signedness is tracked separately from that rank. A byte class must
not be treated as U64 merely to reuse its runtime word tag. Private
`Stored_byte` storage narrows memory while the existing I64/U64 runtime words
and public result API remain unchanged. Native optimizer by-value assignment
and other recorded optimizer differences retain their separate compatibility
boundaries.

## Strides, aliases and bounds

U8 pointer indexing advances one byte. `U8 a[2][3]` has strides three and one,
and a six-byte object extent. Grouping, array decay and partial-row pointers
follow the [existing array rules](integer-arrays.md#strides-grouping-and-pointer-values).
Indices use flat offsets, so `a[0][3]` aliases `a[1][0]`. Intermediate brackets
produce an address without loading a row.

Every reference retains the original live invocation, object base and extent,
checked element width, exact pointee type and byte offset. Pointer copies and
fixed calls preserve these facts, including across recursion. An interior
pointer may index backward within the same object. A scalar byte has one byte
of extent and cannot grant access to a neighboring local.

The base and each index execute once before the assignment RHS. The destination
reference survives RHS effects, and the store checks final access bounds after
those effects. Intermediate offsets may leave the object; pointer
materialization admits zero through one-past inclusive; final loads and stores
require a complete element inside the original object. Checked scale/add
arithmetic rejects overflow. Unsigned indices beyond signed int64 range remain
outside the hosted address domain.

Reached unwritten bytes use HCIRVM0012, invalid or expired references use
HCIRVM0018, object-bound failures use HCIRVM0019, and checked address overflow
uses HCIRVM0020. Faults retain their instruction, source span, active function
and initializer phase where applicable. These are hosted diagnostics, not
native TempleOS invalid-pointer behavior.

Frame budgets count the exact checked local allocation, including alignment,
plus fixed parameter slots. Pointer and argument slots remain eight bytes.
Host element-cell counts are checked separately before allocation against
`Sys.max_array_length` and checked numeric conversions. Every call contributes
its exact frame charge under the shared active-frame and call-depth limits.
Returning invalidates that invocation; a new program execution creates fresh
storage images.

## Source evidence and validation

All source locations refer to TempleOS
`c26482bb6ad3f80106d28504ec5db3c6a360732c`:

- `Compiler/CInit.HC:3-14` defines U8 as one byte.
- `Compiler/PrsVar.HC:247-281,532,590-606` retains dimension products, complete
  object size and automatic allocation alignment.
- `Compiler/PrsExp.HC:72,97-118,151-161,739-746,766-774,1055-1098` establishes
  grouping, array addresses, element dereference and index scaling;
  `201-208,470-484` preserves assignment and fixed-call operand order.
- `Compiler/BackLib.HC:510-535,550-572` selects zero-extending byte loads and
  destination-width stores. `453-509` supplies the register-value path.
- `Compiler/BackC.HC:159-204` copies the RHS into the ordinary assignment
  result separately from the destination memory write.
- `Compiler/OptLib.HC:96-179` selects operand class and operation signedness;
  `Compiler/OptPass012.HC:866-896` preserves assignment conversion boundaries.

`test/test_integer_bytes.ml` and the program CLI suite cover the source
connection. The nineteen byte groups cover narrowing, zero extension, assignment
results, neighboring bytes, dimensions and grouping, caller writeback,
evaluation order, negative interior indices, unwritten bytes, bounds, overflow,
malformed width/type/stride joins, rejected frame escapes, recursive ownership,
fresh images and exact limits with one-below failures. Verification also
preserves the earlier I64/U64 execution results and instruction counts.
The PR records full-suite, corpus, review and CI results against the tested
revision. Graph-only execution retains its I64/U64 type and diagnostic contract;
the original byte slice requires a checked function frame. The later
[owned-string connection](integer-strings.md) also supplies a checked literal
context for program-entry byte computations.

## Remaining work

[U8 numeric parameters and returns](integer-byte-signatures.md) now execute.
Other narrow integer types, automatic
whole-array initialization or assignment, general pointer arithmetic, casts involving U8,
pointer returns and deeper pointers remain outside this slice. Unary minus
on U8 remains explicitly unsupported: `Compiler/OptPass012.HC:180-192` changes
its internal unsigned class to I8, whose computation rules are outside this
execution domain. Logical not and bitwise complement retain their checked
classes. Runtime lifetime checks remain present; accepted source cannot produce
an expired pointer because frame escapes are rejected.

Owned strings are connected in [#617](integer-strings.md), consuming #490's
canonical literal IR. [Captured runtime output](integer-output.md) now connects
checked Print/PutChars calls, including the supported formatting subset and
bounded variadic arguments. Full formatting/runtime linking, stateful compilation
and #exe, optimizer parity, native backends, TempleOS BIN/loader acceptance and
bootstrap remain requirements of the full compiler.
