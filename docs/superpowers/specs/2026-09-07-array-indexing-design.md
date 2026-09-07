# Automatic scalar arrays and indexed lvalues

## Goal and evidence

Issue #613 follows merged #611/#612 at ffe1931fffd9cd6127c456b0c89d8f2c95423f95.
Its eight source gates currently fail with HCRUN0003 in both modes; all have
pinned-source expected final value 42. Connect them through the existing run
pipeline. The full compiler goal remains active, including general memory,
stateful compilation, optimizer parity, native/BIN/loader execution and bootstrap.

This is architectural work across semantic evidence, expression planning and
the existing storage interpreter. The user's continuing implementation and
protected-integration authorization applies; no additional approval is needed.

## Selected architecture

Retain exact index children, emit the source's canonical stride/index/add forms,
and extend existing references with declared-object extents. A detached array
evaluator would duplicate expression/call semantics. Accepting arbitrary pointer
arithmetic would lose the checked stride/rank boundary. A separate public index
execution context would duplicate evidence already present in checked frames and
canonical IR. The selected approach preserves the current public result APIs.

### Semantic evidence

Add `result_index_operands : expression_result -> (expression_result *
expression_result) option`. Only `type_index` supplies the exact checked base
and index value; other constructors default to None. Preserve origins, IDs,
rank, source type, lvalue/value category and intrinsic integer conversion.
Extend recursive expression consumers to visit these children. Callback/member
index typing keeps its existing result behavior; execution admits only the
audited ordinary scalar storage shapes.

### Checked lowering

Use an indexed-address plan separate from ordinary Alias and numeric Binary.
Validate child source identity against `index_base`/`index_value`, integer index
type/conversion, result rank/type/category and the exact root frame location.
Retain redundant integer Result_to_int evidence but visit the checked integer
producer with Keep_result: this is a no-op for admitted words, not permission
for floating execution or arbitrary graph-only conversion flags. Include
literal, arithmetic and direct-call indices in the tests.
Flatten a complete array bracket chain into its checked base and ordered
indices. Derive byte strides from the original checked dimensions and element
size. Emit base address, then stride constant, index, IC_MUL and IC_ADD for each
bracket; intermediate brackets never load a row. A pointer base uses an 8-byte
stride and its materialized runtime reference.

Use the same address path for loads, stores, updates and address-of. Complete
the destination before RHS effects and read its word at the update instruction.
Ordinary array-to-pointer materialization at any rank produces IC_ADDR over
the checked root or partial index address. The retained array operand keeps
its element type/rank. Grouping discards the dimension cursor and produces
an element-pointer Object_value of rank zero; subsequent grouping can alias
that pointer. Ordinary array dereference loads the element at every rank.
Exact ordinary-declarator evidence guards all three operations against callback
arrays. Explicit &a, including &a[1] while rank remains, adds another layer to
the implicit array pointer and stays outside one-level pointer execution.
Support pointer initialization, exact pointer assignment and fixed arguments,
including Set(a) and Set(&a[0]). Fully indexed &a[i] does not load the element.

### Frame and preflight evidence

Flatten admitted automatic I64/U64 arrays into element cells while retaining
each declared object's first cell, element count and ordered byte strides.
Scalar and pointer objects still have one cell. Named parameters remain scalar
or one-level pointers, so their initial slots retain source positions. All
elements count against the existing active-frame-byte limit. Canonical RBP
offset lookup identifies declared roots, not arbitrary interior cell offsets.
Check the total flattened cell count against Sys.max_array_length, and validate
host integer conversions, before constructing any element list or array.

Classify pointer-typed stride constants and IC_MUL results as index-offset
metadata. An IC_ADD can consume such an offset only with an exact array root,
the next checked intermediate array address, or a materialized scalar pointer.
The stride must equal the checked next dimension stride or pointer element
size. Carry the remaining rank; only fully indexed addresses admit memory
access. IC_ADDR also admits checked array-address materialization for value
contexts. Stride/scale metadata cannot be stored, pushed or returned.
Canonical frame address ticks and all old scalar counts remain unchanged.

### Runtime references and bounds

Extend runtime_address with object base/count and an int64 byte offset relative
to that object. Copying or passing a pointer retains these values and the actual
storage instance. Compute index scale/add in checked signed int64 arithmetic;
reject unsigned indices above signed int64 range and multiplication/addition
overflow with an explicit hosted address diagnostic. Never convert to a host
array index before proving the final offset is aligned and within the object.

Intermediate array addresses can be outside the object while later indices
bring the final offset back inside: a[0][3] aliases a[1][0], and a[2][-1]
reaches a[1][2] in a[2][3]. Address materialization and pointer copies/calls allow
aligned offsets from zero through one-past inclusive. Memory operations require
an offset strictly before the end. Thus p=&a[1];p[-1] accesses a[0], while a
scalar pointer's extent remains one word. Unknown elements use HCIRVM0012;
invalid/lifetime references retain HCIRVM0018; object bounds and address overflow
receive distinct explicit diagnostics with normal instruction/owner/phase
provenance. Returned-frame invalidation and fresh execution images remain.

These bounds/overflow checks are hosted restrictions, not native TempleOS
invalid-pointer behavior. Pointer-valued arrays, zero-sized array storage,
global/static array images, array initializers/whole-array assignment, floating
index execution and general pointer arithmetic remain
explicit boundaries until their source-grounded connections are implemented.

## Verification and integration

Run all eight gates red before implementation and green afterward in both
modes. Cover U64 bits, base/index/RHS effect order, one-time index evaluation,
Set(a), uninitialized-address creation, unwritten elements, cross-row and
intermediate-address behavior, one-past materialization, interior negative
indices, adjacent objects, overflow, copies/calls and recursive frames. Include
malformed child/type/layout/stride/rank/owner IR and semantic joins. Original
first-three limits are 16 bytes/depth1, 24 bytes/depth2 and 48 bytes/depth1.

Add a checked-in caller-element CLI fixture, determine exact instruction count,
then assert exact and one-below step/frame/depth limits and deterministic dumps.
Preserve all #611 and earlier executable values/counts. Run full tests, CLI,
formatting/generated/build/install, 82 checksums and exact lexer/parser
baselines. Obtain independent review and all five exact-head CI checks, merge
normally, verify equal source/merge trees and exact rebuilt versions, then
update issue/project/epic/desktop state.

## Pinned source

c26482bb6ad3f80106d28504ec5db3c6a360732c: PrsVar.HC:247-281,532,590-606;
PrsExp.HC:72,97-118,151-161,201-208,470-484,739-746,766-774,1055-1098;
PrsVar.HC:607-615; BackA.HC:555-566.
The preceding independent audit established strides, child retention, array
address behavior, final-object bounds and the eight expected values. No new
native capture is claimed.
