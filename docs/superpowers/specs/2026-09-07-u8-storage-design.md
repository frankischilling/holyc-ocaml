# U8 storage and indexed byte access

Issue #615 extends the array and pointer execution path merged at
`6bc38f436fa9296ec9d438c7bf1e55fc2604b98d`. The issue's Sum fixture fails with
HCRUN0003 in both modes at that revision. Its source-derived final value is 42.
The continuing implementation authorization covers this change and its protected
integration. General storage, output, stateful compilation, native backends,
TempleOS modules and bootstrap remain part of the full compiler goal.

## Connection through the existing pipeline

Keep semantic types and checked frame layouts authoritative. They already record
U8 element size, dimensions, object extent and aligned local allocation. Extend
expression storage admission to ordinary automatic U8 scalars and arrays,
plain assignment, address materialization, exact U8* locals and fixed arguments.
Derive every array and pointer stride from the checked element type. Preserve
I64/U64 numerical parameters and returns, and the existing numeric index domain.

Use the current canonical address/load/store instructions and interpreter. Each
element retains an independent initialized cell, with a checked element width;
cell count is an implementation allocation measure, not target frame bytes.
Charge the exact checked frame allocation plus fixed parameter slots, including
alignment, throughout nested and recursive execution. Check host cell limits
before expanding arrays. Keep the public I64/U64 result API.

References retain the live invocation, original object base/count, element width,
exact pointee type and byte offset. Copies and fixed calls preserve all of them.
Validate width, type, owner and stride joins before effects. Bounds use original
object bytes; intermediate offsets may leave the object, materialization permits
one-past, and final memory access requires a complete in-object element. Checked
scale/add arithmetic rejects overflow. No reference can reach adjacent locals.

## Values and stores

U8 stores retain the low eight bits and loads zero-extend them. Ordinary
assignment results retain the computed RHS payload independently: `(a=298)`
produces 298 while a subsequent read of `a` produces 42. This follows
`Compiler/BackC.HC:159-204`, where the ordinary assignment result receives arg2,
and `BackLib.HC:453-572`, where memory width and register values differ.

Keep private byte computation classes distinct from I64/U64 promotion and
operation signedness. `OptLib.HC:96-179` selects the greatest raw class and
separately tracks unsigned operands. U8 arithmetic values must not be narrowed
until storage. Preserve existing raw interpreter versus optimized native-source
boundaries; the optimizer's by-value assignment path is separate evidence.

## Validation

Maintain the Sum source as `examples/integer-bytes.hc`, with public API and CLI
checks in JIT and AOT. Cover narrowing 256/298/-1, zero extension 128/255,
multidimensional and grouping strides, aliases and caller writes, ordering,
negative interior indexing, unknown bytes, object bounds, overflow, malformed
joins, recursive ownership, fresh images and exact budgets with one-below
failures. Preserve prior scalar/array/pointer/static/initializer instruction
counts. Serialize builds and executable use with stable source/test files.

Run focused, full and CLI tests, formatting, generated checks, build/install,
reference verification, provenance scenarios and both corpus comparisons.
Obtain independent review and all five final-commit CI checks before merge.
The reference remains `c26482bb6ad3f80106d28504ec5db3c6a360732c`; no new native
capture is claimed. Persistent byte storage, other narrow types, U8 updates,
pointer arithmetic/casts/returns and string/runtime output remain explicit
boundaries of this issue.
