# Runtime layout expressions

Ordinary JIT array bounds and aggregate `$$` offsets execute while their original
parser callback is live. IR and `host-jit-task` support integer global, automatic
and static bounds, primitive class and union member bounds, direct and callback
calls, saved arguments, and supported internal calls. Their results set fixed
dimensions, strides and layout metadata. Function calls reuse those results.

```text
holyc run --mode=jit --report-version=2 --format=json examples/runtime-layout-expressions.hc
holyc run --target=host-jit-task --mode=jit --code-byte-limit=262144 --report-version=2 --format=json examples/runtime-layout-expressions.hc
```

Both commands print `dimoff` once and return 42. IR executes 54 instructions
with six initializer preparation steps. Native execution uses 69 instructions,
forty preparation steps, sixteen logical global bytes, fourteen output-work
units and 83,852 cumulative code bytes. The explicit code allowance covers
retained provider bodies emitted into several fragments.

The pinned `Compiler/PrsVar.HC:247-283` evaluates each bound before checking its
closing bracket and rejecting a negative count. `PrsVar.HC:408-449` evaluates
each `$$` expression before checking its semicolon. Class offsets replace the
current size; union offsets replace the union base. Negative offsets also record
the final padding. `PrsVar.HC:660-721` places each member using its original
dimensions and completes the layout. Reached effects, output and work survive a
negative bound, missing delimiter, invalid following member or later command
failure. These source audits and fresh hosted tests add no TempleOS capture.

Native bounds and offsets execute their actual machine entry with zero
interpreted instructions. Each request retains its original typed expression,
task, namespace, parser receipt and storage snapshot. The C bridge creates a
typed opaque capture only after successful native execution. Completion requires
the physical original program, actual work and live arena, and consumes the
capture once. Equal metadata, another image or arena, guessed work, foreign
domains, expiration and replay cannot authorize a layout result.

Checked member dimensions identify their original member, command, table,
namespace and predecessor chain. Intermediate aggregate sizes retain runtime
dimension and offset dependencies. Completed `sizeof` values, derived bounds
and automatic frames carry those dependencies too. Native source admission
checks them against the owning task's successful original executions. Standalone
callable compilation continues to reject runtime-derived frames.

Automatic bounds have no live function invocation frame while the declaration
is parsed. AOT runtime relocation, runtime floating bounds, aggregate object
execution, class declarations inside function bodies, native `#exe` and stream
services, exported ABI, loader acceptance and bootstrap remain separate work.
