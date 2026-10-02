# Internal calls in retained source tasks

Retained `#exe` tasks execute the supported numeric `IC_TOUPPER` and `IC_STRLEN`
operations through their original task declarations. Both outer source modes
use the retained JIT task namespace:

```c
#exe {
  _intern 0x1e I64 ToUpper(U8 ch);
  StreamPrint("%d;",ToUpper('a'));
}
```

The generated expression returns I64 65. An outer AOT declaration alone does
not publish a function into this task namespace.

```text
holyc run --target=ir --mode=jit --format=json examples/stream-internal.hc
holyc run --target=ir --mode=aot --format=json examples/stream-internal.hc
```

The maintained example calls both operations, including ToUpper from a retained
function body, and generates a Print call. It captures `A:3`, returns I64 42 and
uses 55 output-work units. Cumulative execution uses 56 steps in JIT and 55 in
AOT, including task setup and generated code. One fewer step retains the output
and reports `HCIRVM0007` without a final result.

## Header and executable timing

The original `_intern` expression executes after expression lookahead and before
type validation or function publication. Supported integer expressions can use
parentheses, arithmetic, original queries, task globals and retained function
calls. Evaluation runs once and saves its numeric value. A later type or header
error preserves reached effects, output and charges. Lookahead inside the target
expression can affect the value before evaluation; header lookahead cannot
replace the saved value.

```c
#exe {
  I64 Count=0;
  I64 Target() { Count++; return 0x1e; }
  _intern Target() I64 Convert(U8 ch);
  StreamPrint("%d*100+%d;",Convert('a'),Count);
}
```

This generated expression returns I64 6501. The maintained
[effectful binding example](../examples/stream-internal-binding.hc) captures
`target:1;A:1` and returns I64 42. It captures 12 bytes with 75 output-work units,
uses three preparation units and executes 58 steps in JIT or 57 in AOT. Target
evaluation shares the task's preparation, execution, storage, output, frame and
call-depth limits. The program retains the private lowering result for its exact
typed expression, globals and executed graph. The owning VM registers the exact
successful result; equal source metadata cannot authorize another header or task.

The parser publishes an incomplete function before it reads the parameters.
That publication and each successful parameter/default event advance the exact
retained native allocation. A numeric target in the source does not yet grant
an internal executable: fresh incomplete headers retain their undefined extern
state through parameter and closing-parenthesis lookahead. Reached calls at
those phases report `HCIRVM0030`.

After the original header completes, its receipt installs the numeric operation,
sets the internal flag and clears the extern flag. Calls can then use the shared
checked intrinsic path, before the enclosing source command completes. Later
completion advances the same retained predecessor.

Nested headers can replace the shared member cursor and install an operation
while an outer header is suspended. The current member owner, immutable source
transcript and installed executable source are retained separately. An outer
parameter event preserves the nested installed operation; completing an outer
internal header replaces that target with its own original numeric binding.
An ordinary extern header preserves the nested internal installation. A header
started after the record becomes non-extern creates a fresh allocation.

Identifier selection keeps its original parser entry and admitted retained
publication. If nested headers have advanced the allocation, selection checks
the original native identity and forward revision history. Argument and emission
captures still sample the actual shared record at their own source events.
Copied metadata, foreign namespaces, reconstructed headers and replayed
transitions grant no authority.

## Operation and resource boundaries

Calls reuse the [full-word ASCII conversion](internal-toupper.md) and
[owned-byte length scan](internal-strlen.md) contracts. Their original numeric
binding and exact supported signature remain required. Names, later macros and
current header spelling cannot replace the installed target's source receipt.
The interpreter preserves existing instruction, storage, output and preparation
limits. Unsupported calls retain earlier task output and reached work.

Retained tasks accept supported scalar integer binding expressions through
their original evaluation and installation receipts. Floating target evaluation,
ordinary output-source binding expressions and native retained execution remain
unfinished. Other internal operations, default-bearing and
variadic signatures remain outside the execution gate. The retained compiler
rejects a source body on a shared record carrying a nested internal flag before
admitting that body. The native frontend
currently rejects active `#exe` with `HCPP0008`; native retained source execution
remains under #704. Broader compiler state and bindings remain under #684 and
#695.

## Source evidence

The reference is `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
`Compiler/PrsStmt.HC:62-146` joins the native header and finishes its argument
count after parameter parsing and lookahead. Lines 244-249 then assign the
numeric executable, set `Ff_INTERNAL` and clear `Cf_EXTERN`.
`Compiler/PrsExp.HC:440-586` separates argument processing from emission and
reads the selected record's internal flag and executable at emission.
`Compiler/PrsStmt.HC:1055-1061` evaluates the target before validating the type.
`Compiler/PrsExp.HC:1116-1154` compiles the original expression, executes it and
converts an F64 result to I64. The hosted retained integer path preserves those
boundaries; F64 execution remains under #701.

Issues #753 and #755 connect those phases to the retained task catalog. Tests cover both
outer modes, generated code, retained bodies, all byte inputs, shared and fresh
allocations, nested/default/closing lookahead, unsupported bindings, original
installation authority and exact cumulative budgets. These are source-derived
and hosted execution checks; this change adds no TempleOS oracle capture.
