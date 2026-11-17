# References in zero-byte initializers

Live JIT global and static initializers can now evaluate supported reference
values for a zero-byte leaf. This includes function addresses, callback words,
integer and class addresses, array decay, string addresses and returned data
references. The expression still runs. The destination receives no address bits,
source bytes or initialization flags.

```c
I64 target=7,count=0;
I64 *Get() { count++; return &target; }
class Box { U0 marker; I64 value; };
Box object={Get(),42};
```

The initializer calls `Get` once and leaves `object.value` equal to 42. A static
initializer does the same work while its function is declared, even if that
function is never called. Later calls do not repeat it.

`examples/zero-byte-reference-initializers.hc` returns 42 with two declaration-time
calls. Run it with `holyc run --mode=jit --target=ir` or
`holyc run --mode=jit --target=host-jit-task`.

## Evaluation and completion

The original typed expression, leaf, declared object, allocation and parser
attempt must agree. Reference leaves use the existing expression evaluator and
address-ownership checks. A source integer cast does not create a data reference,
and discarding a numeric callback word does not make it callable.

An owned reference produces no scalar word in the initializer result. Native
execution retains the actual checked no-value site; it does not substitute zero
or expose a machine address as an integer. Numeric callback words retain their
ordinary numeric result but acquire no executable ownership. Empty copies still
require their original once-only completion receipt and live arena. Wrong work,
copied programs or copy tokens, another arena and replay all reject.

All source effects remain observable. Uninitialized pointer reads still fail.
If a call completes a previously zero-sized class into nonzero storage, the
initializer rejects after the call's earlier effects rather than inventing a
word to copy. A zero-byte copy cannot initialize the following member. String
expressions still allocate their checked literal bytes, and expression and copy
work retain separate resource charges.

Callback values use their physical pointer class rather than their function's
nominal return type. Selecting a callback that returns U0 is not a U0 call and
does not produce `U0 Expression`.

## Source and verification

The reference is `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
`Compiler/PrsVar.HC:1-120` compiles and calls each scalar expression before
copying the reached byte count. Reference values do not take the floating-point
conversion branches. Lines 209-246 retain global and static traversal;
`PrsType` at lines 313-368 separates callback storage from return signatures.

`test/oracle/zero-reference-initializers` records ten native JIT observations
twice. Tests compare the observed values and declaration-time call counts
through IR and native source execution. Shared cases cover supported integer
pointees, class and nested leaves, strings, indexed side effects, empty roots,
unknown neighboring storage and callee failures. Native authority checks compile
both ABIs and reject forged completion attempts after real native execution.

This change adds live JIT evaluation. AOT and isolated execution keep their
existing paths and boundaries; these captures do not establish original AOT
behavior or emitted-byte equality. General floating execution, numeric pointer
forging, nonzero copies without the required value or reference evidence, and
other callback families remain outside this feature. Issues #686 and #690 remain
open.
