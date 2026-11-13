# Persistent U0 declarations

Scalar U0 globals and function-static locals retain zero-byte storage. They have
no scalar value cell or initialization flag. Uninitialized declarations work in
both outer modes through IR, source-task native and isolated native execution.
Arrays, reading a U0 object and taking its address remain separate boundaries.

Live JIT initializers evaluate their original expression once. Integer results,
U0 calls and supported references all complete an empty copy. Function-static
initializers run while the function is declared, including unused functions;
calling the function later does not repeat them.

```c
I64 calls=0;
U0 Effect() { calls++; }
U0 global=Effect();
I64 Result() {
  static U0 local=Effect();
  return calls+sizeof(global)+sizeof(local)+40;
}
Result();
```

This returns 42. Both U0 calls emit `U0 Expression` before execution. Integer and
reference expressions do not emit that warning. Empty destinations do not
initialize an adjacent integer or grant ownership to a numeric pointer.

## Storage and completion

The storage shape distinguishes a scalar U0 declaration from an empty class.
Its byte cursor uses the existing byte-addressed initializer path, but its
allocated byte count and scalar cell count are both zero. It retains its U0 type
and is never exposed as a U8 object. No default integer word is constructed for
zero-cell storage.

Each initializer still requires the original typed expression, destination,
source attempt and execution receipt. Native captures contain either the actual
integer result or checked non-scalar completion. Copying zero bytes still requires
the original live arena and can complete only once. The same authority tests
cover class leaves and scalar U0 globals, including copied programs, wrong work,
foreign arenas, cross-leaf reuse and replay.

## Reference and limits

The reference is `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
`Compiler/PrsVar.HC:1-120` evaluates each scalar initializer and copies the selected
class size. U0 has size zero. Global and static traversal remains in
`PrsVarInit2`; automatic initialized locals take the separate assignment/reparse
path and are not covered here.

`test/oracle/persistent-void` retains ten native JIT observations twice: sizes,
warning counts, global effects, static declaration-time effects and unchanged
target storage. Tests compare those observations through IR and native source
execution. Public-command checks cover all three executors, both outer modes,
exact and one-below limits, unknown neighbors and unsupported uses.

AOT scalar U0 statics accept supported closed integer initializers. The
expression runs while its declaration is parsed, even if the function is never
called. A fault or exhausted preparation limit stops later `#exe` blocks. The
result is discarded without creating a value cell or writing neighboring storage.
For example, `I64 F(){static U0 x=42;return sizeof(x)+42;}F();` returns 42.

AOT scalar U0 globals, nonconstant U0 static roots and the separate
globals-on-data-heap path remain unsupported. Isolated JIT retains its existing
closed-initializer restrictions. The AOT checks are source-based and hosted;
they do not establish emitted-byte equality or complete initializer parity.
Issues #686 and #690 remain open.
