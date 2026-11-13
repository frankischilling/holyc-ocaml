# Owned callback fields

One-level callback members in classes and unions support typed stores, copies
and calls through IR, isolated native programs and native source tasks in JIT
and AOT modes. Member arrays retain every declared dimension and require full
indexing before a call. Nested objects, inheritance, unaligned members, static
and global objects keep their original selected layouts and callback headers.

```holy-c
I64 Add(I64 n) { return n+2; }
class Box { I64 (*call)(I64 n=40); };
I64 Run() {
  Box box;
  box.call=&Add;
  return box.call();
}
Run();
```

The result is 42. The callee is loaded and saved before the arguments are
evaluated. An argument can replace the field without changing the callback
already selected for that call. Its original header supplies fixed defaults,
parameter transport, variadic count and cleanup. Integer and Bool returns and
U0 effects follow the existing callback execution paths. A stored body must
match the selected field's signature; a mismatch reports `HCIRVM0014`.

The visible field occupies eight bytes, independently of the callback's return
metadata. A separate ownership shadow retains the original executable body or
provider. The interpreter serializes a virtual code-address token; native
storage contains its code-address word. Reading those bytes as integers does
not transfer executable ownership. Any overlapping scalar or byte store
invalidates the shadow, including a write that restores identical bits. Typed
callback assignment can copy ownership from another field, a callback cell or
an original function address. Whole class-word transport copies the prefix
bytes without copying callback ownership.

Every byte must be initialized before a field load. Unknown bytes report
`HCIRVM0012`. A numeric word, restored address bytes or an invalidated shadow
cannot authorize a call; reaching it reports `HCIRVM0024`. Native field access
also checks the containing object's bounds and activation lifetime before
touching storage. Shadows share the original source-task arena and the existing
64 MiB native metadata payload limit.

Numeric callback fields support the existing [callback update rules](callback-updates.md).
Their increment is eight bytes, and `+=`/`-=` scale the right operand by eight.
An owned address remains opaque to numeric updates: the executor reports
`HCIRVM0024` after the right operand's effects and before changing the field.
Explicit full-word integer views can return numeric callback words through
either native runner; a dynamic ownership check rejects an owned address.

The shared fixtures check independent values for integer return widths, Bool,
U0, variadic calls, fixed defaults, arrays, inheritance, copies and argument
side effects. Existing provider fixtures also check stored PutChars and Print
entries, including an earlier provider after a joined definition. Fault cases
cover initialization, overlapping writes, restored
bits, incompatible signatures, opaque updates and out-of-object windows.
Proof controls and exact/one-below frame, instruction and image budgets
exercise the callback path alongside the existing aggregate suites.

Source evidence uses TempleOS commit
`c26482bb6ad3f80106d28504ec5db3c6a360732c`:

- `Compiler/PrsVar.HC:285-372`, complete `PrsType`, separates the anonymous
  function header from RT_PTR storage.
- `Compiler/PrsStmt.HC:62-140`, complete `PrsFunJoin`, retains return and
  parameter headers, offsets and cleanup flags.
- `Compiler/PrsExp.HC:960-1115`, complete `PrsUnaryModifier`, selects the member
  offset and callback header through member-array indexing.
- `Compiler/PrsExp.HC:383-591`, complete `PrsFunCall`, loads and saves the
  indirect callee before appending argument expressions and the indirect call.

The invalid-memory and executable-ownership checks are hosted policy. These
tests add no TempleOS runtime capture or machine-byte parity evidence. Extra
callback indirection, callback pointer returns, F64 execution and wider ABI
families keep their existing boundaries. Local callbacks still require a
declaration followed by assignment under the current parser boundary. Full
aggregate and compiler acceptance remain under #686 and #682.
