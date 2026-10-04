# Numeric callback updates

One-star callback cells containing numeric words support prefix and postfix
`++`/`--` and the ten compound assignment operators through the IR and native
runners. This covers automatic and static cells, named callback parameters,
globals, and fully indexed callback arrays in JIT and AOT source modes.
`*callback` and `*callbacks[index]` select the same original cells as their
forms without the canceled star. Grouping under the star remains a boundary.

```holy-c
F64 (*callback)(I64 n);
callback=26;
I64 value=(callback+=2); // 42
```

The declaration's `F64` return type stays in its original callback header.
Storage is a separate eight-byte RT_PTR word. Increment and decrement change
that word by eight; `+=` and `-=` multiply their right operand by eight before
updating the cell. Other compound operators use signed 64-bit computation,
including division, remainder and right shift with a `U64` right operand.
Arithmetic wraps where the existing integer runner wraps; division faults keep
their existing diagnostics. Prefix expressions return the new word, and postfix
expressions return the old word.

An update result can feed ordinary integer arithmetic. Following `+` and `-`
retain the parser's eight-byte scaling, including across parentheses and chained
operations. The optimizer chooses the computation class separately: a `U64`
operand makes the resulting word unsigned. Later comparisons and right shifts
use that class. The callback header and original storage type remain intact.

The lowerer retains the original checked storage operand and source expression.
An index is evaluated once, followed by the compound right operand. The update
then checks the selected cell and reads its current word. A right operand that
changes the cell therefore affects the old value used by the compound operation.
Earlier output remains available after a bounds, uninitialized-read, arithmetic
or ownership fault. Successful numeric update results can initialize integer
locals or supply integer and callback arguments. Their bits do not authorize a
function call or an object reference.

## Source evidence

The reference revision is `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
`Compiler/PrsVar.HC:350-357` separates the callback return header from RT_PTR
storage. `Kernel/KernelA.HH:1572-1574` identifies RT_PTR with signed RT_I64.
`Compiler/PrsExp.HC:15-63` scales pointer addition and subtraction before the
compound instruction; `Compiler/BackB.HC:304-380` uses the pointee size for
prefix and postfix updates. `Compiler/OptPass012.HC:824-895` preserves the
left storage class for the remaining compound operations.
For arithmetic following an update, `PrsExp.HC:15-48,223-240` preserves the
source class used for scaling. `OptLib.HC:96-179` then chooses the common raw
computation class; `OptPass012.HC:485-486,619-620` applies it to addition and
subtraction. This distinction prevents a `U64` result from becoming signed just
because the original callback cell uses RT_PTR.

The existing `STORAGE` observation in
`test/oracle/callback-storage-and-calls.json` records an eight-byte callback
increment. That TempleOS fixture also performs arithmetic on a function address.
The numeric cases here use its storage rule and the pinned source; they do not
reproduce that complete address-arithmetic fixture or add a new oracle capture.

## Execution boundary

An owned function address still has no numeric arithmetic representation in the
hosted runner. Reaching an update on such a cell reports `HCIRVM0024` after the
right operand's effects and leaves the stored address and owner intact. Native
status 22 is accepted only at an original callback-update site with consumed
work. An arbitrary numeric result never acquires the private owner needed for
native invocation.

Callback-valued right operands retain the existing native arithmetic admission
boundary. Member storage, remaining dereferences, multistar callback consumers,
arithmetic on owned function addresses, live native task linking, and general
`F64` or aggregate execution remain unfinished under issues #801, #704 and #688.

`examples/callback-updates.hc` returns 42 through both runners and source modes.
The focused source and native suites cover storage shapes, return metadata,
scaling, signed operations, effects, faults, retained storage and exact runtime
limits. See [testing](testing.md) and
[native callback storage](native-local-callbacks.md).
