# Native scalar functions and U0 procedures

Issue #663 extends `run --target=host-jit` with all eight nonzero scalar integer
types and U0 procedures. It uses the existing source checker, integer lowerer,
call context, native encoder and execution bridge. Both JIT and AOT preprocessing
modes reach this path; neither mode produces a standalone object or TempleOS BIN.

```text
opam exec -- dune exec --root . -- bin/holyc.exe run --target=host-jit --format=json examples/native-scalar-functions.hc
opam exec -- dune exec --root . -- bin/holyc.exe run --target=host-jit --mode=aot --format=json examples/native-u0-functions.hc
```

The scalar fixture returns I64 42. The U0 fixture reaches a procedure call after
a prior 42-valued expression and therefore finishes successfully without a final
numeric value. The ordinary `run` target remains the checked interpreter.

## Declared storage and register values

Named fixed parameters, automatic locals and scalar returns admit I8, U8, I16,
U16, I32, U32, I64 and U64, including their checked intrinsic storage spellings.
The source type identity stays separate from the value's computation class.
Generated primitive metadata and `Ir.Integer_scalar_storage` determine width
and signedness; there is no additional native primitive registry.

| Declared object | Bytes read or written | Loaded word |
| --- | --- | --- |
| I8 / I16 / I32 | 1 / 2 / 4 | Sign-extended I64 |
| U8 / U16 / U32 | 1 / 2 / 4 | Zero-extended U64 |
| I64 / U64 | 8 | Full I64 / U64 word |

Each fixed parameter still occupies eight bytes in the private call convention.
Its object accesses use the declared width. Automatic locals occupy their exact
checked ranges within the padded semantic frame. Preflight verifies the complete
range, declared slot size and owner, including overlap with adjacent objects.
Hidden initialization flags, spills and call staging remain outside those ranges.

The encoder uses signed or unsigned extending byte/word loads, MOVSXD for signed
dword loads, and a dword MOV for unsigned loads. Stores select the exact byte,
word, dword or qword form. Narrow writes cannot overwrite an adjacent local.
Legacy qword frame operations retain their alignment checks when given an address
created through the new scalar-address API.

A returned expression retains its full register bits. The declared return width
does not introduce an extra storage conversion:

```c
I64 Read(I8 value) { return value; }
Read(255); // I64 -1 after the parameter object is read.
```

```c
I8 Wide() { return 255; }
Wide(); // I64 255: the returned register has not been stored in an I8 object.
```

Plain assignment and compound assignment similarly store the declared bytes but
return the full computed register value. Prefix updates return the normalized new
stored value; postfix updates return the normalized old value. For a U8 local
starting at 255, `x+=2` produces 257 while storing 1, `++x` produces and stores 0,
and `x++` produces 255 while storing 0. These examples each start with a fresh
local; they are not a sequence of updates to one object.

Compound division, remainder and right shift use the destination's computation
class. Public and intrinsic spellings retain their original class until the
appropriate forwarding or unary-fixup consumer. In particular, a public unsigned
function return and an unsigned local load need not undergo the same unary-minus
class transformation. [Narrow integer execution](integer-narrow.md) records those
source-derived distinctions and the separate initializer compatibility limits.
Native storage follows the same checked memory model; this is not a claim of
TempleOS register-allocation or optimizer parity.

## Saved defaults

The existing constant-only native preparation path now accepts the same eight
integer parameter types. A narrow default retains its full saved 64-bit word and
the exact declared type. For `U8 value=554`, preparation retains 554; a read of the
callee's U8 parameter yields 42. Preparation does not silently replace the saved
word with 42. Each successfully prepared value still consumes eight saved-payload
bytes, regardless of the declared object's width.

Default expressions execute once at their original declaration callbacks, even
for unused functions or calls with every argument supplied. Repeated and recursive
calls reuse the saved value. The original receipt, completed preparation, selected
and source-owned headers, parameter, immediate payload and complete native bundle
must still match exactly. A saved integer or reconstructed execution object alone
does not authorize native default use.

The expression, source-order and resource restrictions in
[native defaults](native-defaults.md) remain: only supported closed constant
preparation is admitted, default-bearing definitions precede executable entry
statements, and effectful/referenced/string/`lastclass` defaults reject. Reached
preparation work and completed saved bytes remain separate from native steps.

## U0 completion

A source-defined U0 procedure may fall through or execute a bare `return;`.
`U0` and `U0i` retain their exact checked type spelling while sharing the no-value
return kind. I0 and I0i are not admitted as substitutes. Procedures may call other
admitted functions or themselves through the same bounded direct-call path.

The backend keeps the checked U0 call-end identity separate from numeric values.
It does not stage RAX as a result, synthesize zero, or let an ignored callee word
satisfy its caller's required return. A completed procedure result may only reach
its checked discard. Arithmetic, conditions, stores and arguments cannot consume
it as a word. A U0 function containing a value-return instruction rejects.

The last reached top-level expression determines the program result:

```c
U0 Visit() { 99; return; }
42;
Visit(); // Successful completion with final_value: null.
```

Placing `42;` after `Visit();` instead yields I64 42. Discards inside Visit never
update the entry result. A reached top-level U0 discard clears the private value
site and bits together, so a prior numeric result cannot leak into the report.
The integer-program-v2 schema is unchanged.

Word-returning functions retain the existing whole-body preflight requirement:
every reachable terminal return must have a checked word value. Bare returns or
fallthrough without that value reject even when the definition is unused. This
is stronger than the interpreter's reached missing-return fault and remains an
explicit native admission restriction.

## Bounds, verification and remaining work

The existing instruction, code, per-owner frame, simultaneous semantic-frame,
call-depth and active native-stack bounds apply. Narrow objects do not reduce
eight-byte parameter charges or remove private initialization/staging costs.
U0 recursion is still metered and charged for every active generated frame.
Checked faults unwind the actual call chain and restore the shared counters;
the next invocation receives fresh state.

Ordinary tests check source admission, original type and call authority, encoder
bytes, exact compilation bounds and unsupported neighboring types without
executing machine code. The explicit native scalar API and CLI suites compare
independent expected values and fresh public interpreter sessions in both
preprocessing modes. Exact native runtime meters are checked against interpreter
execution of the corresponding isolated, checked source unit. A default activates
the ordinary JIT source task, whose separate declaration units also execute their
own `IC_END` instructions; that whole-source task meter is not the native batch
unit's meter. Tests retain both domains without subtracting an observed difference.
They cover storage normalization, adjacent slots, full register returns, updates,
saved defaults, void completion and fault/resource boundaries. CI runs
these through `@native-tests` on Windows and Linux alongside the existing native
and unwind suites.

The reference is TempleOS commit
`c26482bb6ad3f80106d28504ec5db3c6a360732c`. Relevant consumers include
`Compiler/BackLib.HC:281-309,509-534,550-572` for storage movement,
`Compiler/OptPass789A.HC:710-717,779-782,1026-1030` for update and return results,
`Compiler/PrsVar.HC:619-657` for eight-byte argument slots and saved defaults,
`Compiler/PrsExp.HC:438-586` for arguments/calls and their result classes, and
`Compiler/PrsStmt.HC:150-169,1110-1119` for function tails and return parsing.
Instruction forms come from the pinned `OpCodes.DD` and assembler REX/ModRM
consumers. These source audits and hosted executions are not new TempleOS oracle
captures.

Ordinary scalar globals/statics and one-level scalar pointer aliases are covered
by [native globals](native-globals.md) and [native pointers](native-pointers.md).
Automatic integer arrays are covered by [native arrays](native-arrays.md).
Global/static arrays, closed initializers and strings are covered by
[persistent storage](native-persistent-storage.md).
Broader pointer storage, automatic initialized arrays, aggregate values, Bool/I0/F64 storage,
variadic/indirect/external calls, prototypes, explicit register/function flags,
runtime output and general declaration/`#exe` execution remain outside this gate.
Optimizer parity, complete HolyC ABI, assembly/object/BIN output, actual loader
acceptance, whole-tree compilation and bootstrap remain required project work.
