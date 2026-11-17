# Native `lastclass` defaults

An omitted `lastclass` argument uses the forwarded base class of the last
supplied fixed argument in that call. Ordinary omitted defaults leave that
selection alone. An explicitly supplied value in a later slot replaces it.

```c
I64 Inspect(I64 value,U8 *name=lastclass,I64 ordinary=11,
            U8 *again=lastclass)
{
  return name[3]+again[3]+ordinary;
}

Inspect(7,,,);
```

The supplied integer has class `I64i`. Both omitted names therefore contain
`I64i`, and this example returns `221`. The ordinary default saves eight bytes
at declaration time. Neither `lastclass` marker executes a declaration
expression or saves an expression payload.

Run the maintained example from the repository root:

```powershell
opam exec -- dune exec --root . bin/holyc.exe -- run --target=host-jit-task --mode=jit examples/native-lastclass-defaults.hc
opam exec -- dune exec --root . bin/holyc.exe -- run --target=host-jit-task --mode=aot examples/native-lastclass-defaults.hc
```

The isolated `host-jit` entry also accepts this example in both source modes.

## Source behavior

The authority is TempleOS commit
`c26482bb6ad3f80106d28504ec5db3c6a360732c`.
[PrsVar.HC](../third_party/TempleOS/Compiler/PrsVar.HC), lines 629 through 656,
sets the `MLF_LASTCLASS` and `MLF_DFT_AVAILABLE` flags without compiling or
calling a default expression. The member record starts with a zero payload;
see [LexLib.HC](../third_party/TempleOS/Compiler/LexLib.HC), lines 150 through
154.

[PrsExp.HC](../third_party/TempleOS/Compiler/PrsExp.HC), lines 455 through 484,
updates `last_class` only after a supplied argument. It forwards the class,
removes pointer stars and reads the base class name. This gives `I64` for an
`I64 *` argument, `I64i` for an ordinary `I64` value, and the original aggregate
name for an aggregate pointer. The declaration's formal argument type does
not replace the class of the supplied expression.

An array expression retains its decayed pointer class before forwarding, so
an `I64` array supplies `I64`, while an explicit `I64i *` supplies `I64i`.
String and floating literals use the records selected by
[AsmInit.HC](../third_party/TempleOS/Compiler/AsmInit.HC), lines 197 through
206, from [CInit.HC](../third_party/TempleOS/Compiler/CInit.HC): `U8` and
`F64`. Those records remain distinct from explicit `U8i` and `F64i` types.
Grouping and arithmetic retain the selected original expression class.

The explicit F64 class cases preserve the original binary64 word. A native
F64-to-F64 HolyC cast accepts a literal, loaded value, or materialized call
result. It adds no numeric conversion or extended x87 link; casts around an
extended arithmetic producer still require separate source proof.

The source modes have different storage behavior:

| Mode | Original emission | Hosted storage |
| --- | --- | --- |
| JIT | `IC_IMM_I64` with the selected member class and the original class-name address | One owned name region per forwarded class in the native task or image |
| AOT | `IC_STR_CONST` with internal `U8 *`, followed by `StrNew` | A separate owned string region for each omitted occurrence |

JIT occurrences share name storage even when they occur in separate commands
or retained function bodies. Writes through one name reference remain visible
through later references to the same class. AOT occurrences have independent
bytes. Native references retain their normal bounds and lifetime checks.

Compiler lookup metadata currently keeps its original names. For example,
changing the first byte of an `I64i` name to `Z` changes later native references
to that shared name, but a later explicit `I64i` declaration still finds the
original hosted compiler entry. TempleOS instead exposes the class record's
own `str` bytes, and [KHashA.HC](../third_party/TempleOS/Kernel/KHashA.HC),
lines 53 through 78, compares those bytes during lookup. Synchronizing name
writes with compiler hash entries belongs to the remaining compiler-state
mutation work tracked by #684. Native byte sharing alone does not provide
that behavior.

The native literal quota counts the bytes including the terminating zero. Two
JIT omissions for `I64i` share five bytes; two AOT omissions require ten.
`max_default_bytes` does not charge a copied declaration payload for
`lastclass`.

## Call and ownership checks

Named calls, anonymous callback headers and implicit output retain their
original typed substitution. The call lowerer emits from that receipt, and
the runtime call context independently checks the selected parameter, mode,
producer class, source span and exact name bytes. Literal admission uses the
sealed original instruction graph. Replacing a byte payload or presenting a
copied graph cannot obtain the original call's storage authority.

Historical function bodies keep the headers selected when those bodies were
compiled. Evaluating the preceding argument still happens once in the
ordinary call sequence. A subsequent call failure preserves any output
already reached.

## Coverage and remaining boundaries

`test/native/test_native_lastclass_defaults.ml` covers forwarded primitive and
aggregate names, omitted slots between supplied arguments, anonymous
callbacks, implicit output, historical calls, JIT sharing, AOT copies, literal
limits, effects and failures. The public CLI cases execute the maintained
example in both source modes and both native entry targets. Unused integer,
pointer and F64 `lastclass` declarations require no preparation execution.

A JIT omission with no preceding supplied argument keeps the zero record
word. Scalar integer parameters receive zero, and an F64 parameter retains
the positive-zero bit pattern. The F64 case uses the exact original zero
producer; it does not admit other integer immediates as floating values.
Arithmetic performed by the callee still requires the ordinary x87 capability.
Data-pointer parameters receive
a null reference with zero data address, extent, offset and lifetime epoch.
It allocates no literal region and grants no object access. Passing or
returning that reference preserves the zero descriptor; a reached load still
reports the existing `HCIRVM0019` bounds error and consumes its source
instruction. Tests check the exact reached instruction allowance and the
one-instruction-below failure. An ordinary one-star callback parameter can
receive the zero word with no executable owner. A reached call through it
preserves argument effects and reports `HCIRVM0024`. Interpreting a nonzero
class-name address as an integer or F64 value remains separate native ABI work.

Callback defaults keep the physical pointer class chosen by
[PrsVar.HC](../third_party/TempleOS/Compiler/PrsVar.HC), lines 350 through 356.
The callback's return type does not turn its zero pointer into an F64 value
or an owned data reference.

An AOT omission with no preceding supplied argument is rejected. The pinned
source calls `StrLen` on the zero record word before `StrNew`;
[Templates.HC](../third_party/TempleOS/Compiler/Templates.HC), lines 87 through
94, dereferences that address. This implementation does not substitute an
empty string for that unresolved target behavior. These native preparation
paths also do not provide the complete TempleOS object loader or bootstrap
environment.
