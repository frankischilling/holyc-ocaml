# Native numeric initializers

Both native targets prepare closed integer/F64 initializer expressions in JIT
and AOT source modes. Globals, static locals, fixed arrays and packed aggregate
leaves retain their selected scalar width. Native JIT source tasks also convert
results from original retained expressions, while original nonconstant AOT
leaves execute as load-time assignments.

The [numeric initializer example](../examples/native-numeric-initializers.hc)
returns I64 `42`. Its packed object combines integer-to-F64 conversion, a narrow
integer copy and exact negative-zero bytes. A static array and a narrow static
integer keep their prepared values across repeated calls.

```sh
holyc run --target=host-jit --mode=aot examples/native-numeric-initializers.hc
```

The same example accepts `--target=host-jit-task` and either source mode.

## Supported phases

| Initializer source | `host-jit` JIT | `host-jit-task` JIT | Both native targets in AOT |
| --- | --- | --- | --- |
| Closed numeric expression | Declaration preparation | Declaration preparation | Declaration preparation |
| Original expression with retained references or calls | Outside isolated preparation | Declaration preparation | Original nonconstant load assignment |
| Integer/F64 conversion | After the closed expression returns | After the original expression returns | After a closed expression returns, or at the original load assignment |

Static preparation occurs while the function is declared, including functions
that are never called. Later calls reuse the stored bytes. AOT load assignments
run in source order with entry statements and include statics in unused
functions. Their expressions cannot read a future enclosing call's parameters
or automatic locals.

## Declaration conversion and copy

TempleOS's `PrsVarInit` compiles the original expression, calls its temporary
program and receives a full word. It then compares the expression's class with
the selected destination class before copying the destination's byte count.
The native preparation path retains that order:

1. Execute the checked expression with its original result class.
2. Convert the completed word when one side is F64 and the other is integer.
3. Copy only the selected leaf's bytes and complete its original destination.

An expression with floating operands can return an integer, as in
`I64 value=1.5<2.0;`. Its original floating work still executes natively, and the
captured integer result controls the subsequent declaration conversion.

For example, `U8 value=257.75;` converts the returned F64 to the integer word
`257`, then copies its low byte, `1`. `I8 value=255.75;` copies `0xff`, which a
later I8 read sign-extends to `-1`. Narrowing belongs to the copy and subsequent
read; it does not change the expression's captured class.

Integer-to-F64 declaration conversion reads the full returned word as signed
I64. Thus `F64 value=0xffffffffffffffff;` stores `-1.0`, even though the literal
has an unsigned expression class. Conversion rounds to the nearest binary64
value with ties to even. F64-to-integer conversion truncates toward zero; NaN,
infinity and out-of-range inputs produce the signed-minimum conversion word
before the selected low bytes are copied. Same-class F64 copies preserve all
eight payload bytes, including negative zero and signaling NaNs, without
an arithmetic conversion.

Each global or static scalar leaf keeps its own destination class. A packed
class containing both F64 and integer members does not impose its enclosing
class on those individual expressions. Fixed and multidimensional arrays use
the original sequential initializer cursor.

## AOT assignment boundary

`PrsVarInit` selects its separate nonconstant AOT branch when the original
expression sets `CCF_NOT_CONST` and globals are not on the data heap. That branch
records an `IET_MAIN` routine containing `IC_ASSIGN`. The native load path retains
the original expression, destination and assignment conversion flags.

These load assignments use the runtime assignment's floating plan. They do not
use the declaration-time completed-word conversion described above. Supported
mixed arithmetic therefore keeps its original conversion and materialization
points. Same-class F64 assignments still transport their exact eight bytes.
Scheduling an original load leaf charges no declaration preparation work;
executing its calls, conversions and stores consumes runtime instructions.

The precision regression uses `I64 value=Seed()+1.0;`, with `Seed` returning
binary64 `9007199254740992.0`. Live JIT declaration preparation first
materializes the expression as binary64 and saves `9007199254740992`. The
original nonconstant AOT assignment keeps the admitted arithmetic result
extended until integer conversion and saves `9007199254740993`. Global and
static cases check this distinction.

Only original AOT load leaves can follow an executable top-level statement in
isolated native compilation. A later closed constant or byte-copy initializer
still rejects before preparation. The existing `Globals_on_data_heap` global
restriction and static nonconstant preparation boundary also remain.

## Execution evidence and limits

A closed native preparation request retains its original parser source, typed
root, lowered graph and empty storage context. Its private executable image
produces an opaque capture containing the exact program, live image and actual
work. Completion consumes that capture once while the image remains live.
The capture's numeric class must match the expression, including when an integer
expression initializes an F64 destination. An integer-shaped report alone cannot
authorize floating execution or publish initializer bytes.

The call context must also retain the exact original typed result. A separately
retyped copy of the same source cannot replace it, even when that context matches
the same call-free graph. Capture checks reject copied programs, changed work
counts, foreign domains, replay and publication after image release.

Native preparation shares cumulative IR, emitted-code and instruction limits
with the surrounding native execution. Initializer work separately charges the
reached expression and destination copy width. A simple F64 scalar leaf needs
three expression instructions and eight copy bytes: an initializer allowance of
11 succeeds, while 10 fails before the copy. Prior completed work and reached
output remain visible after a later parser error, expression fault or exhausted
copy allowance. AOT load assignments retain their distinct runtime accounting.

## Source and checks

The source authority is TempleOS commit
`c26482bb6ad3f80106d28504ec5db3c6a360732c`:

- `Compiler/PrsVar.HC:53-112` selects declaration execution or an AOT load
  routine, converts the completed declaration word and copies `tmpc->size` bytes.
- `Compiler/PrsVar.HC:215-246` shares that traversal for statics and records
  their original load routines.
- `Compiler/OptPass012.HC:866-873` chooses the result conversion flags for the
  separate `IC_ASSIGN` path.

`test/native/test_native_numeric_initializers.ml` covers both source modes and
native targets, closed layouts, integer storage widths, signed full-word
conversion, rounding ties, retained numeric leaves, exceptional F64 payloads,
unused statics, later faults and exact work limits. The public CLI checks in
`test/test_native_f64_cli.ml` exercise the maintained example and load-assignment
conversions. Source-derived expected words and hosted execution provide this
evidence; it includes no new TempleOS runtime capture.

The integer interpreter still does not execute F64 expressions. General x87
normalization, isolated preparation with module data or callable dependencies,
inferred-size multi-pass initialization, wider pointer and callback value
families, exported ABI and loader acceptance retain their separate requirements.
The broader initializer, floating and native-session work remains open under
issues #690, #689 and #704.
