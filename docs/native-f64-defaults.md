# Native F64 parameter defaults

The `host-jit-task` target executes supported F64 and mixed integer/F64
parameter defaults in the original live JIT task. It also prepares closed numeric
F64 defaults in ordinary AOT source before executing the outer module. Isolated
`host-jit` prepares admitted numeric F64 defaults using original native fragments
in both JIT and AOT modes. Named functions and anonymous callback headers
evaluate their original expressions when their declarations are parsed, even if
unused or every call supplies an argument. The resulting full 64-bit word
belongs to that header; an omitted argument later uses the saved word without
rerunning its expression. This extends the [native default contract](native-defaults.md)
under issue #685, alongside the related numeric work in #689.

Run the maintained example on a supported Windows or Linux x86-64 native host:

```sh
holyc run --target=host-jit-task --mode=jit --format=json examples/native-f64-defaults.hc
```

`examples/native-f64-defaults.hc` saves an I64 expression converted to F64 on a
named parameter, then saves that named function's F64 result in an anonymous
callback parameter. The callback uses the value saved by its anonymous header.
Separate tests check targets that declare a different default. The final value is F64
`3.75`, raw bits `0x400e000000000000`; the two successful defaults retain
16 payload bytes. Source tasks execute actual native fragments and report zero
interpreted task instructions.

For closed numeric F64 defaults, the maintained AOT example runs through either
native target and either source mode:

```sh
holyc run --target=host-jit-task --mode=aot --format=json examples/aot-native-defaults.hc
holyc run --target=host-jit --mode=aot --format=json examples/aot-native-defaults.hc
```

`examples/aot-native-defaults.hc` has named F64 defaults, an F64-to-I8 default,
and a selected anonymous callback with its own default. It returns F64 `5.25`
(`0x4015000000000000`) after preparing four original defaults, or 32 saved bytes.
The same example is accepted in JIT mode. A limit of 31 saved-default bytes
rejects the fourth default after nine preparation steps, retaining 24 saved
bytes. The exact allowance of 12 preparation steps succeeds; an allowance of
11 fails during the fourth default and retains the same 24 saved bytes.

The anonymous declaration saves `2.75` before the top-level statement
`Selected=&Target;` assigns the callback. This assignment runs after declaration
preparation in all four target/mode combinations. The initializer form
`F64 (*Selected)(F64 value=2.75)=&Target;` still reports `HCRUN0006` in isolated
JIT: it would need a live executable owner and completed initialization during
that parser callback. Moving it to final image entry would change reached work
and failure order when a later declaration fails. The example does not claim
support for that initializer path.

## The original declaration-time conversion

At pinned TempleOS revision
`c26482bb6ad3f80106d28504ec5db3c6a360732c`,
`Compiler/PrsVar.HC:619-656` compiles the original default with
`LexExpression2Bin` and stores the expression's returned word in `dft_val`.
It then follows the parameter's backing with `OptClassFwd` and converts that word.
For an F64 parameter, an integer expression is converted to binary64. For a
non-F64 parameter, an F64 expression is converted to integer. The
`MLF_DFT_AVAILABLE` flag marks the completed value as available.
`Compiler/PrsExp.HC:455-469` then supplies that saved value for an omitted fixed
argument, retaining its original parameter position and selected header.

The native preparation paths preserve the original typed expression program,
declaration attempt, source scope, retained image and native step count. The C native
entry creates an opaque scalar capture containing its actual completed word.
Completion can consume that capture once for its exact program, work, active
scope and live task arena or private image. Only after successful native execution does
`Default_fragment_destination.convert_native_word` apply the declared numeric
conversion. Completion independently converts the captured word and compares
the exact bits to the submitted saved value before publishing it. Reported bits,
a copied saved object, a different image, a replayed capture, or reported work
alone cannot establish execution authority.

## Numeric bits and initialized FPU state

The guest native entry initializes the x87 environment with `FNINIT`, saving and
restoring surrounding host state through `FXSAVE64`/`FXRSTOR64` on supported
builds. The initialized x87 control word is `0x037f`: all floating exceptions
masked, 64-bit extended significand precision, and round-to-nearest with ties
to even (RN-even). `Kernel/KStart64.HC:5-7` establishes the pinned original
`FNINIT`/`FXSAVE` path; `Compiler/BackLib.HC:277-280` lists the `FLD`, `FSTP`,
`FISTTP` and `FILD` instruction templates used by the compiler. The hosted
post-Call conversion operates directly on integer bit patterns to reproduce the
source-selected materialized word behavior; it does not silently round-trip
through an OCaml or host-language float.

An integer-to-F64 default uses signed 64-bit input, even when the source
producer is spelled `U64`. It rounds the binary64 significand RN-even: the
integer `9007199254740993` becomes the F64 word for `9007199254740992`, while
`9007199254740995` rounds to `9007199254740996`. An F64-to-integer default
truncates toward zero, so `42.75` becomes `42` and `-2.75` becomes `-2`.
Under the masked x87 invalid-conversion contract, NaN, infinity and
out-of-range F64 inputs yield the integer-indefinite word
`0x8000000000000000`; the valid negative integer `-2^63` has the same bit
pattern. Same-class F64 defaults simply retain the original eight bytes,
including negative zero and signaling-NaN payloads. Parameter entry then
performs the existing declared-width storage normalization, independently of
the full saved default word.

## Source modes, evidence and limits

Ordinary `host-jit-task --mode=jit` supports the admitted live named and
anonymous F64 defaults. In ordinary `--mode=aot`, supported closed numeric
defaults run during parser preparation through their own original native images,
before the outer AOT module is executed. Isolated `host-jit` uses that native
capture path for admitted F64 defaults in both preprocessing modes. Preparation
charges actual native execution steps, initializer work, saved payload, IR
instructions and code bytes cumulatively. It retains reached preparation and
saved values after later failures, including unused headers and anonymously
selected defaults.

A nested JIT `#exe` block retains live JIT behavior even when the enclosing
source mode is AOT. For example:

```c
#exe {
  F64 Nested(F64 value=1.5) {return value;}
  Nested();
  StreamPrint("42;");
}
```

The block generates an outer I64 result of `42` in both enclosing modes. Its
single completed F64 default consumes eight saved-payload bytes; a bound of
seven rejects before publication. Initializer-work and native execution-step
limits remain distinct, with actual reached work preserved when a later
default or call faults. Already committed output remains observable after a
later native failure. Historical callers keep their originally selected header
and saved value across later same-name definitions.

The maintained coverage consists of
`test/native/test_native_f64_defaults.ml` (public source-task and CLI results),
`test/native/test_native_source_authority.ml` (genuine captures, both private
ABI builds, substituted results/work and replay rejection), and
`test/test_binary64_conversion.ml` (independently specified bit vectors).
Tests exercise signed-zero, high-bit integer words, tie rounding, NaNs,
infinities, integer narrowing, unused declarations, explicit and omitted
arguments, nontrailing defaults, callback headers, nested `#exe`, quotas,
reached effects and failure boundaries. The expected bits are derived from
the pinned source and architectural conversion behavior; these hosted tests
are not fresh native TempleOS oracle captures.

The independent integer interpreter does not execute F64 defaults. Original
literal numeric `_intern` calls use the [source intrinsic path](aot-intrinsic-defaults.md)
without requiring an output address. Ordinary AOT still lacks the relocation
or preloaded executable authority needed for defaults that call ordinary
functions or reference storage in the not-yet-loaded output
module. For example, `I64 Seed(){return 41;} F64 F(F64 n=Seed()){return n;} F();`
requires that authority and remains rejected in ordinary AOT. The nested JIT
`#exe` path has its own live task and does not resolve this outer AOT dependency.
General pointer and aggregate conversions, variadic F64 arguments, and remaining
floating normalization or x87 linking questions have separate acceptance criteria.
[Native `lastclass` defaults](native-lastclass-defaults.md) retain their distinct
call-time class-name materialization. Existing string-producing defaults retain their
[data-pointer contract](data-pointer-defaults.md). Issue #685 remains open for
the full default semantics; #682 still requires exported HolyC ABI, object/BIN
loader acceptance and bootstrap support.
