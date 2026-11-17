# Numeric intrinsic defaults in ordinary source

Ordinary AOT source and isolated native compilation can prepare parameter
defaults that call an original literal `_intern` declaration for `IC_TO_F64` or
`IC_TO_I64`. The intrinsic executes in the default's temporary native image.
Its result then follows the formal parameter's full-word conversion and saved
default rules. Named functions and anonymous callback headers own separate
saved values, including unused functions and calls that supply explicit values.

Run the maintained example on Windows or Linux x86-64 with x87 and FISTTP:

```sh
holyc run --target=host-jit-task --mode=aot --format=json examples/aot-intrinsic-defaults.hc
holyc run --target=host-jit --mode=aot --format=json examples/aot-intrinsic-defaults.hc
```

The example declares both numeric intrinsics from their pinned opcode values.
`Base` saves `40.0`. The target saves `99.0`, while its callback header saves
`ToF64(ToI64(2.75))`, or `2.0`. The omitted callback argument uses that header's
value, so the final result is F64 `42.0`, bits `0x4045000000000000`. The three
defaults occupy 24 payload bytes.

## Source rules

The authority is TempleOS revision
`c26482bb6ad3f80106d28504ec5db3c6a360732c`:

- [`Compiler/CompilerA.HH:49-50`](https://github.com/cia-foundation/TempleOS/blob/c26482bb6ad3f80106d28504ec5db3c6a360732c/Compiler/CompilerA.HH#L49-L50)
  assigns `0x1B` and `0x1C` to the two conversion operations.
- [`Kernel/KernelB.HH:120-121`](https://github.com/cia-foundation/TempleOS/blob/c26482bb6ad3f80106d28504ec5db3c6a360732c/Kernel/KernelB.HH#L120-L121)
  declares their argument and result classes through `_intern`.
- [`Compiler/PrsExp.HC:545-571`](https://github.com/cia-foundation/TempleOS/blob/c26482bb6ad3f80106d28504ec5db3c6a360732c/Compiler/PrsExp.HC#L545-L571)
  emits an internal function's opcode directly. Ordinary, extern and import
  calls use their own address-bearing paths.
- [`Compiler/PrsVar.HC:637-653`](https://github.com/cia-foundation/TempleOS/blob/c26482bb6ad3f80106d28504ec5db3c6a360732c/Compiler/PrsVar.HC#L637-L653)
  executes the default expression, converts the returned word to the forwarded
  formal class, saves it and frees the temporary code.
- [`Compiler/OptPass012.HC:1004-1022`](https://github.com/cia-foundation/TempleOS/blob/c26482bb6ad3f80106d28504ec5db3c6a360732c/Compiler/OptPass012.HC#L1004-L1022)
  folds literal `ToF64` to an F64 constant. The `ToI64` pass at
  [lines 939-957](https://github.com/cia-foundation/TempleOS/blob/c26482bb6ad3f80106d28504ec5db3c6a360732c/Compiler/OptPass012.HC#L939-L957)
  consumes that rounded word. The regressions check
  `ToI64(ToF64(9007199254740993)) == 9007199254740992` and the opposite
  rounding direction for `9007199254740995`, which becomes `9007199254740996`.
- [`Compiler/OptPass3.HC:444-505`](https://github.com/cia-foundation/TempleOS/blob/c26482bb6ad3f80106d28504ec5db3c6a360732c/Compiler/OptPass3.HC#L444-L505)
  leaves explicit conversion operations outside floating link selection.
  [`Compiler/BackB.HC:277-286`](https://github.com/cia-foundation/TempleOS/blob/c26482bb6ad3f80106d28504ec5db3c6a360732c/Compiler/BackB.HC#L277-L286)
  performs them at the instruction boundary. Nested calls with unchanged
  numeric classes therefore pass a materialized word between conversions.

## Ownership and limits

Each intrinsic reference keeps its original parser selection and completed
source header. Source declarations from one original parser context reconcile
in order, retaining earlier versions when AOT joins a later declaration to the
same record. Each header keeps its original compiler options. The checked
call context proves the opcode, signature and original argument producers.
Every retained reference must be used as an authenticated numeric intrinsic
call. A familiar function name alone supplies no authority.

Native execution captures the result with the exact original program, mapped
image and reached work. The preparation callback consumes that capture before
releasing the image. Its saved word is single-use at publication; later omitted
arguments reuse the saved value. Faults and later syntax errors preserve
earlier completed defaults and reached work.

Preparation consumes the cumulative native instruction and initializer-step
allowances. Each successful scalar default consumes eight bytes of
`--default-byte-limit`. Temporary default images and the final module share the
code and IR limits. Closed integer defaults that contain no intrinsic call keep
their established preparation path and counts.

`test/native/test_native_source_intrinsic_defaults.ml` covers named and
anonymous defaults, macro aliases, source header replacement, non-trailing
omissions, explicit arguments, unused declarations, nested conversions, exact
limits and failures. It also checks the maintained example through both native
CLI targets.
`test/test_native_numeric_conversion_authority.ml` checks the original nested
producer and consumer receipts, including rejection after a copied or changed
instruction replaces the captured graph record.

This source preparation path accepts literal numeric `_intern` targets,
including integer literals produced by macro expansion. Earlier same-name
literal numeric internal headers and ordinary extern prototypes retain their
source history. Extern prototypes converted to imports need separate loader
authority. Nonliteral binding expressions, other binding histories,
intrinsic addresses, data references, ordinary function bodies and extern
calls require additional execution or relocation authority. They remain
outside this path. These restrictions do not complete the remaining ABI,
loader, bootstrap or full HolyC compatibility work.
