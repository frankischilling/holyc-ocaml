# Class default words in source compilation

AOT IR and native JIT/AOT source compilation prepare closed class and union
defaults at their original declaration callbacks. Integer backings and the
ordinary signed `RT_PTR` class view use a full saved word. JIT IR defaults that
activate the retained task compiler still need separate class storage support.

```c
U16 class Packet { U16 low; U8 guard; };
I64 Take(Packet packet=0x07002a,I64 tail=0) {
  if (packet.guard!=7) return -1;
  return packet+tail;
}
Take();
```

This returns 42. The saved default contains all of `0x07002a`. Omitting the
argument initializes all eight bytes of its class parameter slot, including
the guard byte. Reading `packet` then selects its two-byte `U16` prefix. A
provided argument supplies its own word. Passing the loaded prefix to another
function carries 42 and leaves the new slot's higher bytes zero; it does not
copy the original class object.

The example is in `examples/class-default-words.hc`:

```text
holyc run --mode=aot examples/class-default-words.hc
holyc run --target=host-jit examples/class-default-words.hc
holyc run --mode=aot --target=host-jit examples/class-default-words.hc
```

All nine integer backings, unions, original named backing chains and
inheritance compose with these defaults. Multiple class and scalar defaults
retain their individual parameter positions. Empty classes still receive an
eight-byte argument slot. A class larger than eight bytes receives the same
slot, so its ninth byte cannot borrow storage from another argument.

The original selected class remains attached to each parameter and saved
default. A later class with the same name cannot replace it. AOT IR also checks
prototype replacement: a new header uses its own prepared default, while a
call emitted against an earlier header keeps that header's saved word. Native
source compilation retains its existing gate on ordinary prototypes.

Preparation uses the declaration work allowance. A narrow class default still
charges eight saved payload bytes under the native default byte limit. Failure
publishes no prepared value. Supplying every argument does not bypass the
original default's declaration-time preparation.

The semantic fragment carries the parameter's frozen class selection and
checks it against the consuming namespace. Completed class snapshots supply
raw type metadata without member storage authority. Saved values keep their
nominal class separately from the full integer word used by the omitted
argument producer. Call regions and native validation require the original
prepared value and declaration. A raw numeric default or another parameter's
fragment cannot manufacture that evidence.

The pinned reference is TempleOS commit
`c26482bb6ad3f80106d28504ec5db3c6a360732c`.
`Compiler/PrsVar.HC:609-674` allocates fixed argument words and prepares
`dft_val` by calling the compiled default expression. Its conversions cover
the integer/F64 distinction; they do not clamp integer defaults to a class's
memory prefix. `Compiler/PrsExp.HC:453-470` materializes the saved word with the
original parameter member. The hosted tests derive their expected values from
these source paths. They contain no TempleOS runtime capture or machine-byte
comparison.

Reference-bearing defaults, including calls to class-returning functions,
remain outside this source preparation path. Retained JIT class parameter
storage, native task classes, ordinary native prototypes, owned strings,
`lastclass`, class callback signatures, F64/pointer backings, general aggregate
copies and complete HolyC ABI/compiler parity remain open under #686/#702/#682.

Related behavior is documented in [class value parameters](class-value-parameters.md),
[class value returns](class-value-returns.md), [integer defaults](integer-defaults.md)
and [native defaults](native-defaults.md).
