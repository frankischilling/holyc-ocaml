# AOT zero-byte initializer assignments

A zero-byte JIT initializer copy is not equivalent to an AOT load-time
assignment. Native TempleOS captures now demonstrate the difference for both
global and function-static objects:

```c
I64 Get() { return 7; }
class Box { U0 empty; I64 value; };
Box object={Get(),42};
```

JIT leaves `object.value` equal to 42. After AOT compilation and loading the BIN,
it is 7. Replacing the following value with `0x112233445566772a` distinguishes
the store width: the loaded value is `0x1122334400000007`. The U0-target assignment
writes four bytes, not zero or eight. A constant initializer `{7,42}` still
leaves 42 in both modes.

## Original phases

`Compiler/PrsVar.HC:52-111` first compiles the expression with the AOT flag
cleared. A nonconstant expression outside the globals-on-data-heap path is
reparsed into an AOT assignment on the second pass. That assignment has its own
machine storage operation; it does not use the scalar `MemCpy` byte count from
the compile-time branch.

`Compiler/BackC.HC:159-204` lowers the assignment through `ICMov`.
`Compiler/BackLib.HC:136-155,282-312,445-578` selects the machine store. For the
observed U0 destination, the default store opcode and REX selection produce a
32-bit write. Other destination raw types need their own checked assignment
semantics.

## Hosted boundary

Nonconstant zero-byte AOT leaves now reject with `HCRUN0006` rather than silently
using JIT empty-copy semantics. This applies to global and static initializer
leaves. The diagnostic retains the original expression span. Checked constant
zero-byte copies and live JIT effects remain supported.

The prior AOT success tests compared hosted implementations with each other;
they did not establish original AOT behavior. Those expectations have been
replaced with explicit rejection tests. A future implementation needs a distinct
source-owned load-assignment operation, its actual store width, root bounds and
initialization effects. U0 results also need original machine-register evidence;
an absent value must not be replaced with an invented word.

## Evidence

`test/oracle/aot-zero-initializers` contains three source/capture pairs, each run
twice. They cover a successful 42-valued AOT control, a U0 callee that places 7 in
RAX, integer-returning global/static leaves, constant leaves and a patterned
following member. Every compiled probe reported zero errors before loading.
The AOT sources include `KernelA.HH`, where the public `I64` type is defined;
the AOT compiler does not inherit the JIT task's type table.

Tests verify the complete captures and hashes, preserve constant/JIT results,
and require the explicit hosted boundary for nonconstant AOT assignments.
This closes an incorrect admission, not the broader AOT implementation work.
Issues #686 and #690 remain open.
