# Automatic aggregate byte views

The interpreter and native executor can allocate a nonempty automatic class or
union object whose exact layout completed before its function declaration. An
explicit primitive pointer cast exposes the object's bytes:

```c
class Packed { U8 tag; U16 value; };
I64 F() {
  Packed object;
  U8 *bytes=(&object)(U8*);
  bytes[0]=99;
  U16 *value=(bytes+1)(U16*);
  *value=42;
  return *value;
}
F();
```

`holyc run --target=ir` and `holyc run --target=host-jit` execute this source in
JIT and AOT mode. The checked frame uses the selected class identity and byte
size. A later same-name definition cannot change an earlier function's object.
Named and anonymous unions, nested aggregate members, primitive member arrays,
backing classes and explicit padding contribute their existing closed layouts.
The numeric offsets in these examples follow the pinned packed layout rules;
they do not exercise direct member projection.

Each object starts with unknown bytes. A store initializes only its access
window, and overlapping views observe the same bytes. Reads apply the primitive
view's width and signedness. Unaligned windows, updates, primitive pointer
parameters, byte scans and captured assignment destinations use the existing
owned pointer operations. A scan can stop at a known terminator before later
unknown bytes. Each invocation has fresh initialization state.

Bounds use the original object extent, including explicit padding. A cast
changes the view without extending that extent. Native descriptors retain
individual byte initialization flags and the invocation's lifetime. Frame,
instruction and image limits are checked before their corresponding allocation
or reached operation. Numeric bits, callback addresses and foreign frames do
not acquire object ownership.

The maintained `examples/automatic-aggregate-byte-views.hc` prints `AB` and
returns 42. API and CLI tests cover both modes and executors, all nine integer
view spellings, overlap, packed windows, nested layouts, padding, aliases,
shadowing, unknown bytes and out-of-object writes. Native image tests compile
both x86-64 ABIs and execute fresh host images. Exact and one-below controls
cover runtime frame bytes and instructions, plus native stack and encoded image
bytes. These are source-derived expected results and hosted checks, without a
TempleOS runtime capture.

Inheritance still follows a separate metadata path that grants no runtime
layout authority. Retained JIT commands cannot import aggregate object storage
through this path. Zero-sized objects, aggregate arrays, persistent objects,
whole-object values and copies, direct or pointer member projection, aggregate
pointer parameters and returns, and original named-local size/position queries
remain unfinished. A class defined inside a function also lacks the required
earlier layout. These boundaries remain under
[issue #686](https://github.com/frankischilling/holyc-ocaml/issues/686).

The source rules come from TempleOS commit
`c26482bb6ad3f80106d28504ec5db3c6a360732c`:

- `Compiler/PrsVar.HC:530-531,590-618` computes local storage from class size
  and dimensions, then places it in the frame at the original alignment.
- `Compiler/PrsVar.HC:660-671` places packed class members and overlapping
  union members; `Compiler/PrsStmt.HC:41-57` retains the base and completed size.
- `Compiler/PrsExp.HC:1017-1055` changes the preceding operand class for an
  explicit postfix cast and emits `IC_HOLYC_TYPECAST`.
- `Compiler/BackLib.HC:693-707` selects the pointed-to load width, and
  `Compiler/BackC.HC:159-204` stores at the pointed-to width.

The unknown-byte, ownership and extent checks are hosted execution policy.
They do not describe TempleOS behavior for invalid or uninitialized memory.
