# Original inherited layout metadata

Source execution retains the size of the original class or union selected as a
base. A later class with the same name cannot replace that selection:

```c
class Base { U8 bytes[34]; };
class Child:Base
  #exe { class Base { U8 replacement; }; }
{ I64 last; };
sizeof(Child); // 42
```

The pinned `Compiler/PrsStmt.HC:46-57` selects the base entry before the base
name's following lookahead. It copies that entry's current size after lookahead
and before validating the opening brace. Completing the selected extern forward
during JIT lookahead therefore supplies its completed size. Replacing a completed
class starts another identity. A partial base contributes only the size reached
at attachment; later growth does not change the child. The lookahead itself can
observe the newly published child's size as zero.

Members extend the copied size using their original checked dimensions and
offsets. Class `$$` directives replace the current size. Each class records its
own negative-offset padding. Root union members still overlap from zero, as in
`Compiler/PrsVar.HC:408,660-721`; an inherited prefix does not move the union
base. A forward or self base can supply zero without inventing a completed
object layout.

Runtime dimension and offset dependencies travel through every inherited size,
saved `sizeof`, default, derived bound and frame. Their original source calls
and effects occur once. Native admission still requires the owning task's actual
successful executions. Copying a size or proposing equal numeric metadata does
not authorize an unexecuted dependency. Foreign namespaces, changed definitions,
stale partial snapshots, expired phases and replay cannot supply layout proofs.

The maintained example prints `dimoff42` and returns 42:

```sh
holyc run --mode=jit --target=ir examples/source-inherited-layouts.hc
holyc run --mode=jit --target=host-jit-task --code-byte-limit=524288 examples/source-inherited-layouts.hc
```

Native tests require every original fragment to complete in machine code with
zero interpreted instructions. Tests also cover exact and exhausted preparation,
instruction, code, IR and output limits, malformed following input, overflow,
partial layouts and forward completion. IR supports closed inherited metadata
in both modes. An AOT outer class and a JIT `#exe` class belong to different
original parser environments; completing a same-name class in the directive
does not complete the selected outer forward.

Retained inherited declarations provide metadata for their captured queries.
They grant no aggregate object storage or member index. The ordinary semantic
layout APIs keep their completed-base and member-index requirements. General AOT
module layouts, runtime AOT relocation, synchronous native StreamExePrint,
aggregate object execution, wider ABI, BIN and loader compatibility, whole-tree
compilation and bootstrap remain open.
