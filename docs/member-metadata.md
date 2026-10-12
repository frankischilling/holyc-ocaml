# Member metadata preparation

Class and union members retain their original metadata names and prepared values.
The parser publishes each entry after member placement and before validating the
member delimiter. Completion checks the same AST nodes in source order. Later
same-name class declarations leave the saved entries intact.

Strings combine adjacent decoded segments, including embedded zero bytes. Closed
numeric expressions retain their result word. F64 expressions keep their raw
bits without an integer conversion. Original `sizeof` reads and captured `$$`
positions can supply numeric operands. In a union, `$$` reads the original union
base rather than its largest member extent.

The compiler record stores entries in the prepended order used by TempleOS.
`aggregate_field_metadata_find` returns the first matching entry, so duplicate
names resolve to the last source declaration. An absent entry returns `None`;
an entry whose word is zero remains present. These compiler-record APIs grant no
data pointer or executable ownership.

Numeric preparation consumes the shared initializer allowance. The source
manifest carries the exact table, namespace, phase and reached work into a task.
Activation replay charges each original entry at its own event. Isolated source
preparation settles its entries before later initializers. Foreign namespaces,
expired phases, repeated preparation and substituted source chains fail checks.
Strings remain compiler metadata and do not allocate VM literal storage here.

IR, native programs and native source tasks accept this preparation path.
Isolated native preparation also charges metadata alongside native defaults and
initializers, including reached work on failure. Tests cover class,
union, inherited, nested, callback, multiple-declarator and function-local
members, original class replacement, raw floating bits, joined strings, and
exact and one-below preparation allowances.

Live expressions, data and function addresses still need runtime preparation.
They report `HCRUN0006`; IR no longer silently skips their effects. Runtime
compiler reflection through `CHashClass`, `CMemberLst`, `CMemberLstMeta`,
`HashFind`, `MemberMetaData` and `MemberMetaFind` remains unfinished. The complete
`Demo/ClassMeta.HC` example depends on those consumers and live addresses. This
preparation layer does not establish completion of #686.

The reference is `c26482bb6ad3f80106d28504ec5db3c6a360732c`. The audit covers
complete `PrsVarLst` at `Compiler/PrsVar.HC:408-742`, complete `LexExpression`
at `Compiler/PrsExp.HC:1168-1178`, complete `MemberMetaData` and `MemberMetaFind`
at `Compiler/LexLib.HC:45-66`, and complete `LexExtStr` at
`Compiler/LexLib.HC:248-275`. `Kernel/KernelA.HH:792-799` supplies the metadata
node and string flag. The complete `Demo/ClassMeta.HC` identifies the remaining
runtime consumers. Evidence consists of pinned source and hosted tests; there is
no TempleOS runtime capture or emitted machine-code comparison. Bounded work,
finite floating values and ownership checks are hosted policy.
