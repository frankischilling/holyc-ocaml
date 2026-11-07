# Member metadata preparation

[The example](../examples/member-metadata.hc) prints `MM` during declaration
parsing, generates `42;` and returns 42 through IR and native source tasks in
both outer modes.

Class and union members retain their original metadata names and prepared values.
The parser publishes each entry after member placement and before validating the
member delimiter. Completion checks the same AST nodes in source order. Later
same-name class declarations leave the saved entries intact.

Strings combine adjacent decoded segments, including embedded zero bytes. Closed
numeric expressions retain their result word. F64 expressions keep their raw
bits without an integer conversion. Original `sizeof` reads and captured `$$`
positions can supply numeric operands. In a union, `$$` reads the original union
base rather than its largest member extent.

Live integer expressions also execute in original JIT source tasks. This includes
variable reads and updates, named and callback calls, original `sizeof` reads and
`$$` values. Calls follow the pinned reverse argument order. Metadata in an
uncalled function still executes while its class declaration is parsed.
Lookahead directives run before the expression is evaluated, as in the original
expression parser. Earlier output survives a later expression or delimiter error.

Each live expression has a separate metadata fragment and source authority. Its
typed root, original member, snapshot, graph and call context must match. Only
successful execution can publish the saved word. Foreign runtimes, substituted
graphs or work counts, repeated completion and expired parser phases fail checks.
Native execution claims its request once and returns its own scalar capture.

Live preparation and execution share the initializer allowance with other source
work. IR reserves reached work before entering child source, and recomputes its
allowance when the caller resumes. Nested child preparation remains charged.
One-below limits return `HCIRVM0007` with the reached counter, including failures
after output. Physical graph work can differ between IR and native execution.

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

Live floating expressions, data and function addresses still need their
preparation and owned transport. Ordinary AOT and isolated native live metadata
also remain unfinished; original JIT `#exe` tasks run in either outer mode. These
boundaries report `HCRUN0006`. Runtime
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
