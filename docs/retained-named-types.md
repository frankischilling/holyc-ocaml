# Retained named aggregate types

An original function header now keeps the aggregate type selected before later
lexer reads execute a nested directive. This source admits the function and
returns I64 42 with empty ordinary output in both outer modes:

```c
class C {};
I64 F(C *p)#exe {class C {};}{return 0;}
42;
```

The second `class C` does not replace the type already selected for `p`.
Previously the JIT path rejected that type with HCRUN0004, while AOT reached
the separate HCIRVM0011 frame-storage boundary. The implementation connects the
retained type evidence and checked aggregate-pointer frame slot; both are needed
for this example. Run the maintained source with:

```powershell
opam exec -- dune exec --root . -- bin/holyc.exe run --mode=jit examples/stateful-exe-named-type-selection.hc
opam exec -- dune exec --root . -- bin/holyc.exe run --mode=aot examples/stateful-exe-named-type-selection.hc
```

## Original selection and ownership

Each located token already carries its original visibility lookup. The parser
now preserves the selected Class entry alongside the exact named-type AST
node, before reading pointer stars, names, suffixes or following tokens. Direct
function returns and parameters retain that selection in their existing private
publication receipts. Completed headers reuse those same receipts; they do not
look up the spelling again. Recursive callback-signature children do not acquire
a direct parameter receipt through this change.

Seeded public primitive unions carry an explicit primitive payload in their
visibility entries. A new Class with the same name remains an aggregate, even
if a caller supplies equal source-origin metadata. Copying an environment keeps
the original entry and payload. This distinguishes built-in `I64` from a later
`class I64` without reconstructing primitive identity from its spelling.

The task ledger maps the selected frontend entry to its exact aggregate semantic
publication. `Source_type_reference.select_aggregate` accepts the original
function-return or parameter owner while its callback is current, and checks
the table, namespace, environment, selected entry and source publication. The
owner supplies the original pointer-layer objects. A caller cannot mint the
proof using a separately constructed list with matching spelling and spans.

The resulting opaque proof can survive the callback. Its `selected` consumer
requires the same type node and pointer children, then constructs a pointer to
the retained semantic aggregate. The ledger keeps proofs by physical source
occurrence so a native member inherited from another suspended header still
uses its own original selection. Provisional and completed type resolution use
the same resolver. A missing proof remains an error; a later same-name class
or a matching symbol ID from another table is not a replacement.

Completed source commands freeze the original proofs with their exact AST.
Body preparation consumes those proofs under the command's semantic namespace;
it does not re-resolve the header against later declarations. Forward
completion keeps the first unresolved class's canonical symbol. The definition
has its own source-site symbol, and member collection belongs to that site;
typed members, layouts and pointer types share the retained canonical symbol.
Repeated forward declarations and later definitions of an already completed
class create fresh identities according to their original Class-filtered
predecessor. This keeps previously constructed type references valid through
completion without mutating them.

This type proof grants no execution, layout, default-evaluation or source-replay
authority. Those consumers keep their existing event and task checks. The
proof's ability to outlive parsing does not reopen its declaration callback.

## Execution boundary

A checked direct aggregate pointer occupies an eight-byte frame slot, including
when the pointed-to aggregate has no executable storage layout. The frame still
requires the exact function/member identities, slot size and byte quota.
Aggregate-valued slots, callback declarators and pointer arrays retain their
existing rejection paths. The general memory-operation classifier is unchanged.

An original direct pointer local or parameter also supports its type-only
`sizeof` query. The checked original pointer children select the fixed pointer
width before any pointee-layout resolution, so `C *p; sizeof(p)` is eight even
when `p` has no stored value. This does not initialize `p` or read its value.
Array extents still use their checked declaration dimensions, and callback
signatures retain their existing separate boundary.

Integer argument bits do not become guest pointers. Existing pointer transport
retains storage lifetime and exact semantic identity checks. This gate does not
add aggregate dereference/member execution, arbitrary pointer casts, pointer
return execution or general aggregate-value ABI support. A pointer-return
prototype can retain a correct type without making a pointer-return body
executable. Broader type uses and callback signatures still need their own
original selection receipts and consuming implementations.

## Evidence and tests

The reference remains `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
`Compiler/PrsVar.HC:472-489` saves `cc->hash_entry` before advancing past a type.
`PrsType` at `:285-308,332-360` carries that class through pointer/name lookahead;
`:521-529` attaches the resulting member class before the following default.
`Compiler/PrsStmt.HC:223-264` passes the retained return class into function
parsing, and `:111-115` installs it before parsing the parameter list.
`PrsStmt.HC:6-36` distinguishes fresh extern allocation from reuse of the
selected unresolved class during its definition.

`test/test_retained_named_aggregate.ml` covers original selection, shadowing,
retained type identity and foreign/substituted proof inputs. Existing provisional
and completed-header tests preserve callback and source-child checks. The public
CLI executes the maintained fixture in both outer modes. These are hosted
source and ownership tests, not a new TempleOS native execution capture.
