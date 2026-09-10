# Completed function headers

A completed source header can now be typed before its function body exists.
`resolve_completed_function_header` consumes an opaque declaration witness
and returns the existing semantic signature representation. It retains the
original function symbol, parameter AST nodes, defaults, register requests,
recursive function-pointer signatures and variadic bindings. It creates a
parameter scope under the original namespace, with no locals or body.

This is a prerequisite for runtime admission of a pending definition. A typed
header alone does not publish an executable, admit a task command or create a
runtime provider. Calls selected from an incomplete source publication still
report HCRUN0003. The ordinary complete-function path remains available.

## Source ownership

`Compiler_record.declare_function` requires the original parser header and
its exact source publication, namespace and semantic table. It can create a
witness during the header's live callback or while that exact event is active
in the namespace's source replay journal. An absent event, an inactive journal,
another active event and a consumed journal grant no authority. Callback
authority ends on normal return, rejection or exception.

The task declaration ledger caches one witness during the live callback after
validating the completed defaults. Metadata-only replay retains its existing
phase checks but creates no witness. Later consumers can retrieve the
original live source evidence; this
does not reopen the callback. Header typing independently checks namespace
and table ownership. The resulting header-local collection uses item index
zero; that index does not stand for a published module item.

Primitive, pointer and recursive function-pointer types use the existing
signature converters. Named aggregate types currently lack retained source
visibility in this API and return an explicit error before allocating a
function scope or parameter symbols. A caller that needs repeated access must
retain its typed result: each successful resolution creates a new parameter
scope. Runtime admission and eventual body completion will need to share one
retained typed header and preserve its source ancestry.

## Native phase evidence

The reference revision is `c26482bb6ad3f80106d28504ec5db3c6a360732c`.

- [PrsStmt.HC](../third_party/TempleOS/Compiler/PrsStmt.HC), lines 96–123,
  creates a fresh JIT function record with `UndefinedExtern` (AOT uses its
  current output offset), then finalizes its
  argument count and offsets after parameter parsing returns.
- [PrsVar.HC](../third_party/TempleOS/Compiler/PrsVar.HC), lines 445–447 and
  700–703, advances past `)` before returning. A directive reached by this
  lookahead sees a provisional header, before completed-header publication.
- [PrsStmt.HC](../third_party/TempleOS/Compiler/PrsStmt.HC), lines 925–929,
  advances past a block's `}`. A directive reached there runs before the
  enclosing function compilation and executable publication at lines 183–193.
- [PrsExp.HC](../third_party/TempleOS/Compiler/PrsExp.HC), lines 430–489 and
  544–569, prepares arguments before the indirect call. A completed pending
  header must therefore support argument evaluation before a reached
  [UndefinedExtern](../third_party/TempleOS/Compiler/CExcept.HC) failure.

The parser regressions preserve these distinct boundaries:

| Source at the first directive | Completed headers | Completed bodies |
| --- | ---: | ---: |
| `I64 F(I64 n)#exe {}{return n;}` | 0 | 0 |
| `I64 F(I64 n){return n;}#exe {}` | 1 | 0 |
| `I64 F(I64 n){return n;};#exe {}` | 1 | 1 |

Moving body publication earlier would erase an observable native phase. A
fresh provisional header also differs from an extern record whose parameters
are being replaced. [PrsLib.HC](../third_party/TempleOS/Compiler/PrsLib.HC),
lines 62–76, zero-allocates a new record; PrsStmt.HC, lines 90–94, replaces a
reused record's member list while retaining its previous argument count until
the final assignment at lines 114–115.
Neither state may be relabeled as a completed header.

[Header tests](../test/test_completed_function_header.ml) cover original child
identity, recursive type parity, ownership, callback lifetime, journal replay,
ledger retention, unsupported aggregate visibility and directive lookahead.
These are hosted tests and pinned source evidence; no native execution capture
is claimed. Runtime pending-header admission, provisional parameter records,
native extern slots, linking and the complete compiler remain unfinished.
