# Completed function headers

A completed source header can now be typed before its function body exists.
`resolve_completed_function_header` consumes an opaque declaration witness
and returns the existing semantic signature representation. It retains the
original function symbol, parameter AST nodes, defaults, register requests,
recursive function-pointer signatures and variadic bindings. It creates a
parameter scope under the original namespace, with no locals or body.

A retained JIT task admits this header at its original completed-header event,
after preparing its defaults and admitting its predecessors. Calls can then be
checked before a body exists. A reached call evaluates its arguments and reports
HCIRVM0030 if it has no executable or approved provider. A skipped call remains
unexecuted. Provisional parameter lists still report HCRUN0003. Ordinary AOT
image compilation and its separate JIT directive task retain distinct phases.

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
scope. The task ledger retains one collected and typed header. Body completion
extends its original parameter scope with locals, keeps the same typed header
and signature, and completes its exact pending declaration once. Retained
adapters check original modifiers and prototype bindings as well as names,
types and parameter children before allocating locals or consuming the header.

## Pending calls and completion

Pending state is distinct from declaration kind. A pending definition retains
extern call access without becoming an extern AST or an approved provider.
Calls keep their original header, defaults and argument protocol. Once the
original body is published, an earlier retained caller reaches that body through
its exact declaration ancestry. A later independent JIT shadow cannot redirect
the caller. Providers captured through a prior extern remain available until
compatible source publication, following the existing extern rules.

Reached failures preserve earlier writes, output and retained commands. The
native extern flag remains set until successful body publication: after a
pending definition fails, a later definition can join that unresolved function.
After successful publication, another same-name JIT definition creates a shadow.
This distinction follows PrsFunJoin and the final Cf_EXTERN clear in PrsFun.

Nested replacement of the same function header before the outer body completes
preserves separate header and executable versions. For example,
`#exe {I64 F(I64 n){return n;}#exe {I64 F(I64 n){return 99;}}StreamPrint("%d;",F(42));}`
now returns 42. PrsStreamBlk shares the hash table while saving/restoring the
compiler context. The inner PrsFunJoin replaces the same native record's header;
the outer PrsFun later writes its executable address without restoring its old
header. The semantic declaration retains its original body source and the
current lookup header separately. Completion requires the original pending
source and the latest version of that exact record, even when a newer record
hides it by name. Historical entries retain exact declaration and classification
ownership; they provide captured bindings without becoming name-lookup entries.

Source-derived acceptance cases use an outer default of 40 and
body `return n+2;`, then a nested default of 99 and body `return 7;`. A caller
compiled against the original pending header later returns 42; a direct
call compiled after the nested body keeps returning 7; a new defaulted call
after outer completion returns 101. An additional resolved shadow stays
the current name binding while outer completion updates only its original
hidden record. These expectations follow PrsExp.HC, lines 560-569, and pass
hosted tests in both outer modes. Incompatible header changes also affect native
optimizer and return cleanup behavior and remain separate work. No native capture is
claimed for these cases.

A self-call parsed while the header remains pending keeps its mutable extern
target, including when executing a captured older body. Ordinary and implicit
calls parsed in the outer body retain their exact historical header. Initializer
checks follow selected call bindings and visit executable bodies separately;
static roots and transitive recursive calls cannot bypass optimizer checks by
sharing a function symbol with another body.

`examples/stateful-exe-function-versions.hc` combines the three results as
`42+7+101-108`, returning I64 42 with empty ordinary output in JIT and AOT outer
modes. The tests also retain variadic cleanup and exact/one-below step,
preparation, frame and call-depth limits.
The example uses 71 runtime instructions in JIT outer mode and 73 in AOT, with
six preparation instructions in each; the CLI tests enforce those exact limits
and their one-below failures.

Run the retained pending-call example in either outer mode:

```powershell
opam exec -- dune exec holyc -- run --mode=jit examples/stateful-exe-pending-header.hc
opam exec -- dune exec holyc -- run --mode=aot examples/stateful-exe-pending-header.hc
```

The nested directive defines `Saved` while `F` has a completed header and no
published body. After `F` completes, `Saved` uses its saved default and variadic
tail to return 42. The directive inserts `42;` into the outer stream. Ordinary
output is empty. The source tests exercise exact runtime/preparation, frame and
depth limits, plus one-below failures, without changing the CLI report contracts.

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
lines 62–76, zero-allocates a new record. PrsStmt.HC, lines 90–94, saves a reused
record's previous count for header comparison, then replaces its member list.
`ClassMemberLstDel` in LexLib.HC, lines 209–218, resets both member and argument
counts to zero. The final assignment at PrsStmt.HC:115 uses the current member
count. Nested declarations can modify that same record before the outer parser
resumes.
Neither state may be relabeled as a completed header.

[Parameter delimiters](function-parameter-delimiters.md) retain trailing comma
and semicolon syntax through both completed-header typing and body completion.
Empty entries remain source evidence and do not become signature slots.

[Header tests](../test/test_completed_function_header.ml) cover original child
identity, recursive type parity, ownership, callback lifetime, journal replay,
ledger retention, unsupported aggregate visibility and directive lookahead.
These are hosted tests and pinned source evidence; no native execution capture
is claimed. [Runtime tests](../test/test_pending_function_header.ml) also cover
argument faults, skipped calls, retained defaults, variadics, shadows, recovery,
provider timing, expired admission and resource limits. Provisional parameter
runtime records, named aggregate visibility, native extern slots, linking and the
complete compiler remain unfinished.

The [provisional source transcript](provisional-function-members.md) now retains
original member phases before header completion. It does not yet admit runtime
calls against those partial headers.
