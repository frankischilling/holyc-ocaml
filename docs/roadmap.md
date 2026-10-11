# holyc-ocaml roadmap

Issue #711 connects shared assembler operand parsing and form selection to
HolyC blocks, direct function instructions and the standalone `asm` command.
The source mode, operand masks, shortest-form rule, aliases, unsized warnings
and expression provenance appear in deterministic dumps.
The shared instruction encoder from #712 was completed in
[PR #830](https://github.com/frankischilling/holyc-ocaml/pull/830).
Standalone directive output under #713 adds data, repetition, alignment,
explicitly rooted binary input and ordered metadata. See
[assembler support](assembler.md) and [directive output](assembly-directives.md).

Label/import fixups and branch layout are implemented under #714. Live compiler
expressions and source assembly loading and execution remain under #715,
and BIN serialization/loading under #717. [Disassembly](disassembly.md) under
#716 provides public binary inspection, bounded source lookup and native
compiler/kernel captures. Intact DolDoc oracle evidence remains under #375;
these standalone byte-output
features do not establish full compiler parity.

Issue #686 now includes owned integer and class pointer fields in IR. Member
arrays, function-local types, captured destinations, overlap and expired
targets use checked field proofs and stored pointer ownership. Native pointer
fields still need durable allocation identities and storage metadata. Persistent
aggregate objects and member metadata evaluation also remain under this issue.
See [pointer fields](aggregate-pointer-fields.md).

Issue #822 connects original class and union member `sizeof` queries to IR and
native execution. Named type, automatic object and parameter roots retain
selected member identities, per-member array counts and immutable query-time
sizes. Issue #824 connects the six builtin integer unions through generated
field declarations, including nested fields and supported scalar global/static
objects and arrays. Persistent user-defined aggregate storage remains under
#686. See [member sizes](aggregate-member-sizeof.md).

Issue #818 adds automatic objects for classes and unions declared inside a
function. Original declaration and expression positions admit their completed
layouts while preserving exact types, function identities and retained JIT
contexts. Member access, nested and inherited objects, arrays and class pointer
operations share the existing IR and native storage paths. See
[function-local objects](function-local-aggregate-objects.md).
Persistent aggregates, general object copies, native pointer fields and class
pointer returns remain part of the broader work under #686.
See [owned aggregate members](aggregate-members.md) and
[automatic aggregate byte views](automatic-aggregate-byte-views.md).

Issue #47 now matches the original absent identifier check for expression
operands. IR and native saved inputs retain its counted cause, owned cleanup
and reached effects in either outer mode. Unresolved statement starts, export
flags, assembly expression paths and the remaining original expression
producers remain open. See [compiler exceptions](compiler-exceptions.md).

Issue #704 now connects native AOT source sessions: original JIT `#exe` tasks
generate source during parsing, and a distinct native module runs afterward.
Instructions, output, preparation, defaults, code and storage stay bounded
across both contexts. Synchronous StreamExePrint, runtime AOT dimensions,
reference relocation and wider outer module support remain open. See
[native AOT sessions](native-aot-source-sessions.md).

Original inherited size metadata now carries the selected base's reached layout
and runtime dependencies into saved queries, defaults and frames. Partial bases,
forward completion and same-name replacement follow the original parser phases.
Aggregate object storage and member-index admission remain open. See
[inherited layouts](source-inherited-layouts.md).

Issue #704 now connects integer runtime array bounds and aggregate `$$` offsets
to original IR and native JIT task execution. Primitive member layouts retain
runtime dependencies through completed sizes and derived frames. AOT relocation,
floating bounds, aggregate object execution and synchronous native StreamExePrint
remain open. See [runtime layout expressions](runtime-layout-expressions.md).

Issue #704 now connects integer `_intern` target expressions to original JIT
task execution in IR and native code. Supported internal calls share the native
task's owned storage and budgets. Floating targets, synchronous native
StreamExePrint and exported executable addresses remain open. See
[live internal bindings](native-internal-bindings.md).

Issue #704 now connects one-level primitive data-pointer defaults to original
JIT headers, saved calls and native task storage. Miscellaneous-data expressions
copy their resulting terminated string. AOT relocation, pointer returns,
persistent pointer variables, source variadic pointer ownership and the wider
memory model remain open. See [saved data defaults](data-pointer-defaults.md).

Issue #704 connects original scalar and fixed-array JIT initializer leaves,
retained direct functions and resumed commands to one native arena through
`host-jit-task`. Source requests execute once under a shared native budget.
Each caller compiles its original function bodies and historical global bindings.
Retained Print and PutChars calls use those original contexts and share the
task's output allowance. Joined JIT extern calls retain their selected bodies
and source contexts.
Original task literals now keep their mutable bytes and reference tables across
fragments, including retained formats and initializer calls.
Integer function statics now allocate at the original live declaration and
execute initializer leaves once in source order. Retained calls share their
original private storage, including historical bodies after replacement.
The compiler's direct literal-copy branch now initializes original fixed byte
array statics under the same task ownership and initializer allowance.
Named integer defaults now execute once at their original live header callback,
including storage effects and calls to retained functions. Later and historical
calls require the exact original saved word and native completion receipt.
Wider defaults, synchronous native StreamExePrint and AOT reference relocation
remain open. See
[native source tasks](native-source-tasks.md).

Issue #797 adds [conditional integer chains](conditional-comparison-chains.md)
with original middle values, branch-skipped effects, cumulative classes and
checked cross-block definitions. Pending reductions and floating chains remain
open; the native fixture identifies fifty field/source pairs observed twice.

Issue #795 fixes [primitive type visibility](primitive-type-shadowing.md)
through original token selection, preserving local post-body lookahead.
The separate pending comparison-stack requirements remain under #593, and the
complete compiler release remains under #682.

Issue #793 connects original constant shifts and scalar right-shift updates to
retained default/global/static preparation with sealed source authority.
Nineteen repeated native fields and original effects/faults back the supported
path. Direct/unsealed preparation, broader optimizer passes, native retained
publication and loader/bootstrap requirements remain open. See
[retained shift preparation](prepared-integer-shifts.md).

Issue #783 adds [owned scalar pointer difference](pointer-difference.md),
including signed element counts, one-past references, operand snapshots and
original subtraction/size/division authority. Issue #785 prepares wider
differences reached through retained calls with original source authority.
General initializer division and raw address/class-query parity remain
under #685/#696/#697/#687/#699.

Issue #781 adds [owned scalar pointer ordering](pointer-ordering.md),
including all four operators, unknown pointees, one-past values and original
comparison authority. Unrelated objects retain a checked mismatch fault; raw
address placement, raw difference and broader pointer domains remain open.

Issue #779 adds [owned scalar pointer equality](pointer-equality.md),
including unknown pointees, one-past values, stable aliases across descriptor
tables and original comparison authority. Raw ordering, raw difference and broader
pointer storage/lifetime domains remain open under #687/#699/#700.

Issue #777 adds [owned scalar pointer subtraction](pointer-subtraction.md),
including signed offset overflow, minimum-word handling and original producer
sealing. Raw difference, raw ordering, compound updates and broader pointer
storage/lifetime domains remain open under #687/#699/#700.

Issue #775 adds [owned scalar pointer addition](pointer-addition.md),
including original pointee scaling, captured references, signed offsets and
one-past addresses. Raw difference, raw ordering, compound updates,
escapes, aggregates and full optimizer/native preparation remain open.

Issue #773 adds [owned swap calls](swap-internals.md) used by compiler
backend and assembler routines. The four scalar widths, both original pointer
roles, read-before-write order, actual U0 completion and retained mutation are
supported. General pointer and aggregate callers, optimizer parity and native
retained publication remain open.

Issue #771 adds [owned pointer bit calls](pointer-bit-internals.md) for
plain numeric Bt/Bts/Btr/Btc targets. Original owned storage, argument phases,
prior-bit results and retained mutation are supported. Locked/concurrent forms,
optimizer rewrites, aggregate flags and broader pointer domains remain open.

Issue #769 adds [canonical Bool execution](canonical-bool.md), including
the pinned ToBool signature, scalar storage, calls and original saved defaults.
Bool identity, signed byte reads and full computed returns remain distinct.
Broader runtime, optimizer and native retained work stays under #695/#685/#704.

Issue #767 adds [owned ModU64 calls](internal-mod-u64.md) for numeric
formatting and date-conversion patterns. Original pointer ownership, argument
order, quotient mutation and fault phases are checked through IR and both host
ABIs. General pointer and full runtime work remain under #687/#699 and #695.

Issue #765 adds [Bsf/Bsr runtime calls](internal-bitscan.md) through shared
IR and checked native encoder forms. Pinned alignment and power-of-two callers
supply the source contract; the optimizer's immediate rewrite and broader
preparation remain separate requirements.

Issue #763 adds [signed and unsigned Min/Max calls](internal-minmax.md)
through the shared interpreter and both host ABIs. Original two-argument call
authority and retained defaults are implemented; broader runtime and compiler
requirements remain open under #695 and #682.

Issue #761 connects [unary integer internal calls](internal-integers.md)
to IR, retained tasks and hosted native execution. ToBool, AbsI64, SignI64 and
integer squares preserve full-word results and original call authority. Broader
runtime, floating and native frontend work remains open.

Issue #753 connects supported numeric internal declarations to retained IR
tasks, including shared records changed during nested header lookahead.
Issue #755 evaluates original integer binding expressions before type validation
and header publication, including retained calls with effects.
See [retained internal calls](retained-internal.md). Floating targets,
ordinary output-source binding expressions and native retained source execution
remain under #701, #695 and #704.

The PutChars increment under #705 captures native packed-character output through
original provider calls and explicit byte/work limits. The checked Print
provider also executes natively; the broader runtime-provider connection
remains open. See [native output](native-output.md).

Issue #751 connects [internal ASCII conversion](internal-toupper.md) to
interpreted and native source execution through the original numeric internal
target. General binding expressions and other required runtime services remain
under #695.

Issue #698 extends native storage with global/static arrays, mutable literal
objects and original initializer leaves. It retains declared extents, independent
element state, static sharing, distinct literal ownership and fresh execution
images. See [persistent native storage](native-persistent-storage.md).

Issue #681 connects closed automatic integer arrays to native indexed reads,
assignments, updates and stable element aliases. Original dimension and object
metadata control scaling, bounds and per-element initialization; private
reference records remain bounded before allocation.
See [native automatic arrays](native-arrays.md).

Issue #679 adds native one-level scalar pointer aliases in automatic locals and
fixed parameters, with checked object ownership, private descriptor storage and
existing initialization/resource faults. Pointer returns, persistent pointer
storage, general pointer arithmetic and the full ABI remain
separate compiler work.
See [native pointers](native-pointers.md).


GitHub milestones M0 through M10 hold measurable exit criteria. Current work spans M5 (IR and interpreter) and the bounded M6 hosted x86-64 backend. Earlier milestone gaps, including compile-time execution, remain tracked in their original issues.

The [1.0 release contract](release-contract.md) defines the required Linux
and Windows x86-64 host matrix, canonical source coverage, artifact and TempleOS
target gates, whole-tree reporting and bootstrap lineage. Completing that contract
defines the release criteria; implementation gates remain tracked through M10.

M0 covers the build, source model, diagnostics, pinned reference, first lexer slice, tests, and CI. Later milestones cover the source audit, integrated preprocessor, parser, semantic model, verified IR, interpreter, hosted x86-64 backend, TempleOS assembler, `.BIN` and JIT support, whole-tree compatibility, bootstrap work, and the 1.0 release.

M6 now has a bounded source-to-native expression gate in issue #642. The own
encoder and explicit Windows/Linux x86-64 execution path cover bounded
I64/U64 expressions, with later increments below connecting control flow and
direct scalar functions. General memory, floating point, relocations and
object/BIN output remain open backend work.
Issue #644 extends that gate with six integer comparisons and logical NOT.
Issue #646 adds eager binary logical values and ordinary comparison chains,
including corrected forwarded computation classes in the shared lowerer.
Conditional and multiple-pending-reduction chains remain separate work.
Issue #648 adds reusable private spill slots, a separate frame quota and
Windows unwind registration for high-pressure expressions. Larger/probed
frames and the complete HolyC register-allocation and call ABI remain required.
Issue #652 adds left and arithmetic/logical right shifts, including masked
counts and fixed-RCX transport with preserved live values. Issue #654 adds
guarded signed and unsigned division/remainder plus a private checked arithmetic
fault channel across the Windows/System V host boundary.
Issue #657 extends the shared backend to closed source programs with structured
branches and loops, a generated per-IR step budget, exact fault sites and one
bounded spill frame. The public `run --target=host-jit` path compiles the entry
and retains native progress without interpreting ordinary source commands.
Issue #659 connects source-defined fixed I64/U64 functions, automatic scalar
storage and direct generated calls. It preserves checked body/frame/call identity,
caller live values, nested argument staging and per-activation initialization.
Generated checks bound simultaneous semantic frames, named-call depth and
physical native stack bytes; checked faults unwind the complete call chain.
Issue #660 connects bounded, source-owned scalar I64/U64 defaults. Every admitted
default prepares at its original header callback, including unused functions
and functions called with explicit arguments. Calls reuse saved values; exact
header/preparation/call evidence controls native admission. Preparation work and
saved payload bytes have explicit limits and survive later failures in reports.
Default-bearing definitions must precede executable top-level statements.
Effectful defaults, interleaved declaration execution, owned strings, `lastclass`
and broader default types remain separate work, alongside storage and full-ABI
requirements. See [native defaults](native-defaults.md).
Issue #663 extends the same pipeline with I8/U8/I16/U16/I32/U32 storage and
signatures, narrow constant defaults, and U0 procedure completion. Declared-width
loads/stores remain distinct from full register results; U0 completion cannot
be consumed as a word or leave a stale top-level result. Original frame ranges,
call/default authority and all resource limits remain checked. See
[native scalar functions](native-scalars.md).
Issue #664 connects existing function-local label resolution and goto fragments
to shared executable blocks. Both interpreter and native source execution use
the same owned occurrences and reserved label targets; labels are structural
boundaries and gotos use the existing checked jump. Source order stays separate
from `for` update execution order, and skipped initialization, return completeness
and resource limits remain checked. See [goto execution](integer-goto.md).
Issue #668 connects original closed case preparation and bounded integer switch
dispatch to both execution targets. Issue #691 extends that path to bracketed
dispatch, `start:`/`end:` setup regions and repeated defaults. The shared graph
preserves range holes, fallthrough, nested breaks and source ownership.
Bracketed selectors outside the prepared table report `HCSW0005`; setup
prefixes use checked local continuations. JIT/current-task scalar case
expressions retain their reached preparation effects and original selections.
Ordinary AOT nonconstant endpoints still require source-image authority and
reject with `HCRUN0006`. Preparation work, cumulative table slots and native
instruction sites remain bounded. See [switch execution](integer-switch.md).

Issue #667 adds [native scalar globals](native-globals.md), shared by entry
and generated calls with exact declared widths, AOT zero/JIT unknown state and
fresh private arenas. Issue #673 adds closed scalar global initializers, prepared
once at their original leaf callbacks under the shared declaration-work budget.
Native execution restores their declared-width values from owned preparation
receipts. Effectful initializers, interleaved declaration execution and broader
persistent storage remain open.
Issue #675 connects uninitialized scalar static locals to the native arena with
exact function ownership, padded allocation quotas and sharing across recursive
calls. Static initializers and broader persistent storage remain separate gates.
The complete HolyC ABI, persistent and pointer memory operations, F64/x87 and
conversions, runtime output, and general declaration/`#exe` native integration
remain open. Optimizer parity, assembler and object/BIN output, actual TempleOS
loader acceptance, whole-tree compilation and bootstrap retain their own gates.

Issue #801 connects original callback storage and checked integer/U0 invocation
through IR and hosted native execution. Automatic, static and global cells,
fully indexed arrays and callback parameters retain their selected headers,
saved defaults and original calling flags. Explicit canceled dereferences reuse
the same cells for calls, reads, assignments and numeric updates. Arithmetic
after an update preserves eight-byte scaling and its I64 or U64 computation
class. Supported global initializer regions and top-level calls also execute.
Native source-session scheduling and shared task storage remain under #704;
callback members, effectful defaults, owned-address arithmetic, remaining
indirections and broader executable return domains keep #801 open.

This file does not mark planned work as implemented. Current support is listed in the README and generated compatibility reports.
