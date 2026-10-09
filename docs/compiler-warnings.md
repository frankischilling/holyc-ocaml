# Compiler warnings

JIT extern-header joins emit return-class mismatch HCSEMA0037 followed by
argument-list mismatch HCSEMA0038 when the completed header's option 19 is
enabled. The check follows closing-parenthesis lookahead and precedes body
parsing and runtime header admission. A body error therefore retains reached
header warnings. A directive's JIT headers follow this path in either outer
compilation mode; ordinary AOT header joins still need their own phase evidence.

The comparison uses the native cursor saved before the extern record is
cleared, its actual argument count, and the current shared record's return
owner. A nested header can change that owner independently of the outer source
transcript. The full member list includes argc/argv and members beyond arg_cnt.
It also retains original body-local allocations when a nested header reuses a
record before its outer definition completes, including their insertion order
and MemberAdd class-base flags.
Comparison rejects unavailable cursors or counts that differ from the original
insertion receipts.
PrsDotDotDot inserts the synthetic pair without increasing member_cnt, so the
saved count can stop comparison before those members. Two nonempty lists match
at count zero; an empty list and a nonempty list do not.

Defaults are read from original successful evaluations. Parsed or pending
receipts supply no available word, and comparison does not rerun expressions.
Scalar defaults retain their full saved bits before argument-width conversion.
Captured callbacks compare the selected executable owner in this private
runtime. Data defaults compare live interpreter objects and byte offsets or
addresses read from the original native arena under its capture and lease
checks. Copied string defaults retain their string flag and actual copied
bytes, compared through the first NUL. Names and member classes short-circuit
these payload reads.

[The header example](../examples/compiler-header-warnings.hc) keeps default
side effects, narrow values, data aliases and callback ownership visible. It
prints five copies of 42 followed by semicolons and returns 42. Its four header
warnings come from Count, Width and Both; Quiet uses the disabled header mask.
Each of its six extern joins also emits an unused-extern warning, including
Quiet, for ten diagnostics in total.
Native execution of this combined example uses
`run --target=host-jit-task --code-byte-limit=1048576`.

The bounded integer source pipeline emits `HCSEMA0034` for an unused parameter
or local when `OPTf_WARN_UNUSED_VAR` is enabled. Each original completed function
body keeps the mask reached at its closing boundary. A call to `Option(16,...)`
while preparing a local array bound therefore affects that function's warning.
Later changes cannot rewrite an earlier warning or its saved mask.

`no_warn` updates the source use count and the effective
`MLF_NO_UNUSED_WARN` flag. It emits no runtime instructions. An ordinary use
plus a suppression, or two suppressions, can produce `HCSEMA0035` for an
unneeded `no_warn`, even when option 16 is disabled. The spelling `_anon_`
remains exempt from this unneeded-suppression warning. Initializer resets and
specialized name queries retain the existing source counting rules.

Warnings are structured diagnostics with their source location and warning
severity. They do not enter captured program output or make a successful
command fail. An active source task emits a function's warnings once after its
checked integer body compiles. Nested directives and synchronous source children
deliver them to the enclosing diagnostic stream in reached order. An ordinary
child starts with its caller's current options and keeps its changes separate.
A later parser, execution or budget failure retains warnings already emitted.
A later AOT native module failure also keeps warnings from its completed
directive task.

[The example](../examples/compiler-warning-options.hc) prints `42;42;42;42;`,
returns 42 and emits three diagnostics: an unused parameter in `Loud`, an
unneeded suppression in `Suppression`, and an unused parameter in `Child`.
`Quiet` and `Parent` remain silent. The CLI fixture checks IR and native task
execution in both outer modes, exact and one-below instruction limits, failed
bodies and the absence of executable work for `no_warn`.

The standalone local-warning analysis API still accepts one batch mask. The
source driver supplies the original per-function snapshots through checked
symbol and command identities. A saved snapshot is evidence; it grants no
authority to mutate a closed compiler context. Copies of a completed header or
body cannot substitute for their original receipts.

The pinned local rules are in Compiler/PrsStmt.HC:193-207. These PrintWarn
calls do not increment warning_cnt. Header mismatches increment the focused
parser control's counter once per warning. Directives share that counter;
ordinary child controls start at zero while forwarding their diagnostics.
Activation can admit an earlier observed header without replaying its warning
phase.

Option 18 now emits `HCSEMA0076` at the original automatic-local allocation
callback, after type and dimension parsing and before allocation or initializer
parsing. Only the first declarator in each comma declaration enters the
function's type index. Static locals and parameters do not enter it. The index
is populated even when the option is off; a later enabled declaration compares
against those earlier types. Each duplicate increments the active native
`warning_cnt`, and a rejected receipt replay changes neither the index nor the
counter. Reached warnings survive initializer, body and later execution errors.
An ordinary duplicate member name emits `HCSEMA0015` before the type warning;
the original `pad`, `reserved` and `_anon_` exceptions remain legal. This
structured error does not yet implement native LexExcept/error counting.

The comparison uses exact zero-depth class identity. Pointer depth and array
dimensions do not distinguish bases. Public `I64` and intrinsic `I64i` stay
distinct, as do `Bool` and `I8`. A completed forward keeps its canonical class;
a new same-spelling class has a separate identity. Local callback declarations
use the original intrinsic `RT_PTR` base, retained from source-order type
seeding, independently of their return type, signature or later `I64i` shadows.
Original type-token selections remain frozen across nested directives.
JIT checks use the current shared record's retained member cursor, so a
reentrant header replacement resets the index seen by the resumed body.
An untracked predecessor supplies no validated native cursor. Warning-off
execution retains its existing metadata path; enabling option 18 rejects that
unavailable evidence instead of assuming an empty type index.

[The duplicate-type example](../examples/compiler-duplicate-types.hc) prints
`42;`, returns 42 and warns for `Sum`'s second `I64` declaration and
`CallbackBase`'s second callback. The parser and source tests also check comma
declarations, option changes during dimension evaluation, register qualifiers,
forward completion, type shadows, child inputs and failure ordering. IR and
native CLI checks exercise both outer modes. These warnings use the retained
semantic class identities; full native class/member objects, warning terminal
formatting and wider runtime storage still require their own implementation.
The original rules are Compiler/LexLib.HC:120-141,
Compiler/PrsVar.HC:350-356 and 525-528, and Compiler/AsmInit.HC:197-206.

## Unnecessary parentheses

Option 17 emits `HCSEMA0077` while parsing expressions. The parser retains the
original maximum and left precedence, association bits, unary prefixes and
postfix modifiers. Unary and binary Pratt recursion share that state; grouped
expressions, arguments and index expressions use separate states. Checks run
after term modifiers, after binary-operator lookahead and before final stack
reduction. They do not traverse a completed AST. The warning's primary span is
the reached lookahead; a related span identifies the opening parenthesis.

The source has two definition-input checks. A grouped expression starting in
a definition suppresses its outer warning even after the replacement ends.
`ParenWarning` also suppresses a warning whose current lookahead remains in a
definition. These checks use the original input reached by token character
consumption and lookahead. For example, a replacement `42` exits its input
while reading the next character, whereas `42 ` reads that character inside
the replacement. A token's source or definition trace cannot replace this
selection. Generated source streams are distinct from definitions.

Each reached warning reads the live option and increments the focused native
warning field once. Directives can change that option during lookahead;
ordinary children copy the caller's current mask. Warnings survive later
delimiter, operand, backend and execution failures. Required grouping,
postfix casts, updates, macro boundaries and original association rules have
separate controls. The [example](../examples/compiler-parenthesis-warnings.hc)
prints `42;` and returns 42 through IR and native tasks in both outer modes.
Checked full-word parenthesized casts also execute natively; the word backend
retains the original zero/one cast payload and target computation class.

References: Compiler/PrsExp.HC:67-85, 120-127, 191-197, 244-248 and 728-748;
Compiler/CExcept.HC:110-115; Compiler/Lex.HC:107-242 and 474-482;
Compiler/CompilerA.HH:335-356 and Compiler/CInit.HC:288-329.

These are structured parser warnings. Full native intermediate-code type
checks, LexExcept interruption and terminal formatting remain unfinished;
semantic errors still diagnosed after grammar parsing can reach different
later warnings from the native compiler.

Each original parser input owns native field storage with the pinned CCmpCtrl
prefix through `warning_cnt`. `opts` is at byte 320 and `warning_cnt` at byte
352. Source GetOption/Option, counted warnings and the original declaration,
header, body and command snapshots use these fields. Root inputs start with
`0x90000`; a directive shares its caller's allocation. An ordinary nested input
copies the caller's live options into a fresh allocation with zero counts.
Uncounted PrintWarn diagnostics leave the native warning field unchanged.
Current-context, source-manager and domain checks still precede mutation.

The private allocation ends at byte 360. Its other fields remain zero; it
provides no full CCmpCtrl, CmpCtrlNew task queue, native table context, lexical
buffer setup or exported ABI. Error-count producers, remaining LexWarn paths
and TempleOS terminal formatting remain open. An independent x86-64 oracle
compares literal field offsets and BT/BTS/BTR/INC instructions with production
operations over all 64 bits, eight initial patterns, repeated writes and warning
wraparound. It also checks that neighboring fields remain unchanged.

Full warning timing remains unfinished. TempleOS tests unused locals after
`COCCompile`; this pipeline emits after checked integer lowering, before native
code generation. An ordinary isolated module emits its warnings after the
whole module compiles, so an earlier function's warning is not retained when
later source prevents module compilation. Ordinary AOT joins, broader default
and miscellaneous-data behavior and return-warning
consumers still need their original phase integration. Callback owner equality
does not establish original executable-PC or exported-ABI parity. The remaining
compiler options, typed compiler exceptions, wider
execution, exported ABI, object/BIN loader, bootstrap and release requirements
remain open.

The frontend now exposes opaque lookup observations for the unused-extern
work. `Preprocessor.create` and the parser entry points accept a lexical
observer. It sees each identifier or keyword read once, including directive
operands and definition-expansion input. Captured definition replacements and
raw inactive-branch scans create no observations. Lookahead already read while
evaluating a directive retains its original observation. Each receipt retains
the physical token, writer environment, compilation mode, selected symbol and
stream ordinal. It expires when the callback returns, raises, reads another
token or switches environments; restoring an environment cannot revive it.
Repeated semantic queries create no observations.

Execution-enabled parsers now select a scoped lexer consumer through
`Parser.command_sink.lexical_lookup`. It receives the physically focused
compiler context and original receipt before the input's inspection observer.
The first lookahead occurs after sequence startup. A directive installs its
own consumer; a missing child service masks its suspended parent's consumer.
Every exit restores the predecessor and expires the last receipt. Consumer
errors retain reached diagnostics and abort the sequence; exceptions propagate
after cleanup. A lexer read does not advance command observation counts or
executable cursors.

The IR and native source drivers route those reads into their active declaration
ledger. Each ledger validates original source/runtime ownership, current
context, environment, mode and domain, and rejects an already-consumed stream
ordinal. Its read total records accepted source observations; it is not
`CHash.use_cnt`. The owned JIT function consumer separately checks its original
registry, source manager, writer environment, domain and selected entry before
incrementing a native count. The scoped service alone grants no execution or
native counter authority.

Function publications and non-extern aggregate publications retain a separate
kind-filtered join receipt before parameter or body input. JIT searches the
current writer's table; AOT also searches visible baseline entries. Extern class
forward declarations have no join receipt because `PrsClass` publishes them
directly. The selection precedes native extern/import filtering and does not
assert that the selected record will be reused. These receipts expire with the
original focused declaration callback, including on a later parser failure.

Owned JIT function records now allocate the 24-byte `CHash` prefix with the
pinned name, function type and U32 `use_cnt` field. Original lexer selections,
function joins and admitted implicit-output selections increment that shared
field through native bucket selection. Each registry owns a table containing
only its admitted function allocations. Fresh publications insert at the head
of their native bucket; extern reuse retains the allocation and insertion.
The original source receipt supplies the expected physical record. A different
native selection invalidates count knowledge and increments neither record.
Explicit function aliases follow their physical source ancestry; copied
names, origins and call shapes do not establish shared storage. Local member
selections skip hash counting. A parameter name is read before `MemberAdd`, so
that initial read can still select an existing function of the same name.

A joined explicit extern emits `HCSEMA0075`, `Unused extern '<name>'`, when its
known count is below three. The declaration-name read and join each increment
before this check. The warning precedes parameter input, increments the focused
warning counter, and survives a later parameter error. It ignores option 19.
The reused prefix then resets to zero before parameters are read. Bare
`I64 F();` defines an empty function: its record receives the join lookup count
before the non-extern filter, but is not reused or warned about as an extern.
Count updates do not advance executable revisions or grant call authority.

[The unused-extern example](../examples/compiler-unused-extern.hc) disables
option 19, emits one unused-extern warning, prints `42;` and returns 42. Its CLI
checks run through IR and native tasks in both outer modes. Reached `defined`
operands count; captured macro replacement text and skipped branch text do not.
The lookahead already read before skipping a branch still counts.
Warnings remain in the diagnostic stream when later source fails.

Every identifier or keyword read advances a source journal, even without a
consumer. Omitted reads invalidate count knowledge, including reads through
views that share or copy original entries. An untracked predecessor retains an
unavailable count; it cannot warn from an assumed zero. Reuse resets a known
owned prefix for the new header even when the preceding total was unavailable.

Source definitions publish their exact replacement objects
in symbol order, and expansion consumes the original lexer selection. Locals
and selected nondefinition symbols suppress expansion. Predefined fallbacks and
library definitions injected without a symbol entry remain separate metadata;
the frontend selection does not establish native hash-record ownership.

The native primitives also provide the original 32-byte `CHashTable` layout,
byte hash, head insertion, low-U32 type masks, selected instances and successor
search. The remaining instance spans table boundaries, and only the selected
record receives a wrapping U32 increment. Native leases keep buckets, records
and successors alive after their OCaml handles are collected. Duplicate record
insertion and cyclic table chains reject before mutation. The arithmetic and
selection are independently compared with the pinned x86-64 instructions,
including collisions, masks, misses, chain priority and U32 wrapping.

This registry table covers the function prefix and these admitted producers.
It does not reconstruct `Fs->hash_table`, `cmp.asm_hash` or an AOT chain, and is
not a complete `CHashFun`, `CCmpCtrl` or exported HolyC ABI. Full counts still
need the remaining compiler, assembler and loader producers, original task and
compiler table setup, and class/global/member records. Reached `try`, `catch` or `asm` input invalidates
the current admitted totals because those additional consumers are unfinished.
Ordinary AOT joins do not use this registry. The original sites are
Kernel/KHashA.HC:31-70, Compiler/Lex.HC:492-513 and Compiler/PrsStmt.HC:62-112.
Runtime calls and AST reference totals cannot replace these source observations.
