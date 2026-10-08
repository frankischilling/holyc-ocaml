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
phase. This counter does not provide a native CCmpCtrl object, the remaining
LexWarn/LexExcept counters or TempleOS terminal formatting.

Full warning timing remains unfinished. TempleOS tests unused locals after
`COCCompile`; this pipeline emits after checked integer lowering, before native
code generation. An ordinary isolated module emits its warnings after the
whole module compiles, so an earlier function's warning is not retained when
later source prevents module compilation. Ordinary AOT joins, broader default
and miscellaneous-data behavior, parentheses, duplicate-type and return-warning
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
`CHash.use_cnt`. The next consumer still needs original hash-record ownership
and shared counts before it can emit an unused-extern warning. The scoped
service alone grants no execution or native counter authority.

Function publications and non-extern aggregate publications retain a separate
kind-filtered join receipt before parameter or body input. JIT searches the
current writer's table; AOT also searches visible baseline entries. Extern class
forward declarations have no join receipt because `PrsClass` publishes them
directly. The selection precedes native extern/import filtering and does not
assert that the selected record will be reused. These receipts expire with the
original focused declaration callback, including on a later parser failure.

These observations do not yet increment native `CHash.use_cnt` or emit unused
extern warnings. Source definitions now publish their exact replacement objects
in symbol order, and expansion consumes the original lexer selection. Locals
and selected nondefinition symbols suppress expansion. Predefined fallbacks and
library definitions injected without a symbol entry remain separate metadata;
the frontend selection does not establish native hash-record ownership. The
remaining consumer must validate
record ownership, share counts across actual reused records and aliases, wrap
increments as U32, and warn before resetting a joined record's count. The
original sites are Kernel/KHashA.HC:31-70, Compiler/Lex.HC:492-513 and
Compiler/PrsStmt.HC:1-112. Runtime calls and AST reference totals cannot replace
these source-phase observations.
