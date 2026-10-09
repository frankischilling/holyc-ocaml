# Compiler exceptions

Executable source now rejects `return` without an active function at the original
`PrsStmt.HC:1089-1091` check. `HCPARSE0168` points to the keyword. The check runs
before consuming it or reading the return expression, so a following `#exe`,
preprocessor error or invalid token cannot run first. The callback-free parser
still represents standalone return syntax for semantic and AST clients.

These audited producers follow `LexExcept` in `CExcept.HC:81-96`. Each increments the
original control's `error_cnt` at byte 344 and creates one private `Compiler`
receipt. Its diagnostic, count, exact context, position, observed events and
domain come from that original parser phase. A diagnostic with the same code,
message or span cannot create a receipt. Ordinary parser, preprocessor,
unsupported execution, authority and quota failures remain separate.

`ExePutS` copies the saved function context in `CMain.HC:582-585`. An inherited
active function therefore prevents this missing-function producer. Executable
returns through that saved function still require their original lowering and
report the unsupported boundary `HCPARSE0169` without incrementing the error
count or issuing a `Compiler` receipt. A nested `#exe` clears its own function
context; a completed new function also leaves the child without an active
function, following `PrsStmt.HC:206`. These source checks do not construct the
native `htc.fun` or `return_class` pointers.

IR compilation and execution reports and `Native_source_execution` expose the
reached receipts, including failed ordinary children. The IR execution error
retains its caller's stage and position; the receipt preserves the original
child keyword location. An ordinary child has fresh counts and copies live
options. A `#exe` directive shares the original parent allocation. Closed
contexts cannot read or change native fields; the
receipt retains the reached count and diagnostic. Physical ownership can still
be checked in the original domain after unwinding and collection. Receipt
ownership does not admit syntax, runtime work or an executable result.

A consumed suspended input can retain a private `failed_input` when an audited
original producer stops its parser. The receipt binds the exact suspension and
registered source to the complete abort chain, including nested `#exe` scopes.
Every scope must close at its original position and event count and deliver its
abort checkpoint successfully. An unexpected observer or cleanup exception, a
rejected abort checkpoint or a matching diagnostic cannot create this proof.

The original domain can inspect the failure after its parent closes. Claiming
it requires the unclaimed receipt and the still focused, unchanged parent.
Entering another child expires the earlier claim authority, even if the parent
has not advanced its command. Copied sources reject before consuming the token.
Collection preserves the original ownership, and a successful claim rejects
replay. Runtime, session, semantic table and resource checks must finish before
claiming; the parser receipt cannot complete or abandon runtime work itself.

Tests cover both outer modes, direct and block returns, loop bodies, failure
before expression lookahead, directives, nested inputs and children reached
inside a function. Earlier output and machine work remain charged. Later output
does not run. Active function returns and the representation parser retain
their behavior. Native field checks compare unsigned increment, including wrap,
against literal `INC 344` and guard the complete 360-byte prefix; field tests also
check child isolation, invalid operations and foreign domains. Built, staged
and installed CLI coverage uses the same source cases as the library tests.

Other original `LexExcept` producers and direct `throw('Compiler')` paths still
need their actual source phase and ownership. A direct throw need not increment
`error_cnt`. Full native function pointers, IC type-check interruption, native SysTry
header selection and SysTry/SysUntry calls remain open. Parsing can still reach a
later producer after a fault the native compiler would interrupt earlier.
PrintErr, AdamErr, LexPutPos, FlushMsgs, boot/debug behavior and native exception
stacks are not implemented by these structured reports.

`StreamExePrint` now catches these original child `Compiler` failures and returns
zero, following `ExePutS` in `CMain.HC:589-596`. It starts a private runtime scope
before entering the input and records each participating namespace under the
original shared resource owner. The exact session, semantic tables, registered
source, saved parent, domain, stream stack and source activation must match.
The complete physical lifecycle journals must account for every original abort.
All preflight checks finish before the parser failure is claimed once. Copied
contexts and lifecycle events provide no authority.

The scope retains snapshots of work present before entry and work reached in the
failed child. Only newly introduced child work can be excluded from enclosing
completion. Parent initializers, dimensions, defaults and bindings that invoked
the child remain parent work and must finish themselves. Reached instructions,
output, formatting work, allocations, declarations and quota charges remain.
Incomplete child scalar storage and callback bindings stay unavailable. Neither
the failed AST nor unfinished work becomes successful.

Successful children still require their original accepted sequence, admitted
execution and completed introduced work. Unhandled faults record a new failure
generation for their enclosing input. A later independent input captures its own
baseline; no catch restores an earlier failure counter. Ordinary diagnostics,
unsupported inherited returns, quota faults and unexpected observer or native
exceptions continue to propagate. A later fault remains a failure even after a
child Compiler exception has been caught.

For example, in an active directive, `I64 n=StreamExePrint("return 42;");`
completes its own initializer with zero and lets the parent continue. The child
still has its original failed syntax, diagnostic and counted exception receipt.
The matched producers include the missing-function return and statement checks
below. Runtime Break, additional original Compiler producers and direct throws
remain open, along with
the native exception stacks and terminal behavior described above. Source
function presence still does not lower an inherited-function return. Direct
string storage in IR defaults and ordinary AOT outer-table joins also remain
unfinished.


## Statement failures and break targets

The executable parser also matches these `PrsStmt.HC` checks. The representation
parser keeps its existing AST behavior and issues no Compiler receipt.

| Original check | Diagnostics | Pinned source |
| --- | --- | --- |
| `if` and `while` opening/closing parentheses | HCPARSE0052/0053/0058/0059 | 464-469, 490-497 |
| `do` trailing `while`, parentheses and semicolon | HCPARSE0063-0066 | 514-525 |
| `for` opening, condition semicolon and closing | HCPARSE0067/0069/0070 | 535-557 |
| `switch` opening, closing delimiter and body brace | HCPARSE0085-0087 | 592-618 |
| `goto` identifier | HCPARSE0075 | 1122-1123 |
| Statement terminators after completed output, expression, break, goto or value return | HCPARSE0046/0047/0072/0073/0076 | 1210-1213 |
| Missing break target | HCPARSE0170 | 1133-1136 |
| Missing SysTry/SysUntry function headers or trailing catch | HCPARSE0171/0080 | 842-850, 893-895 |

An invalid `break` consumes the keyword and reads the next token before checking
its target. Its diagnostic therefore points to that current token. A directive
reached during this read runs first; an earlier lexer or directive failure
prevents the later break producer. This differs from the missing-function
return check, which runs before consuming `return`.

Source break targets belong to actual parser scopes. While, do, for bodies and
switch regions supply a target. Blocks and if branches retain it. For
initializers and updates, lock bodies, try/catch bodies, new function bodies and
new compiler inputs start without one, as the original PrsStmt calls do. Each
scoped change restores the previous target on success or unwinding. These source
owners do not construct native CCodeMisc labels.

Try header checks run after the initial lexer read and before parsing the body.
They use Function-kind lookup in the current source tables, so a same-named
variable cannot supply a header and a header published during that lexer read
can be found. Both SysTry and SysUntry must be present. This establishes source
presence only; native header addresses, flags and the original calls still need
their full implementation. A parsed try/catch has no execution authority from
this check.

The shared statement fixtures check exact original diagnostic tokens, reached
counts, first-producer ordering, header lookup and scope restoration in both
outer modes. IR and native runs retain reached directive output. Saved inputs
catch the same original failures and resume their parents with zero. Native
runs require actual completed machine fragments and zero interpreted task
instructions. Matching callback diagnostics still cannot create throw or catch
authority. Other argument, expression, declaration and lexer producers and
direct Compiler throws remain unfinished.

## Call failures and argument metadata

The executable parser matches the following checks in `PrsExp.HC:380-560`:

| Original check | Diagnostic | Pinned source |
| --- | --- | --- |
| Missing Function-kind Print/PutChars header at the literal marker | HCPARSE0172 | 394-406 |
| Missing separator during captured fixed or variadic argument traversal | HCPARSE0024 | 440-452, 512-520 |
| Missing closing parenthesis after captured direct-call arguments | HCPARSE0025 | 532-536 |
| Implicit Print argument separator or parenthesized PutChars closing delimiter | HCPARSE0167 | 440-445, 495-503, 532-536 |

Print and PutChars header lookup happens before consuming even an empty marker.
A later lexer failure therefore cannot replace the missing-header producer.
Lookup uses the original Function-kind entry; a variable with that name cannot
supply a function header. Variadic Print requires a comma before another
argument whenever the current token is not a semicolon. A closing brace reaches
that argument check before the statement terminator check.

With no fixed parameters and no enabled variadic traversal, a nonempty marker
stays at the current token. Print with no variadic tail and unparenthesized
PutChars, including its variadic form, finish their argument and emission phases
without reading that literal. The statement terminator then issues HCPARSE0046
at the original marker. It does not join a following string or read a later
directive or lexer error. Empty markers still advance the lexer; Print's
variadic traversal still consumes its first expression.

Direct-call delimiter producers use the shape returned at the original call
start. An unshaped legacy or indirect call retains its existing diagnostics and
does not gain a Compiler receipt from those diagnostics. A missing operand or
unsupported default also remains separate until its underlying original
expression producer is implemented.

Each call or implicit argument receipt retains the exact successful callback
result after unwinding. Runtime catch preflight compares physical shape identity
with the ledger's original native argument capture. An equal copied shape, a
copied call start or a copied implicit selection cannot replace that evidence.
The original producer phase and complete failed-input journals must also match.
When an AOT saved input reaches a nested directive, that producer belongs to the
directive task's ledger. Preflight checks its closed original journal and shared
runtime resources alongside the saved input's own ledger.
These checks grant no authority to incomplete argument traversal or emission.

Tests exercise ordinary JIT source and caught saved inputs in both outer modes,
including a failure in a nested directive. IR and native parents resume with
their reached effects intact. Ordinary AOT calls without an owned native argument
phase remain outside this match. Full argument/expression producers, direct
Compiler throws, runtime Break and the native exception machinery remain open.
