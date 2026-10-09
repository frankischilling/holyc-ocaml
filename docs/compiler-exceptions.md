# Compiler exceptions

Executable source now rejects `return` without an active function at the original
`PrsStmt.HC:1089-1091` check. `HCPARSE0168` points to the keyword. The check runs
before consuming it or reading the return expression, so a following `#exe`,
preprocessor error or invalid token cannot run first. The callback-free parser
still represents standalone return syntax for semantic and AST clients.

This producer follows `LexExcept` in `CExcept.HC:81-96`. It increments the
original control's `error_cnt` at byte 344 and creates one private `Compiler`
receipt. Its diagnostic, count, exact context, position, observed events and
domain come from that original parser phase. A diagnostic with the same code,
message or span cannot create a receipt. Ordinary parser, preprocessor,
unsupported execution, authority and quota failures remain separate.

IR compilation and execution reports and `Native_source_execution` expose the
reached receipts, including failed ordinary children. The IR execution error
retains its caller's stage and position; the receipt preserves the original
child keyword location. An ordinary child has fresh counts and copies live
options. A `#exe` directive shares the original parent allocation. Closed
contexts cannot read or change native fields; the
receipt retains the reached count and diagnostic. Physical ownership can still
be checked in the original domain after unwinding and collection. Receipt
ownership does not admit syntax, runtime work or an executable result.

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
`error_cnt`. Full native function pointers, IC type-check interruption, SysTry
header selection and SysUntry calls remain open. Parsing can still reach a
later producer after a fault the native compiler would interrupt earlier.
PrintErr, AdamErr, LexPutPos, FlushMsgs, boot/debug behavior and native exception
stacks are not implemented by these structured reports.

`StreamExePrint` still propagates failed child diagnostics. Its original
`ExePutS` consumer catches only Compiler and Break, returns zero and preserves
reached effects. Completing that behavior requires authentic failed-input
ownership and safe exclusion of that child's unfinished work from enclosing
completion. Restoring a failure counter or marking unfinished work successful
would admit invalid execution and is not a catch implementation.
