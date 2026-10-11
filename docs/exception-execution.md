# Verified HolyC exception execution

Issue [#692](https://github.com/frankischilling/holyc-ocaml/issues/692)
covers source `try`, `catch` and `throw` in the bounded integer program
interpreter. Checked exception regions execute through the JIT
and AOT source path, including transfers across direct function-call
frames in the current thread. Whole-compiler and native execution
compatibility remain outside this scope.

This document records the TempleOS contract and the tests added for it. Native
exception execution, including machine stack restoration and native handler
addresses, is separate work in
[#706](https://github.com/frankischilling/holyc-ocaml/issues/706).
A native source command containing a `try` must report unsupported execution
until that work is implemented.

## Source behavior

`Compiler/PrsStmt.HC:842-902` creates three labels for every source pair:
the catch entry, the untry entry and the statement after both arms. It calls
`SysTry(catch_start,untry_start)` before the try body. The normal path runs
`SysUntry` and jumps around the catch. A throw instead enters the catch
without running the rest of the protected body. The catch ends with
`IC_RET`, which returns to the exception dispatcher; it does not act like an
ordinary function return.

The TempleOS task owns a queue of active `CExcept` entries
(`Kernel/KernelA.HH:3116-3121,3320-3325`).
`SysTry` allocates one entry and saves the catch and untry destinations,
the stack context and the registers needed by exception transfer.
`SysUntry` removes and frees the current entry
(`Kernel/KExcept.HC:64-82`).
Each invocation and each newly entered region gets its own entry; a
recursive function cannot reuse an earlier invocation's active handler.
The hosted interpreter tracks the active handlers separately from the
`CTask` queue fields whose kernel behavior is not modeled.
The pinned `CExcept` record occupies 120 bytes: two eight-byte queue links
and thirteen eight-byte context fields. The hosted program's frame-byte
limit charges 120 bytes for each live exception record. `SysTry` requires
another 16 argument bytes at its call peak and releases them when it returns.
The two `throw` arguments also occupy 16 bytes, which remain charged while
the dispatcher searches and executes handlers. Accepted transfer or the end
of unwinding releases those argument slots. Admission checks each peak
against already live frame allocations. Normal completion, handled transfer
and propagation release each handler record once.

TempleOS's `throw(I64 ch=0,Bool no_log=FALSE)` is an ordinary function. It saves its
numeric exception code in `Fs->except_ch`, captures caller addresses, clears
`Fs->catch_except` and visits active handlers from innermost outward.
Codes can be 64-bit integers; character literals of up to eight bytes are
the usual names. A bare `throw;` is a call using the default code `0`. It
starts a new exception; it is not C++ rethrow syntax. The second argument
controls AdamLog output and does not change the handler search
(`Kernel/KExcept.HC:84-100`; `Doc/HolyC.DD:230`).

A handler must explicitly assign `Fs->catch_except=TRUE` to accept the
exception. `PutExcept()` logs the exception and marks it accepted, but the
public fixture uses the task field directly because full address logging
depends on formatter and runtime work outside #692. A handler which returns
without marking acceptance lets the dispatcher inspect its next outer
handler. The code remains in `except_ch` during propagation. A nested throw
can replace the shared code and catch flag; later dispatch observes their
current values. Throwing a new value during catch execution can reenter the
same handler because its own entry is still active. The tests include a bounded
conditional reentry which terminates by marking the second exception
accepted. Without a terminating condition, such code can repeatedly enter
the handler.

An accepted exception resumes at the original untry destination, removes
its handler once, then continues after the paired catch. A rejected
exception removes that entry once before consulting the outer handler.
Every skipped statement remains skipped. Earlier mutations and output from
the protected block or its callees remain observable. If no handler accepts
the exception, the source runtime calls `PutExcept(FALSE)` and enters
`Panic("Unhandled Exception")` (`Kernel/KExcept.HC:100-112`). A hosted
unhandled exception is a runtime failure; it must not produce a successful
final word.

## Task layout and field access

The source contract uses the current task fields. The fixture header binds
`public _intern 21 CTask *Fs();` and declares `SysTry`, `SysUntry` and
`throw` with their original signatures. It supplies a partial `CTask` view
through `catch_except`; an explicit byte array represents fields preceding
`except_ch`. This preserves the original offsets of the exception code and
catch flag.

The offsets are independently derived from
`Kernel/KernelA.HH:41,3255-3256,3271-3325`. The initial pointer and task
metadata through `draw_it` consume 456 bytes. The title, name and wallpaper
arrays consume 144 + 32 + 144 bytes, followed by four `U8` color/source
fields, two `U16` fields and the eight-byte stack pointer. The pair of
`CExcept *` queue links then places `except_rbp` at 808,
`except_ch` at 816, eight caller pointers at 824 through 887 and
`catch_except` at 888. After the helper declarations, the fixture defines
a checked function and calls it from the source stream:

```holyc
I64 ExceptionLayout(){return offset(CTask.except_ch)==816 &&
  offset(CTask.except_callers)==824 && offset(CTask.catch_except)==888;}
if(!ExceptionLayout())throw('Layout');
```

`ExceptionLayout` supplies the checked aggregate context for each semantic
`offset` query. A raw top-level `offset(CTask.field)` expression cannot
resolve this fixture's aggregate, and the preprocessor's `#assert` has no
semantic member-offset support (`HCPP0022`). The reached function call
raises `Layout` if any of the three offsets has changed, without extending
preprocessor evaluation. Reached source execution lazily allocates 889
task-owned bytes, charged to the cumulative global-byte limit, to represent
the prefix through the catch flag. The runtime initializes only
`Fs->except_ch` and `Fs->catch_except`. Other bytes within that prefix begin
unknown; ordinary owned-memory writes can initialize them, and reads retain
the normal initialization and bounds checks. Such writes do not implement
the kernel's exception queue or caller-address capture. Fields beyond the
889-byte prefix lie outside this allocation. A foreign pointer or ordinary
user allocation cannot supply `Fs` authority.

## Control transfers and frame lifetime

`Compiler/PrsStmt.HC:1089-1119` emits one `SysUntry` for each active
try region before the expression of a source `return`. Verified function
returns must therefore release each active handler exactly once, including
when nested `try` statements return early. The value expression keeps its
original evaluation order. Returning *from a catch* bypasses the dispatcher
and is explicitly warned against in `Demo/Exceptions.HC:1-2`; checked
execution rejects this form.

`goto` emits an ordinary `IC_JMP` without exception-stack cleanup
(`Compiler/PrsStmt.HC:1121-1132`). The interpreter can execute a goto
within the same protected arm, but a transfer into a try or catch would
skip registration, and a transfer out would leak its handler. Checked
source lowering must reject a jump whose source and destination have
different protected-arm ancestry, including unreachable code and jumps
between sibling arms. The source parser does not pass an outer loop's
break label into either arm (`PrsStmt.HC:877,899`). A `break` directly in
the try or catch of an outer loop is rejected; a break in a loop introduced
within the arm is valid.

Calls and automatic storage have independent invocation lifetimes. The
hosted exception dispatcher keeps a throwing child's frame live during
catch evaluation and releases it after acceptance and transfer to the
normal continuation. The regression fixtures observe this through frame
byte limits, without escaping a pointer to the child's automatic storage.

`Child` allocates `I64 Held[64]` (512 bytes), writes `Saved=42` to an
ordinary retained integer global, then throws. `Inspect` allocates a
separate `I64 Scratch[64]` (512 bytes) and reads `Saved`. Calling
`Inspect` during the catch reaches a frame-budget peak of
512 child bytes + 120 active `CExcept` bytes + 16 retained throw-argument
bytes + 512 inspection bytes = 1160 bytes. The test requires success
at 1160 and failure at 1159. Calling `Inspect` after the catch finishes with
`Fs->catch_except=TRUE` and the dispatcher resumes normal execution no longer
overlaps the child's frame. The earlier peak is 648 bytes
(512 + 120 + 16), so that separate fixture specifies success at 648 and
failure at 647. It repeats the accepted-throw and inspection sequence to
check that the child frame has been released between invocations. Both
compilation modes exercise these exact and one-below frame budgets.

A separate supported case passes the address of a *parent-owned* local
to `Child(I64 *result)`. The child writes `*result=42` before throwing;
the parent observes the value after its catch. Global/static
data-pointer variables and escaped child-local addresses are outside
the current source execution path, so neither appears in the lifetime
tests. Reached global updates and output remain visible across a throw;
hosted frame and step limits still apply to the work actually reached.

## Failures eligible for catch

Only a reached source `throw` or another source-grounded exception producer
creates exception transfer within its current thread. The arithmetic
trap is a concrete source producer: `Compiler/BackA.HC:355-439` emits
`DIV`/`IDIV` for `/`
and `%`, and `Kernel/KInts.HC:150-164` raises `throw('DivZero')` on
core zero. This covers division or remainder by zero and the signed
`I64_MIN / -1` quotient overflow, including when only the remainder is
used. The captured TempleOS
[integer-division oracle](../test/oracle/integer-division.json)
records `FAULT=DivZero` for both categories and confirms continuation
after catch. This is an existing oracle fixture, reused for the arithmetic
cases; this work has made no fresh TempleOS runtime capture. The low-level
interpreter and native ABI still distinguish their own fault categories for
uncaught diagnostics. Pure constant preparation reports arithmetic errors
without allocating task exception storage or a throw frame.

`CExcept.HC:98-101` establishes `'UndefExt'` as the source code raised
by `UndefinedExtern`. A reached call through a checked ordinary unresolved
extern delivers that code in the hosted interpreter. JIT and AOT fixtures
cover this call path as well as explicit throws of the same value. Missing
or foreign binding evidence and failed call preflight remain diagnostics;
they do not execute `UndefinedExtern`.

`CExcept.HC:81-96` throws `'Compiler'` after its own error-count side
effects; several direct compiler throws use the same code without that
increment. Compiler-local handlers in `PrsExp.HC:278-291` and
`CMain.HC:77-103` can consume such failures before user source execution
resumes. At the hosted `ExePutS` boundary, the selective handler consumes a
genuine authenticated parser `Compiler` receipt and returns zero. It sets
`Fs->except_ch='Compiler'` and `Fs->catch_except=TRUE`, lazily allocating
the 889-byte task exception view when needed. This local handling does not
deliver the parser failure to an enclosing source catch. Ordinary parser
diagnostics without the original receipt remain diagnostics.

Host callback/provider exceptions, failed runtime image preflight, invalid
addresses, expired frame references, memory and output limits, step limits,
call-depth exhaustion and frame-budget faults remain structured host
failures. They cannot be transformed into `Fs->except_ch` values or
silenced by a source catch. The generic processor fault dispatcher
(`Kernel/KInts.HC:61-91` and `Kernel/KDbg.HC:596-627`) goes to the
debugger, not the HolyC exception queue.

## Nested `StreamExePrint`

`Compiler/CMain.HC:673-689` formats the nested source and calls
`ExePutS(buf,,,htc)` at line 685 using the saved compiler context.
Inside `ExePutS` (`Compiler/CMain.HC:589-598`), the catch accepts only
`'Compiler'` and `'Break'`. It sets `Fs->catch_except=TRUE` and returns
a zero result for those two codes. The hosted boundary also consumes
authenticated parser `Compiler` receipts and updates `Fs->except_ch` and
the catch flag, charging the same lazy task storage. The catch leaves
other source exception codes unmarked, so the original dispatcher
continues looking for an enclosing handler.

Issue #692 adds a private source-exception channel across the nested
`StreamExePrint` execution boundary. An explicit exception code other
than `'Compiler'` or `'Break'`, or a reached `'DivZero'` from the
child, can continue to an enclosing source `try`/`catch`. Earlier child
effects and output remain. Host resource limits, invalid pointers and
callback/provider failures remain fatal host diagnostics, including
when the caller has a catch.

The bridge tests cover runtime `'Compiler'`/`'Break'` handling, consumption
of genuine parser `'Compiler'` receipts, task state and global-byte
charging, and fatal provider errors. A nested 800-byte frame limit checks
that child frames remain charged during catch evaluation and can be
released and reused after acceptance. Native exception propagation remains
separate under #706.

## Evidence and tests

The source contract comes from TempleOS commit
`c26482bb6ad3f80106d28504ec5db3c6a360732c`, including
`Compiler/PrsStmt.HC`, `Compiler/CExcept.HC`,
`Compiler/BackA.HC`, `Compiler/BackB.HC`,
`Kernel/KExcept.HC`, `Kernel/KInts.HC`, `Kernel/KernelA.HH`,
`Kernel/KernelC.HH`, `Doc/HolyC.DD` and `Demo/Exceptions.HC`.
The tests reuse the original source signatures and derived field offsets.
They cover top-level and function-local execution, normal exit, inner and
outer handlers, conditional reentry, zero and named codes, no-log throws,
nested and recursive calls, thrown call arguments, parent-owned pointer
writes before a throw, preserved output, early returns, return operands
evaluated after the current handler is unregistered, legal internal
jumps, illegal protected jumps, reached `UndefinedExtern`, divide/modulo
traps, exact child-frame peak/release budgets, runtime limits and repeated
invocation.

`test/exception_execution_cases.ml` defines the shared cases.
`test/test_exception_execution.ml` exercises the public library source
runner in both compilation modes. `test/test_exception_execution_cli.ml`
checks JSON source runs in both modes, including final values, exact output,
diagnostics and exact/one-below step limits, plus native rejection under #706.
`test/test_exception_regions.ml` checks original parser headers, source
ancestry, function ownership and calling conventions, including retained
task inputs. It rejects reconstructed source children, foreign records and
owners, copied or moved markers, and control edges that cross protected
regions. `test/test_exception_source_bridge.ml` exercises the nested source
boundary and shared resource accounting.

The case table records implementation regression expectations derived from
the pinned source. The existing integer-division oracle supplies captured
TempleOS results only for the arithmetic cases. Full native exception
delivery, a complete `CTask` model, thread migration and the `PutExcept`
caller-address formatter remain outside #692.
