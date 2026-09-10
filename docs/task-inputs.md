# Incremental task inputs

`Integer_task.run` parses an exact registered source in the task's persistent
frontend. It evaluates supported integer defaults, initializers and array
bounds through their original parser callbacks. Each command compiles and
executes when the parser resumes it after lookahead. Separate inputs use the
same task globals and retained function declarations.

For example, after `I64 N=38;`, successive inputs
`extern I64 Saved(I64 n=++N);` and
`I64 Saved(I64 n=++N){return n+2;}` evaluate each default once. `Saved;` then
returns 42 without changing `N`. A later `I64 A[++N-39];` allocates two elements
and leaves `N` at 41.

Earlier reached commands survive a later failure. In `N=42;1/0;N=99;`, the
division fails after the first assignment; a later input can read 42. Reached
storage and resource charges also remain. Parsing or execution failure does
not roll back declarations or earlier writes. Per-command execution charges
each command's terminating instruction, including provider declarations.

Each successful result retains only that input's last root expression value,
with cumulative task instruction and preparation counts frozen at its original
accepted completion. A declaration-only input has no final value. A prior
failed input does not prevent later successful inputs, but failure or unfinished
work during the current input prevents its successful completion.

An input can run inside an existing generation buffer. It must restore the
exact buffer stack it started with; replacing a buffer at the same depth does
not satisfy this requirement. Its root value is returned independently of the
outer task's previously captured value. Nested generated source and temporary
initializer execution do not replace that input's root value.

The VM retains each input's original context, completion receipt, work boundary
and result. Delayed starts, unaccepted completions, foreign tasks and stale
completion receipts cannot certify a new result. Retained identifiers in local
initializer expressions preserve their original function occurrence; matching
names and locations alone do not establish ownership.

Implicit string output through a retained `Print` or `PutChars` declaration
still requires original target-selection support. An inline provider header
followed by implicit output is now separate commands in `Integer_task.run`
and reports HCRUN0003. The callback-free `compile_ast` batch path retains its
existing local-header behavior. Explicit supported provider calls require
checked declarations already installed in the task.

This entry point executes the checked integer subset. It does not complete
ordinary AOT dynamic defaults or bounds, general default values, native extern
linkage, the native backend, BIN loading or bootstrap.
