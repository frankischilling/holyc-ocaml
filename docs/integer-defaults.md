# Declaration-time integer defaults

`holyc run --mode=jit --format=json examples/integer-jit-defaults.hc` returns
I64 42 without a directive. Parsing `Saved` calls `Next()` once and saves 21.
The later assignment sets N to zero; both omitted arguments still receive 21.
Unused function definitions and prototypes also execute their expression
defaults at declaration time.

The first expression-default boundary activates the original JIT source journal,
including earlier commands and initializers. The triggering default executes
once through that journal; subsequent defaults use live callbacks. Compilation
reports expose the completed task and its separate units. Provider headers are
installed only when execution actually enters a `#exe` block. Ordinary inputs
without expression defaults or active directives retain their isolated path.

`holyc run --mode=aot --format=json examples/stateful-exe-defaults.hc` returns
I64 42. Inside the directive, parsing `Saved` evaluates `Next()` once and stores
21 as its parameter default. The later write to `N` leaves that value intact;
both calls to `Saved()` receive 21. StreamPrint emits the outer expression `42;`.
The example uses 58 runtime instructions, three preparation instructions, eight
global bytes, four literal bytes, 24 active frame bytes and seven formatting-work
units. Exact limits pass; reducing each of those six limits by one rejects.

The live task parser now evaluates supported integer defaults at their original
parameter boundary, after expression lookahead and before parameter delimiter
handling. This includes unused functions and prototypes. A later parameter or
directive sees the effects already reached. Function, parameter position, source
AST, identifier/query selections and retained environment stay attached to the
evaluation. No replacement declaration or provided argument AST is constructed.

Constant preparation and effectful evaluation reuse initializer guards, shared
expression/call lowering and task resource limits. A reached fault preserves
earlier writes and work, stops following defaults and leaves the last outer
expression value intact. Replayed, skipped or foreign attempts cannot complete
the function's original source command. A pure default fragment also requires
its exact environment even when it has no identifier occurrences.

Completed headers associate saved values with the exact original parameters.
Later top-level calls, retained function bodies, nested calls and subsequent
default expressions materialize those saved bits through the shared call path.
The immediate keeps the declared parameter class; the existing ABI entry path
narrows integer arguments. The runtime call context verifies the exact immediate
bits, type, origin and push position before admitting narrow or public-class
default producers. Task snapshots retain their own prepared-default set.

The implementation follows pinned `Compiler/PrsVar.HC:631-656` for evaluation
and `Compiler/PrsExp.HC:455-468` for later materialization. The reference remains
`c26482bb6ad3f80106d28504ec5db3c6a360732c`; these hosted checks are not a new
native TempleOS execution capture.

General default support remains unfinished: floating conversion, pointer and
owned string defaults, `lastclass` materialization, defaults nested inside
callback declarators, cross-command extern joins, partial-header calls, and
ordinary outer AOT declaration preparation still need
integration. The execution path operates in live parser tasks, including AOT
`#exe` bodies and activated outer JIT source. Outer JIT activation consumes
earlier original default receipts once at the first default or directive;
later defaults use their live parser callbacks.
Issue #635 and draft #636 remain open, together with the full compiler, native
backend, BIN/loader and bootstrap requirements.

Defaults containing string storage reject with HCRUN0006 before evaluation,
including an integer-returning call with a string argument. Native
`PrsExp.HC:691-704` marks this miscellaneous storage and `PrsVar.HC:649-652`
copies the resulting default with StrNew. Ordinary integer preparation cannot
stand in for that ownership operation.
