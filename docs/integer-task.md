# Incremental integer task execution

`Holyc_lib.Integer_task` compiles separate JIT commands against retained global
objects. For example, running `I64 N=40;`, then `N+=2;`, then `N;` in one task
returns 42 from the last two commands. The initializer runs once. Fixed arrays,
narrow integer storage, updates, references passed to newly compiled functions,
and checked Print/PutChars arguments use the same storage rules as a complete
integer program.

Create a task with `Integer_task.create session`. Add each source to that session
and call `Integer_task.run task ~source`, or pass a parsed module to `compile_ast`
and execute the resulting opaque command. `output_bytes`, `output_work`,
`executed_steps` and `initializer_steps` expose cumulative task results. The
existing complete-program APIs still allocate fresh execution images.

## Ownership and timing

Compilation captures an immutable view of the task's published globals. Each
entry retains the exact semantic symbol, declaration metadata and final storage
object. A checked outer identifier emits a `Retained_global` IR payload carrying
an opaque reference. A spelling, numeric symbol ID or ordinary Symbol payload
cannot select an earlier command's object.

The VM retains each admitted command's fixed-size allocation. Old arrays and
scalars keep their original storage identity and offsets after later allocations.
Earlier commands never contribute new initializer work to a later command.
Unknown JIT cells remain unknown until reached writes initialize them.

Every body and entry passes preflight before new storage becomes visible.
Preflight failure leaves runtime cells, output and execution receipts unchanged.
Compilation preparation is a separate phase: work already performed there stays
charged even if later compilation or preflight fails. Once execution starts,
success and faults both consume the command. Reached writes and output remain.
Recompiling the exact parsed command, including module/item/statement wrappers
around its retained contents, cannot execute it again. Separately parsed commands
may contain identical text.

A pending command keeps its selected object even if another admitted command
shadows that name. Thus an earlier compiled `N+=2;` still updates the earlier N,
while a newly compiled `N;` selects the newer declaration. Parser-aware pending
and predecessor receipts are still needed before connecting this API to #exe.

## Bounds

Positive creation limits cover cumulative runtime instructions, constant
preparation, global bytes, literal bytes, output bytes and formatting work.
Preparation includes pending commands and reached constant-evaluation faults;
commands requiring no preparation remain possible when that budget reaches zero.
Frame bytes and call depth bound active calls. Rejected runtime admission does
not charge new global/literal allocations. Literal limits include every site's
terminator, including unreachable sites. Function activations are invalidated
when they return or unwind; admitted task allocations survive command completion.

`test_integer_task.ml` covers separate scalar/array commands, narrow updates,
unknown cells, independent tasks, selected references after shadowing, calls and
formatting with retained arrays, preflight/fault behavior, replay and cumulative
limits. Its preparation controls include a configured limit above 100,000.

## Remaining #635 work

This API does not yet retain callable function definitions across commands or
connect StreamPrint to parser generation. Calling a newly compiled function
within its own command works, including access to earlier globals. Calling that
function from a later command remains unsupported. Old function-owned statics,
mutated literal sites, extern joins, provisional declaration publication,
selected query receipts, parser pending-command authority and the fourteen
maintained #exe execution groups remain part of issue #635.

The reference remains `c26482bb6ad3f80106d28504ec5db3c6a360732c`. The hosted task
tests do not constitute a new native TempleOS capture.
