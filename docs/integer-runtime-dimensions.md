# Declaration-time array dimensions

`holyc run --mode=jit --format=json examples/integer-runtime-dimensions.hc`
returns 42. `Next()` runs once while the declaration is parsed, and its result
sets the array's fixed extent. Changing `N` later does not resize the array.
The example uses 41 runtime instructions and nine initializer preparation
instructions, with no captured output or closed-dimension preparation work.

Ordinary JIT bounds can read retained globals, update them, and call supported
functions. The same bounds work inside `#exe` in either outer compilation mode.
Automatic and static local bounds run when the function declaration is compiled,
including declarations in functions that are never called. They do not run again
when the function is called. Parameters and future automatic storage do not
provide a live frame for this temporary expression evaluation.

The pinned `Compiler/PrsVar.HC:247-283` calls `LexExpressionI64` after reading
each bound expression and before checking its closing bracket. The hosted path
preserves that ordering: directives reached during expression lookahead run
first, and bound effects survive a negative extent, missing bracket, or later
failure. Each later bound requires successful completion of its predecessor.
This is checked against the pinned source; no native execution trace was captured.

Closed numeric bounds keep their existing numeric-visit preparation charges.
Runtime bounds share their task's initializer, instruction, storage and output
allowances. Their temporary result does not replace the preceding ordinary
expression result. Grammar, layout and `sizeof` reuse the prepared count.

The parser scopes preparation and completion receipts to their original
callbacks. Semantic shapes retain explicit dependencies on runtime outcomes;
the VM checks the owning task, original receipt, returned count and preparation
work before consuming them. These dependencies survive constant folding and
function-frame construction, including unused static storage. Standalone
execution cannot borrow a task's prepared extent.

Ordinary AOT runtime bounds still report `HCRUN0006`: output relocation and
callable address authority remain unresolved. Closed AOT bounds retain their
existing path. Runtime F64 bounds, aggregate-member bounds, general callbacks
and the remaining initializer optimizer cases are outside this implementation.
