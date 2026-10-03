# Function joins across task commands

`holyc run --format=json examples/stateful-exe-function-joins.hc` returns 42
in JIT and AOT modes. Separate directive commands replace two extern headers
and then define their function. Each header evaluates its own default once;
calls use the definition's saved value. The counter finishes at 41.

JIT joins reuse the newest unresolved extern in the current task table.
The canonical callable symbol stays the same, while the new definition keeps
its own source symbol, parameters, locals and frame. A completed definition
causes a new identity to shadow the old one. Earlier compiled calls to that
definition retain its body and dependencies.

Each accepted replacement preserves the prior record's accumulated flags.
Public visibility is replaced on each header; variadic and Ret1 flags remain
sticky. A retained variadic flag prevents a later fixed header from newly
deriving Ret1. Header comparison can receive the prior header's exact saved
default inputs without evaluating its syntax again.

The semantic passes retain the exact predecessor declaration, mode and
classified flag record. Task admission requires that predecessor to remain
the newest publication. Old publication snapshots remain available to compiled
consumers. An earlier unresolved extern call can reach a later definition
through its exact joined declaration ancestry. It keeps its original header
and saved defaults; a new JIT shadow cannot replace that target. See
[extern call publication](integer-extern-calls.md).

The behavior follows the pinned `Compiler/PrsStmt.HC:63-139` and
`Compiler/PrsVar.HC:378` at `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
JIT uses the current hash table for joins. AOT directive commands use their
separate JIT task; this does not join the isolated outer AOT image to that task.

`Integer_task.run` uses the same original runtime callbacks across separate
inputs. A later header can evaluate its default against live task storage and
join the newest retained unresolved extern. See [incremental task inputs](task-inputs.md).

Native extern address-slot updates, parent-table joins, general default values,
alternate bindings and native linkage remain unfinished.

Callback cells now retain their original recursive declarator and physical
storage class separately from the callback return type. Their symbolic frame
addresses and scalar frame loads check the exact frame binding and declarator.
Calls retain their original callee value before arguments, and the callee
fragment loads and captures a supported frame callback in RAX. Resolved function addresses now retain their original registered publication,
prepared body and executable owner in the IR runner. Scalar automatic callback
locals and named callback parameters can store, copy, clear and compare those
values in JIT and AOT mode. An isolated JIT source bundle retains earlier
function-address values across same-name replacement. Scalar frame callbacks
now invoke the captured original prepared body, including an earlier executable
after a later same-name publication. The callee is captured before right-to-left
arguments. Reached target mismatches preserve earlier argument effects.
Live task address linking, callback globals/statics/arrays, owned-code defaults and native
invocation remain under [issue #801](https://github.com/frankischilling/holyc-ocaml/issues/801). The broader callback and mixed-value ABI requirements remain in
[issue #688](https://github.com/frankischilling/holyc-ocaml/issues/688).

Named-function callback parameters can now reuse their original saved integer
default. Declaration-time effects occur once; omitted arguments materialize the
saved word using the callback parameter's physical pointer class. That word
does not become executable code. Function addresses encountered after a default
activates the JIT task retain their original outer occurrence, declaration and
registered code owner, including an earlier body after same-name replacement.
Anonymous signatures now have their own original declaration scope, ordered
member/default receipts and successful saved-word preparations. The source
ledger evaluates each integer default once and publishes it with that signature,
including defaults inside unused function bodies. Indirect invocation consumes
the original callback default rather than the executable target's default.
Closed expressions work in both IR modes; JIT references and effects retain the
original task snapshot. Numeric callback-member defaults have no executable
authority. Equal signatures, copied producers and raw graphs cannot adopt the
saved value. Anonymous compiler-position defaults now retain each original
lexical write and exact `$$` node. Nested headers leave their last write current,
and the default evaluator saves that checked integer once. Fresh named AOT
headers use their own source member cursor; reused or unknown AOT headers have
no inferred position. Defaults containing owned
executable values still need their own preparation and value receipts.
