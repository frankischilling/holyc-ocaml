# Function-local goto execution

Issue #664 connects the existing label resolver and goto fragment lowerer to
checked source execution. Both `run --target=ir` and `run --target=host-jit`
accept function-local language labels and direct `goto` statements in their
otherwise supported function bodies. JIT and AOT select preprocessing mode;
the native target still executes an isolated generated image, not a TempleOS BIN.

```text
opam exec -- dune exec --root . -- bin/holyc.exe run --target=ir --format=json examples/integer-goto.hc
opam exec -- dune exec --root . -- bin/holyc.exe run --target=host-jit --mode=aot --format=json examples/integer-goto.hc
opam exec -- dune exec --root . -- bin/holyc.exe dump-ir --program examples/integer-goto.hc
```

The maintained fixture returns I64 42. Its independent checks exercise forward
and backward jumps, skipped side effects, equal names in separate functions,
consecutive and trailing labels, nested loops and breaks, reordered `for`
updates, recursive frames, narrow saved defaults and U0 procedures. Additional
source tests jump directly into conditional and loop bodies, bypass their lexical
condition or initializer, and retain the selected body's normal continuation.

```c
I64 Walk(I64 count)
{
    I64 result=0;
again:
    if (count==0) goto done;
    result+=7;
    count--;
    goto again;
done:
    return result;
}
Walk(6);
```

This returns 42. `again` and `done` are ordinary labels. `start` and `end` are
reserved sub-switch keywords and are not interchangeable example names.

## Source identity and shared lowering

Semantic preparation resolves every function's labels using the original
function collection and source AST. The existing semantic resolver establishes
function-wide scope, stable label identities, forward and backward references,
and missing/duplicate checks before publishing labels. Equal spelling in another
function does not provide a target. Unused definitions and unreachable source
retain their validation requirements.
TempleOS's statement classification still takes precedence: an identifier already
visible as a local variable or callable starts an expression rather than a label
definition, so a following colon rejects. An earlier label may share a spelling
with a local declared later; that label must not hide the later object's binding.
The source consumers are `PrsStmt.HC:1169-1184`, not C label-name rules.

The driver associates each original AST statement with its exact resolved
occurrence and owner. Lowering consumes that occurrence once, verifies its kind
and source evidence, and rejects foreign, missing, duplicated or unconsumed
input. Identifier provenance remains distinct from the complete original jump
statement span. Included and generated source keep their original locations.
The resolved occurrence retains the complete statement origin separately from
the target identifier origin. A macro may supply only `goto` or only the target,
so those primary spans can belong to different expansion frames. Composition
checks the retained original statement span exactly rather than assuming that
the identifier must lie inside it in the same physical source.

`Ir.Goto_label_lowering` assigns stable target blocks. The structured integer
lowerer reserves those identities after entry and shared leave blocks, before
allocating conditional or loop blocks. It derives targets from the checked
mapping instead of accepting an unrelated caller-selected block number.

A `goto` emits the existing checked `IC_JMP`. A language label ends the current
block and starts its reserved target, with ordinary fallthrough where needed.
The composed executable graph contains no `IC_LABEL` operation. Consecutive
labels may therefore produce empty fallthrough blocks, and a trailing label
falls through to the existing function leave. Labels after an unconditional
transfer or return still begin independently reachable blocks.

Source order and emitted order are not interchangeable. A `for` update is parsed
before the body but executes after it. The occurrence association follows the
original AST identity, allowing a goto in that update to target a label in the
body without consuming the wrong occurrence or reassigning its block.

This is one shared source-to-graph path. The interpreter and native backend use
their existing jump and fallthrough consumers; native execution does not route
the new statements through an interpreter fallback.

## Storage, calls and returns

An ordinary jump changes control flow without recreating the invocation frame.
Local writes survive backedges, and each recursive call retains its own values
and initialization state. Skipping a declaration's initializing store does not
invent a value: a reached read or update of that uninitialized object faults
through the existing checked storage mechanism. This initialization policy is a
hosted safety boundary, not a claim about arbitrary uninitialized TempleOS memory.

Structured branches, loops and breaks keep their original targets. A label
inside a supported nested block remains function scoped. Direct-call arguments,
saved defaults, complete original preparation evidence and caller continuation
retain the contracts in [native scalar functions](native-scalars.md) and
[native defaults](native-defaults.md).

U0 procedures may reach a trailing label and fall through or take a bare return.
A reached top-level U0 call still clears the final numeric value. Word-returning
native functions retain whole-graph return-completeness validation: a jump to a
path without a required word does not bypass that gate. The ordinary interpreter
retains its reached missing-return diagnostic.

## Bounds and unsupported regions

Each reached `IC_JMP` consumes an instruction step. Structural empty-block
fallthrough consumes none. Every cycle introduced by language gotos contains
an executable jump, so it cannot evade the step budget through empty labels.
The native instruction, block, code, per-owner frame, active semantic-frame,
call-depth and physical-stack bounds remain in force. Checked failure unwinds
the generated call chain, and a fresh invocation starts with fresh status.

Top-level language labels and gotos remain rejected. This gate does not add
assembly-label execution, computed/indirect goto, switch dispatch or jumps
through lock/exception/sub-switch regions. Those enclosing unsupported forms
still reject, including when unreachable. Full label-warning behavior,
output-address validation for repeated assembly labels, general memory,
exceptions, optimizer parity and object/BIN/loader/bootstrap work remain separate.

## Evidence and verification

Reference commit: `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
`Compiler/PrsStmt.HC:1121-1131` resolves a goto target and emits `IC_JMP`;
`1182-1199` publishes a language label, rejects duplicates and rejects global
labels. `Compiler/PrsLib.HC:152-162` searches the current code-control label
namespace; `79-97` records the emitted instruction's flags and source line.
`PrsStmt.HC:459-565` supplies structured loop and `for` execution order.
The existing semantic and fragment APIs from issues #172 and #514 remain the
source of checked scope and target identity.

Ordinary tests cover public source execution, unsupported targets, exact original
diagnostic source IDs and spans (including included files), deterministic graphs,
initialization faults and exact step budgets. Parser-valid sources exercise each
excluded assembly, switch, lock, exception and sub-switch region at the execution
gate. Word-return completeness tests distinguish a reached interpreter fault
from native pre-entry rejection and retain positive U0 and returning-word controls.
Separate native API and CLI tests exercise the same source families in both
modes, with independent expected values and fresh public interpreter runs.
Exact native runtime meters use the corresponding isolated checked unit rather
than subtracting stateful JIT declaration-task work. Low-level composition tests
exercise owner and occurrence rejection without executing machine code.
These are source-derived and hosted-execution checks, not new TempleOS oracle
captures or whole-language compatibility claims.
