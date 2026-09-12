# Source definitions joined to extern prototypes

Issue [#623](https://github.com/frankischilling/holyc-ocaml/issues/623) connects
source definitions to the callable identities selected by preceding extern
prototypes. Renamed parameters and definition-owned locals execute normally:

```c
extern I64 Add(I64 a, I64 b);
I64 Add(I64 x, I64 y) {
    I64 sum = x + y;
    return sum;
}
(Add(20, 22));
```

```text
holyc run --mode=jit --format=json --step-limit=29 --frame-byte-limit=24 --call-depth-limit=1 examples/integer-joined-definitions.hc
holyc run --mode=aot --format=json --step-limit=29 --frame-byte-limit=24 --call-depth-limit=1 examples/integer-joined-definitions.hc
```

Both modes report final I64 42, 29 runtime instructions, zero preparation,
empty output and zero output work. The same limits apply to the original Add
without its prototype: the prototype creates no frame or executable work.
Limits of 28 instructions or 23 frame bytes fail explicitly. CLI v2 and explicit
`--report-version=1` retain their existing reporting contracts.

## Callable identity and definition ownership

The canonical symbol selected by a call can differ from the definition's own
header symbol. `Function_body.with_definition` checks the association using
the exact classified Definition declaration, typed source and frame. The body
symbol, scope, parameter/local members, statics and frame remain owned by the
definition. `callable_symbol` supplies the canonical identity for callee lookup,
duplicate checks and initializer traversal.

Typed call results retain physical declaration membership through the checked
call-resolution and conversion-policy producers. Frames retain their exact
input header. A replayed declaration chain around an original header, or a
reconstructed header with equal symbol/scope/item values, cannot substitute for
either producer. The adaptor also checks return type, members, stored flags,
compiler options and declaration position. Invalid associations report
HCIR0027. A bound body requires its exact associated frame during VM preflight.
Raw `Function_body.create` preserves its previous identity and frame contract;
it exposes no unchecked alternate callable symbol.

Every source call retains its selected declaration snapshot. An extern call
can acquire the executable published by an exact later join, while keeping
its original header, argument protocol and saved defaults. Equal names or
symbol identifiers alone cannot establish this relationship: the selected
declaration must be a strict ancestor in the checked predecessor chain.

In this example, JIT captures A before the source definition is published;
the second call executes that body. AOT links the image's source definitions
before entry, so both calls execute the body and capture is empty. Both modes
leave G equal to 42:

```c
I64 G=0;
extern U0 PutChars(U64 ch);
PutChars('A');
U0 PutChars(U64 word){G=42;}
PutChars('B');G;
```

Post-definition calls use ordinary IC_CALL. Earlier selected extern calls
retain IC_CALL_INDIRECT2 in JIT or IC_CALL_EXTERN in AOT and resolve their
joined executable at invocation. A reached unresolved extern reports
HCIRVM0030 during execution; unused bodies and skipped calls may retain
unresolved externs. A published body incompatible with the captured return,
fixed parameter, variadic signature or cleanup reports runtime HCIRVM0014, without
falling back to a hosted provider. An approved provider remains available
until a source definition is published. Multiple supplied AOT bodies sharing
a callable identity still fail explicitly. A new JIT identity after a
completed definition cannot replace an earlier call's joined body; AOT
import barriers retain their existing source-resolution rules.

See [extern call publication](integer-extern-calls.md) for forward calls,
retained callers across task inputs and the remaining native linkage boundary.

Static storage and initializer calls use the actual definition owner.
Initializer arithmetic checks traverse the bound body, and JIT publication
uses its frame's definition item index. An earlier prototype cannot make that
body available before its definition. Exact initializer-expression ownership,
fault phase, prior output and fresh execution images remain checked.

## Dump format

For each distinct callable/definition pair, `dump-ir --program` adds a
`holyc-ir-function-binding-v1 reference=<pinned-commit>` component followed by:

```text
function=^f<function-id> definition=@s<definition-symbol-id> callable=@s<canonical-symbol-id> item=<definition-item-index>
```

The existing `holyc-ir-function-v1` body follows unchanged. Unjoined definitions
and raw body dumps keep their existing form. Tests compare exact IDs and item
positions and deterministic replay in both modes.

## Source evidence and historical verification

The reference is `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
`Compiler/PrsStmt.HC:62-109` selects JIT/AOT joins; lines 110-137 replace the
header; lines 151-190 compile and publish the definition and clear extern state.
`Compiler/PrsExp.HC:545-586` supplies call order, opcode and cleanup rules.
`Compiler/LexLib.HC:157-180` and `PrsStmt.HC:124-135` show that changed parameter
names can trigger an optional header warning without preventing the join.
Warning parity remains unfinished. These are pinned-source audits and hosted
tests, with no new native capture.

The following verification records the original #623 increment, before later
extern publication work. All four initial gates failed HCRUN0001 at the prior merge
`374a3184fd929521f4b5b36422b10abed4c3873f`. Fourteen maintained groups now cover
those gates, repeated prototypes, I64/U64/U0, pointers, recursion, statics,
initializer effects and faults, declaration snapshots, malformed provenance,
raw APIs, publication timing, resources and dumps. Local OCaml 5.4.1 / Dune
3.24.2 verification passed all 1,870 tests in 42.782 seconds and CLI checks;
the final strengthened dump assertions passed with all 14 groups in 0.072
seconds. Independent review found no remaining production blockers. Formatting,
generated-source and build/install checks passed, along with 82 pinned
checksums and all 11 incremental provenance scenarios. Lexer JSON matches the
preceding capture exactly: 528/528 with zero errors. Parser JSON and normalized
text match the committed AOT baseline: 25/528 standalone and 126/528 with the
prelude. Final source CI and merge evidence is recorded in #623.

Source-backed extern publication now includes retained task callers.
[Source-defined variadic frames](task-implicit-output.md) execute supported
integer tails, and [stateful compilation/#exe](integer-task.md) supports bounded
task commands and generated source. Machine-address reads, native import and
extern-slot linking, full native ABI/header-mismatch behavior, full formatting,
broader memory, optimizer parity, native backends, TempleOS BIN/loader
acceptance and bootstrap remain required work.
