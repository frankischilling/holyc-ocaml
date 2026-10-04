# Native automatic local callbacks

`run --target=host-jit` can invoke one-star automatic local callback cells with
fixed integer parameters and integer or U0 returns. The source pipeline works
in JIT and AOT preprocessing modes. The generated call uses the project's
private fixed-RSP adapter; it does not export the HolyC ABI.

```sh
opam exec -- dune exec --root . -- bin/holyc.exe run --target=host-jit --format=json examples/native-local-callbacks.hc
```

The maintained example returns I64 42. It copies an original function address
between local cells, clears the first cell and invokes the saved value. It also
invokes a U0 callback. All eight integer parameter/return widths use the same
normalization and full-register return rules as [native scalars](native-scalars.md).

## Executable ownership and capture

The backend accepts only the original sealed function-address producer and its
original local body in the same checked image. RIP-relative LEA materializes that
body's generated address. Equal instruction records, foreign contexts, numeric
identifiers and another declaration with the same name cannot select it.

Every callback-cell write must come from an owned function address, another
checked automatic callback cell or literal zero. Full-word I64/U64 integer views
preserve that ownership. The backend collects the possible original bodies
through all checked cell-copy dependencies before emitting dispatch. Callback
cell addresses cannot escape through object references, and code values cannot
escape through ordinary word cells, parameters or returns. These admission rules
let the consumer check the saved address against an owned body without granting
executable authority to arbitrary numeric words. Other numeric stores reject
before native execution.

The original callee load runs before arguments. Its value is saved in a separate
private stack slot at the original call start. Nested calls and argument stores
cannot replace that snapshot. Arguments retain the source's reverse evaluation
order and their checked fixed positions. The native indirect CALL reads the saved
slot after checking the original target's return type, parameter types and cleanup
policy against the selected anonymous header.

A reached null callee reports `HCIRVM0024`; an incompatible signature or cleanup
policy reports `HCIRVM0014`. Both faults occur after argument effects, including
published output. An uninitialized cell reports `HCIRVM0012` during its earlier
load. A valid target reserves depth, semantic frame and physical native stack
quotas before invocation. Completion and faults unwind through the existing
private adapter and restore those quotas.

Owned code values support equality with other owned code or literal zero.
Discarding one clears the entry's numeric result instead of exposing a host
address. Concrete address arithmetic, numeric address output and general code
truth tests remain outside this native gate.

## Source and verification

The pinned reference is TempleOS `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
`Compiler/PrsExp.HC:541-586` saves the indirect callee, appends reverse arguments
and selects callback cleanup, including the saved-callee slot.
`Compiler/OptPass789A.HC:723-732` emits CALL through an RSP displacement.
`Compiler/OpCodes.DD:573,833` supplies the 64-bit indirect CALL and LEA forms.
These are source audits; this increment adds no TempleOS oracle capture.

Ordinary tests check both image ABIs, exact instruction bytes, signed RIP range,
byte/frame quotas, copied/foreign graph rejection and callback fault-site identity.
The native API and maintained CLI execute the host ABI in both source modes.
Independent expected values and fresh public IR runs check semantics; isolated
checked-batch IR checks exact runtime work. Cases include all integer widths,
cell copies, branch-selected targets, nested calls, reverse effects, saved callee
mutation, U0, null/signature faults, retained output and recursive exact/one-below
quotas followed by recovery on the same image. CI runs these tests on Windows
and Linux through `@native-tests`.

## Remaining callback work

Native static/global/array/member storage, callback parameters, initializers and
updates, saved callback defaults, word-tail variadics, retained publication after
same-name replacement, unresolved extern slots and live task linking remain
unfinished in [issue #801](https://github.com/frankischilling/holyc-ocaml/issues/801).
General F64, aggregate and mixed-value execution remains in #688. The wider IR
callback consumer has a broader admitted domain; see [global callbacks](global-callbacks.md).
Neither this gate nor passing native CI completes the full HolyC ABI or compiler.
