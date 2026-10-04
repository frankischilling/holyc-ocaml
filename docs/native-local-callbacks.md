# Native local callbacks and fixed callback parameters

`run --target=host-jit` invokes one-star automatic callback cells and fixed
callback parameters with integer arguments and integer or U0 returns. JIT and
AOT preprocessing modes use the same source pipeline. Generated calls enter the
project's private fixed-RSP adapter.

```sh
opam exec -- dune exec --root . -- bin/holyc.exe run --target=host-jit --format=json examples/native-callback-parameters.hc
```

The example returns I64 42. It forwards an original function address through two
named callback parameters, copies the final parameter to a local cell, overwrites
the parameter with a numeric word and invokes the saved callback. The separate
[local example](../examples/native-local-callbacks.hc) covers local copies, null
clearing, mixed integer widths and a U0 callback.

## Executable ownership and capture

The backend accepts the original sealed function-address producer and its
original local body in the same checked image. RIP-relative LEA materializes that
body's generated address. Equal instruction records, foreign contexts, numeric
identifiers and another declaration with the same name cannot select it.

Private ownership words accompany code values, callback cells and callback
parameters. An original address carries its body index; a numeric producer
carries zero. Copies and full-word I64/U64 views preserve the ownership word.
Numeric stores clear it, even when they overwrite a previously owned callback.
Source expressions cannot address these metadata words. Callback-cell addresses
still cannot escape through object references, and owned code cannot pass through
ordinary integer cells, ordinary parameters or returns.

Checked copies and fixed-parameter transfers form dependencies across the entire
original bundle. The backend closes those dependencies before dispatch budgeting
and machine allocation, including cycles caused by forwarding and recursion.
Only original function-address producers contribute executable targets.

The original callee load runs before arguments. Its address and ownership word
are saved in separate private stack slots at the original call start. Nested calls
and argument stores cannot replace either snapshot. Arguments retain reverse
evaluation order and their checked fixed positions. Callback parameters use a
private ownership lane after the ordinary eight-byte argument homes; the callee
copies that lane into its own private frame metadata. These extra words count
toward physical frame and stack limits, while source activation charges stay the
same.

A native indirect CALL requires both the saved ownership word and the address to
identify the same original body. The selected anonymous header then checks the
original return type, parameter types and cleanup policy before activation quota
reservation. Numeric words cannot acquire executable authority by matching
address bits.

A reached null or numeric callee reports `HCIRVM0024`; an incompatible signature
or cleanup policy reports `HCIRVM0014`. Both faults occur after argument effects,
including published output. An uninitialized local reports `HCIRVM0012` during
its earlier load. Valid calls reserve depth, semantic frame and physical stack
quotas. Completion and faults unwind through the private adapter and restore
those quotas.

Owned code supports equality with owned code or null. Two numeric callback values
use word equality. Comparing owned code with a nonzero numeric callback reports
`HCIRVM0024` at the reached comparison. Discarded owned code never exposes a host
address as a numeric result. Concrete address arithmetic, numeric address output
and general code truth tests remain outside this native domain.

## Source and verification

The pinned reference is TempleOS `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
`Compiler/PrsExp.HC:541-586` saves the indirect callee, appends reverse arguments
and selects callback cleanup, including the saved-callee slot.
`Compiler/PrsVar.HC:619-657` retains eight-byte fixed parameter slots.
`Compiler/OptPass789A.HC:723-732` emits CALL through an RSP displacement.
`Compiler/OpCodes.DD:573,833` supplies the 64-bit indirect CALL and LEA forms.
These are source audits; this work adds no TempleOS oracle capture.

Ordinary tests check both image ABIs, exact instruction bytes, signed RIP range,
code/frame quotas, copied/foreign graph rejection and fault-site identity.
Native API and maintained CLI tests execute the host ABI in both source modes.
Independent expected values and fresh public IR runs check semantics; isolated
checked-batch IR checks exact runtime work. Coverage includes all integer widths,
ordinary calling flags, local and parameter copies, forwarding, nested calls,
reverse effects, saved callee mutation, U0, numeric/null/signature faults and
reached output. Recursive exact and one-below frame, depth, physical-stack and
step limits recover on the same image. CI executes these through `@native-tests`
on Windows and Linux.

## Remaining callback work

Native static/global/array/member storage, initializers and updates, saved
callback defaults, callback-valued parameters of an indirect callback signature,
word-tail variadics, retained publication after same-name replacement, unresolved
extern slots and live task linking remain unfinished in
[issue #801](https://github.com/frankischilling/holyc-ocaml/issues/801). Ordinary
word storage and return paths still need ownership-preserving consumers. General
F64, aggregate and mixed-value execution remains in #688. The IR callback
consumer has a broader admitted domain; see [global callbacks](global-callbacks.md).
HolyC ABI exports, RET-imm execution, interrupt entry and the full compiler remain
unfinished.
