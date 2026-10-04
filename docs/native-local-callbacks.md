# Native callback cells, arrays and parameters

`run --target=host-jit` invokes one-star automatic, static and global callback
cells, fully indexed arrays and fixed callback parameters. Calls admit integer
fixed arguments and bounded integer word tails, with integer or U0 returns.
JIT and AOT preprocessing modes use the same source pipeline. Generated calls enter the
project's private fixed-RSP adapter.

```sh
opam exec -- dune exec --root . -- bin/holyc.exe run --target=host-jit --format=json examples/native-callback-parameters.hc
```

The example returns I64 42. It forwards an original function address through two
named callback parameters, copies the final parameter to a local cell, overwrites
the parameter with a numeric word and invokes the saved callback. The separate
[local example](../examples/native-local-callbacks.hc) covers local copies, null
clearing, mixed integer widths and a U0 callback.

The [storage example](../examples/native-callback-storage.hc) also returns I64 42.
It copies a global array element into a static array, clears the original cells,
then transfers the saved callback through a parameter into an automatic array.
The static element survives a second function activation.

The [defaults example](../examples/native-callback-defaults.hc) returns I64 42.
Its target function saves 17, its global callback array saves 7, and its callback
parameter saves 42. Each invocation uses the selected callback declaration's
saved value. Copying an executable address does not copy the source cell's defaults.

[Native word tails](native-word-tails.md) add matching variadic callback calls,
source-defined variadic bodies and their original `argc`/`argv` storage. Fixed
callback parameters can also be forwarded through a variadic function.

## Declaration defaults

Original callback signatures on automatic/static/global cells, arrays and named
callback parameters admit closed scalar integer defaults in both source modes.
The parser prepares each expression once, including defaults in unused
declarations and calls with explicit arguments. Defaults share declaration work
and saved-byte budgets with named function defaults; each successful default
retains eight bytes with its full register bits. Narrowing occurs at callee entry.

Native admission requires the original anonymous signature, parameter receipt,
saved object and consumed preparation receipt for the exact compiled bundle.
Matching bits, copied objects, foreign namespaces and omitted or duplicate
completions cannot supply that authority. Omitted arguments use the sealed
immediate producer while explicit arguments retain reverse evaluation order.
Defaults must precede executable top-level statements. Value/function references,
side effects, owned code, strings, `lastclass` and non-integer defaults still need
broader declaration execution; this consumer does not implement those cases.

## Executable ownership and capture

The backend accepts the original sealed function-address producer and its
original local body in the same checked image. RIP-relative LEA materializes that
body's generated address. Equal instruction records, foreign contexts, numeric
identifiers and another declaration with the same name cannot select it.

Private ownership words accompany code values, callback cells, every array
element and callback parameters. An original address carries its body index; a numeric producer
carries zero. Copies and full-word I64/U64 views preserve the ownership word.
Numeric stores clear it, even when they overwrite a previously owned callback.
Source expressions cannot address these metadata words. Callback-cell addresses
still cannot escape through object references, and owned code cannot pass through
ordinary integer cells, ordinary parameters or returns.
Numeric assignment results can supply ordinary integer arguments. Object-pointer
parameters still require checked object references.

Checked copies and fixed-parameter transfers form dependencies across the entire
original bundle. The backend closes those dependencies before dispatch budgeting
and machine allocation, including cycles caused by forwarding and recursion.
Only original function-address producers contribute executable targets. Persistent
cells share those dependencies across all functions in the sealed image. Array
storage uses a conservative union of its original stored targets; the selected
element's dynamic owner and address must still agree before invocation.

Array roots retain the original frame location or persistent allocation, anonymous
header, dimensions and cumulative strides. Only a fully indexed load can invoke a
callback. Signed scale/add overflow, final object bounds and the selected
element's initialization flag are checked before reading its address and owner.
Index effects precede callee capture. Flat multidimensional indexing retains the
existing final-address bounds rule, including offsets restored by a later index.

Each callback element charges eight source-visible bytes. Private frame or arena
ownership words and initialization state have separate physical limits. Persistent
ownership occupies non-executable memory and is bounded before allocation. Every
execution restores the sealed initial data, flags and zero ownership words. Static
cells persist across activations within that execution. In the hosted JIT policy,
an untouched persistent cell faults at its load before argument effects; AOT
preprocessing gives an initial numeric zero that faults at invocation after
arguments. The pinned native JIT fill policy depends on `sys_var_init_flag`; these
host checks add no independent TempleOS initialization oracle.

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

Owned code supports equality with owned code or null. Numeric callback values
also compare with scalar integer operands using full-word equality. Comparing
owned code with a nonzero numeric operand reports
`HCIRVM0024` at the reached comparison. Discarded owned code never exposes a host
address as a numeric result. Concrete address arithmetic, numeric address output
and general code truth tests remain outside this native domain.

## Source and verification

The pinned reference is TempleOS `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
`Compiler/PrsExp.HC:541-586` saves the indirect callee, appends reverse arguments
and selects callback cleanup, including the saved-callee slot.
`Compiler/PrsVar.HC:285-369` retains the anonymous header and RT_PTR storage
before parsing array dimensions. Lines 521-532 multiply the physical element size
by the dimension count; lines 590-628 allocate automatic locals, and 619-657
retain eight-byte fixed parameter slots. Lines 628-656 evaluate each original
default and save its value on that member, including anonymous signatures.
`Compiler/OptPass789A.HC:723-732` emits CALL through an RSP displacement.
`Compiler/OpCodes.DD:573,833` supplies the 64-bit indirect CALL and LEA forms.
These are source audits; this work adds no TempleOS oracle capture.

Ordinary tests check both image ABIs, exact instruction bytes, signed RIP range,
code/frame quotas, copied/foreign graph rejection and fault-site identity.
Native API and maintained CLI tests execute the host ABI in both source modes.
Independent expected values and fresh public IR runs check semantics; isolated
checked-batch IR checks exact runtime work. Coverage includes all integer widths,
ordinary calling flags, local/global/static cells, indexed element copies,
parameter forwarding, nested calls,
reverse effects, saved callee mutation, U0, numeric/null/signature faults and
reached output. Recursive exact and one-below frame, depth, physical-stack and
step limits recover on the same image. CI executes these through `@native-tests`
on Windows and Linux.

[Native callback arguments](native-callback-arguments.md) extend indirect fixed
signatures with original callback parameter declarators. Private owner lanes
survive forwarding, recursive calls and variadic parents; the destination
parameter retains its own nested header and saved defaults. Dispatch separates
callback and object-reference parameter kinds even when physical types match.

## Remaining callback work

Native member storage, JIT owned-code/effectful callback initializers and updates, effectful/owned-code
callback defaults,
pointer/owned-code variadic tails, retained publication after same-name
replacement, unresolved extern slots and live task linking remain unfinished in
[issue #801](https://github.com/frankischilling/holyc-ocaml/issues/801). Ordinary
word storage and return paths still need ownership-preserving consumers. General
F64, aggregate and mixed-value execution remains in #688. The IR callback
consumer has a broader admitted domain; see [global callbacks](global-callbacks.md).
HolyC ABI exports, RET-imm execution, interrupt entry and the full compiler remain
unfinished.
