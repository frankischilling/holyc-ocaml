# Native callback arguments

`run --target=host-jit` accepts original one-star callback parameters in an
indirect call's fixed signature. JIT and AOT source modes use the same checked
integer/U0 execution path.

```sh
opam exec -- dune exec --root . -- bin/holyc.exe run --target=host-jit --format=json examples/native-callback-arguments.hc
```

The example calls `Apply` through `invoke` and passes the retained `Add` address
from `saved`. It returns I64 42. `Apply`'s parameter owns the default 40 used by
its inner call. The other declarations retain their defaults 12 and 10; copying
an executable address does not copy a declaration's defaults. All three original
defaults prepare once, consume nine preparation steps and retain 24 saved bytes.

[Callback-word defaults](native-callback-word-defaults.md) also prepare original
closed numeric words for omitted callback-valued arguments. Those words retain
zero executable ownership and do not select bodies.

Each fixed callback argument stages its value and a private executable owner.
Those owner words follow the complete outgoing argument area, including any
hidden count and integer tail. They precede the caller's saved callee and result
stages. Logical frame accounting still charges source storage; private owner
words count toward compiled-frame and active native-stack limits.

The compiler closes ownership transfers across the original bundle before
dispatch budgeting and emission. An indirect transfer reaches a parameter cell
only when the captured parent can own that original target body. Forwarding and
recursive calls resolve through the same closure. An unrelated body with an
equal signature receives no executable authority from that transfer.

IR and native dispatch compare the original callback/object parameter kinds as
well as physical fixed types, return type, variadic shape and cleanup. A pointer
slot's physical type alone cannot authorize a callback owner lane. The actual
destination parameter's original nested header governs its later invocation,
including defaults and nested signature checks.

The parent address and owner are captured before reverse argument evaluation.
Numeric and null callback arguments carry zero owner words and may remain
unused. Invoking them faults after the inner argument effects. Parent signature
faults preserve outer effects; uninitialized or out-of-bounds callee captures
fault before those effects. Callback cells, indexed arrays, static/global state,
multiple parameter lanes and variadic parents retain these rules.

Tests compare independent values with fresh public IR and separately executed
checked-batch IR in both source modes. They cover all integer widths, U0,
forwarding, defaults, capture mutation, reached faults, exact and one-below
compile/runtime quotas and recovery on the same image. Both image ABIs compile;
Windows and Linux CI execute their own host ABI. Copied/foreign receipts, entry
graphs and parameter frames cannot supply ownership.

The pinned source audit uses `Compiler/PrsVar.HC:285-369,521-532` for recursive
anonymous headers, RT_PTR storage and `MLF_FUN`, and
`Compiler/PrsExp.HC:435-586` for argument ordering, saved callee and cleanup, at
TempleOS commit `c26482bb6ad3f80106d28504ec5db3c6a360732c`. Private owner lanes
enforce the hosted execution contract; this adds no TempleOS execution capture
or exported HolyC ABI proof.

Ordinary object-reference parameters in indirect native signatures, callback
indirection beyond one star, pointer/owned-code tails, member/initializer/update
consumers, effectful or owned-code defaults, ordinary code-word storage/returns,
retained replacement/linking and general F64/aggregate execution remain open.
The full compiler and release remain unfinished under #801, #688 and #682.
