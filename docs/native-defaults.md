# Native scalar defaults

Issue #660 adds declaration-time I64/U64 parameter defaults to the bounded
`run --target=host-jit` source path. The original parser callback prepares each
admitted default once. Native calls load the saved word when an argument is
omitted; they do not reevaluate its expression. Unused functions and calls with
all arguments supplied still require successful preparation of every declared
default. Issue #663 extends the same preparation and authority path to narrow
integer parameters; [native scalar functions](native-scalars.md) describes their
storage and register-value behavior.

```text
opam exec -- dune exec --root . -- bin/holyc.exe run --target=host-jit --format=json examples/native-integer-defaults.hc
opam exec -- dune exec --root . -- bin/holyc.exe run --target=host-jit --mode=aot --format=json examples/native-integer-defaults.hc
```

Both preprocessing modes use their original source identities and produce a
hosted native image. AOT mode here does not write an object or TempleOS BIN.

The separate JIT [native source-task path](native-source-tasks.md) executes
named and anonymous integer and F64 defaults containing live storage reads,
updates and retained calls. One-star callback parameters can also save original
owned values. It uses original native expression fragments and saved-header
receipts. See [live anonymous defaults](native-anonymous-defaults.md).
Scalar source-task defaults bind their saved word to an opaque capture from the
actual native entry. Completion checks the original program, work, source scope
and live arena before publication. Isolated `host-jit` uses the same native
capture mechanism for F64 defaults; its integer-only closed defaults keep the
existing VM preparation path.

The native source task also admits original scalar defaults whose expression and
selected parameter are in different integer/F64 families. TempleOS
`PrsVar.HC:629-656` evaluates the expression before converting its full saved
word to the forwarded parameter class. Integer-to-F64 uses signed integer input
and binary64 nearest-even rounding; F64-to-integer truncates toward zero. NaN,
infinity and out-of-range integer conversions produce the masked x87
integer-indefinite word. A same-class F64 default preserves all eight source
bits, including signed zero and signaling-NaN payloads. Conversion runs only
after the original native scalar capture; completion separately converts those
captured bits before comparing them to the saved value. No OCaml floating-point
reinterpretation or fabricated native result authorizes a default.

The maintained [live F64 default example](../examples/native-f64-defaults.hc)
saves one converted named value and one anonymous callback value, then returns
F64 3.75 (`0x400e000000000000`) through `host-jit-task --mode=jit`.
The dedicated [F64 default guide](native-f64-defaults.md) describes the
initialized x87 conversion profile and exact supported execution boundary.
The [public regression suite](../test/native/test_native_f64_defaults.ml)
checks exact bits, declaration-time effects, nontrailing omission, unused
headers, native-only execution, source-selected callback defaults and
preparation/payload limits. Closed numeric F64 defaults now run through original
native preparation in ordinary AOT and isolated `host-jit` under JIT or AOT.
The [closed numeric example](../examples/aot-native-defaults.hc) returns F64
5.25 (`0x4015000000000000`) and saves four defaults (32 bytes) with either
target and mode; its CLI checks include exact work and byte limits. An original
JIT `#exe` block entered while the outer compiler is in either JIT or AOT mode
uses the JIT task path: the regression
executes `Nested(F64 value=1.5)`, saves eight bytes and injects a final
`42;` statement. Seven saved-default bytes reject before publication.
One-level primitive [data-pointer defaults](data-pointer-defaults.md) retain
original task objects and views; string-producing expressions copy their
terminated result into fresh mutable storage.
The closed-default rules below describe the isolated `host-jit` target. Its
F64 defaults use original native source images and opaque captures; supported
integer-only defaults continue through their existing bounded VM preparation.

## Supported domain

Defaults belong to fixed I8/U8/I16/U16/I32/U32/I64/U64 and admitted F64
parameters on the source-defined functions admitted by
[native programs](native-programs.md) and original
[callback declarations](native-local-callbacks.md). Callback calls use the
selected anonymous declaration's saved value even when the reached target
function declares a different default. The existing checked constant-preparation
engine handles integer-only expressions. Closed integer arithmetic such as
`84/2` prepares the word 42. Source-selected queries
are usable only when their original evidence and the preparation engine support
them. Original literal numeric `_intern` calls also prepare through the
[native intrinsic path](aot-intrinsic-defaults.md). Other function or data
references, storage effects and copied string defaults still need their own
execution and storage authority in isolated compilation.
[Native `lastclass` defaults](native-lastclass-defaults.md) materialize the
previous supplied argument's forwarded class name at the call, without running
a declaration expression. [Closed class and union defaults](class-default-words.md)
retain integer words independently of their backing width and nominal size.
Prototypes remain outside the native function gate.

[Callback-word defaults](native-callback-word-defaults.md) admit original
one-star callback-valued parameters. Their defaults retain full integer words
independently of the callback return class and receive zero executable ownership.
Selected original headers own omission; an effective `noreg` uses the existing
stack parameter path. Explicit register allocation remains outside this adapter.

Integer defaults use the preparation engine's supported integer arithmetic;
admitted floating defaults use their original typed native fragment and full-word
conversion after the captured result.
Source-owned shift cases retain full-word saved bits; this does not establish
general optimizer parity.
A default containing `1/0` reaches `HCIRVM0009` during preparation, including
when its function is never called. The closed integer preparation path reaches
that fault in the VM. Native preparation retains its actual reached work and
publishes no saved value when execution fails.

Default-bearing definitions must precede executable top-level statements.
Interleaving a default declaration after an entry statement rejects explicitly:
batch native compilation cannot execute an earlier JIT statement before reading
the later default. This restriction preserves the order of admitted defaults
without interpreting ordinary commands or activating a stateful source task.
Preparation stops at the first failure; later defaults do not prepare.

Supplied argument expressions retain the native function path's right-to-left
evaluation and formal-position binding. Omitted arguments use their exact saved
words, including high-bit U64 values. Repeated and recursive calls reuse those
values without extra declaration-preparation work or saved-payload charges.
For a narrow parameter, the saved word keeps all original bits; the callee's
declared-width object access performs sign or zero extension. An `U8` default of
554 therefore retains 554 in its preparation proof and reads as 42 in the callee.
Narrow defaults still retain eight payload bytes each.

## Ownership and admission

The source ledger observes the actual parser lifecycle and default/header
callbacks. Native preparation has its own entry point. Ordinary AOT prepares
closed numeric defaults before the separate output module is executed, while
live JIT defaults execute in their owning source task.
Successful native preparations remain tied to the exact receipt, publication,
typed fragment and completed header, and are published only after their work is
charged to the owning preparation budget.

For integer-only closed defaults, the helper evaluates the original private
lowering once and consumes its opaque VM result. That result retains the actual
full word, reached work and owning invocation. Admitted F64 defaults instead
execute an original private native image and consume its one-use scalar capture
from the live runtime and arena. The completed conversion is checked against
the saved word before publication. Matching raw fields or work charges cannot
authorize evaluation. Failed preparations retain the work already reached and
cannot restart their original receipt.

The certificate and preparation helpers live in `Driver`. The native expression
and program entry wrappers live in `Hosted`, above both the backend and host
runtime. Their public `Native_expression` and `Native_program` APIs remain in
place. This separation lets the backend consume read-only admission evidence
without depending on native execution or introducing a module dependency cycle.

The native bundle certificate joins those completed preparations to the exact
globals, initialization, runtime call context, entry and function bodies.
Selected and definition-owned headers are both checked, including unused
functions and functions called only with explicit arguments. Each omitted
argument must retain its original prepared word and canonical typed producer.
Foreign or reconstructed headers, mismatched values, duplicate or missing
preparations and a certificate from another bundle reject before executable
allocation. Low-level `X86_64_program.compile_callable` still rejects defaults
when no certificate is supplied.

Scalar statics with or without closed initializers compose with saved defaults and consume the
global storage quota. Default evaluation still uses its empty fragment.
Scheduled global/static initialization remains excluded. Closed global and static
initializers require their separate original preparation certificate; a default
certificate cannot authorize them. Likewise, accepting immediate producers does
not authorize arbitrary literals as defaults.

## Bounds and reports

`Native_program.compile` and `evaluate` accept `max_initializer_steps`
(default 100,000) and `max_default_bytes` (default 65,536). Both must be positive.
The CLI names are `--initializer-step-limit` and `--default-byte-limit`.
Closed global and static initializers share the work allowance with defaults, in declaration
order. Their payload uses the global storage quota, not the saved-default quota.
The preparation engine charges reached work, including faults. Each successfully
prepared scalar default retains eight payload bytes. The byte allowance is
checked before attempting the next default; repeated calls consume no new
payload. This is a saved-value quota, not a claim to measure the OCaml heap.

`Native_program.preparation_steps` and `default_bytes` retain reached preparation
and successful payload bytes after later parsing, compilation, host or native
failures. JSON reports expose them as `compiled_initializer_steps` and
`prepared_default_bytes`; human reports use `compiled-initializer-steps` and
`prepared-default-bytes`. The requested byte bound appears under
`native.limits.default_bytes`. Native `executed_steps` counts actual native
instructions, including temporary default images and final entry execution.
Reached native preparation remains visible after later failures. The isolated
report has no native count when no native image has entered.

## Reference and remaining work

The reference remains TempleOS commit
`c26482bb6ad3f80106d28504ec5db3c6a360732c`.
`Compiler/PrsVar.HC:629-657` calls the compiled default expression at declaration
time, converts its result, stores `dft_val` and sets `MLF_DFT_AVAILABLE`.
`Compiler/PrsExp.HC:455-468` substitutes the saved value for an omitted argument.
The hosted constant-preparation engine is an explicit bounded implementation of
that behavior, not evidence that general default expressions execute natively.

For isolated `host-jit`, effectful defaults, interleaved source execution,
copied string defaults and other unsupported value classes still require
further work. Ordinary AOT admits closed numeric F64 defaults and the checked
numeric intrinsic calls. A default that references the outer module's functions
or storage needs real output relocations or preloaded executable authority.
A nested JIT `#exe` task cannot
supply that authority for ordinary outer AOT. Issue #685 remains open for
full default semantics. Optimizer
work under #585/#593/#696/#697 remains separate. Fully constant source shifts now fold to known words under #787 while retaining original preparation proofs; nonconstant shifts keep their gate. Full HolyC ABI, assembler/BIN output,
actual TempleOS loader acceptance, whole-tree compilation and bootstrap under
#682 are not completed by this gate.
