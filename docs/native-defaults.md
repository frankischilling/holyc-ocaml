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

## Supported domain

Defaults belong to fixed I8/U8/I16/U16/I32/U32/I64/U64 parameters on the source-defined
functions admitted by [native programs](native-programs.md) and original
[callback declarations](native-local-callbacks.md). Callback calls use the
selected anonymous declaration's saved value even when the reached target
function declares a different default. The existing
checked constant-preparation engine handles the original expression. Closed
integer arithmetic such as `84/2` prepares the word 42. Source-selected queries
are usable only when their original evidence and the preparation engine support
them. References to values or functions, storage effects, string ownership,
`lastclass`, pointer/function-pointer parameters and non-integer parameter types
remain unsupported. Prototypes remain outside the native function gate.

Default expressions use the preparation engine's supported integer arithmetic.
Source-owned shift cases retain full-word saved bits; this does not establish
general optimizer parity.
A default containing `1/0` reaches `HCIRVM0009` during preparation, including
when its function is never called. Failed preparation does not enter native code.

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
callbacks. Native preparation has its own entry point; ordinary AOT source
preparation and stateful JIT execution retain their existing contracts.
Successful native preparations remain tied to the exact receipt, publication,
typed fragment and completed header, and are published only after their work is
charged to the owning preparation budget.

The helper evaluates the original private lowering once and consumes its opaque
VM result. That result retains the actual full word, reached work and owning
invocation. Only this successful path can issue a native completion receipt.
The certificate checks the actual result against the saved word; replacing its
bits while retaining the receipt rejects before native allocation. Matching raw
fields or work charges cannot authorize evaluation. Failed preparations retain
the work already reached and cannot restart their original receipt.

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
`native.limits.default_bytes`. Native `executed_steps` remains a separate actual
entry-execution count and is absent when preparation fails before native entry.

## Reference and remaining work

The reference remains TempleOS commit
`c26482bb6ad3f80106d28504ec5db3c6a360732c`.
`Compiler/PrsVar.HC:629-657` calls the compiled default expression at declaration
time, converts its result, stores `dft_val` and sets `MLF_DFT_AVAILABLE`.
`Compiler/PrsExp.HC:455-468` substitutes the saved value for an omitted argument.
The hosted constant-preparation engine is an explicit bounded implementation of
that behavior, not evidence that general default expressions execute natively.

Effectful defaults, interleaved source execution, owned strings, `lastclass`,
non-integer/pointer types and native `#exe` require further work. Optimizer
work under #585/#593/#696/#697 remains separate. Fully constant source shifts now fold to known words under #787 while retaining original preparation proofs; nonconstant shifts keep their gate. Full HolyC ABI, assembler/BIN output,
actual TempleOS loader acceptance, whole-tree compilation and bootstrap are not
completed by this gate.
