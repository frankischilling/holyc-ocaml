# Captured Print and PutChars output

Issue [#621](https://github.com/frankischilling/holyc-ocaml/issues/621) connects
checked output declarations, arguments and call instructions to captured bytes
in the integer interpreter. The maintained example is:

```c
extern U0 Print(U8 *fmt,...);
"42\n";
42;
```

```text
holyc run --mode=jit --format=json --step-limit=10 --frame-byte-limit=16 --call-depth-limit=1 --literal-byte-limit=4 --output-byte-limit=3 --output-work-limit=7 examples/integer-output.hc
holyc run --mode=aot --format=json --step-limit=10 --frame-byte-limit=16 --call-depth-limit=1 --literal-byte-limit=4 --output-byte-limit=3 --output-work-limit=7 examples/integer-output.hc
```

Both forms capture `34320a` (42 followed by newline) separately from the final
I64 value 42. Explicit `Print("42\n");`, implicit `'42\n';`, and explicit
`PutChars('42\n');` use the same checked path with the corresponding prototype.
The Print example takes ten IR instructions and zero preparation instructions.
Its companion `examples/integer-putchars.hc` takes nine IR instructions, zero
preparation and six output-work units, with eight active ABI bytes and depth one.
The original four tests reject the declaration with HCRUN0001 on the U0 merge
`6dd1a1fa53098d316b768b1bd463613cbc2bbb99`.

## Declaration and call ownership

The driver consumes the existing function and top-level implicit-output target
and argument-binding passes. Checked adapters reuse direct-call lowering,
including right-to-left argument evaluation, pushed producer identities,
hidden variadic count, eight-byte slots, selected opcode and cleanup.
`Runtime_call_context` validates these facts against the completed entry and
exact function bodies. It retains each selected declaration/header snapshot;
a later same-name declaration cannot change an earlier call's approval.
Scheduled calls must belong to the exact retained initializer expression tree,
including nested provided arguments. A call from a different expression cannot
borrow its initializer region; global and static source owners remain distinct.
Implicit output statements cannot acquire initializer-region ownership.
Static storage checks distinguish prototypes from definitions: a prototype has
no execution frame, while every definition still requires its exact checked
frame and source owner.

The initial hosted providers accept the declarations in `Kernel/KExts.HC:83-84`:
U0 Print(U8*,...) and U0 PutChars(U64). Parameter names are immaterial. Provider
selection requires the checked symbol, declaration, signature, flags, linkage
and mode. JIT extern uses IC_CALL_INDIRECT2; AOT extern uses IC_CALL_EXTERN.
Unsupported externs and imports retain explicit execution boundaries. A source
definition named Print or PutChars executes its own body. [Joined extern/body definitions](integer-joined-definitions.md) now execute
through exact checked associations; earlier selected providers retain their
declaration snapshots. User-defined variadic frames remain unsupported.

Implicit statement origins also travel in the checked context. `42;"x";`
retains final 42; `42;Print("x");` has no final word because it ends in an
ordinary U0 expression. An implicit output targeting a word-returning source
function discards that call's result and preserves the preceding ordinary
expression. This is the hosted report policy, not a native return-value claim.

## Formatting and byte ownership

Print supports ordinary bytes, `%%`, `%d`, `%s` and `%c`. `%d` interprets the
word's bits as signed 64-bit decimal. Format and `%s` scans stop at zero and
retain the VM's exact U8 pointee, storage lifetime, initialization and original
object extent checks. Mutable formats are read at the reached call.

`%c` reads at most eight low-to-high packed bytes and stops at the first zero.
PutChars skips interior zero bytes, continuing while the packed word is
nonzero. Thus `0x00420041` produces A through `%c` and AB through PutChars.
Unsupported directives and flags fail explicitly. Missing consumed arguments
fail; unused trailing arguments are allowed and their expressions still run.
Unused arguments can retain the VM's supported I64/U64/U8 scalar pointers;
only a consumed string argument performs a U8 scan.
The implementation uses the pinned HolyC rules, without host printf formatting.

Print forms a bounded draft and publishes it only after the whole call
succeeds, following `Kernel/StrPrint.HC:890-895`. A failed Print contributes no
bytes. PutChars publishes each nonzero byte as it visits it, matching
`Kernel/KeyDev.HC:1-25`; a later resource fault retains that prefix. Bytes from
earlier successful calls survive any later execution error.

## Limits and reports

Source array dimensions have an independent `--dimension-work-limit` (default
100000), also available as `max_dimension_work` through the compile/run/report
APIs. One visit is charged on entry to each evaluated numeric leaf or operator;
grouping and skipped short-circuit operands add nothing. A failed expression or
missing closing bracket retains its reached visits. Later sizeof and layout
reads reuse the checked value without more work. The option also applies to
`dump-ir --program` and is validated before parsing.

V2 reports add `dimension_work_limit` and `dimension_preparation_work`; the latter
is available through `integer_program_report_dimension_work` even on failure.
V1 fields and existing initializer/runtime counts keep their meanings. Ordinary
source initializer limits remain independent: the maintained persistent-array,
byte-signature and narrow fixtures still need 23, 7 and 9 initializer units,
respectively, with 4, 1 and 2 separate dimension visits. Runtime-bound task
commands use their task's cumulative preparation allowance and retain a separate
numeric tally through `Integer_task.dimension_work`. Callback-free AST compilation
retains its existing layout path; checked legacy extent retention remains pending.

`--output-byte-limit` and `--output-work-limit` are independent positive bounds,
each defaulting to 1,048,576. Work charges one unit before each format or `%s`
byte fetch, including terminators and failed fetches; one per packed-byte
inspection; and one per candidate output byte. Append work precedes the
capacity check. A failed charge leaves the count at its limit. Failed Print
drafts keep their charged work even though their bytes are unpublished.

Print of `"42\n"` takes seven work units, while PutChars of `'42\n'` takes six.
Empty Print still costs one terminator read. `%c` with zero costs four units.
Each provider consumes one active call depth and its ABI argument slots:
Print requires at least 16 active bytes for the fixed pointer and hidden count;
each supplied variadic value adds eight. PutChars requires eight bytes.
These charges add to active caller frames. Ordinary IR step, literal, global
and initializer preparation budgets remain separate. Host allocation bounds
are checked before expansion; the CLI reserves space for hexadecimal encoding.

The public `run_integer_program_report` returns a report for success or failure.
`integer_program_report_outcome` retains the established checked result or
diagnostics. `integer_program_report_output_bytes` and
`integer_program_report_output_work` expose immutable capture and charged work.
The VM has the corresponding `execute_program_report` and `report_*` accessors.
Existing `run_integer_program` and `execute_program` project the same outcome.
Each execution has fresh capture. Configuration and preflight failures capture
nothing; scheduled initializer faults keep their declaration and phase evidence.

`holyc run` defaults to `holyc-integer-program-v2`. JSON writes one stdout
document on success or failure; it contains `outcome`, `output_hex`,
`output_byte_length`, `output_work`, limits and diagnostics. Source-independent
configuration failures use `command_error`. Failure has null `final_value` and
`termination`; unavailable counts are null, while runtime diagnostic notes keep
their execution progress. Successful warnings belong to the same JSON report.
Human reports use `output-hex=...`, with readable diagnostics on stderr. No raw
program bytes are mixed into either metadata stream. Exit status remains zero
for success and one for a handled source, configuration or execution failure.
Command-line syntax errors remain Cmdliner's standard errors.

`--report-version=1` preserves the former success-only report and stderr
diagnostic contract. It projects the outcome and omits captured bytes. `eval`
and `dump-ir` keep their existing interfaces. Consumers needing output or
failure capture should use v2 or the report API.

HCIRVM0022 reports output capacity, HCIRVM0023 work exhaustion, HCIRVM0024
unsupported/malformed formatting, and HCIRVM0025 missing or wrongly typed
consumed arguments. Existing call-preflight, reference and resource diagnostics
retain their codes and source/function/instruction evidence.

Full HolyC formatting, arbitrary runtime linking and device behavior remain
unfinished. Stateful compilation/#exe, optimizer parity, native backends,
TempleOS BIN writing and loader acceptance, broader memory and bootstrap remain
required. This increment uses pinned-source audit and hosted tests; it claims
no new native capture. The reference is
`c26482bb6ad3f80106d28504ec5db3c6a360732c`.

## Verification

Local OCaml 5.4.1 / Dune 3.24.2 verification passed all 1,856 tests in 54.260
seconds, including 18 output groups, plus the program CLI checks. The initializer
ownership regression first reproduced an accepted borrowed call, then passed
with the exact subtree guard; nested global and static initializer calls remain
accepted. Independent review found no remaining blockers. Formatting, generated
source, build/install, 82 pinned checksums and all 11 incremental provenance
scenarios pass. Lexer acceptance remains 528/528 with zero errors. The complete
parser JSON matches the committed AOT baseline, including its text after Windows
newline normalization: 25/528 standalone and 126/528 with the prelude. Final
source CI and integration evidence are recorded in #621.
