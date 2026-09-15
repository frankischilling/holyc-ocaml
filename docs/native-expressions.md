# Native integer expressions

`holyc eval-native` compiles one checked source expression with the project's
OCaml x86-64 encoder and explicitly executes the resulting machine code.
Windows x86-64 and Linux x86-64 have separate host bridges. Other platforms
can use the encoder but cannot execute its output through this bridge.

```text
opam exec -- dune exec --root . -- bin/holyc.exe eval-native --format=json examples/native-integer-expression.hc
opam exec -- dune exec --root . -- bin/holyc.exe eval-native --mode=aot --format=json examples/native-integer-expression.hc
opam exec -- dune exec --root . -- bin/holyc.exe eval-native --format=json examples/native-integer-predicates.hc
opam exec -- dune exec --root . -- bin/holyc.exe eval-native --format=json examples/native-integer-logical.hc
```

The fixture contains `(6*7);`. It lowers to five IR instructions and emits
25 bytes: two `MOV r64,imm64` instructions, `IMUL RAX,RCX`, and `RET`.
The machine multiplication returns 42 through RAX. The process exits zero on
success; it does not truncate the value into the process exit status.
`--mode=jit` and `--mode=aot` select the existing preprocessing/typing mode;
both execute the checked expression immediately. AOT mode does not produce an
object, executable or TempleOS BIN file.

The predicate fixture combines all six comparisons, logical NOT and arithmetic.
It returns I64 42 in both modes from 38 IR instructions and 272 emitted bytes.
Its 51 machine instructions use three registers and include the actual flag
producers, condition-byte writes and full-width zero extension.

The logical fixture also returns I64 42 in both modes. It combines eager
`&&`, `||` and `^^` values, ordinary comparison chains and complements whose
unsigned computation class must survive later chain links.
It lowers to 61 IR instructions and emits 562 bytes in 120 machine
instructions, with a peak of four registers and no frame.

## Supported domain

After preprocessing, the input must be exactly one ordinary expression
statement. Parentheses and unary plus retain the existing lowering behavior.
The native compiler admits internal I64/U64 literals, unary minus, bitwise
complement, addition, subtraction, multiplication, bitwise AND, OR and XOR,
the six comparisons (`==`, `!=`, `<`, `>=`, `>`, `<=`), logical NOT (`!`),
and eager logical AND, OR and XOR (`&&`, `||`, `^^`).
Arithmetic retains the low 64 bits. Declared result type and intermediate
computation class remain distinct: complement returns I64 but may retain an
unsigned computation class consumed by its parent operation.

Each comparison returns I64 zero or one. Ordered comparisons use unsigned
order when either operand has a forwarded U64 computation class; equality
compares all 64 bits. For example, `(~0x8000000000000000)<-1` is true because
the complement forwards U64. An ordinary parenthesized comparison produces
its own I64 class for an enclosing operation. Logical NOT tests all 64 bits
and preserves the operand's forwarded class: `!~0xFFFFFFFFFFFFFFFF` is U64 one.

Binary logical operations return I64 zero or one after testing every bit of
each input independently. Thus `2&&4` is one and `2^^4` is zero. Their operands
are evaluated eagerly in the existing IR order. This value path does not
implement conditional short-circuit control flow, and every producer is still
preflighted even in `0&&(1/0)` or `1||(1/0)`.

Ordinary supported comparison chains retain their shared middle operand and
cumulative computation class. `(~0x8000000000000000)>0>-1` returns zero because
COM forwards U64 into the first comparison and the following `0>-1` comparison
must remain unsigned. The shared lowerer derives this class from semantic
operand evidence, including COM and grouping, rather than its I64 result type.
An internal word view selects U64 for a shared signed word without rerunning
its producer. An already unsigned middle operand needs no extra view.
`((~0x8000000000000000)>0)>-1` instead returns one: the parenthesized comparison
is an independent I64 value. The unresolved multiple-pending-reduction shapes
in #593, including `1==2<3==1`, remain rejected by source lowering.

The verified IR must contain one entry block with no graph edges, zero flags,
and exactly one final `IC_RETURN_VAL`, `IC_RET` pair. Every instruction is
preflighted, including unused producers. Narrow/public primitive producers,
general conversions, floating point, pointers, memory, declarations, calls,
branches, division, remainder, shifts and conditional comparison chains
remain outside this native gate.
They receive diagnostics from their first unsupported source or IR stage.
Raw division and shift interpretation remain available through `eval`;
their native optimizer policies remain separate work in #585 and #574.
The only admitted cast form is a full-width internal I64/U64 word view with
`IC_HOLYC_TYPECAST`, integer payload zero and zero flags. It preserves all bits
and selects the target computation class. The internal source spellings
`I64i` and `U64i` in `CInit.HC:12` reach this form: for example,
`0x8000000000000000(I64i)<0` returns one while preserving the original bits.
A cast whose immediate source operand is parenthesized carries payload one
and remains unsupported; so do public/narrow types, pointers and floating
conversions. The parentheses around a postfix cast's target name alone do not
set that payload. Accepting a
checked word-view instruction does not give arbitrary source or byte data
execution authority.

The allocator uses only RAX, RCX, RDX and R8 through R11, which are volatile
in both supported host conventions. It reuses registers after their final
operand use and preserves shared/duplicate values. It rejects an expression
requiring spills. There are no stack frames, stack arguments, saved-register
prologues, relocations or host calls in the generated image. This leaf bridge
does not establish a general HolyC calling convention implementation.

Predicates read the input registers with CMP or TEST before a destination can
overwrite either operand. SETcc consumes those flags without an intervening
flag-changing instruction. MOVZX then normalizes all 64 result bits, including
when the destination previously held a nonzero high byte or a high-bit value.
The allocator can reuse either dying comparison input or select another free
register while shared inputs remain live.

Binary logical values use two working registers. Each TEST is followed by
SETNE and MOVZX before the next flag producer; AND, OR or XOR combines the
normalized words. When the result reuses the right input, that input is tested
first. Shared inputs remain intact, and the temporary working register counts
toward the seven-register peak and pressure limit even though it has no IR
value owner. Word views copy a still-live source and reuse a dying one.

## API and limits

`Holyc_lib.Native_expression.compile` reuses `Integer_expression.lower` and
returns an opaque `X86_64_expression.t`. It does not allocate executable
memory. `Native_expression.evaluate` compiles and executes, returning the
image, all 64 result bits and the selected platform. Low-level callers can
compile an `Ir_x87_stack.t` with `X86_64_expression.compile`, then explicitly
call `Native_execution.execute` with the checked image.

`X86_64_expression.code` returns a fresh string copy. Changing an exported
byte string cannot change the image used by a later execution. There is no
public constructor accepting arbitrary executable bytes.

The CLI defaults are 4,096 IR instructions and 65,536 emitted bytes. The
`--ir-instruction-limit` and `--code-byte-limit` options accept positive
values up to 100,000 instructions and 16 MiB. The source driver validates
configuration before parsing. Compilation checks the actual instruction
count before constructing its maps, then checks planned code size before
allocating bytes. The five-instruction, 25-byte fixture succeeds at both
exact limits; either corresponding one-below limit fails. These counts are
compiler resource bounds, not measured CPU cycles or a native timeout.
The `1<2;` fixture requires five IR instructions and 31 code bytes; `!0;`
requires four and 21. Both have exact-limit and one-below API/CLI controls.
Each simple two-literal logical value requires five IR instructions, 44 code
bytes and ten machine instructions. Its exact-limit and one-below tests also
check the literal emitted bytes, independently of the encoder's size reports.

| Diagnostic | Meaning |
| --- | --- |
| HCBACK0001 | Invalid compilation limit or exceeded IR instruction limit |
| HCBACK0002 | Unsupported opcode, flags, type or graph shape |
| HCBACK0003 | Malformed expression, type relationship or return tail |
| HCBACK0004 | Register pressure requires an unimplemented spill |
| HCBACK0005 | Emitted code exceeds the byte limit |
| HCNATIVE0001 | Unsupported execution platform |
| HCNATIVE0002 | Native allocation, protection, cache or teardown failure |
| HCNATIVE0003 | CLI source-loading or preprocessor-configuration failure |

Existing lexer/parser/semantic/lowering diagnostics retain their codes and
source locations. Cmdliner argument errors, including an absent input path,
retain the shared text-on-stderr, exit-124 behavior even with `--format=json`.
Failures after argument parsing exit one and use the native report envelope
in JSON mode. The existing `eval` and `run` contracts are unchanged.

## Execution and reporting

The small C bridge detects the host, allocates private read/write pages,
copies the completed image, changes pages to read/execute, synchronizes the
instruction cache, calls the leaf, and releases the mapping before allocating
the OCaml return box. Linux additionally refuses `READ_IMPLIES_EXEC`, which
would invalidate the non-executable writable phase. OS failures are returned
as errors; they do not silently invoke the interpreter. All compilation,
instruction selection and byte encoding stay in OCaml.

Native execution occurs inside the current process and is not a sandbox.
The finite supported instruction sequence has no loops or external calls.
An encoder defect could nevertheless fault the process; there is no native
fault recovery or CPU deadline. Ordinary preprocessing, `eval`, `run` and
`dune runtest` do not execute generated machine code.

The JSON schema is `holyc-native-expression-v1`. It records implementation
and reference revisions, preprocessing mode, native target/platform,
requested limits, diagnostics, command errors and success/failure. A
successful image reports complete hexadecimal bytes, IR and machine
instruction counts, register peak and zero frame bytes. `final_value`
contains the I64/U64 type, exact decimal string and 16-digit hexadecimal
bit string. Values above JavaScript's exact-number range and unsigned
maximum remain lossless. Failed reports expose no final value or image.

## Evidence and remaining work

The encoder consumes the existing generated opcode forms from pinned
`Compiler/OpCodes.DD`: MOV at lines 265/276, ADD 330, AND 353, OR 399,
SUB 445, XOR 501, NOT 675, NEG 680, two-operand IMUL 694 and RET 961.
`PrsExp.HC:1117-1127` appends the expression return pair;
`OptPass789A.HC:779-782` transfers the word to RAX. `BackA.HC:224-280`
supplies the original multiplication context. This allocator and its
two-operand multiplication are a hosted subset, not a reproduction of
TempleOS's instruction selection or optimization pipeline.

Issue #644 adds CMP (`OpCodes.DD:376`), TEST (`:461`), MOVZX (`:893`) and
the signed/unsigned SETcc forms (`:981-994`). `BackB.HC:10-27` supplies the
logical-NOT TEST/SETZ/MOVZX consumer. `BackB.HC:102-200` selects comparison
order, and `OptPass012.HC:153-179,725-822` separates complement, NOT and
comparison result classes. `OptLib.HC:103-122,171` supplies the forwarded
operand classes. The hosted selector implements comparison results with
CMP/SETcc/MOVZX while retaining the same zero/one and signedness rules.

Issue #646 follows `BackB.HC:30-100` and `OptPass012.HC:693-722` for eager
logical values and I64 results. `OptPass012.HC:87-110,141-150,809-822` and
`OptLib.HC:103-122,171` establish internal word views and cumulative comparison
classes. The hosted selector uses TEST/SETNE/MOVZX and existing bitwise forms;
it does not reproduce TempleOS's branch-based instruction selection.

`test/test_native_expression.ml` runs under ordinary `dune runtest` without
entering native code. It checks exact bytes, source/type rules, sharing,
register pressure, immutable exports, malformed/dead instructions and
resource boundaries. Run the actual machine execution and CLI regressions
explicitly on a supported host:

```text
opam exec -- dune build --root . '@native-tests'
```

`test/native/test_native_execution.ml` compares boundary cases, shared IR
graphs and 500 deterministic source expressions against the existing integer
interpreter, including exact result types and all return bits. It repeats
execution after mutating exported copies. The separate CLI suite exercises
the checked-in fixture in both modes, full-width reporting, source and
configuration failures, limits and the unchanged interpreter command.
Predicate coverage adds a separate deterministic source generator, signed/
unsigned boundary cases, complement forwarding, normalized high bits, all
condition codes, extended byte registers and shared-input lifetimes. The
maintained predicate fixture and small exact-byte CLI cases use the public
command in both preprocessing modes.
Logical coverage adds independent truth-table and chain expectations, source
compositions, every I64/U64 word-view pair, shared/duplicate input lifetimes,
temporary-register exhaustion and both preprocessing modes. Chain regressions
also run through the ordinary interpreter and function-call paths. Comparing
native results only against the same faulty lowerer would miss the original
COM regression, so its zero result is checked as a literal expectation.
Success-path repetition does not establish injected OS-failure coverage.
These tests execute the hosted encoder, not the TempleOS reference compiler.

The CI workflow explicitly runs native tests on Windows x86-64 and both
Linux OCaml jobs. Unsupported hosts fail this requested target instead of
skipping it. General spills, frames, calls/HolyC ABI, control flow, memory,
x87 behavior, relocations, integrated assembler operands, object/BIN output,
actual-loader acceptance and bootstrap remain required later gates.
