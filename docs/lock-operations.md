# Lock regions and checked atomic updates

HolyC `lock` marks a lexical statement region. Each eligible memory update in
that region executes atomically. Other accepted operations retain their ordinary
behavior, and another thread may run between two updates in the same region.
Nested regions carry the same flag; entering or leaving a region creates no
runtime lock object.

This page records the contract for issue [#693](https://github.com/frankischilling/holyc-ocaml/issues/693).
The checked integer path is implemented. Its source authority, supported
operations, optimizer differences, and verified local results are recorded
below.

## Source authority

The language authority is TempleOS commit
`c26482bb6ad3f80106d28504ec5db3c6a360732c`. The complete producers and consumers
establish these boundaries:

| Source | Relevant behavior |
| --- | --- |
| `Compiler/PrsStmt.HC:963-969` | `KW_LOCK` increments `lock_cnt`, parses one ordinary statement, and restores the counter. The body may be a block, an unbraced statement, a nested lock, or a comma-linked statement sequence. |
| `Compiler/PrsLib.HC:79-96` | `ICAdd` adds `ICF_LOCK` while the counter is nonzero. |
| `Kernel/KernelA.HH:1626-1627` | `ICF_LOCK` is `0x010000000`, bit 28. |
| `Compiler/BackLib.HC:282-310,445-659` | Memory arithmetic forms can receive the prefix. `ICMov` temporarily clears it while emitting moves and restores it afterward. |
| `Compiler/BackB.HC:304-381` | Prefix and postfix integer increments and decrements update the selected memory width. Their returned values use separate reads. |
| `Compiler/BackA.HC:442-571` and `Compiler/BackC.HC:539-599` | Addition, subtraction, and bitwise compound assignments have memory update consumers. The valid immediate path reloads a used result after updating memory. |
| `Compiler/OptPass3.HC:173-213` and `Compiler/BackA.HC:56-164` | Immediate integer ADD and SUB have separate consumers. A signed 32-bit addition delta can use ordinary register arithmetic. |
| `Compiler/OptLib.HC:252-258,379-385` and `Compiler/BackC.HC:1-157` | An AND used as a condition becomes an ordinary test and branch. A prefix-decrement condition can instead branch on the update's flags. |
| `Compiler/BackB.HC:202-263` and `Compiler/BackC.HC:208-324` | Bit set, reset, and complement consume the flag; bit test explicitly omits the prefix. Used bit results come from carry. |
| `Compiler/OptPass012.HC:934-938` and `Compiler/OptLib.HC:260-289,387-416` | `LBts`, `LBtr`, and `LBtc` force the flag, including when an optimizer combines their result with a branch. |
| `Compiler/OptPass789A.HC:449-804` | The final opcode dispatcher selects the integer, floating, update, and bit consumers. |
| `Demo/MultiCore/Lock.HC:1-7,32-48` and `Doc/HolyC.DD:236` | The intended use is multicore memory updates. The demo warns that misuse can emit invalid instructions. |

The [Intel instruction reference, LOCK entry](https://cdrdv2-public.intel.com/812383/253666-sdm-vol-2a.pdf)
supplies the machine rule for `LOCK`: the supported instruction must use a
memory destination. A prefix on register arithmetic is invalid. HolyC's
original backend has paths that emit such combinations, so the checked
compiler diagnoses unsupported shapes before execution or image publication.

## Operation selection

Selection follows the retained opcode after the existing source rewrites.
The spelling of the containing statement does not by itself establish an
atomic operation.

| Retained operation | Effect of the lock region |
| --- | --- |
| Integer `+=`, `-=`, `&=`, `\|=`, `^=` | One atomic update at the selected scalar width. A used result is read afterward. |
| Integer prefix `++` and `--` | One atomic update, followed by a read of the stored value. |
| Integer postfix `++` and `--` | Read the expression result, then perform one atomic update. |
| `Bts`, `Btr`, `Btc` | Atomically change the selected bit and return its previous state. |
| `LBts`, `LBtr`, `LBtc` | The same atomic bit operation, including outside a `lock` region. |
| `Bt` | Ordinary bit test. |
| Assignment, loads, casts, comparisons, logical tests, and structural control flow | Ordinary behavior. |
| Residual integer multiplication, division, modulus, and shifts, including their compound forms | Ordinary behavior; their original integer consumers do not emit `LOCK`. |
| Scalar addition with a proven signed 32-bit immediate on either side; scalar subtraction whose proven right operand negates to a signed 32-bit value | Ordinary full-word arithmetic through the original safe immediate path. |
| Scalar AND whose result has exactly one use, as a zero/nonzero branch condition | Ordinary test and branch. |
| Residual scalar addition, subtraction, and bitwise arithmetic with no checked memory update destination | Rejected when the source flag would select an unsupported locked instruction. |

The locked update families use the existing checked integer storage domain.
They retain the selected object's type, byte extent, initialization state,
and lifetime. A lock region adds no permission to access arbitrary addresses
or memory belonging to an unrelated invocation.

[Numeric callback storage](callback-updates.md) retains its existing
eight-byte stride for `+=`, `-=`, and prefix/postfix updates; bitwise
compounds use raw words. This follows the RT_PTR storage selection in
`Compiler/PrsVar.HC:350-357`, pointer scaling in
`Compiler/PrsExp.HC:40-48`, and the update consumers in
`Compiler/BackB.HC:304-381`. Executable-owned callback targets remain
protected by the existing ownership checks.

The original backend sometimes emits invalid instructions for a compound
assignment with a dynamic right operand or a used result. The checked path
uses the verified memory destination for the complete supported update
family. It retains the valid source memory effect and after-update result
read while avoiding those invalid instruction combinations. This extension
does not claim byte-for-byte output parity with every original optimizer
choice.

There is also an original no-op optimization boundary.
`Compiler/BackA.HC:76-78,116-117` omits immediate addition, subtraction, OR,
and XOR by zero before it can emit a prefix. The checked update path retains
an atomic memory access for these operations, including a constant zero
operand. Its synchronization and access behavior can therefore be stronger
than that original optimization, even though the stored value is unchanged.

## Expression results and widths

Signed and unsigned integer storage retains its selected width of one, two,
four, or eight bytes. A memory update wraps at that width. The subsequent
read uses the selected signed or unsigned interpretation; `Bool` follows its
existing signed byte storage rule.

For example, starting with `U8 value=255`, `lock value++;` stores zero.
`lock result=value++;` reads 255 into `result` before the update. Both
`lock result=++value;` and `lock result=(value+=1);` read zero after the
update. Starting with `Bool value=127`, the corresponding after-update read
is -128. Untagged compound assignments keep their existing expression
semantics.

The read and the update are separate events for prefix, postfix, and
compound expressions. Under contention, the returned value can reflect a
different intervening update. These expressions provide an atomic memory
mutation without promising that the expression result identifies that
mutation. A bit update's returned Boolean instead belongs to its atomic bit
change, as the original `SETC` result does.

The original optimizer has a more specific path for a prefix-decrement
condition. `Compiler/OptLib.HC:255-258,382-385` can fuse `--value` with its
branch. `Compiler/OptPass789A.HC:164-170` then branches on the decrement's
flags. Checked execution currently evaluates the decrement expression and
then its condition. This preserves deterministic results, but its separate
native result read can observe an intervening update. The implementation
does not claim the original fused branch's result timing under contention.

All operand expressions and the destination selection retain their existing
evaluation order. Native retry code for a bit operation reuses already
evaluated values; it cannot repeat a source call or other operand effect.

## Ordinary operations and optimizer effects

`Compiler/BackA.HC:224-440` emits ordinary integer multiply, divide, and
modulus operations, including their compound assignments. Its shift
consumers at lines 573-658 also ignore `ICF_LOCK`. Such operations remain
ordinary even when another statement in the same region is atomic. Calls
use the called function's own compiled body, so a surrounding lock does not
add flags to a separately compiled callee.

`Compiler/OptPass3.HC:173-213` selects separate immediate ADD and SUB
consumers. For full-word integer results, `Compiler/BackA.HC:56-164` emits
ordinary register arithmetic when the resulting addition delta fits a signed
32-bit immediate. Addition accepts a proven constant on either side.
Subtraction requires it on the right and negates it first, so `value-2147483648`
fits this path while `value-(-2147483648)` does not. A narrow input still
promotes normally: `U8 value=255; lock result=value+1;` produces 256.

The constant must come from an immutable literal producer in the checked
graph. A runtime value that happens to fit the range supplies no such proof.
The graph also identifies an AND result used only by one zero/nonzero branch,
matching the ordinary `TEST` path at `Compiler/BackC.HC:1-157`. A separate
used arithmetic result or an additional consumer does not qualify. Compiler
helpers for switch dispatch remain ordinary; the selector's source
expression keeps its own operation checks.

The original early optimizer can change this classification.
`Compiler/OptPass012.HC:828-853` turns a constant power-of-two `*=` into
`<<=`, `/=` into `>>=`, and `%=` into `&=`. Consequently, `lock value%=8;`
can select an atomic AND update, while a residual `lock value%=7;` uses the
ordinary modulus consumer. The implemented source rewrites run before lock
selection, and the retained replacement keeps its region provenance.

Constant expressions can also disappear before emission.
`Compiler/OptLib.HC:161-177,196-223` transfers flags when it folds immediate
operands; `Compiler/OptPass012.HC:458-494` replaces constant bitwise and
addition expressions with immediates. A closed expression such as `2+3`
therefore does not require locked register addition. Eligible memory updates
remain observable effects and optimizer barriers after selection; folding
must not remove or reorder them.

## Checked memory and native emission

The native path validates the final destination before issuing an atomic
instruction. The complete selected width must belong to the original live
object and must be initialized for a read-modify-write access. A wider
instruction cannot borrow adjacent bytes from a neighboring scalar or from
an initialization or ownership record. Failed checks retain the existing
source and instruction fault owner, and earlier successful statements keep
their effects.

Atomic scalar accesses require natural alignment for the selected width in
the hosted execution model. This restriction is a hosted boundary: the x86
instruction reference permits misaligned locked accesses. Ordinary access
rules remain separate from this atomic check.

The original bit consumer often accesses 32 or 64 bits even though the
public parameter is `U8 *`. Checked bit operations use only the selected
owned storage width. Byte objects therefore need a width-preserving atomic
path rather than a wider `BTS`, `BTR`, or `BTC` memory access. The prior bit
must come from the successful atomic update, including when a retry is
necessary.

`Ir.Lock_semantics` classifies the retained opcode and keeps `ICF_LOCK`
separate from other instruction flags. `Ir.Lock_validation` derives the
static immediate and branch-use proofs from the checked graph.
`HCLOCK0001` reports an unsupported
locked operation and `HCLOCK0002` reports a failed atomic alignment check.
Instruction flags and source locations survive lowering. Only supported
operation and operand combinations reach native emission. The `LOCK` prefix
does not change the instruction's own flag semantics, and a move used to
read an expression result does not become a second locked operation. The
interpreter executes the same checked state changes deterministically; it
does not simulate a concurrent scheduler.

## Verification

The fixture inventory covers every admitted scalar width and operation,
lexical nesting, ordinary operations in a region, expression results,
aliases, numeric callback storage, and repeated calls. The shared positive
fixtures run in both preprocessing modes. These checks passed locally on
Windows x86-64 with OCaml 5.4.1.

| Coverage | Verified checks | Source |
| --- | --- | --- |
| Shared source fixtures | 213 positive fixtures and 10 runtime fault fixtures | `test/lock_execution_cases.ml` |
| Interpreter | 435 cases, including both modes and nine additional boundary cases | `test/test_lock_execution.ml` |
| Native API | 431 cases, including both modes and five additional boundary cases | `test/native/test_native_lock_execution.ml` |
| Public CLI | 73 successful interpreted runs and 40 invalid or faulting runs; 141 successful native and retained-task runs and 80 invalid or faulting runs, plus exact resource limits | `test/test_lock_execution_cli.ml`, `examples/lock-execution.hc` |
| Encoding and contention | Four cases covering exact bytes, generated source instructions, concurrent scalar updates, and prior-bit results | `test/native/test_native_lock_encoding.ml`, `test/native/native_lock_test_stubs.c` |

The encoding checks cover 48 explicit memory instruction forms across all
four widths and two register combinations, plus invalid LOCK IMUL rejection.
Source image checks connect the public compiler to those tested forms.
Contention probes use three native threads and verify the final value and
neighboring byte canaries for addition, subtraction, bit set, bit reset, and
bit complement. Bit probes also check the total of returned prior states.

The complete local Dune gates passed: `@fmt`, `@all`, `@install`, `@runtest`,
and `@native-tests`. The 4,161-case main suite includes the 435 lock
interpreter cases above. Generated-source checks, pinned reference
verification, all 15 release contracts, version metadata, all 528 lexer
corpus files, and the reviewed parser baseline also passed.

Negative checks cover malformed flags and operands, unsupported locked
instructions, uninitialized or expired storage, bounds, alignment, and
ownership failures. Exact and one-below resource limits stop before
the rejected operation and preserve the original failure location. Retained
task tests reject foreign owners and repeated command execution without a
second mutation, and retain completed updates when a later operation fails.

Native encoding checks and controlled contention serve different purposes.
Encoding checks establish the memory form, width, and prefix placement.
Contention checks establish that concurrent updates do not lose changes and
that modifying bit calls return the previous state of their own update.
They must not require a prefix, postfix, or compound expression's separate
read to identify its update. Source-derived expected words and native
contention results are separate evidence; interpreter/native agreement
alone cannot establish atomicity.
