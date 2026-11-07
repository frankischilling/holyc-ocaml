# Narrow register variable analysis

TempleOS can keep a narrow integer variable in a register without narrowing the result of a compound shift back to the declared type. With a signed byte parameter initialized to 127, `x <<= 1` followed by `return x` produces 254 when the parameter is selected for a register. An explicit `noreg` parameter or taking its address produces -2 instead. The unsigned byte counterparts produce 510 and 254. The reference captures in `test/oracle/register-variable-analysis` record both JIT and compiled observations.

Selection depends on more than the declared type. The original `Compiler/OptPass3.HC` scores frame members and assigns a limited set of registers; escape and register pressure can prevent selection. The captures include a larger pressure case where the signed result is -2. The retained option experiment returns 254 despite enabling `OPTf_NO_REG_VAR` before the function header. The following `#exe` clears it during body-closing lookahead, before allocation. The compiler now uses that completed-body snapshot rather than the header's linkage options.

For parameters, the analysis covers a straight-line function with two to six scalar integer parameters, one compound shift, addition, subtraction, multiplication, division, remainder or bitwise update on an 8-, 16- or 32-bit parameter, and a direct return. It ties the result to the original typed source, frame and instruction graph and rejects changes to that evidence. Explicit `noreg` is classified as stack storage. Address escapes, extra statements, control flow and larger graphs remain unproved. Within the supported shapes, distinct requests from the variable-register banks described below are covered. Fixed registers and conflicting requests remain unproved; this models their observable value behavior rather than binding host machine registers.

The updated variable and operand may appear in any parameter position. For this bounded class, the interpreter and native backends preserve the full register value through the update and return. Arguments are first converted to their declared narrow type, as in the entry-conversion captures. Explicit `noreg` continues to use narrow memory storage. The native entry path normalizes the incoming value before preserving its full-width register value.

The analysis also covers a function with one narrow local initialized from an integer literal and returned directly, optionally after a literal shift, compound addition, subtraction or multiplication. This does not implement general register allocation. Other local patterns, more parameters, aliases and other control flow remain outside the proof. Both the interpreter and native source-task paths handle this bounded case in retained initializers. The interpreter initializer guard still rejects unproved cases. The native path may still produce a narrow result for other register-selected updates. Issue #703 remains open.

The repeated 16- and 32-bit reference captures cover register results, `noreg` storage, parameter conversion and retained default initializers for each signed and unsigned width.

Local initialization differs from parameter entry: the reference can retain an out-of-range literal initializer in the register, whereas parameter entry first converts to the declared type. The local reference captures record this distinction and the corresponding `noreg` behavior.

For this parameter shape, original JIT and compiled captures also cover compound addition across all six signed and unsigned widths, with `noreg` controls.

Local compound addition and subtraction are limited to nonnegative signed-32-bit literals. Larger literals reached invalid-opcode faults in reference probes for both operations, so that optimizer edge remains outside this proof. Partial output and debugger screenshots are kept as incomplete evidence in `test/oracle/register-variable-analysis/local-add-high-fault.*` and `local-sub-high-fault.*`; they do not establish AOT results.

Original JIT and compiled captures cover the same bounded parameter and local shapes for compound subtraction, including `noreg`, retained defaults and the signed-32-bit immediate boundary.

Original JIT and compiled captures cover the bounded parameter and local shapes for compound multiplication, including high-bit operands and retained defaults.

Original JIT and compiled captures cover parameter compound AND, OR and XOR using a second parameter, including `noreg` and retained defaults. Literal bitwise updates on locals remain outside this proof because native bit operations can affect neighboring memory.

Compound division is supported for the two-parameter shape only. Original JIT and compiled observations cover signed and unsigned widths, negative operands, truncation toward zero, `noreg` and retained defaults. A zero divisor reaches the original `DivZero` exception; native exception handling remains a separate boundary.

A narrow local initialized from one signed or unsigned integer parameter and returned directly is also covered. It may be updated first with a literal shift, addition, subtraction or multiplication; addition and subtraction are limited to nonnegative signed-32-bit literals. Original JIT and compiled observations cover signed and unsigned locals, a high-bit unsigned parameter, `noreg` and retained defaults. A second signed or unsigned 8-, 16-, 32- or 64-bit parameter may supply the operand for shifts, addition, subtraction, multiplication, division, remainder or bitwise updates. Division by zero and signed quotient overflow raise the original `DivZero` exception. Additional locals and other control flow remain outside this case; native exception unwinding remains a separate boundary.

A one-parameter function can also return a literal-initialized narrow local, with or without a literal update. The same local may be updated using its full-word parameter with shifts, arithmetic or bitwise operations. Division by zero and signed quotient overflow retain their original fault behavior. Other uses of that parameter remain outside this proof.

The analysis also covers a narrow parameter or literal-initialized local incremented or decremented as a standalone statement before direct return. The reference captures cover prefix and postfix forms, explicit `noreg` storage and signed and unsigned widths. Other uses of the update expression remain outside the proof.

The same bounded parameter and local shapes may directly return a prefix or postfix increment or decrement. Prefix returns the updated value; postfix returns the previous value. Repeated original JIT and compiled captures cover each narrow width and `noreg` controls.

A literal-initialized narrow local can also directly return a supported literal compound update. Original captures cover addition, subtraction, multiplication and shifts at each narrow width. For the proven `noreg` shape, addition and subtraction return the narrowed stored value, while multiplication and shifts return the wider intermediate. Addition and subtraction retain the signed-32-bit immediate bound described above.

The same directly returned literal updates are supported for a single narrow parameter. Parameter entry first converts the argument to its declared type. For `noreg`, addition and subtraction return the narrowed stored result; multiplication and shifts return the wider intermediate.

A separate proof covers a zero-argument, zero-local function that returns a narrow global's literal addition or subtraction update, either directly or as the left side of addition, subtraction or multiplication by another nonnegative signed-32-bit literal, or addition, subtraction or multiplication by a signed/unsigned 64-bit global value, or as the sole argument to a direct function with a 64-bit parameter and result. It checks the original source and instruction graph, including the exact global identity. The literal is limited to a nonnegative signed-32-bit value. The reference's `ICAddSubEctEqu` implementation in `Compiler/BackA.HC` distinguishes the direct by-value immediate path from the indirect path. Original captures cover signed and unsigned widths in JIT and compiled execution, as well as direct calls in compile-time execution. Dynamic update operands, pointer accesses and retained initializer preparation are not included in this proof. Other nested expressions and call shapes remain outside the supported case.

A call-free function with up to six scalar parameters may also apply shifts, addition, subtraction, multiplication, division, remainder or bitwise updates using one or two unchanged parameters to two distinct scalar parameters, at least one narrow, and return their sum or either updated parameter. The analysis checks both updates, their order, and the original return expression. Mixed `reg` and `noreg` storage and distinct R12–R15 requests are supported in this shape. The reference captures include a signed byte paired with a 64-bit parameter in both operand orders. Other combinations remain unproved. `examples/register-variable-selection.hc` demonstrates the difference between register and stack values.

For three to five distinct scalar parameters, at least one narrow, a straight-line function can apply supported shift, arithmetic or bitwise updates using unchanged parameters, then return one updated variable or a sum of two to six operands, including repeated variables and parenthesized groupings. The parameters may have different widths or signedness, including full-width integers. Original JIT and compiled captures cover each operator and width, mixed-operator patterns and independent operands, with register and stack controls. The proof checks each intermediate sum type and verifies that every sum operand is one of the updated variables. Other multi-update sequences remain outside this case.

The ordered sequence can revisit a parameter in two to five updates. Every
right-hand operand must still be an unchanged parameter, and the return must
read an updated parameter or a supported sum of updated parameters. For example,
an `I8 x` parameter entering with 127, with an unchanged `step` parameter of 2,
retains 258 after `x+=step; x*=step;` in a selected register. The `noreg` control
stores the low byte after each update and
returns 2. The maintained example is
[repeated register updates](../examples/repeated-register-updates.hc).

This extension follows the pinned `OptPass3.HC` member-scoring loop: every
compound update scores its destination's existing frame member, and register
assignment is performed once for that member after the scan. Repeated uses do
not allocate new variables or force a narrow store. Tests cover signed and
unsigned 8-, 16- and 32-bit parameters, explicit registers, stack controls,
interleaved destinations, unchanged operands and exact execution limits through
both source modes and all three hosted executors. These are source-derived
expectations and hosted tests, not new TempleOS runtime captures. Changed operand
parameters, six-update sequences and general control flow
remain outside this proof.

Parameter remainder updates use the same proof as division, including single,
paired and repeated updates. The pinned `OptPass3.HC:223-247` scores
`IC_MOD_EQU` with the other compound updates. `BackA.HC:372-423` emits division
and writes RDX for remainder, retaining the full value for a selected register
and narrowing a stack store. Thus `I8 x=127` supplied as a parameter can retain
129 through `x+=step; x%=divisor;` when `step` is 2 and `divisor` is 227.
The `noreg` control returns -127.
[Register remainder](../examples/remainder-register-updates.hc) is a runnable example. Signed remainder follows the dividend's sign;
zero divisors and signed quotient overflow retain their fault diagnostics.

Hosted tests cover all six narrow widths, register and stack storage, negative
and unsigned operands, mixed update sequences and the existing proof boundaries.
These expectations come from the pinned source, not new TempleOS captures.
The restrictions on unchanged operands, parameter count and control flow still
apply. This does not close the general allocation work in #703.

## Parameter values across branches and loops

A separate whole-function proof covers two to 32 checked integer frame members
across branches, `while`, `do`, `for`, early returns and gotos. Compound updates must be
standalone statements with a direct parameter or local read as their operand.
Parameters may
change between updates, including self-updates and mutually updated parameters.
Locals need the initializer proof described below. The proof rejects escaped
addresses, Boolean members,
immediate compound operands, consumed compound results, conflicting register
requests and unsupported compiler options. It admits at most 64 blocks and 512
instructions. The register-bank checks below account for explicit requests
and `noreg`.

`OptPass3.HC:223-247,511-621` scores member accesses across the whole function and
selects nonescaping variables. `CInit.HC:244-257` and
`KernelA.HH:1804-1805` define six general register-variable slots and two
non-pointer slots. The proof requires enough unreserved capacity for every
automatic candidate without depending on score tie ordering.
`OptPass4.HC:196-219,348-398` replaces the member accesses with register operands;
`OptPass789A.HC:698-719` converts incoming parameters before keeping their full
register values. `PrsStmt.HC:150,842-852` clears the no-register flag at ordinary
function entry and sets it for try/catch, which this proof excludes.

For a signed byte parameter starting at 127, adding 2 on three loop iterations
now returns 133. Its `noreg` control returns -123. The
[control-flow example](../examples/register-control-flow.hc) subtracts those
results and returns 256. Tests cover all six narrow widths and ten compound
operators, register and stack controls, branches, backedges, changed operands,
zero iterations, exact step limits and arithmetic faults in both source modes
and all three hosted executors. These expectations follow the pinned source;
no new TempleOS runtime capture is claimed. This models source-visible values,
not a general physical register allocator.

## Values held across named calls

The same parameter proof now covers calls to original named definitions, with
provided fixed arguments and integer or U0 results. It checks each call symbol
against the retained typed source, including the named address-slot path used
for live JIT recursion. A same-name symbol from another compilation is not that
call target. Callbacks, omitted default arguments, variadic calls and extern-only
declarations remain outside the proof. Passing a parameter's address also
rejects the proof, including addresses carried by an implicit push flag.

`OptPass789A.HC:387-405,698-719` saves and restores selected variable registers
for ordinary activations. `OptPass6.HC:32-92` separately tracks temporary values
and call clobbers. `PrsExp.HC:549-577` pushes ordinary arguments and distinguishes
direct executable calls, named JIT address slots and computed callees. The
hosted backend keeps the corresponding variable values in owned full-width
frame storage, independently of temporary register allocation.

A selected signed-byte parameter starting at 127 can retain 129 across a call,
then become 131 after another addition of 2. Its stack control returns -125.
Recursive calls still convert their own incoming arguments to the declared
width. The [recursive example](../examples/register-call-lifetimes.hc) retains
129 in the first caller while its descendants retain -125, -123 and -121. Their
sum is -240; the `noreg` counterpart returns -496.

Tests cover all six narrow widths, explicit registers, stack controls, nested
calls, live arguments, procedures, loops containing calls, recursion, callee
arithmetic faults and same-image recovery after a depth fault. The recursive
checks run with both source modes and all three executors. These remain
source-derived expectations, not new TempleOS runtime captures or a general
physical register allocator.

## Local values across control flow and calls

The whole-function proof also covers automatic integer locals initialized in
its entry block. Parameters and locals together must pass the register-bank
capacity check below.
Each local needs exactly one initializer from its original typed source: an
integer literal or a direct parameter read. The initializer must be a standalone
assignment. Later assignments, conditional initializers, computed initializers,
address escapes, Boolean members and unsupported options remain unproved.
Compound updates may read another checked frame member.

`OptPass3.HC:22-48,223-247,511-621` scores local and parameter frame members in
the same allocation pass. `OptPass4.HC:348-398` redirects an assignment to a
selected register. Unlike a narrow parameter's entry conversion, an initializer
can retain a wider integer in that register. Existing original initializer
captures establish that distinction. The extension across blocks and calls is
source-derived, not a new TempleOS runtime capture.

For example, `I8 x=129` followed by three additions of 2 in a loop retains 135;
its `noreg` control returns -121. Locals remain private to each recursive
activation. The [recursive local example](../examples/register-local-lifetimes.hc)
passes each value through an I64 parameter and sums 129, 131, 133 and 135 to 528.
Tests cover all six narrow widths, literal and parameter initializers, register
and stack controls, adjacent locals, calls, and repeated execution after a
recursion-depth fault. General allocation and floating values remain open under
#703.

## Variable register bank capacity

Explicit requests now cover all eight TempleOS variable registers: RSI, RDI,
R10, R11, R12, R13, R14 and R15. Distinct requests reserve their slots even when
the variable is unused. RAX, RBX, RCX, RDX, R8, R9, RBP and RSP remain outside
this proof, as do duplicate requests. The host backend preserves the selected
values in private storage; these requests do not bind host machine registers.

The whole-function proof accepts two to 32 scalar frame members, provided every
automatic candidate fits without a score-dependent eviction. `noreg` members
consume no register slots. Unused ordinary parameters consume none either, but
an unused `reg` request still counts because `OptPass3.HC:22-48` gives it a high
initial score. The original address and escape checks still apply.

The capacity check follows `OptPass3.HC:536-621`, not just the sum of two masks.
TempleOS opens the R12/R13 non-pointer bank only when more than six automatic
candidates survive its scan. Explicit requests that occupy the general bank do
not lower that threshold. The proof therefore requires capacity even if later
optimization removes some counted accesses. With all six general slots free,
up to eight automatic integer candidates fit, less any explicit R12/R13
reservations. Otherwise, automatic candidates must fit the remaining general
slots. More demanding cases need score and tie-order analysis and remain
unproved.

`KernelA.HH:1802-1805`, `CInit.HC:244-257`, `OptPass6.HC:32-92` and
`OptPass789A.HC:387-405,698-719` separate these banks from temporary, fixed and
frame registers and preserve them across ordinary calls. The
[register-bank example](../examples/register-bank-capacity.hc) uses all eight
explicit registers across recursion and returns -240 after narrow argument
conversion. Hosted tests cover all six narrow widths, division/remainder
clobbers, calls, the 32-member bound and mixed `noreg` frames. These are
source-derived expectations, not new TempleOS runtime captures or a completed
physical register allocator.

## Compiler option snapshots

The proof admits changes to the four warning options (bits 16 through 19) and
`OPTf_NO_REG_VAR` (bit 34). Other option changes remain outside this proof.
Turning off the unused-variable warning must not turn a selected signed-byte
result of 129 into -127. Warning choices now leave that allocation decision
alone.

`OptPass3.HC:22-48` reserves explicit register requests before the automatic
selection guard at lines 536-537. Consequently, `OPTf_NO_REG_VAR` forces
ordinary and plain `reg` candidates to stack storage, but supported explicit
requests such as `reg R12` still retain full register values. `noreg` remains
stack storage in either state. When automatic allocation is disabled, its
register-capacity bound does not reject an otherwise supported frame.

The function body's option mask must equal its authenticated completed-body
snapshot. Header linkage options remain separate. A later `Option` call cannot change an already compiled function's
selection. Tests also reject a forged body mask, even when it contains only an
otherwise supported warning bit. `CMisc.HC:1-8` makes `Option` and `GetOption`
operate on the current compiler control; that mutable control is not a
replacement for the definition snapshot.

The [option snapshot example](../examples/register-option-snapshots.hc) defines
one selected function, enables `OPTf_NO_REG_VAR` before defining a second, then
clears it before calling both. Their results still differ by 256. Tests cover
six narrow widths, four warning masks, automatic/plain-reg/noreg/explicit
requests, local initializers and loops, retained definitions, a setter called
while a selected frame is active, and a child compiler's isolation from its
parent. These run through live IR and native task execution in both source
modes. The closed native batch gate does not provide a live `Option` control.

`PrsStmt.HC:923-930` advances the lexer after a closing brace. A following
`#exe` runs immediately through `Lex.HC:1031-1035` and `PrsStreamBlk`
(`PrsStmt.HC:805-839`), on the same compiler control. `PrsFun` compiles only
after that lookahead (`PrsStmt.HC:163-184`). Thus the option fixture's later
`Option(34,0)` affects allocation of the preceding function. Tests cover both
setting and clearing the bit at this boundary, including an explicit request.
The authenticated body options survive semantic analysis and IR binding;
changing the earlier header mask is neither necessary nor correct. Persistent
static ownership still checks that original header mask, even when an
initializer changes the compiler options.

A `#exe` block shares its parent's options. In contrast, `StreamExePrint` calls
`ExePutS`, which copies options into a child compiler control
(`CMain.HC:579-588,673-686`). Tests distinguish these two paths.

The matrix expectations come from pinned source and existing register/stack
captures. The lookahead regression also reproduces the recorded result in
`test/oracle/register-variable-analysis/option.hc`; no new TempleOS capture is
claimed. Other option-dependent optimizer paths remain outside this proof.

## Literal and unary loop updates

The whole-function proof now admits statement-only addition, subtraction and
multiplication by nonnegative signed-32-bit literals, shifts by normalized
literal counts, and discarded prefix or postfix increment/decrement. These
work in branches, `for`, `while` and `do` loops, including automatic locals and
values retained across named calls. For example, a selected `I8` starting at
127 reaches 129 after two iterations of `x+=1`; `noreg` reaches -127.
[The loop example](../examples/register-loop-updates.hc) compares both storage
choices with literal and unary updates.

`OptPass3.HC:149-165,223-248` scores unary and compound accesses to frame
members. `OptPass4.HC:239-261,351-380` maps those accesses to the selected
variable register. The existing literal and unary captures supply the value
semantics. `OptPass012.HC:828-854` also rewrites constant multiplication,
division and remainder. This extension admits the supported multiply and
shift forms; division/remainder and bitwise literals remain outside this
whole-function proof, as do consumed update results and large add/subtract
immediates.

Each accepted update must match the original typed operator, operand bindings
and source position. A substituted operand, changed operator or injected
payload cannot acquire a fresh proof. The same checks cover the earlier
variable-sourced updates. Register capacity, address-escape restrictions and
single-entry local initialization requirements are unchanged. The tests use
six widths, four register qualifiers and four loop/call shapes, with independent
expected register and stack arithmetic. These are hosted tests backed by pinned
source and existing captures, not new TempleOS runtime captures.
