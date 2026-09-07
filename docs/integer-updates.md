# Scalar integer update expressions

`holyc run --target=ir examples/integer-updates.hc` returns 42 in 40 runtime
instructions plus 3 constant-preparation instructions in JIT and AOT modes.
`Total+=n` shares the original scalar global with both calls. Local
loops can also use `for(;n<42;n++)`, and update results compose in arithmetic,
arguments, returns, conditions and declaration initializers.

The supported objects are checked I64/U64 parameters, automatic local slots
and ordinary scalar code-heap globals. All ten compound assignments (`+=`,
`-=`, `*=`, `/=`, `%=`, `&=`, `|=`, `^=`, `<<=`, `>>=`) and both prefix/postfix
forms of `++` and `--` retain their HolyC ICs and exact destination identity.

## Read, mutation and result order

The address is prepared once. Compound RHS effects run before the update IC
reads the old destination word, computes, stores and returns its new word.
`I64 G;G+=(G=2);G;` therefore returns 4 even in JIT mode: the inner assignment
initializes G before the outer update reads it. Starting at G=1, `G+=G++`
leaves 3, while `G+=++G` leaves 4.

Prefix returns the new word; postfix returns the old word. For example, with
G=41, `++G` returns 42 and `G++` returns 41 while storing 42. Call arguments
retain right-to-left execution: `Sub(G++,G++)` receives the later old word as
its first formal argument. Conditional AND/OR still short-circuit; ordinary
logical value expressions remain eager. Comparison-chain middle operands
execute once.

The exact destination class owns update results and division/right-shift
signedness. An unsigned RHS does not promote a signed destination. Thus
`I64 G=-7;U64 D=2;G/=D;` stores -3; the corresponding `%=` stores -1. I64/U64
addition, subtraction, multiplication and increment/decrement wrap at 64 bits.
Raw runtime shift counts use the existing low-six-bit rule.

Pinned reference `c26482bb6ad3f80106d28504ec5db3c6a360732c` supplies these rules:
`PrsExp.HC:98-118,201-208` retains the update opcode and removes the destination
load before parsing a compound RHS; `BackB.HC:304-385` distinguishes old/new
results; `BackA.HC:372-431,442-595,603-658` reads the destination at the update
and selects division/right-shift behavior from its class.

## Validation, bounds and initializers

The complete program preflights location identity, exact target/result type,
RHS word type, flags and payloads before execution. A numeric word cannot stand
in for a checked address. Every run owns fresh frame/global words. Reached
unknown reads retain the hosted HCIRVM0012 boundary; zero divisors and signed
quotient overflow fault at the update operator without storing a result.
One update IC consumes one step, in addition to address/RHS instructions.
Existing step, global-byte, active-frame-byte and call-depth limits apply.

`CInit.HC:54-59,88-97` marks updates as constant barriers. `I64 H=G++;` therefore
keeps a scheduled initializer region with H's owner and JIT compile-initializer
or AOT load-initializer phase, including through calls and faults. Its execution
does not replace the last ordinary expression value.

This is raw hosted IR arithmetic. Optimizer parity remains tracked by #574 and
#585. In particular, `OptPass012.HC:827-854` rewrites compound multiplication,
division and remainder by powers of two. Signed `G%=2` may become a mask in
TempleOS and differ from raw signed remainder. The initializer guard rejects
compound shifts and known-constant divisor `/=` or `%=` with HCRUN0006, including
transitive callees, as it already does for ordinary arithmetic. Dynamic scalar
divisors remain in the accepted runtime domain. Multiplication's power-of-two
rewrite preserves the same 64-bit result within this scalar scope.

Pointer scaling, narrow/floating/aggregate objects, imported storage, static
locals, locked operations and general memory remain explicit boundaries.
Stateful compilation, optimizer parity, native backends, loader acceptance and
bootstrap remain unfinished. These are hosted regressions and a pinned-source
audit, without a new native TempleOS capture.
