# Numeric callback expressions

Original one-star callback cells can supply integer unary, arithmetic, bitwise,
shift, logical and comparison operators. Plain assignment results can feed the
same consumers. Physical storage stays an eight-byte signed RT_PTR word; the
selected callback return class and anonymous signature remain separate metadata.
This applies to automatic, static and global cells, fully indexed arrays and
fixed callback parameters, including headers with F64, U0 or named class return
annotations.

```holy-c
I64 (*p)();
(p=40)|2; // 42
(p=34)+1; // 42: the parser scales 1 by eight
```

The parser retains its left operand's class before optimization and inserts
size placeholders for addition and subtraction. `OptFixSizeOf` resolves their
width from the opposite operand's optimized class. A callback on the right can
therefore scale a scalar left operand by eight. Operators selecting an ordinary
scalar class, such as `p|2`, can remove scaling from following arithmetic.
Grouping does not insert a cast. Adding two callback words adds their raw bits.
Subtracting expressions whose parser classes are both callback pointers
subtracts their wrapped byte words, then divides by eight using signed division.
That division consumes the pointer class: `(p-q)+1` adds one. A completed
comparison chain also consumes it. Shared chain operands execute once.

Numeric computation uses the original raw classes. Callback reads and assignment
results start as signed I64; an ordinary U64 operand can select unsigned division,
shift or comparison. Callback assignments retain their signed destination class.
Complement declares I64 and retains the original parser node class: `~(p|n)`
can be signed even when U64 `n` made the inner OR unsigned. The first optimizer
pass resolves size placeholders; the next pass can change a producer class
without restoring a removed multiplier. `(p*2)+1` adds one, while `(p*1)+1`
scales one by eight. Later raw selection can restore U64 after a scaled addition.
Existing source optimizer rules still apply, including masked shift counts and
constant power-of-two division. Ordinary object pointers keep their address
semantics.

Lowering preserves physical callback loads and stores. A numeric consumer gets
a full-word view of the original producer, with its dynamic executable owner
intact. Native arithmetic checks that owner at the reached operation. Numeric
words execute; owned function addresses fault with `HCIRVM0024` after earlier
operand effects. Assignment and callback copies preserve valid owners, so
`q=(p=&Function)` can still invoke the original function through q. Numeric
results create no callback signature, object reference or executable authority.
Bounds, initialization, division and quota faults retain reached output and work.

The IR runner covers JIT and AOT sources. Native source tasks execute the host ABI
in JIT mode, and original requests compile for both private x86-64 ABIs. Closed
programs use the existing isolated native domain. Saved JIT defaults and original
static initializer leaves can consume these expressions through their existing
declaration paths. Native AOT source tasks, callback members, wider indirection,
F64 or aggregate invocation, concrete arithmetic on owned code, exported HolyC
ABI entry and full compiler acceptance remain open.

## Source and verification

The reference is TempleOS `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
`Kernel/KernelA.HH:1572-1574` makes RT_PTR signed RT_I64.
`Compiler/PrsVar.HC:350-357` separates physical callback storage from its return
header. `Compiler/PrsExp.HC:15-63,174-181,203-253` retains the parser's left class, inserts
pointer scaling and difference division, and closes comparison chains with
internal I64 bookkeeping. `Compiler/OptLib.HC:96-177,195-228` chooses raw binary
and unary computation classes; lines 484-505 resolve size placeholders after
those classes are selected. `Compiler/OptPass012.HC:1259-1266` applies the
opposite operand's class to each placeholder. Lines 151-160 keep COM's node
class separate from its I64 stack class; lines 458-485 and 866-895 apply
bitwise operators and keep assignment destination classes.
`Compiler/PrsLib.HC:249-263` runs the two optimizer passes in order. These are source
audits and add no TempleOS runtime capture.

`test/callback_expression_cases.ml` supplies source-derived expected words to
the IR and native suites. Tests cover storage shapes, selected return classes,
assignment consumers, nested scaling, signed differences, unsigned operations,
chains, effects, owned-code guards, faults and exact cumulative limits.
`examples/callback-expressions.hc` returns 42 using a static callback array,
scaled difference and assignment result after a later class shadow. Maintained
CLI tests also check runtime fault reports and retained output.
