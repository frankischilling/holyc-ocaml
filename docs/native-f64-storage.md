# Native F64 storage

Both native source targets can use F64 scalars, fixed arrays and packed members
in automatic, global and static storage in JIT and AOT source modes. One-level
owned F64 pointers can borrow that storage across generated calls and recursion.
Reads feed arithmetic, comparisons, control flow and numeric conversions through
the original source expression's x87 plan. Stores retain eight binary64 bytes.
The integer interpreter remains outside this execution subset.

The [storage example](../examples/native-f64-storage.hc) returns F64 3.75. It
borrows an unaligned packed array through a pointer parameter, reads the next
element with pointer addition, and adds two materialized values.

The [storage execution example](../examples/native-f64-storage-execution.hc)
returns I64 `42`. It combines global and static arrays, an unaligned packed
record, compound updates, numeric member conversions and loops. Both native
targets run it in either source mode:

```sh
holyc run --target=host-jit-task --mode=jit examples/native-f64-storage-execution.hc
```

## Expressions and updates

Each operand keeps its original storage owner, selected type and address proof.
The planner replays the checked source region before admitting its floating
operations. Global, static, array and member reads can therefore feed the same
extended arithmetic links as proved frame reads. A stored read supplies a
binary64 word; an arithmetic result can remain extended only until the next
source-selected boundary. For `a=9007199254740992.0` and `b=1.0`, `(a+b)-a`
returns `1.0`, while storing `a+b` in an F64 element before subtracting `a`
returns positive zero. See [native scalar F64](native-f64.md) for link selection.

| Operation | Storage behavior |
| --- | --- |
| `=` | Converts between integer and F64 when the original assignment requires it, then writes the selected destination width |
| `-x`, `+`, `-`, `*`, `/` | Uses the original x87 operand order and proved extended links |
| `%` and `%=` | Uses repeated FPREM reduction and materializes binary64; integer destinations then truncate |
| Prefix and postfix `++` / `--` | Updates the stored binary64 word and retains the original prefix/postfix result rules |
| `+=`, `-=`, `*=`, `/=` | Captures the address once, evaluates the RHS, then reads and updates that destination |
| `&=`, `\|=`, `^=`, `<<=`, `>>=` | Changes the raw destination word after converting an F64 RHS to an integer |
| Comparisons and conditions | Uses the original bitwise equality, ordered predicate and raw-word truth rules |

An F64 arithmetic compound update can consume a proved extended RHS, but its
store rounds to binary64. Remainder updates materialize both operands. For an
integer destination, the source first converts the old integer to binary64,
performs the operation, stores its F64 result and truncates it before the
integer store. Thus `I64 value=9007199254740993; value%=3.0;` stores `2`.
The remainder operator follows the dividend's sign, including negative zero,
and masked invalid operations produce NaN instead of an integer divide fault.

Prefix and postfix updates can donate their new extended value to a later
floating operation. A materialized postfix result still contains the old word.
With `value=9007199254740992.0`, `(value++)-9007199254740992.0` can return `1.0`
through the proved link even though the store rounds back to `9007199254740992.0`.
Saving the postfix result in a separate F64 local preserves the old materialized
word. These rules follow `Compiler/BackFB.HC:573-614` and
`Compiler/Templates.HC:178-190`.

Immediate arithmetic-update operands also retain the original function's
`No_builtin_const` setting. Starting with stored `-3.141592653589793`, adding
the immediate `3.141592653589793` leaves bits `0x3ca1a80000000000` when FLDPI
is selected, or positive zero with `Option(36,1)` active during function
compilation. Later option changes do not replace that function's snapshot.

The value of an arithmetic compound expression needs its own source proof.
Fixed frame locations and proved code-heap globals, members and constant array
indices can use the source's `ICF_BY_VAL` result reload. Observed indirect
`+=`, `-=`, `*=` and `/=` results reject when their original register supplier
is unproved; this includes explicit `*(&value)` wrappers. Dynamic narrow integer
compound results with an F64 operand have the same boundary. Discarding those
results still permits the supported store. A JIT global allocated with
`Option(35,1)` lacks the code-heap address guarantee. Increment, decrement and
remainder retain their separately proved result paths. The source distinction
is in `Compiler/BackFB.HC:123-126,220-222` and
`Compiler/BackFA.HC:549-605`.

## Persistent storage

Native globals and static locals also retain F64 scalars and fixed arrays.
Same-class F64 assignments, owned byte views and later reads work on both native
targets in both source modes. Storage geometry counts bytes without classifying
the value as an integer. The original F64 initializer type and byte extent remain
separate from the U8 units used by allocation and initialization cursors.

| Declaration behavior | Native source-task JIT | Native source-task AOT | Isolated native source |
| --- | --- | --- | --- |
| Uninitialized declaration followed by assignment | Supported | Supported | Supported in both modes |
| Same-class F64 initializer with an original reference | Supported | Supported at load time | Supported in AOT mode |
| Closed F64 declaration initializer | Supported | Supported during preparation | Supported in both modes |
| Closed integer/F64 initializer conversion | Supported | Supported during preparation | Supported in both modes |
| Integer/F64 initializer conversion with an original reference | Supported | Supported at load time | Supported in AOT mode |

JIT leaves execute once during declaration preparation, including statics in
unused functions. Scalar copies preserve all eight bytes, then later calls reuse
the stored value. The [persistent example](../examples/native-persistent-f64.hc)
prints `A` once and returns F64 3.75 through `host-jit-task --mode=jit`.
Earlier output and preparation work survive a later initializer fault, including
in an unused function's static array. [Numeric initializer conversion](native-numeric-initializers.md)
uses the completed expression word before copying the selected leaf's bytes.

Unread JIT bytes still fault. Original AOT zero images remain initialized;
partial byte writes do not erase that distinction. Native source-task replacement
keeps a saved function's original global. Explicitly retained images preserve
their writes across activations and GC compaction; ordinary image execution uses
a fresh arena. Instruction, logical global-byte and initializer-work limits
remain separate. One scalar F64 initializer needs three native expression steps
and eight copy bytes: 11 succeeds and 10 fails before the copy.

## AOT load initializers

In AOT mode, an original nonconstant F64 initializer can retain a load-time
assignment to its global or static destination. Both `host-jit` and
`host-jit-task` support same-class scalar, fixed-array, multidimensional and
packed aggregate leaves and integer/F64 conversions. Each assignment uses the
original sequential byte cursor and selected scalar type. Same-class F64 stores
preserve eight bytes without a conversion, including negative zero and
signaling-NaN payloads. Mixed leaves use the original `IC_ASSIGN` conversion
flags and runtime floating plan.
Static leaf expressions retain their own result class while the enclosing
declaration keeps its storage identity. A packed class containing an F64 member
therefore does not impose an integer conversion on that member's initializer.

These assignments run in source order with the top-level statements in the AOT
entry. Statics in unused functions take part in the same load sequence; later
calls reuse their stored values. A static initializer cannot borrow an enclosing
call's parameter or automatic local. Reached calls, output and faults use the
runtime instruction budget, and a later fault retains earlier output and work.
Scheduling a leaf does not charge compile-time initializer work.

Only original AOT load leaves can follow an executable top-level statement in
isolated native compilation. A later constant or byte-copy initializer rejects
before its preparation begins. JIT declaration preparation, parameter defaults
and new class declarations retain their existing ordering boundaries.

The [AOT example](../examples/native-aot-f64-initializers.hc) prints `A` once and
returns F64 3.75. Run it with either native target:

```sh
holyc run --target=host-jit --mode=aot examples/native-aot-f64-initializers.hc
```

The original source expression selects either the nonconstant load path or
declaration preparation. Closed F64 and mixed numeric leaves prepare through
their original temporary native programs in both modes. Their completed-word
conversion and copy remain separate from the AOT assignment boundary.
`Globals_on_data_heap` retains its original global rejection and static
nonconstant preparation boundary.

## Addresses and ownership

Taking an address does not read the pointee. Array subscripts retain their
selected eight-byte stride, including multidimensional arrays. Packed fields
can start at unaligned byte offsets. Saved references retain the original
allocation, offset and activation; changing the index that produced a pointer
does not retarget it. Floating pointer fields use a distinct pointee identity
in the existing ownership metadata.

Pointer addition/subtraction scale integer offsets by eight bytes. Difference
divides the byte distance by eight and requires one live original object and
extent. One-past formation does not permit dereference. Earlier successful
writes and reached instruction charges survive a later fault.

The hosted guards still reject unread bytes, out-of-window accesses and retired
stack owners. These are hosted safety rules, not claims about TempleOS fault
behavior. Pointer-array storage, deeper pointers, unproved compound result
suppliers and arbitrary linked floating regions retain separate requirements.
Isolated JIT reference-bearing initializer preparation also remains outside the
supported paths. The JIT aggregate initializer path is described in
[aggregate initializers](aggregate-initializers.md).

## Source basis

The reference is TempleOS commit
`c26482bb6ad3f80106d28504ec5db3c6a360732c`.

- `Compiler/PrsExp.HC:960-1115` selects member classes and emits their offsets
  and array strides before the corresponding index expression.
- `Compiler/PrsStmt.HC:350-463` allocates and publishes original global storage,
  zeroes AOT image bytes and reaches `PrsGlblInit`. `Compiler/PrsVar.HC:1-246`
  supplies sequential initialization, same-class F64 copying and static
  preparation; its declaration branch at lines 536-587 runs before publication.
- `Compiler/PrsVar.HC:53-112` separates original nonconstant AOT leaves from
  compile-time copies. It emits an assignment routine registered as `IET_MAIN`;
  `PrsStaticInit` at lines 215-246 shares that traversal for static declarations.
- `Compiler/OptPass012.HC:866-873` selects numeric conversion flags for the
  original AOT `IC_ASSIGN`, independently of the post-Call declaration copy.
- `Compiler/PrsExp.HC:15-63`, `Compiler/OptLib.HC:484-506` and
  `Compiler/OptPass012.HC:1259-1265` produce and resolve pointer offset scaling
  and the division used for pointer difference.
- `Compiler/OptPass789A.HC:461-468,606-608` routes address, dereference and
  assignment operations to the original backend helpers.
- `Compiler/BackLib.HC:445-691,693-708` implements `ICMov` and `ICDeref`;
  `Compiler/BackC.HC:159-211` implements `ICAssign`. Same-class RT_F64 memory
  transport uses the eight-byte move path without an arithmetic conversion.
- `Compiler/OptPass012.HC:859-894` selects arithmetic-update and bitwise/shift
  RHS conversions. `Compiler/OptLib.HC:535-554` and
  `Compiler/OptPass3.HC:165-228` select links from the original producers.
- `Compiler/BackFB.HC:1-224,573-614` implements arithmetic compound and
  increment/decrement updates. `Compiler/BackFA.HC:549-605` and
  `Compiler/Templates.HC:23-37` implement remainder updates and FPREM reduction.
- `Compiler/BackA.HC:603-658` and `Compiler/BackC.HC:539-599` implement raw-word
  shift and bitwise updates. `Kernel/KernelA.HH:1555-1557` defines the data-heap
  and builtin-constant options.

`test/test_native_f64_cli.ml` exercises exact values, packed fields, negative
zero, recursive borrowing, pointer fields and hosted storage faults through the
public CLI in both modes and both native targets. Expectations come from the
pinned source and exact binary64 words; no new TempleOS runtime capture is
claimed.
The AOT cases also check source order, unused statics, later faults, exact runtime
limits and the distinct closed-preparation and load-assignment conversions.
`test/native/test_native_numeric_initializers.ml` covers closed numeric global,
static and packed leaves, narrow copies, exceptional words and preparation
limits through both native APIs.
`test/native_initializer_authority/test_native_initializer_authority.ml` checks
original declaration receipts and typed stores before native image emission.

`test/persistent_f64_execution_cases.ml` supplies shared storage, precision,
conversion, comparison, effect and fault programs to
`test/native/test_native_persistent_f64_execution.ml` and
`test/test_persistent_f64_execution_cli.ml`. The native suite also checks
retained images, source-owner mutations, exact resource limits, GC and release.
`test/native/test_native_f64_extended.ml` covers remainder reduction, compound
conversion and option boundaries, raw-word shifts, negative literal comparisons
and rejected result suppliers. Expected words come from the pinned compiler and
instruction semantics; these fixtures add no TempleOS runtime captures.
