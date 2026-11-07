# Source-normalized numeric identity events

Authority: TempleOS commit `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
The numerical expectations here are source deductions, not fresh TempleOS
captures. [The runnable example](../examples/native-numeric-identities.hc)
returns integer `1` when both source-derived contrasts hold.

## Bounded support

The shared IR normalization receipt admits these exact direct-return roots
with a single-use runtime frame load `n`. Supported input→return families are
I64→I64, I64→F64 and F64→I64:

- Deleted operators: `n*1.0`, `n+0.0`, `0.0+n`, `n-0.0`.
- Surviving copy barriers: `n/1.0`, `1.0*n`.

Only the exact positive-zero and positive-one binary64 bit patterns qualify.
The proof does not authorize arbitrary algebraic rewrites. In particular,
negative-zero identity constants, computed inputs, unsigned or narrow input types,
assignment/call contexts, surrounding arithmetic, reassociation and explicit
intrinsic cancellation do not acquire normalization authority from this receipt.
A negative-zero runtime F64 input is supported; it is not a negative-zero
identity constant. Other independently proved compiler paths retain their own
admission rules.

## Why the results differ

For the I64→I64 row with runtime `n=9007199254740993`, the deleted group
returns I64 bits `0x4340000000000000`, the binary64 representation of
`9007199254740992`. The surviving group returns numeric integer
`9007199254740992` instead. For `n=-3`, the results are respectively I64 bits
`0xc008000000000000` and numeric integer `-3`.

These are not interchangeable integer-to-float-to-integer casts:

1. `PrsStmt.HC:1111-1115` creates RETURN_VAL with the declared I64 class.
   `OptLib.HC:124-139` installs RES_TO_F64 on the runtime integer operand,
   without changing that operand's raw ic_class.
2. `OptPass012.HC:376-384,559-566,588-594,661-668` deletes the supported
   right-one/zero identities in pass2, transfers flags and initially copies
   the arithmetic node's ic_class2 to the surviving integer load.
3. The grouped end handler at `715-721` recomputes the survivor's ic_class2
   from its raw I64 class. Separately, `1268` pushes the current, deleted
   arithmetic node's ic_class2. The stacked class is therefore still F64.
4. `OptPass3.HC:80-86` sees RES_TO_F64 and selects an F64 argument carrier,
   but return classification at `249-256` checks the survivor's raw ic_class.
   It does not install RES_TO_INT on the surviving I64 load.
5. `OptPass5.HC:53-76` migrates the MOV/REG conversion to RETURN's argument.
   `OptPass789A.HC:81-85,779-782` emits FILD/FSTP64 and returns the resulting
   raw bits in RAX. No FISTTP is present in this recipe.

In contrast, F64 `/1.0` and left `1.0*` survive pass2 and retain a raw F64
arithmetic producer. Return classification installs RES_TO_INT. The templates
at `BackFB.HC:488-515` explicitly mark the copy as a non-linkable floating
event. The recipe is FILD, FSTP64, bit copy, FLD64, FISTTP64. It must not keep
the integer conversion in extended precision across that copy.

## Cross-family direct returns

The original input class, surviving expression class and declared return class
are distinct receipt fields. The six forms are numerically uniform within each
cross-family row, but their event positions remain different:

- I64→F64 performs one signed FILD/FSTP64 conversion. Deleted identities move
  that conversion to RETURN's first operand. Surviving copy barriers convert
  the integer arithmetic operand before their non-linkable bit copy. With
  `n=9007199254740993`, all six return F64 bits `0x4340000000000000`.
- F64→I64 performs one FLD/FISTTP64 conversion. Deleted identities acquire
  RES_TO_INT on the surviving F64 load during return classification, then
  migrate it to RETURN's first operand. Surviving copies keep a separate
  result conversion after the copy. With `n=-3.75`, all six return integer
  `-3`; negative zero returns `0`.

`OptPass3.HC:249-256` adds return conversion only after identity normalization.
For an I64 alias returned as F64, ORing RES_TO_F64 does not create a second
conversion. For an F64 alias returned as I64, the new RES_TO_INT conversion is
not present before this phase. Operand migration follows `OptPass5.HC:53-76`;
the surviving copy barrier follows `BackFB.HC:488-515`.

These rows do not inherit the raw-bit I64→I64 alias return behavior. Tests cover
I64 extremes, fractions, signed boundaries, infinities and NaNs. Their
assertions concern result bits, not the floating-point exception-status flags.
F64→F64 stays with the existing bit-preserving identity paths rather than gaining
an unnecessary numeric-conversion receipt.

## Authentication and execution

`Numeric_normalization` authenticates the original canonical body, runtime
ownership, original frame header, exact input/result identities, flags,
literal bits and direct return use. It retains separate raw, class2 and
stacked classes, five ordered phase states including declared-return class,
eliminated-instruction identities, and physical steps annotated with the
source instruction owner and argument/instruction/ result event position.

The backend consumes this closed receipt; it does not mutate canonical IR or
reinterpret arbitrary conversion flags. The original canonical types and
conversion flags still prove the checked return carrier, while the normalized
event recipe determines which actual conversion instructions execute. A
deleted source operator is emitted at its original execution site with the
proved materialized result, preserving the hosted instruction-budget
accounting.

Cached receipts revalidate canonical source/header ownership and original graph
records. Tests mutate flags, opcode, operand order, source AST, header and
result class, and test missing/foreign source authority. Exact and one-below
IR/code/step/frame limits exercise retained-image recovery for both recipes.

CPU requirements follow emitted instructions, not the declared return type.
The raw-bit I64-return recipe needs x87 but not FISTTP/SSE3. The I64→F64
recipes likewise need no FISTTP. The I64→I64 copy barriers and all F64→I64
recipes require FISTTP and retain the existing pre-execution CPU gate.