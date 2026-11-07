# Source-normalized numeric identity events

Authority: TempleOS commit `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
The numerical expectations here are source deductions, not fresh TempleOS
captures. [The runnable example](../examples/native-numeric-identities.hc)
returns integer `1` when both source-derived contrasts hold.

## Bounded support

The shared IR normalization receipt admits these exact direct I64-return roots
with a single-use runtime I64 frame load `n`:

- Deleted operators: `n*1.0`, `n+0.0`, `0.0+n`, `n-0.0`.
- Surviving copy barriers: `n/1.0`, `1.0*n`.

Only the exact positive-zero and positive-one binary64 bit patterns qualify.
The proof does not authorize arbitrary algebraic rewrites. In particular,
negative zero, computed integer inputs, unsigned or narrow input types,
assignment/call contexts, surrounding arithmetic, reassociation and explicit
intrinsic cancellation do not acquire normalization authority from this receipt.
Other independently proved compiler paths retain their own admission rules.

## Why the results differ

For runtime `n=9007199254740993`, the deleted group returns I64 bits
`0x4340000000000000`, the binary64 representation of `9007199254740992`.
The surviving group returns numeric integer `9007199254740992` instead.
For `n=-3`, the results are respectively I64 bits `0xc008000000000000`
and numeric integer `-3`.

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

## Authentication and execution

`Numeric_normalization` authenticates the original canonical body, runtime
ownership, original frame header, exact input/result identities, flags, literal
bits and direct return use. It retains separate raw, class2 and stacked classes,
five ordered phase states, eliminated-instruction identities, and physical
steps annotated with the source instruction owner and argument/instruction/
result event position.

The backend consumes this closed receipt; it does not mutate canonical IR or
reinterpret arbitrary conversion flags. The original canonical result flag
still proves the checked integer return carrier, while the normalized event
recipe determines which actual conversion instructions execute. A deleted
source operator is emitted at its original execution site with the proved
materialized result, preserving the hosted instruction-budget accounting.

Cached receipts revalidate canonical source/header ownership and original graph
records. Tests mutate flags, opcode, operand order, source AST, header and
result class, and test missing/foreign source authority. Exact and one-below
IR/code/step/frame limits exercise retained-image recovery for both recipes.

CPU requirements follow emitted instructions, not the declared return type.
The raw-bit I64-return recipe needs x87 but not FISTTP/SSE3. The copy-barrier
recipe and ordinary F64-to-I64 conversion require FISTTP and retain the existing
pre-execution CPU gate.
