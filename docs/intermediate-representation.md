# Intermediate-code specification

All source facts on this page refer to TempleOS commit `c26482bb6ad3f80106d28504ec5db3c6a360732c`.

## Opcode registry

`Holyc_lib.Ir_opcode` exposes one constructor for each of the 185 intermediate codes in `Compiler/CompilerA.HH`. Numeric codes cover `0x00` through `0xB8`; `0xB9` is the count sentinel and is not an operation. Lookups by numeric code, `IC_*` constant name, and display name return an option rather than substituting an unknown code.

Each metadata record retains:

- fixed zero-, one-, or two-argument shape, or the variable-argument form;
- result count;
- the source's null, dereference, assignment, or comparison category;
- `fpop` as `pops_float`;
- `not_const` as `prevents_constant_folding`;
- the `IC_*` name and display name separately;
- definition and metadata line numbers.

The field names describe the pinned table and its observed consumers. They are not a complete side-effect model. In particular, `not_const` is preserved because `OptPass012` reads it; it does not claim that every record with `FALSE` can always be folded.

## Checked instruction sequences

`Holyc_lib.Ir_instruction_sequence` is the first executable data model built on that registry. It accepts source-ordered instruction descriptions through one checked constructor. Instruction and value IDs are explicit nonnegative values; list positions and OCaml object identity never become public IDs. Each description carries the checked opcode, ordered value operands, an optional produced value, an independent target type, an optional immediate or reference payload, the original source span when one exists, and the audited `ICF_*` bits from `Kernel/KernelA.HH`. Keeping the target type independent matches `CIntermediateCode.ic_class`, including typed instructions that produce no value.

Construction checks the opcode table's fixed or variable operand shape and exact result count. A produced value must have a target type; a no-result instruction may still carry one. Construction also rejects invalid spans, flag bits outside the pinned fields, duplicate instruction or value IDs, forward uses, and missing definitions. Failures use stable `HCIR0001` through `HCIR0010` codes and retain the offending instruction and span when available. A successful value is immutable and exposes only ordered traversal.

`human` emits the versioned `holyc-ir-v1` form. It uses `IC_*` source names, decimal signed `I64` values, hexadecimal `F64` bit patterns, escaped bytes, resolved symbol IDs, kinds, and names, explicit block payloads, stable instruction and value IDs, target types, flag masks, and source ID plus byte range. The reference commit appears in the header, so a dump cannot be mistaken for a result from a moving TempleOS branch.

## Generated source

`tools/intermediate_code_gen.exe` verifies `Compiler/CompilerA.HH` and `Compiler/CInit.HC` against `reference/manifest.json`, parses them, and writes `src/generated/intermediate_codes.ml` and its interface. The generated `Ic_*` constructor is a mechanical lowercase form of the source constant, so `IC__PP` remains `Ic__pp` and `IC_PP_` remains `Ic_pp_`.

Display strings are taken directly from `intermediate_code_table`. They are not derived from constructors or constants. This matters for branch comparison records, the `IC_SWAP_I64` / `SWAP_U64` pair, integer min/max records, and integer square records. The complete mapping is listed in [the reference source map](reference-source-map.md#intermediate-code-operation-table).

## Consumer evidence

The metadata has active roles in the original compiler:

- `OptPass012` pops expression operands according to `arg_cnt`, records non-constant expressions from `not_const`, and pushes values according to `res_cnt`.
- `OptLib` reads `fpop` while deciding whether another floating result must be pushed.
- `PrsExp` uses the structural category to distinguish dereferences, assignments, and comparisons.
- `CExcept` uses the display name in intermediate-code dumps.
- later optimizer and backend passes use argument and result counts while rewriting or emitting code.

These uses justify the current typed fields and construction checks. Opcode-specific payload rules, side-effect classes, memory behavior, control-flow behavior, relocation intent, and x87 stack verification require later M5 slices.

## Current boundary

The checked sequence is a structural IR foundation, not a compiled program. It does not yet contain basic blocks, functions, branch-target checks, a lowerer, an optimizer pipeline, or an interpreter. Its payload variant records exact data without claiming that every payload is valid for every opcode; later lowering and verifier work will add those opcode-specific rules. No compatibility result on this page implies that HolyC programs can be lowered or run.

The semantic frontend now assigns immutable typed results to provided fixed and variadic expressions on resolved direct and typed callback calls. Those records keep source expression identity, type, value category, and forwarded class, but they are not IR values. A nested resolved direct or indirect call additionally retains its call-resolution identity and the source-visible return type. A selected declared default has a separate semantic record: it keeps the exact parameter and default-use identity, checked parameter type, forwarded class, ordinary-expression or `lastclass` kind, and the immediate or AOT string-constant path selected by compilation mode. It deliberately has no expression ID because no call-site expression was provided. Fixed expression results can carry a target conversion decision; a `lastclass` default can also carry the preceding provided result and derived base spelling; variadic results retain only their actual class. Ordinary function expression statements keep the same checked result plus `ICF_RES_NOT_USED` intent. Implicit-output statements retain that intent alongside the synthetic `Print` or `PutChars` target, fixed-source form, and checked output arguments. A downstream semantic result identifies the source-visible module header and canonical target or the exact outer-table entry. Function and executable top-level conditions keep the same typed result plus an `if`, `while`, `do while`, or `for` role. The top-level view also records whether the later edge tests zero or nonzero. These records do not create `IC_END_EXP`, calls, output conversions, Boolean conversions, blocks, labels, or branch instructions. None of these paths has an operand graph, use list, block placement, evaluated default payload, side-effect model, relocation intent, string allocation, or machine representation. IR lowering under M5 must consume the semantic result without treating its deterministic traversal ID or call index as an instruction ID.

Executable top-level switch selectors are also contextual semantic records, not IR values. They retain bounded or no-bound mode, source type, result class, containing statement, keyword origin, and nested call identity. They do not subtract the lowest case, construct labels or a jump table, or create `IC_SWITCH` or `IC_NOBOUND_SWITCH`.

Executable top-level switch cases remain on the semantic side of the same boundary. An implicit label has no value, a single label has one exact typed root, and a ranged label keeps its ellipsis and both roots together. An `F64` bound records integer-conversion intent; no bound is evaluated or assigned a table position. These records are not blocks, labels, constants, jump-table entries, `IC_SWITCH`, or `IC_NOBOUND_SWITCH` instructions.

An explicit `return` under executable top-level code is rejected before this semantic tree can be built. The source check in `PrsStmt` requires an active function and would otherwise emit `IC_RETURN_VAL`. That rule does not remove TempleOS's separate `LexStmt2Bin` behavior: the final ordinary top-level expression may later produce `IC_RETURN_VAL2` and `IC_RET`. The current validator enforces the first distinction but does not emit either instruction.

The implicit-output argument binder is also a semantic record, not a call instruction. It pairs each supplied fixed value or declared default with the parameter from the selected checked header, records integer or `F64` conversion intent, and separates the variadic tail. An unchecked outer function remains explicitly deferred. Later lowering must materialize defaults, apply the recorded conversions, promote variadic values, and create the actual call without confusing a semantic slot position with an IR operand number.

Function switch selectors are also semantic results rather than IR values. They retain bounded or no-bound mode, source type, result class, and nested call identity without creating the range subtraction, jump table, or switch instruction.

Switch cases remain semantic patterns at this stage. An implicit label has no fabricated expression. A single label has one checked value, while a ranged label keeps its checked start and end together. An explicit `F64` value records `ICF_RES_TO_INT`, matching the conversion inside `LexExpressionI64`; the value is not evaluated. Later constant execution and switch lowering must assign implicit values, normalize ranges, reject duplicates, and construct labels and jump-table data.
