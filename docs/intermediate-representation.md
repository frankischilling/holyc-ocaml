# Intermediate-code specification

All source facts on this page refer to TempleOS commit `c26482bb6ad3f80106d28504ec5db3c6a360732c`.

## Current implementation

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

These uses justify the current typed fields. Full operand shapes, side-effect classes, memory behavior, control-flow behavior, relocation intent, source spans, and x87 stack verification require the M5 IR instruction model.

## Current boundary

This table is the exhaustive opcode identity layer, not an executable IR. There are no instructions, blocks, functions, verifier, textual dump, lowerer, optimizer pipeline, or interpreter yet. No compatibility result on this page implies that HolyC programs can be lowered or run.

The semantic frontend now assigns immutable typed results to provided fixed and variadic expressions on resolved direct and typed callback calls. Those records keep source expression identity, type, value category, and forwarded class, but they are not IR values. A nested resolved direct or indirect call additionally retains its call-resolution identity and the source-visible return type. A selected declared default has a separate semantic record: it keeps the exact parameter and default-use identity, checked parameter type, forwarded class, ordinary-expression or `lastclass` kind, and the immediate or AOT string-constant path selected by compilation mode. It deliberately has no expression ID because no call-site expression was provided. Fixed expression results can carry a target conversion decision; a `lastclass` default can also carry the preceding provided result and derived base spelling; variadic results retain only their actual class. Ordinary function expression statements keep the same checked result plus `ICF_RES_NOT_USED` intent. Function conditions keep the same typed result plus an `if`, `while`, `do while`, or `for` role. These records do not create `IC_END_EXP`, Boolean conversions, blocks, labels, or branch instructions. None of these paths has an operand graph, use list, block placement, evaluated default payload, side-effect model, relocation intent, string allocation, or machine representation. IR lowering under M5 must consume the semantic result without treating its deterministic traversal ID or call index as an instruction ID.

Function switch selectors are also semantic results rather than IR values. They retain bounded or no-bound mode, source type, result class, and nested call identity without creating the range subtraction, jump table, or switch instruction.

Switch cases remain semantic patterns at this stage. An implicit label has no fabricated expression. A single label has one checked value, while a ranged label keeps its checked start and end together. An explicit `F64` value records `ICF_RES_TO_INT`, matching the conversion inside `LexExpressionI64`; the value is not evaluated. Later constant execution and switch lowering must assign implicit values, normalize ranges, reject duplicates, and construct labels and jump-table data.
