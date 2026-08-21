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

The semantic frontend now assigns immutable typed results to provided fixed and variadic direct-call expressions. Those records keep source expression identity, type, value category, and forwarded class, but they are not IR values. Fixed results can carry a target conversion decision; variadic results retain only their actual class. Neither path has an operand graph, use list, block placement, side-effect model, relocation intent, or machine representation. IR lowering under M5 must consume the semantic result without treating its deterministic traversal ID as an instruction ID.
