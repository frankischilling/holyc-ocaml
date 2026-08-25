# TempleOS assembler

Every source claim on this page refers to TempleOS commit `c26482bb6ad3f80106d28504ec5db3c6a360732c`.

## Current implementation boundary

The project has a checked OCaml representation of the complete `Compiler/OpCodes.DD` file. It includes registers, language keywords, assembler directives, canonical opcode records, instruction forms, and aliases. The generator verifies the pinned blob checksum and CI rejects stale output. The HolyC parser accepts brace-delimited `asm { ... }` statements and function-local direct assembly with source-ordered instruction and directive items. Instructions retain the zero, one, or two operands named by each opcode's first form. Both paths retain their source structure in the AST.

This is still not an assembler implementation. Brace-block body tokens are classified but not split into operations. Direct operands and directive arguments are structurally classified without checking instruction forms, resolving imports, emitting data, changing assembler state, or reading `BINFILE` input. Form selection, ModRM and SIB construction, instruction encoding, fixups, directive execution, and disassembly are not available. There is no `holyc asm` command, and no current command emits instruction bytes.

## Brace-delimited HolyC assembly

`Compiler/PrsStmt.HC:PrsStmt` sends the `asm` keyword through two source paths. The top-level AOT path calls `PrsAsmBlk` directly. Top-level JIT and function compilation join the block through `CMPF_ASM_BLK`. `Compiler/Asm.HC:PrsAsmBlk` requires an opening brace, consumes assembly operations until the block close, and distinguishes assembler directives, opcode records, and label definitions by their hash types. The separate `CMPF_ONE_ASM_INS` path handles direct assembly inside a function and is described below.

The OCaml AST has a distinct `assembly_block_statement` node. It records the keyword, both braces, every body token, and source-line groups used for readable dumps. Newlines do not terminate TempleOS assembly operations, so these groups have no grammatical meaning. An instruction and a directive on the same physical line remain in one group.

Tokens are classified without rewriting their source spelling:

| AST category | Retained information |
| --- | --- |
| Canonical opcode | Canonical spelling and `alias=false` |
| Opcode alias | Source spelling, canonical spelling, and `alias=true` |
| Assembler directive | TempleOS directive ID |
| Register | Register family and number |
| Identifier, keyword, literal, operator, punctuation | Original token and category |

Leading `name:`, `name::`, and `@@name:` pairs are also recorded as ordinary global, exported global, and local labels. Their source tokens stay in the line rather than disappearing into the label annotation. Includes and definition expansions retain their canonical source, invocation site, and definition site in both AST dump formats.

When a block appears in a collected function, `Driver.Label_resolution` adds all three structural forms to the function's semantic label batch. `Compiler/Asm.HC:1081-1091` places the corresponding native record in the code-control namespace used by language `goto`, so the OCaml pass lets a language goto bind to the same stable identity. It also records the source-compatible warning-suppression use on an assembly definition. A repeated assembly spelling remains provisional: `Compiler/Asm.HC:1032-1062` accepts it only when both definitions have the same output address. Issue #174 tracks that layout-dependent check, along with local assembler scope. Operand references, direct-assembly labels, fixups, and emitted `IC_LABEL` operations remain unimplemented.

The checked inventory in `reference/assembly-blocks.json` lists every brace-delimited block found by a raw-token scan of committed `.HC` and `.HH` files under `Compiler`, `Kernel`, `Adam`, and `Demo`. At the pinned commit it contains 45 blocks in 40 files: 39 at brace depth zero and six nested inside another source block. Focused tests parse the top-level and function-local blocks in `Demo/Asm/AsmAndC1.HC` and `Demo/Asm/AsmAndC2.HC`. `HCPARSE0145` reports a missing opening brace, and `HCPARSE0146` reports an unterminated block.

## Function-local direct assembly

Before ordinary identifier-statement parsing, `Compiler/PrsStmt.HC:PrsStmt` checks for both `HTT_OPCODE` and `HTT_ASM_KEYWORD`. It rejects either direct form when no function is active. Inside a function it joins compilation with `CMPF_ASM_BLK | CMPF_ONE_ASM_INS`. `Compiler/Asm.HC:PrsAsmBlk` then skips the opening-brace check, uses `tmpo->ins[0].arg1` and `arg2` to decide whether to parse operands, and can continue while the next hash entry is another opcode or assembler keyword. A physical newline is not a grammar boundary.

`Compiler/Asm.HC:PrsAsmArg` distinguishes register operands, the eight integer size prefixes, segment prefixes, direct expressions, and bracketed addresses. When an integer spelling currently resolves to a class or internal type, lines 84-110 look up the same spelling in `cmp.asm_hash` before classifying it as a size. This matters after `Kernel/KernelA.HH:95` publishes the intrinsic-backed `U64` union: `MOV RAX,U64 8[RBP]` in `Kernel/KInts.HC:156` still uses an eight-byte prefix. The hosted parser applies that fallback only to the eight checked size spellings; locals and unrelated newer symbols still shadow them. `PrsAsmBlk` requires a comma before `arg2`; the first checked form supplies the structural arity. Assembly immediates use the ordinary expression compiler, with `CCF_ASM_EXPRESSIONS` changing symbol and `$$` behavior.

The `inline_assembly_statement` node contains one or more adjacent source-ordered items. An instruction item keeps the classified opcode, source spelling, canonical alias identity, zero to two operands, separating comma, optional semicolon, full span, and include or definition provenance. Each operand is marked register, immediate, or memory and retains every classified source token. An optional size prefix and segment prefix are also exposed directly. Physical newlines remain trivia rather than instruction delimiters.

Directive items cover the function-local source branches in `Compiler/Asm.HC:PrsAsmBlk`: `IMPORT`; `DU8`, `DU16`, `DU32`, and `DU64`, including the postfix `DUP` form; `BINFILE`; `LIST`; `NOLIST`; and `USE16`, `USE32`, and `USE64`. They retain the directive token, argument tokens, comma locations, semicolon, complete span, and provenance. `IMPORT` names and `DU*` values may be adjacent or comma-separated because `PrsAsmBlk` consumes commas when present but does not require them. `IMPORT`, `DU*`, and `BINFILE` require a semicolon; the listing and mode directives do not. The parser records `BINFILE` syntax without opening the path.

`ORG` and `ALIGN` are brace-block-only in the pinned function path and receive a direct-use diagnostic. `I0` through `F64` remain operand or declaration prefixes rather than starting a direct assembly statement. `DUP` is valid only after a `DU*` value and cannot begin an item. These distinctions prevent ordinary declarations such as `I64 value;` from being reclassified as assembly.

Visible compiler symbols control opcode and register recognition, so a local or newer declaration can shadow those spellings. Exact operand-free snippets from `Adam/AMem.HC`, `Kernel/Job.HC`, and `Demo/Graphics/Balloon.HC` parse in JIT and AOT modes. Operand-bearing tests use `MOV AL,0x41`, `CALL PUT_HEX_U8`, and the delay-loop sequence from `Demo/Asm/AsmAndC3.HC`; `CLI` and `LTR AX` from `Demo/Lectures/Ring3.HC`; and `MOV RAX,U64 8[RBP]` from `Kernel/KInts.HC`.

`HCPARSE0147` rejects a direct opcode or supported directive at top level and recommends a brace-delimited block. `HCPARSE0149` reports a missing operand, `HCPARSE0150` a missing comma, `HCPARSE0151` an unclosed address bracket, and `HCPARSE0152` a third operand. `HCPARSE0153` rejects a directive that cannot appear directly in a function, while `HCPARSE0154` through `HCPARSE0156` report malformed `IMPORT`, data, and `BINFILE` items. Direct labels, detailed address validation, form selection, directive evaluation, import publication, data and file emission, lowering, encoding, fixups, and execution remain unimplemented. The AST records syntax only and cannot establish that an item would assemble or run.

## Source grammar

`Compiler/AsmInit.HC:AsmHashLoad` reads four ordered record groups from `Compiler/OpCodes.DD`:

```text
REGISTER_KIND spelling number;
KEYWORD spelling token_id;
ASM_KEYWORD spelling token_id;
OPCODE spelling instruction_form... [: alias...] ;
```

`REGISTER_KIND` is one of `R8`, `R16`, `R32`, `R64`, `SEG`, `FSTK`, `MM`, or `XMM`. The pinned file contains 106 records: 20 R8, 16 R16, 16 R32, 24 R64, 6 segment, 8 x87 stack, 8 MMX, and 8 XMM names. Repeated register numbers are meaningful. For example, `R8` through `R15` and `R8u64` through `R15u64` name the same eight 64-bit register numbers.

An instruction form starts with zero to four opcode bytes. A comma separates those bytes from flags and operands unless the semicolon ends the canonical record immediately. `CInst` then records:

| Source form | Stored meaning |
| --- | --- |
| `NO`, `CB`, `CW`, `CD`, `CP`, `IB`, `IW`, `ID` | Opcode modifier values 0 through 7 |
| `16`, `32` | Operand-size flags |
| `+0` through `+7`, `+R`, `+I` | Add an operand value to the opcode and retain its slash selector |
| `/0` through `/7`, `/R`, `/I` | Fixed, register, or immediate slash selector |
| `!`, `&`, `%`, `=`, `` ` ``, `^`, `*`, `$$` | The eight `IEF_*` flags parsed by `AsmPrsInsFlags` |
| Up to two `ST_ARG_TYPES` names | Operand shapes and sizes derived from `cmp.size_arg_mask` |

The source loader reserves space for 32 forms per canonical opcode. The checked reader rejects a 33rd form, a fifth opcode byte, an unknown modifier or operand name, an out-of-range byte or register number, an empty alias list, duplicate opcode spellings, and records outside their ordered section. It handles the nested block comments accepted by the compiler lexer and does not turn commented records into symbols.

The pinned database has 325 canonical opcodes, 49 aliases, and 924 instruction forms. Aliases copy the canonical `CHashOpcode` record and set `OCF_ALIAS` in TempleOS. The generated OCaml table nests each alias under its canonical record so that relationship cannot be lost.

## Preserved source quirk

`REP_OUTSB` starts with a comma before its first byte sequence. `AsmHashLoad` therefore creates an empty instruction entry before its two encoded forms. The OCaml table retains all three entries, including the empty form at source line 920. Repairing the data during generation would no longer describe what the pinned compiler loads.

## Compiler symbol visibility

`AsmHashLoad` publishes register records as `HTT_REG` and canonical opcodes and aliases as `HTT_OPCODE`. `Driver.Session` seeds all 480 spellings alongside the existing keyword and internal-type records. They can therefore satisfy `#ifdef` and `defined` through the default TempleOS hash mask. Imports remain excluded, and a local symbol can still suppress hash lookup as described in [the preprocessor notes](preprocessor.md).

The session appends these records after the earlier seed groups so existing stable IDs remain unchanged. A new session contains 570 checked compiler symbols in deterministic order.

## Regeneration

```text
dune exec tools/opcode_table_gen.exe -- --source third_party/TempleOS/Compiler/OpCodes.DD --manifest reference/manifest.json --output src/generated/opcode_keywords.ml
dune build @generated-check
```

The output records the reference commit, source path, SHA-256, and source line for every generated name and instruction form.
