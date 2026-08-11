# TempleOS assembler

Every source claim on this page refers to TempleOS commit `c26482bb6ad3f80106d28504ec5db3c6a360732c`.

## Current implementation boundary

The project has a checked OCaml representation of the complete `Compiler/OpCodes.DD` file. It includes registers, language keywords, assembler directives, canonical opcode records, instruction forms, and aliases. The generator verifies the pinned blob checksum and CI rejects stale output.

This is the assembler's source database, not an assembler implementation. Operand parsing, form selection, ModRM and SIB construction, instruction encoding, fixups, directives, and disassembly are not available yet. No current command accepts an assembly source file or emits instruction bytes.

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
