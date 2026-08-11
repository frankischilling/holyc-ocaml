# HolyC language notes

Every compatibility statement on this page refers to TempleOS commit `c26482bb6ad3f80106d28504ec5db3c6a360732c`.

## Primitive types

The current semantic API recognizes these 12 spellings:

| HolyC spelling | Raw code | Raw ID | Bytes | Signedness | Declaration form |
| --- | --- | ---: | ---: | --- | --- |
| `I0` | `RT_I0` | 2 | 0 | Signed | Internal type |
| `I8` | `RT_I8` | 4 | 1 | Signed | Internal type |
| `I16` | `RT_I16` | 6 | 2 | Signed | Public union over `I16i` |
| `I32` | `RT_I32` | 8 | 4 | Signed | Public union over `I32i` |
| `I64` | `RT_I64` | 10 | 8 | Signed | Public union over `I64i` |
| `U0` | `RT_U0` | 3 | 0 | Unsigned | Internal type |
| `U8` | `RT_U8` | 5 | 1 | Unsigned | Internal type |
| `U16` | `RT_U16` | 7 | 2 | Unsigned | Public union over `U16i` |
| `U32` | `RT_U32` | 9 | 4 | Unsigned | Public union over `U32i` |
| `U64` | `RT_U64` | 11 | 8 | Unsigned | Public union over `U64i` |
| `F64` | `RT_F64` | 14 | 8 | Not applicable | Internal type |
| `Bool` | `RT_I8` | 4 | 1 | Not applicable | Internal type using `I8i` storage |

`I0` and `U0` are distinct zero-sized types. `Doc/HolyC.DD` describes `U0` as void-like while stressing that its size is zero. The semantic model retains both types rather than folding either into an OCaml unit value.

`Bool` is also distinct. It shares the `RT_I8` code and one-byte storage with `I8`, but lookup returns a separate `Bool` constructor. Later conversion rules must make any relationship explicit.

## Internal names and public integer unions

`Compiler/CInit.HC` registers names ending in `i` as internal storage types. `I0`, `U0`, `I8`, `U8`, `F64`, and `Bool` also appear directly in that table. The larger public integer names do not. They are unions in `Kernel/KernelA.HH` whose leading type selects `I16i`, `U16i`, `I32i`, `U32i`, `I64i`, or `U64i` storage.

Those unions expose subinteger members such as bytes and words. The current API records which primitive comes from a public union and preserves the storage spelling and declaration line. Member offsets and subinteger access semantics belong to the later aggregate-layout work and are not implemented yet.

Internal spellings such as `I32i` are not accepted by `Primitive_type.of_spelling`. They remain available only as generated source facts used to describe storage.

## Unavailable floating slots

The raw table reserves three slots that are not supported HolyC primitives:

| Raw code | Raw ID | Pinned source marker |
| --- | ---: | --- |
| `RT_F32` | 12 | Not implemented |
| `RT_UF32` | 13 | Not implemented, fictitious |
| `RT_UF64` | 15 | Fictitious |

The generator preserves these records so changes in the reference cannot pass unnoticed. The semantic API has no `F32`, `UF32`, or `UF64` constructor, and lookup rejects all three spellings.

## Pointer raw representation

`RT_PTR` has raw ID 10, the same ID as signed `RT_I64`. The source comment explains that signed representation permits negative error codes. `Primitive_type.pointer_representation` records the alias for later pointer work, but `I64` values do not thereby become pointers.

## Operators and precedence

The generated operator table follows `Compiler/CInit.HC:CmpFillTables`. The binary entries appear in these source bands:

| Precedence constant | Value | Operators |
| --- | ---: | --- |
| `PREC_EXP` | `0x10` | `` ` ``, `<<`, `>>` |
| `PREC_MUL` | `0x14` | `*`, `/`, `%` |
| `PREC_AND` | `0x18` | `&` |
| `PREC_XOR` | `0x1C` | `^` |
| `PREC_OR` | `0x20` | `\|` |
| `PREC_ADD` | `0x24` | `+`, `-` |
| `PREC_CMP` | `0x28` | `<`, `>`, `<=`, `>=` |
| `PREC_CMP2` | `0x2C` | `==`, `!=` |
| `PREC_AND_AND` | `0x30` | `&&` |
| `PREC_XOR_XOR` | `0x34` | `^^` |
| `PREC_OR_OR` | `0x38` | `\|\|` |
| `PREC_ASSIGN` | `0x3C` | `=`, `<<=`, `>>=`, `*=`, `/=`, `%=`, `&=`, `\|=`, `^=`, `+=`, `-=` |

Power and assignments carry `ASSOCF_RIGHT`. Shifts, division, modulo, and subtraction carry `ASSOCF_LEFT`. Other records have no association flag in the source, so the API reports `Unspecified`. A later expression parser must preserve the original parser's use of these flags rather than treating `Unspecified` as a guessed C default.

The backtick operator maps to `IC_POWER`. HolyC logical XOR is `^^` and maps to `IC_XOR_XOR`. No ternary operator appears in the table, matching the language documentation.

The prose precedence list in `Doc/HolyC.DD` omits `%=`. The executable table includes `TK_MOD_EQU` at `PREC_ASSIGN`, mapped to `IC_MOD_EQU`; that source record is authoritative.

Compound recognition is not a flat list in TempleOS. Three ordered arrays resolve shared prefixes, while `Lex` handles `<<=`, `>>=`, `..`, `...`, and `$$` separately. The current lexer consumes the generated spellings in longest-first order, but the generated audit data retains each original recognition group and source line.

## Compiler options

Compiler options are bit indices stored in `CCmpCtrl.opts`. `Compiler/Lex.HC:CmpCtrlNew` initially enables the two warning bits at indices 16 and 19, producing the mask `0x90000`. The source leaves bits 2 through 15 and 20 through 31 unused.

| Source name | Bit | Default | Observed phase | Source note |
| --- | ---: | --- | --- | --- |
| `OPTf_ECHO` | 0 | Off | Lexing | — |
| `OPTf_TRACE` | 1 | Off | Parsing and trace output | — |
| `OPTf_WARN_UNUSED_VAR` | 16 | On | Function diagnostics | Applied to functions, not statements |
| `OPTf_WARN_PAREN` | 17 | Off | Diagnostics | Warns about unnecessary parentheses |
| `OPTf_WARN_DUP_TYPES` | 18 | Off | Parsing and diagnostics | Warns about duplicate local type statements |
| `OPTf_WARN_HEADER_MISMATCH` | 19 | On | Function declarations | — |
| `OPTf_EXTERNS_TO_IMPORTS` | 32 | Off | Parsing and linkage | — |
| `OPTf_KEEP_PRIVATE` | 33 | Off | Symbol registration | — |
| `OPTf_NO_REG_VAR` | 34 | Off | Optimization | Applied to functions, not statements |
| `OPTf_GLBLS_ON_DATA_HEAP` | 35 | Off | Allocation | — |
| `OPTf_NO_BUILTIN_CONST` | 36 | Off | Code emission | Applied to functions, not statements |
| `OPTf_USE_IMM64` | 37 | Off | Optimization and code emission | Not completely implemented in the pinned source |

HolyC's `Option(bit, state)` changes the current compile controller and returns the bit's previous state. `GetOption(bit)` reads that controller. Nested compilation copies its parent's option mask, so source can temporarily change an option around a declaration or function and then restore the previous value. The pinned kernel, startup, and demo sources use this pattern for linkage, private symbols, global allocation, warning control, and optimization boundaries.

`Sema.Compiler_option` currently provides the checked registry, typed lookup, the initial mask, and a pure form of the previous-state update. It does not yet execute `Option` in HolyC input or change parser, optimizer, or backend behavior.

## Current boundary

The implemented language specification currently supplies immutable primitive type facts, operator tables, and compiler-option facts. It does not implement expression parsing, type declarations, promotions, conversions, pointers, aggregate layout, floating execution, compiler-option effects, or semantic diagnostics. Those claims remain absent from the compatibility report until their own source-grounded tests pass.
