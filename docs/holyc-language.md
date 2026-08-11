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

## Current boundary

This slice supplies immutable type identity and representation facts. It does not implement type declarations, promotions, conversions, pointers, aggregate layout, floating execution, or semantic diagnostics. Those claims remain absent from the compatibility report until their own source-grounded tests pass.
