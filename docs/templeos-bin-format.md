# TempleOS BIN format

This document records the module header and patch records used by TempleOS commit `c26482bb6ad3f80106d28504ec5db3c6a360732c`. The checked OCaml specification is generated from that pinned source. It is an input to the future module writer and loader model; the current compiler does not yet read or emit `.BIN` files.

## File layout

A module contains a 32-byte `CBinFile` header, its code and initialized data, and a patch table. `Compiler/CMain.HC:Cmp` sets the patch-table offset to `sizeof(CBinFile) + aot_U8s` and rounds the complete file size up to 16 bytes.

| Offset | Source type | Field | Meaning |
| ---: | --- | --- | --- |
| 0 | `U16` | `jmp` | Encodes the short jump over the header. The writer stores `0x1eeb`, whose bytes are `EB 1E`. |
| 2 | `U8` | `module_align_bits` | The loader computes alignment as `1 << module_align_bits`. |
| 3 | `U8` | `reserved` | Written as zero by the pinned compiler. |
| 4 | `U32` | `bin_signature` | Must equal `'TOSB'`, packed as `0x42534f54`. |
| 8 | `I64` | `org` | Requested module origin, or `INVALID_PTR` when the loader may choose an address. |
| 16 | `I64` | `patch_table_offset` | Byte offset from the beginning of the header to the patch table. |
| 24 | `I64` | `file_size` | Padded size written by the compiler. |

`Kernel/KLoad.HC:Load` rejects a zero alignment or the wrong signature. Its `module_base` is the address immediately after the header, not the address of the header itself. The requested origin applies to the header address.

## Patch-record envelope

Every nonterminal record starts with these fields:

```text
U8  entry_type
U32 value
U8  name[]   // NUL-terminated, and sometimes empty
```

The meaning of `value`, the name, and any following payload depends on `entry_type`. `IET_END` is the single zero type byte; it does not carry the rest of the envelope. The pinned compiler writes exports before imports and ends the table with `IET_END`.

The defined numeric gaps are intentional. Code 1 and codes 12 through 15 are reserved. A decoder must distinguish those values from both known entries and otherwise unknown codes.

## Entry registry

| Code | Source name | Source status | `value` | Name and payload | Loader action |
| ---: | --- | --- | --- | --- | --- |
| 0 | `IET_END` | Active | None | No name or payload | Stops either loader pass |
| 2 | `IET_REL_I0` | Fictitious | Patch offset | First name selects an import; empty names continue it | Import group; no bytes patched |
| 3 | `IET_IMM_U0` | Fictitious | Patch offset | First name selects an import; empty names continue it | Import group; no bytes patched |
| 4 | `IET_REL_I8` | Active | Patch offset | Grouped import name | Writes `target - field - 1` |
| 5 | `IET_IMM_U8` | Active | Patch offset | Grouped import name | Writes the target value |
| 6 | `IET_REL_I16` | Active | Patch offset | Grouped import name | Writes `target - field - 2` |
| 7 | `IET_IMM_U16` | Active | Patch offset | Grouped import name | Writes the target value |
| 8 | `IET_REL_I32` | Active | Patch offset | Grouped import name | Writes `target - field - 4` |
| 9 | `IET_IMM_U32` | Active | Patch offset | Grouped import name | Writes the target value |
| 10 | `IET_REL_I64` | Active | Patch offset | Grouped import name | Writes `target - field - 8` |
| 11 | `IET_IMM_I64` | Active | Patch offset | Grouped import name | Writes the target value |
| 16 | `IET_REL32_EXPORT` | Active | Module-relative value | Required export name | Registers `module_base + value` |
| 17 | `IET_IMM32_EXPORT` | Active | Immediate value | Required export name | Registers `value` |
| 18 | `IET_REL64_EXPORT` | Marked “Not implemented” | Module-relative value | Required export name | Loader has a relative-export case |
| 19 | `IET_IMM64_EXPORT` | Marked “Not implemented” | Immediate value | Required export name | Loader has an immediate-export case |
| 20 | `IET_ABS_ADDR` | Active | Offset count | Empty name, then `value` U32 module offsets | Adds `module_base` to the U32 field at each offset |
| 21 | `IET_CODE_HEAP` | Marked “Not really used” | Reference count | Optional export name, I32 allocation size, then U32 offsets | Allocates from the code heap and adds its address to I32 fields |
| 22 | `IET_ZEROED_CODE_HEAP` | Marked “Not really used” | Reference count | Same shape as code heap | Zero-allocates from the code heap and patches I32 fields |
| 23 | `IET_DATA_HEAP` | Active | Reference count | Optional export name, I64 allocation size, then U32 offsets | Allocates data and adds its address to I64 fields |
| 24 | `IET_ZEROED_DATA_HEAP` | Marked “Not really used” | Reference count | Same shape as data heap | Zero-allocates data and patches I64 fields |
| 25 | `IET_MAIN` | Active | Module-relative entry offset | Empty name, no payload | Ignored in pass 1 and called in pass 2 |

The status column preserves comments in `Kernel/KernelA.HH`. It does not infer support from the loader switch. In particular, the loader contains cases for the two 64-bit export codes even though their definitions say they are not implemented.

## Imports and deferred resolution

Consecutive import records can share one symbol name. `LoadOneImport` reads a nonempty name on the first record and applies later records with empty names to the same resolved value. A later nonempty name ends that group and returns control to the outer pass.

The resolved value comes from a function's executable address, a global's data address, or the generic export value. If the name is not found, the loader retains a `CHashImport` pointing at the first record so `SysSymImportsResolve` can retry it when an export is registered. Silent mode suppresses the unresolved-reference message; it does not discard the deferred import.

Relative imports use the address after the patched field as their base. For a field address `P`, width `W`, and target `T`, the loader stores `T - P - W`. Immediate imports store `T`. The zero-width forms are retained in the registry but do not write a field.

## Exports, allocations, and absolute patches

Relative exports become `module_base + value`; immediate exports keep `value`. Registering an export immediately retries imports for the same name.

`IET_ABS_ADDR` lists module-relative offsets of U32 fields. Pass 1 adds the module base to each field unless `LDF_NO_ABSS` is set. The boot path performs the equivalent absolute patching in `Kernel/KStart32.HC:CPatchTableAbsAddr` before loading the rest of the kernel records.

Heap records optionally publish the allocated address under their name. Code-heap records carry an I32 allocation size and update I32 fields. Data-heap records carry an I64 allocation size and update I64 fields. In both cases the offsets themselves are U32 values relative to `module_base`.

## Two loader passes

`LoadPass1` resolves imports, registers exports, applies absolute patches, and performs heap allocations. `LoadPass2` scans the same table, skips each variable payload by its source-defined width, and calls every `IET_MAIN` entry unless `LDF_JUST_LOAD` was requested. The module's main records therefore run only after pass-1 symbols and relocations have been handled.

`LoadKernel` uses the same pass-1 parser with `LDF_NO_ABSS | LDF_SILENT`; its absolute patches were already applied during boot, and it intentionally does not run pass 2.

## AOT adjustment types

The eight `AAT_*` values describe compiler fixups rather than serialized patch-table entry types. They are still part of the checked specification because `CmpFixUpJITAsm` and `CmpFixUpAOTAsm` apply them while constructing code:

| Code | Source name | Operation |
| ---: | --- | --- |
| 0 | `AAT_ADD_U8` | Add through a U8 field |
| 1 | `AAT_SUB_U8` | Subtract through a U8 field |
| 2 | `AAT_ADD_U16` | Add through a U16 field |
| 3 | `AAT_SUB_U16` | Subtract through a U16 field |
| 4 | `AAT_ADD_U32` | Add through a U32 field |
| 5 | `AAT_SUB_U32` | Subtract through a U32 field |
| 6 | `AAT_ADD_U64` | Add through an I64 field |
| 7 | `AAT_SUB_U64` | Subtract through an I64 field |

Each pair adds or subtracts the current AOT base using the named field width.

## Generated OCaml specification

Regenerate the checked module with:

```text
dune exec tools/bin_record_gen.exe -- --reference-root third_party/TempleOS --manifest reference/manifest.json --output-ml src/generated/bin_records.ml --output-mli src/generated/bin_records.mli
```

`tools/bin_record_source.ml` verifies all 13 source checksums, parses the constant tables and header, checks the writer and loader formulas, scans consumers outside comments and literals, and rejects unknown `IET_*` or `AAT_*` names. `dune build @generated-check` fails when either generated file is stale.

The public API is `Holyc_lib.Templeos_bin_spec`. It provides typed entry and adjustment identities, safe name and number lookups, reserved-code decoding, record shapes, loader-pass actions, source status, checksums, and source references.

## Current boundary

This source audit does not validate a serialized module. A conforming writer still needs checked size arithmetic, little-endian field encoding, patch grouping, deterministic ordering, and relocation overflow diagnostics. The independent loader model must reject truncated strings, truncated payloads, invalid offsets, impossible alignment shifts, integer overflow, and trailing data inconsistencies. Actual compatibility requires representative generated modules to pass the pinned TempleOS loader procedure; agreement between the future writer and the project's own reader will not be enough.
