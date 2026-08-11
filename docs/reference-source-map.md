# Reference source map

All findings in this document refer to TempleOS commit `c26482bb6ad3f80106d28504ec5db3c6a360732c`.

## Compiler assembly order

`Compiler/Compiler.PRJ` includes kernel headers first, enables conversion of extern declarations to imports while loading later kernel headers, and then loads compiler units in this order: templates, extensions, initialization, exceptions, lexer, hashes, assembler, parser, optimizer, backend, and final optimization passes. The OCaml architecture keeps the same conceptual dependencies without copying the source's global-state layout.

## Lexer state

`Kernel/KernelA.HH` defines `CLexFile` and `CCmpCtrl`. `Compiler/Lex.HC:LexFilePush` maintains the include stack, while `LexGetChar` owns one-byte pushback and line tracking. The OCaml source model stores the full byte buffer and precomputed line starts, and each lexer instance owns its current offset. Include frames will be added with the integrated preprocessor.

`Kernel/StrA.HC` shows that the lexer is byte oriented. Identifier starts include ASCII letters, underscore, at sign, and bytes 128 through 255. Digits are accepted after the first byte. The hosted lexer preserves non-ASCII bytes rather than requiring UTF-8.

## Tokens and operators

`Kernel/KernelA.HH` assigns numeric values to compound tokens. `Compiler/CInit.HC:CmpFillTables` defines two-character tokens, three-character shifts, and expression precedence. `Compiler/OpCodes.DD` supplies the keyword spellings consumed by `Compiler/AsmInit.HC`.

The first slice keeps the complete language keyword list and operator spellings in one module each. Numeric TempleOS token IDs remain available for audit and deterministic dumps, but OCaml variants provide compiler identity after lexing.

## Keyword and assembler directive records

`Compiler/AsmInit.HC:AsmHashLoad` parses `Compiler/OpCodes.DD` with the compiler lexer. Its `KEYWORD` and `ASM_KEYWORD` statements each contain a spelling, an integer ID, and a terminating semicolon. The pinned file contains 48 language records with IDs 0 through 47 and 25 assembler directive records with IDs 64 through 88.

`tools/opcode_table_source.ml` accepts only that statement shape inside the two table sections. It rejects missing semicolons, duplicate names, duplicate IDs, unexpected statements, reordered records, and incomplete ranges. `tools/opcode_table_gen.ml` writes the checked OCaml table with the reference commit, the `Compiler/OpCodes.DD` blob checksum, and each original source line. The lexer maps its keyword variants onto those generated records. `Asm_directive` exposes exact-spelling lookup for later assembler work but does not parse assembly yet.

## Primitive raw types and internal names

`Kernel/KernelA.HH` assigns raw IDs 2 through 15. The low bit is `RTF_UNSIGNED`; `RT_PTR` deliberately aliases signed `RT_I64` at ID 10. The same block marks `RT_F32` as unimplemented, `RT_UF32` as unimplemented and fictitious, and `RT_UF64` as fictitious. These slots remain visible in generated audit data but are not semantic HolyC primitive constructors.

`Compiler/CInit.HC:internal_types_table` contains 17 records. Several raw IDs have more than one name. In particular, `Bool` shares `RT_I8`, while the names ending in `i` provide internal storage for public declarations. `Compiler/AsmInit.HC:AsmHashLoad` inserts every record into the compiler hash and leaves the final record for each raw ID in `cmp.internal_types`.

The public `I16`, `U16`, `I32`, `U32`, `I64`, and `U64` names are unions declared near the start of `Kernel/KernelA.HH`. Their leading internal type names select whole-value storage, while their members expose smaller signed and unsigned views. `Primitive_type` records this union-backed declaration form but does not calculate member layout yet.

`tools/primitive_type_source.ml` validates the constants, the six union headers, and every internal record. `tools/primitive_type_gen.ml` embeds the pinned commit, both source checksums, and original line numbers in `src/generated/primitive_raw_types.ml`. `Sema.Primitive_type` uses those records for the 12 supported semantic identities.

## Literals

`Compiler/Lex.HC:Lex` accumulates integers in a target `I64`, so overflow wraps. Hex and binary prefixes are case insensitive. Character constants store up to eight decoded bytes in little-endian order. `LexInStr` defines the recognized escapes. The hosted lexer diagnoses unterminated literals instead of relying on an in-memory NUL terminator.

## Comments

The pinned lexer nests `/* ... */` comments. It consumes `//` through the end of the line. The OCaml lexer retains both as leading trivia so later diagnostics and formatting tools can recover their exact bytes.

## Diagnostics

`Compiler/CExcept.HC` prints the current token, file, line, and source line before raising the compiler exception. Hosted diagnostics retain those source facts in structured values with stable codes. Rendering is separate from construction so JSON and terminal output share the same diagnostic.

## Interpretation decisions

- Files are finite OCaml byte strings. An embedded NUL is diagnosed because TempleOS uses NUL as an internal buffer terminator.
- Lines increment on LF, matching `LexGetChar`. CR is whitespace but does not start a new line by itself.
- A raw lexer command returns `#` as punctuation. Directive execution begins in issue #3 and will replace that tooling behavior only in preprocessed mode.
- Dollar-delimited text is retained as comment trivia in raw-source mode. Interpreting DolDoc markup is outside this slice, while `$$` is recognized as the HolyC current-position token.
