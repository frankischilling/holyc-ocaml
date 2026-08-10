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
- Dollar-delimited DolDoc markup is outside the first raw-source slice. `$$` is still recognized as the HolyC current-position token.
