# Integrated preprocessor

Every source claim on this page refers to TempleOS commit `c26482bb6ad3f80106d28504ec5db3c6a360732c`.

## Current implementation

`holyc preprocess` recognizes `#include "path"` inside the token stream. It removes the directive tokens, pushes a source frame, returns the included tokens in place, and resumes the caller when the included file reaches EOF. Tokens keep the source ID and byte span of the file that produced them. A nested diagnostic carries the complete include chain in both terminal and JSON output.

This command is deliberately separate from `holyc lex`. The raw lexer still returns `#`, `include`, and the string literal as ordinary tooling tokens.

No other directive is implemented in this slice. `#define`, conditional directives, `#assert`, `#exe`, predefined values, and generated source report `HCPP0008` in preprocessed mode. They remain tracked by [issue #3](https://github.com/frankischilling/holyc-ocaml/issues/3).

## Behavior taken from the pinned compiler

`Compiler/Lex.HC:LexFilePush` starts the root source at depth -1, so its first included file has depth 0. `LexGetChar` removes an exhausted included frame and continues from the saved caller position without exposing an intermediate EOF. `LexBackupLastChar` preserves the caller's byte cursor and one-character lookahead before a push. The OCaml implementation uses one lexer per frame, which retains the same resumption boundary without copying TempleOS pointer state.

The `KW_INCLUDE` branch in `Compiler/Lex.HC:Lex` accepts one string token. `Doc/PreProcessor.DD` confirms that HolyC has no angle-bracket include form. `ExtDft` supplies the logical `HC.Z` extension only when the spelling has no extension.

Relative paths follow `FileNameAbs` and `DirNameAbs` in `Kernel/BlkDev/DskStrA.HC`: they start from the compiler working directory, not the directory containing the caller. The hosted search order is therefore:

1. the configured compiler working directory;
2. each `-I` or `--include` root, in command-line order.

For an extensionless spelling, each root is checked for `.HC.Z` and then `.HC`. The second form matches the decompressed files in the pinned Git checkout. Transparent TempleOS `.Z` decompression is not implemented yet; a compressed file found under the first name is read as bytes and will not be presented as decoded HolyC source.

`--templeos-root=DIR` maps spellings beginning with `/` or `::/` into a checked-out TempleOS tree. A `~/` spelling needs task home-directory state that the hosted session does not yet model, so it reports `HCPP0009` instead of guessing. Windows drive spellings are accepted only on Windows and remain confined to the configured roots. The resolver does not add source-relative lookup.

## Hosted safety rules

TempleOS itself does not diagnose include cycles, set a nesting cap, or restrict reads to allowed roots. `holyc-ocaml` adds those checks for untrusted hosted input:

- the working directory, include roots, and TempleOS root are canonicalized before use;
- a resolved file must remain under one of those roots after symbolic-link resolution;
- active canonical paths are compared before a frame is pushed;
- Windows comparisons fold ASCII case and treat slash and backslash as the same separator;
- the default nesting limit is 64 included files, excluding the root source;
- each included file is limited to 64 MiB by default;
- directories and other non-regular targets are rejected.

Use `--include-depth-limit` and `--include-byte-limit` to lower or raise the limits. Adding an include root grants read access within that directory, so it should be done only for source trees that the compilation is meant to inspect.

## Diagnostics

| Code | Meaning |
| --- | --- |
| `HCPP0001` | Missing or unknown directive name |
| `HCPP0002` | Missing, empty, or invalid quoted include path |
| `HCPP0003` | No source candidate was found |
| `HCPP0004` | The canonical target is outside the allowed roots |
| `HCPP0005` | The target is already active and would form a cycle |
| `HCPP0006` | The configured nesting limit would be exceeded |
| `HCPP0007` | The target is unreadable, too large, a directory, or not a regular file |
| `HCPP0008` | A recognized directive belongs to a later preprocessor slice |
| `HCPP0009` | A TempleOS path form has no configured hosted mapping |

A human diagnostic prints each `#include` site from the root toward the failing frame. JSON diagnostics carry the same entries in `include_stack`, separate from secondary spans that explain a cycle or another local relationship.

## Library entry point

The public API requires an explicit configuration:

```ocaml
let config =
  Holyc_lib.Preprocessor.Config.create
    ~working_directory:(Sys.getcwd ())
    ~include_roots:[ "vendor" ]
    ~max_include_depth:32
    ()
  |> Result.get_ok

let tokens = Holyc_lib.preprocess session ~config ~source
```

Configuration creation canonicalizes every root and returns an error before preprocessing if a root is missing or not a directory. The streaming `Preprocessor.next` entry point and the collecting `Holyc_lib.preprocess` entry point share the same frame state.
