# Integrated preprocessor

Every source claim on this page refers to TempleOS commit `c26482bb6ad3f80106d28504ec5db3c6a360732c`.

## Current implementation

`holyc preprocess` recognizes quoted includes and TempleOS text definitions inside the token stream. An include pushes a file-backed frame and resumes its caller at EOF. Expanding a definition pushes a separate string-backed frame and then resumes immediately after the identifier that invoked it. Tokens keep the source ID and byte span of the frame that produced them.

This command is deliberately separate from `holyc lex`. The raw lexer still returns directive and replacement text as ordinary tooling tokens; it neither reads files nor expands definitions.

Conditional directives, `#assert`, `#exe`, predefined values, and general generated source still report `HCPP0008`. They remain tracked by [issue #3](https://github.com/frankischilling/holyc-ocaml/issues/3). In particular, the built-in names in `Kernel/KernelA.HH` expand to `#exe` programs in TempleOS; this implementation does not install those names until the compile-time VM can execute them.

## Behavior taken from the pinned compiler

`Compiler/Lex.HC:LexFilePush` starts the root source at depth -1, so its first included file has depth 0. `LexGetChar` removes an exhausted included frame and continues from the saved caller position without exposing an intermediate EOF. `LexBackupLastChar` preserves the caller's byte cursor and one-character lookahead before a push. The OCaml implementation uses one lexer per frame, which retains the same resumption boundary without copying TempleOS pointer state.

The `KW_INCLUDE` branch in `Compiler/Lex.HC:Lex` accepts one string token. `Doc/PreProcessor.DD` confirms that HolyC has no angle-bracket include form. `ExtDft` supplies the logical `HC.Z` extension only when the spelling has no extension.

Relative paths follow `FileNameAbs` and `DirNameAbs` in `Kernel/BlkDev/DskStrA.HC`: they start from the compiler working directory, not the directory containing the caller. The hosted search order is therefore:

1. the configured compiler working directory;
2. each `-I` or `--include` root, in command-line order.

For an extensionless spelling, each root is checked for `.HC.Z` and then `.HC`. The second form matches the decompressed files in the pinned Git checkout. Transparent TempleOS `.Z` decompression is not implemented yet; a compressed file found under the first name is read as bytes and will not be presented as decoded HolyC source.

`--templeos-root=DIR` maps spellings beginning with `/` or `::/` into a checked-out TempleOS tree. A `~/` spelling needs task home-directory state that the hosted session does not yet model, so it reports `HCPP0009` instead of guessing. Windows drive spellings are accepted only on Windows and remain confined to the configured roots. The resolver does not add source-relative lookup.

## Definition strings

The `KW_DEFINE` branch in `Compiler/Lex.HC:Lex` stores raw text rather than a token list. It disables expansion while it reads the name, discards horizontal whitespace before the replacement, and captures through the end of the physical line. A backslash followed by LF or CRLF removes the line boundary and continues capture. Other backslash pairs remain unchanged.

Double quotes suppress `//` recognition; character quotes do not. A `//` sequence found after capture has begun ends the stored replacement, while `/* ... */` remains in the text and becomes trivia when the replacement is lexed. The pinned code copies the first replacement byte before checking for a line comment, so a replacement beginning with `//` retains those bytes in the definition table. Its later expansion still produces no ordinary token because the lexer reads the retained text as a comment. The tests preserve this source quirk instead of normalizing it to C preprocessor behavior.

Definitions have no parameters, argument substitution, token pasting, or stringification. When an identifier matches the newest visible definition, its text is pushed as another lexer frame and the identifier token is not returned. Empty replacements therefore disappear. `Kernel/KHashA.HC:HashAdd` prepends entries and `HashFind` returns the first match, so a later declaration with the same name shadows the older one without deleting its history.

The TempleOS compiler stores definitions in the task hash table. `holyc-ocaml` keeps them in `Session.t`: includes and later preprocessing calls made with the same session see the same source-ordered environment. Each replacement frame receives a stable source ID, a byte-segment map back to the captured replacement, the invocation span, and the original definition span. Terminal and JSON diagnostics from replacement text report both sites.

The current lexer resumes a caller between tokens. TempleOS can exhaust a lexical frame while it is still scanning a token, which may join punctuation from a replacement with the caller's saved lookahead. That boundary requires an oracle-backed composite byte stream and remains tracked by [issue #24](https://github.com/frankischilling/holyc-ocaml/issues/24). No compatibility result in this slice treats that case as passing.

## Hosted safety rules

TempleOS itself does not diagnose include or definition cycles, set these nesting caps, or restrict reads to allowed roots. `holyc-ocaml` adds those checks for untrusted hosted input:

- the working directory, include roots, and TempleOS root are canonicalized before use;
- a resolved file must remain under one of those roots after symbolic-link resolution;
- active canonical paths are compared before a frame is pushed;
- Windows comparisons fold ASCII case and treat slash and backslash as the same separator;
- the default nesting limit is 64 included files, excluding the root source;
- each included file is limited to 64 MiB by default;
- at most 64 definition frames may be active by default;
- one preprocessing run may inject at most 16 MiB of definition text by default;
- directories and other non-regular targets are rejected.

Use `--include-depth-limit`, `--include-byte-limit`, `--definition-depth-limit`, and `--generated-definition-byte-limit` to change these limits. Adding an include root grants read access within that directory, so it should be done only for source trees that the compilation is meant to inspect.

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
| `HCPP0010` | `#define` is missing a valid name |
| `HCPP0011` | A definition reaches itself while it is active |
| `HCPP0012` | The definition nesting limit would be exceeded |
| `HCPP0013` | The generated definition byte budget would be exceeded |
| `HCPP0014` | An embedded NUL ended a replacement |

A human diagnostic prints each `#include` site from the root toward the failing frame. A failure in replacement text also names the invocation and declaration sites. JSON keeps include entries in `include_stack` and definition provenance in `secondary`.

## Library entry point

The public API requires an explicit configuration:

```ocaml
let config =
  Holyc_lib.Preprocessor.Config.create
    ~working_directory:(Sys.getcwd ())
    ~include_roots:[ "vendor" ]
    ~max_include_depth:32
    ~max_definition_depth:32
    ~max_generated_bytes:(4 * 1024 * 1024)
    ()
  |> Result.get_ok

let tokens = Holyc_lib.preprocess session ~config ~source
```

Configuration creation canonicalizes every root and returns an error before preprocessing if a root is missing or not a directory. The streaming `Preprocessor.next` entry point and the collecting `Holyc_lib.preprocess` entry point share the same frame rules. `Preprocessor.definition_dump` emits the source-ordered `holyc-definition-dump-v1` format for tests and later compiler-state tools.
