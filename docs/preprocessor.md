# Integrated preprocessor

Every source claim on this page refers to TempleOS commit `c26482bb6ad3f80106d28504ec5db3c6a360732c`.

## Current implementation

`holyc preprocess` recognizes quoted includes, TempleOS text definitions, JIT/AOT mode conditionals, and symbol-presence conditionals inside the token stream. An include pushes a file-backed frame and resumes its caller at EOF. Expanding a definition pushes a separate string-backed frame and then resumes immediately after the identifier that invoked it. Conditional state belongs to the preprocessing stream and can span either kind of frame. Tokens keep the source ID and byte span of the frame that produced them.

This command is deliberately separate from `holyc lex`. The raw lexer still returns directive and replacement text as ordinary tooling tokens; it does not read files, expand definitions, or select conditional branches.

`#if` expressions, `defined(...)`, `#assert`, `#exe`, predefined values, and general generated source still report `HCPP0008` when reached in active input. They remain tracked by [issue #3](https://github.com/frankischilling/holyc-ocaml/issues/3). In particular, the built-in names in `Kernel/KernelA.HH` expand to `#exe` programs in TempleOS; this implementation does not install those names until the compile-time VM can execute them.

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

## Symbol conditionals

The `KW_IFDEF` and `KW_IFNDEF` cases in `Compiler/Lex.HC:Lex` set `CCF_NO_DEFINES` before reading one identifier. A definition with that name is found, but its replacement is not injected into the operand. `#ifdef` selects the first branch when the identifier has a compiler hash entry; `#ifndef` reverses that result.

The test is wider than a C macro lookup. `CmpCtrlNew` uses `HTG_TYPE_MASK-HTT_IMPORT_SYS_SYM`, so definitions, globals, classes, internal types, functions, keywords, assembler entries, files, modules, help files, frame pointers, and exports may count. Imports do not. Identifier lookup checks `local_var_lst` before the hash tables. A matching local variable stops the hash lookup and leaves `cc->hash_entry` null, so a local can make `#ifdef` false even when the same spelling exists in the compiler hash chain.

`Driver.Session` owns the hosted visibility environment. New sessions seed the 48 language keywords, 25 assembly keywords, and 17 internal type records from the checked generated tables. Entries retain an explicit TempleOS hash kind, a stable ID, and source provenance. Parser and semantic work can register functions, classes, globals, exports, imports, and other entries between calls to `Preprocessor.next`. A scoped local context models the separate `local_var_lst` result. `holyc-symbol-visibility-v1` is the deterministic state dump for tests and future inspection commands.

The standalone `holyc preprocess` command does not parse declarations. It therefore sees the checked seed entries, definitions encountered in source order, and anything its caller registered in the session, but it cannot infer an earlier HolyC function or class declaration yet. The complete opcode and register seed set is tracked by [issue #30](https://github.com/frankischilling/holyc-ocaml/issues/30). This boundary is reported as a known difference rather than silently treating `#ifdef` as definition-only.

Inactive symbol conditionals use the same raw scan as inactive mode conditionals. Their operand is not tokenized or looked up, so malformed discarded text has no symbol-state side effect. An active directive without an identifier reports `HCPP0020`; its matching branches remain inert during recovery.

## JIT and AOT conditionals

`Kernel/KernelA.HH` assigns `CCF_AOT_COMPILE`, and `Compiler/CMain.HC:CmpBuf` sets it for AOT compilation. A controller without that flag follows the JIT path. `Preprocessor.Config.create` therefore defaults to `Jit` without consulting the host platform. Pass `~compilation_mode:Aot` through the library or `--mode=aot` to `holyc preprocess` to select the other branch.

The `KW_IFAOT` and `KW_IFJIT` cases in `Compiler/Lex.HC:Lex` include the matching branch and scan past the other one. `#else` switches the selected side, and `#endif` closes the innermost conditional. While a branch is inactive, the pinned compiler reads raw bytes until `#` and lexes only the following directive name. The OCaml lexer exposes the same bounded raw scan. It does not expand ordinary discarded identifiers, load an inactive include, install an inactive definition, or report malformed quoted text that lies wholly inside the discarded branch.

The nesting search recognizes `#if`, `#ifdef`, and `#ifndef` with the two mode openers. This prevents a nested conditional inside discarded input from closing its parent early. Active `#ifdef` and `#ifndef` evaluate through the session visibility environment. Active expression `#if` remains unsupported; `HCPP0008` keeps both of its branches inert so later directives do not run after the error.

Conditional boundaries can cross an included file or a definition-backed frame because the state belongs to the stream rather than an individual lexer. A definition may also provide the spelling after `#`; definition recursion and generated-byte guards still apply when that happens.

The hosted stream diagnoses a stray `#else`, duplicate `#else`, stray `#endif`, and EOF before `#endif`. The pinned lexer silently accepts or skips some of those malformed forms. [Issue #27](https://github.com/frankischilling/holyc-ocaml/issues/27) tracks the compatibility rendering and oracle fixtures. Normal hosted runs keep the explicit errors so a typo cannot discard the rest of a file without explanation.

## Hosted safety rules

TempleOS itself does not diagnose include or definition cycles, set these nesting caps, or restrict reads to allowed roots. `holyc-ocaml` adds those checks for untrusted hosted input:

- the working directory, include roots, and TempleOS root are canonicalized before use;
- a resolved file must remain under one of those roots after symbolic-link resolution;
- active canonical paths are compared before a frame is pushed;
- Windows comparisons fold ASCII case and treat slash and backslash as the same separator;
- the default nesting limit is 64 included files, excluding the root source;
- each included file is limited to 64 MiB by default;
- at most 64 definition frames may be active by default;
- at most 64 conditional directives may be nested by default;
- one preprocessing run may inject at most 16 MiB of definition text by default;
- directories and other non-regular targets are rejected.

Use `--include-depth-limit`, `--include-byte-limit`, `--definition-depth-limit`, `--conditional-depth-limit`, and `--generated-definition-byte-limit` to change these limits. Adding an include root grants read access within that directory, so it should be done only for source trees that the compilation is meant to inspect.

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
| `HCPP0015` | `#else` has no active conditional |
| `HCPP0016` | A conditional contains a second `#else` |
| `HCPP0017` | `#endif` has no active conditional |
| `HCPP0018` | A conditional reaches the end of the stream without `#endif` |
| `HCPP0019` | The conditional nesting limit would be exceeded |
| `HCPP0020` | `#ifdef` or `#ifndef` is missing a symbol name |

A human diagnostic prints each `#include` site from the root toward the failing frame. A failure in replacement text also names the invocation and declaration sites. JSON keeps include entries in `include_stack` and definition provenance in `secondary`.

## Library entry point

The public API requires an explicit configuration:

```ocaml
let config =
  Holyc_lib.Preprocessor.Config.create
    ~working_directory:(Sys.getcwd ())
    ~include_roots:[ "vendor" ]
    ~compilation_mode:Holyc_lib.Preprocessor.Aot
    ~max_include_depth:32
    ~max_definition_depth:32
    ~max_conditional_depth:32
    ~max_generated_bytes:(4 * 1024 * 1024)
    ()
  |> Result.get_ok

let tokens = Holyc_lib.preprocess session ~config ~source
```

Register a parser-visible symbol before the stream reaches a later directive when needed:

```ocaml
let symbols = Holyc_lib.Session.symbols session

let _entry =
  Holyc_lib.Symbol_visibility.Environment.add symbols
    ~name:"ParsedFunction"
    ~kind:Holyc_lib.Symbol_visibility.Function
    ()
```

Configuration creation canonicalizes every root and returns an error before preprocessing if a root is missing or not a directory. The streaming `Preprocessor.next` entry point and the collecting `Holyc_lib.preprocess` entry point share the same frame rules. `Preprocessor.definition_dump` emits the source-ordered `holyc-definition-dump-v1` format. `Symbol_visibility.Environment.dump` emits `holyc-symbol-visibility-v1`.
