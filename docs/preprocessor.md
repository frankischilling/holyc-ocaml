# Integrated preprocessor

Every source claim on this page refers to TempleOS commit `c26482bb6ad3f80106d28504ec5db3c6a360732c`.

## Current implementation

`holyc preprocess` recognizes quoted includes, TempleOS text definitions, deterministic `#if` and `#assert` expressions, JIT/AOT mode conditionals, symbol-presence conditionals, `#help_index`, and `#help_file` inside the token stream. An include pushes a file-backed frame and resumes its caller at EOF. Expanding a definition pushes a separate string-backed frame and then resumes immediately after the identifier that invoked it. Conditional and help state belongs to the preprocessing stream and can span either kind of frame. Tokens and metadata keep the source ID and byte span of the frame that produced them.

This command is deliberately separate from `holyc lex`. The raw lexer still returns directive and replacement text as ordinary tooling tokens; it does not read files, expand definitions, or select conditional branches.

`#exe`, predefined values, and general generated source still report `HCPP0008` when reached in active input. They remain tracked by [issue #3](https://github.com/frankischilling/holyc-ocaml/issues/3). In particular, the built-in names in `Kernel/KernelA.HH` expand to `#exe` programs in TempleOS; this implementation does not install those names until the compile-time VM can execute them.

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

## Help metadata

`Compiler/Lex.HC:KW_HELP_INDEX` reads one string and replaces `CCmpCtrl.cur_help_idx`. It calls `LexExtStr` with `lex_next=FALSE`, which joins another string only when an immediate backslash requests continued lexical input. Whitespace alone does not join adjacent strings. An empty value clears the effective index. The hosted stream keeps every index event for traceability and separately exposes the final nonempty value, if one remains.

`KW_HELP_FILE` reads one string, applies the `DD.Z` default through `ExtDft`, resolves it through `FileNameAbs`, and creates a public `CHashSrcSym` with `HTT_HELP_FILE`. `HashSrcFileSet` attaches the current source link and a copy of the nonempty help index. `Frontend.Help_metadata` records the declared spelling, effective path, hosted resolved path, `FL:` source link, active index, directive and value spans, include stack, and definition trace. Records stay in source order across includes and definition expansions. An inactive directive has no effect.

The extension check mirrors `FileExtDot`, including its unusual rule that a qualifying dot anywhere in the full spelling counts as an extension. `FileNameAbs` strips leading and trailing TempleOS whitespace and removes non-whitespace control bytes before path handling; the hosted metadata path applies the same sanitation. An embedded NUL is rejected instead of treating the suffix as invisible.

Hosted resolution is lexical and root confined. It does not stat, open, create, or read the target help file. TempleOS root spellings still require `--templeos-root`, and an escaping path reports `HCPP0004`. The resolved host path and original TempleOS spelling remain separate because their strings are not byte-identical representations.

Use `holyc preprocess --dump-help-metadata FILE` for the `holyc-help-metadata-v1` human report. Add `--format=json` for the versioned JSON object. The same value is returned as `Preprocessor.output.help_metadata`. The pinned `Kernel/KernelC.HH` corpus check records 104 index directives and 20 help-file directives, matching the source counts exactly.

This slice does not parse DolDoc or insert help-file entries into the future semantic hash table. A malformed directive retains its nonstring input and reports `HCPP0028` or `HCPP0029`; the pinned lexer skips the metadata mutation without issuing the same hosted diagnostic. That difference prevents malformed tooling input from appearing successful and does not affect valid corpus input.

## Symbol conditionals

The `KW_IFDEF` and `KW_IFNDEF` cases in `Compiler/Lex.HC:Lex` set `CCF_NO_DEFINES` before reading one identifier. A definition with that name is found, but its replacement is not injected into the operand. `#ifdef` selects the first branch when the identifier has a compiler hash entry; `#ifndef` reverses that result.

The test is wider than a C macro lookup. `CmpCtrlNew` uses `HTG_TYPE_MASK-HTT_IMPORT_SYS_SYM`, so definitions, globals, classes, internal types, functions, keywords, assembler entries, files, modules, help files, frame pointers, and exports may count. Imports do not. Identifier lookup checks `local_var_lst` before the hash tables. A matching local variable stops the hash lookup and leaves `cc->hash_entry` null, so a local can make `#ifdef` false even when the same spelling exists in the compiler hash chain.

`Driver.Session` owns the hosted visibility environment. New sessions seed 570 records from the checked generated tables: 48 language keywords, 25 assembly keywords, 17 internal types, 106 registers, 325 canonical opcodes, and 49 opcode aliases. Registers use `HTT_REG`; canonical opcodes and aliases use `HTT_OPCODE`. Entries retain an explicit TempleOS hash kind, a stable ID, and source provenance. Parser and semantic work can register functions, classes, globals, exports, imports, and other entries between calls to `Preprocessor.next`. A scoped local context models the separate `local_var_lst` result. `holyc-symbol-visibility-v1` is the deterministic state dump for tests and future inspection commands.

The standalone `holyc preprocess` command does not parse declarations. It sees the complete static compiler seed set, definitions encountered in source order, and anything its caller registered in the session, but it cannot infer an earlier HolyC function or class declaration yet. That remaining boundary is reported as a known difference rather than silently treating `#ifdef` as definition-only. [The assembler notes](assembler.md) document the register and opcode source grammar and its current implementation limit.

Inactive symbol conditionals use the same raw scan as inactive mode conditionals. Their operand is not tokenized or looked up, so malformed discarded text has no symbol-state side effect. An active directive without an identifier reports `HCPP0020`; its matching branches remain inert during recovery.

## Constant preprocessor expressions

The `KW_IF` case in `Compiler/Lex.HC:Lex` sets `CCF_IN_IF`, reads the first term, and calls `LexExpression`. `Compiler/PrsExp.HC:LexExpression2Bin` normally sends that expression through the ordinary HolyC compiler and executes it. The expression is not C preprocessor arithmetic.

The `KW_ASSERT` case calls the same evaluator. When the returned 64 bits are zero, it calls `LexWarn` with `Assert Failed`, increments the warning count, and continues with the evaluator's lookahead token. A true assertion is silent. The hosted stream reports `HCPP0024` for a false constant assertion, retains the next token, and keeps a successful process status when no error is present.

The current hosted slice evaluates a deterministic subset before the full parser and compile-time VM exist. It accepts integer, character, multi-character, and floating literals; parentheses; unary `~`, `!`, `-`, and `+`; power; shifts; multiplication, division, modulo, addition, and subtraction; bitwise AND, XOR, and OR; comparisons and equality; and logical AND, XOR, and OR. It gets precedence, association, and IC identity from the checked `Operator.binary_operators` table. Power is right associated. Power and shifts share the source precedence band, so the association of the next operator determines how a mixed expression groups. Comparison chains compare each adjacent pair instead of feeding a Boolean result into the next comparison.

Integer operations use explicit 64-bit values. Arithmetic wraps at the target width, shifts mask the count to six bits, unsigned division and comparison use unsigned `Int64` operations, and signed division rejects the minimum-value divided by negative one case. Integer-to-F64 promotion follows the pinned constant folder's signed I64 conversion even when the integer class is U64. Power returns `F64`. The pinned constant folder compares F64 equality by raw bits but uses floating comparisons for ordering. When the common-type rules promote a shift or bitwise operation to `F64`, the operation also applies to the raw floating bits. Unary `!` retains an F64 result for an F64 operand, so its true value has raw bits equal to one rather than the encoding of `1.0`. Final branch truth uses the returned 64 bits, which means positive floating zero is false and negative floating zero is true at this boundary.

Logical operands are both evaluated. This matches the ordinary expression and optimizer path in the pinned source rather than adding C-style preprocessor short circuiting. For example, `#if 0&&1/0` reports division by zero.

Definitions expand through their ordinary lexical frames before evaluation. `defined` accepts the source form with or without nested parentheses and queries the session symbol environment only when its operand remains an identifier token. A keyword or literal operand is false. Because the pinned `defined` path does not set `CCF_NO_DEFINES`, a text definition may replace its operand before lookup. This differs from `#ifdef`, which suppresses operand expansion. A visible local counts as defined, while an import excluded by the default hash mask does not.

The evaluator retains the first token that does not belong to the expression. A selected `#if` branch therefore receives its first body token, an adjacent `#else` or `#endif` remains visible to the conditional stack, and preprocessing after `#assert` resumes without dropping the next token. Errors inside includes keep the include chain. Errors in definition-expanded terms name both the expansion and declaration sites. An invalid condition leaves both conditional branches inert during recovery. An invalid assertion reports its expression error and continues token collection so tooling can show later diagnostics.

Function calls, mutable globals, memory access, casts, `sizeof`, `offset`, `lastclass`, assignments, `$$`, and other runtime terms report `HCPP0022` in either directive. TempleOS can execute those forms because it compiles the ordinary expression. [Issue #33](https://github.com/frankischilling/holyc-ocaml/issues/33) tracks the verified IR and compile-time VM path; until then, this project does not guess their values or claim full `LexExpression` compatibility.

TempleOS renders a failed assertion at the retained lookahead token because that token is current when `LexWarn` runs. The hosted diagnostic points to the `#assert` directive and names the resume token as related context. This rendering difference keeps the failure location actionable without changing assertion truth, warning severity, token order, or exit behavior.

## JIT and AOT conditionals

`Kernel/KernelA.HH` assigns `CCF_AOT_COMPILE`, and `Compiler/CMain.HC:CmpBuf` sets it for AOT compilation. A controller without that flag follows the JIT path. `Preprocessor.Config.create` therefore defaults to `Jit` without consulting the host platform. Pass `~compilation_mode:Aot` through the library or `--mode=aot` to `holyc preprocess` to select the other branch.

The `KW_IFAOT` and `KW_IFJIT` cases in `Compiler/Lex.HC:Lex` include the matching branch and scan past the other one. `#else` switches the selected side, and `#endif` closes the innermost conditional. While a branch is inactive, the pinned compiler reads raw bytes until `#` and lexes only the following directive name. The OCaml lexer exposes the same bounded raw scan. It does not expand ordinary discarded identifiers, load an inactive include, install an inactive definition, or report malformed quoted text that lies wholly inside the discarded branch.

The nesting search recognizes `#if`, `#ifdef`, and `#ifndef` with the two mode openers. This prevents a nested conditional inside discarded input from closing its parent early. Active `#ifdef` and `#ifndef` evaluate through the session visibility environment, while active `#if` uses the constant evaluator described above. Inactive `#if` text is not evaluated.

Conditional boundaries can cross an included file or a definition-backed frame because the state belongs to the stream rather than an individual lexer. A definition may also provide the spelling after `#`; definition recursion and generated-byte guards still apply when that happens.

The hosted stream diagnoses a stray `#else`, duplicate `#else`, stray `#endif`, and EOF before `#endif`. The pinned lexer silently accepts or skips some of those malformed forms. [Issue #27](https://github.com/frankischilling/holyc-ocaml/issues/27) tracks the compatibility rendering and oracle fixtures. Normal hosted runs keep the explicit errors so a typo cannot discard the rest of a file without explanation.

## Hosted safety rules

TempleOS itself does not diagnose include or definition cycles, set these nesting caps, or restrict reads to allowed roots. `holyc-ocaml` adds those checks for untrusted hosted input:

- the working directory, include roots, and TempleOS root are canonicalized before use;
- a resolved file must remain under one of those roots after symbolic-link resolution;
- a help-file metadata path is confined lexically and never opens the target;
- active canonical paths are compared before a frame is pushed;
- Windows comparisons fold ASCII case and treat slash and backslash as the same separator;
- the default nesting limit is 64 included files, excluding the root source;
- each included file is limited to 64 MiB by default;
- at most 64 definition frames may be active by default;
- at most 64 conditional directives may be nested by default;
- one `#if` or `#assert` expression may contain at most 512 parsed terms, groups, and operators by default;
- one preprocessing run may inject at most 16 MiB of definition text by default;
- directories and other non-regular targets are rejected.

Use `--include-depth-limit`, `--include-byte-limit`, `--definition-depth-limit`, `--conditional-depth-limit`, `--conditional-expression-node-limit`, and `--generated-definition-byte-limit` to change these limits. Adding an include root grants read access within that directory, so it should be done only for source trees that the compilation is meant to inspect.

## Diagnostics

| Code | Meaning |
| --- | --- |
| `HCPP0001` | Missing or unknown directive name |
| `HCPP0002` | Missing, empty, or invalid quoted include path |
| `HCPP0003` | No source candidate was found |
| `HCPP0004` | The resolved include or metadata path is outside the allowed roots |
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
| `HCPP0021` | `#if` or `#assert` has no constant expression term |
| `HCPP0022` | A term or operator requires the later semantic or compile-time execution path |
| `HCPP0023` | A parenthesized preprocessor expression has no closing `)` |
| `HCPP0024` | A constant `#assert` expression evaluated to false; this is a warning |
| `HCPP0025` | Integer division or modulo uses a zero divisor |
| `HCPP0026` | The preprocessor expression node limit would be exceeded |
| `HCPP0027` | Signed division would overflow the 64-bit result |
| `HCPP0028` | `#help_index` or `#help_file` is missing its string argument |
| `HCPP0029` | A `#help_index` continuation is not followed by another string |
| `HCPP0030` | A help-file path contains a hosted-invalid byte or cannot be resolved |

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
    ~max_expression_nodes:256
    ~max_generated_bytes:(4 * 1024 * 1024)
    ()
  |> Result.get_ok

let output = Holyc_lib.preprocess_detailed session ~config ~source
let tokens = output.tokens
let diagnostics = output.diagnostics
let help_metadata = output.help_metadata
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

Configuration creation canonicalizes every root and returns an error before preprocessing if a root is missing or not a directory. The streaming `Preprocessor.next` entry point and the collecting `Holyc_lib.preprocess_detailed` entry point share the same frame rules. The detailed result preserves tokens with warnings and exposes `Preprocessor.has_errors`; `Holyc_lib.preprocess` remains a convenience result that returns `Error diagnostics` only when an error-severity item exists. Use the detailed entry point when warnings or help metadata must be retained. `Preprocessor.definition_dump` emits the source-ordered `holyc-definition-dump-v1` format. `Help_metadata.human` and `Help_metadata.json` emit `holyc-help-metadata-v1`. `Symbol_visibility.Environment.dump` emits `holyc-symbol-visibility-v1`.
