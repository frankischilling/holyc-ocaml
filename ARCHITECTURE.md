# holyc-ocaml architecture

The compiler is split into explicit stages. Each stage consumes immutable inputs and returns either a value or structured diagnostics. Sessions own source IDs and configuration; compiler modules do not rely on hidden global state.

The current slice has six layers:

- `Common` owns source files, byte positions, spans, diagnostics, deterministic rendering, and target-width integer helpers.
- `Frontend` owns token definitions, the audited keyword table, streaming lexical state, canonical include resolution, nested file and definition frames, `#include`, `#define`, constant `#if` and `#assert` evaluation, mode and symbol conditionals, help-directive metadata, and stable token, definition, symbol-visibility, and help-metadata dumps. The preprocessor expression evaluator uses explicit 64-bit target values and the generated HolyC precedence table. Each preprocessing stream owns its mutable cursors, one-token expression lookahead, conditional stack, current help index, ordered help-file records, selected compilation mode, generated-byte budget, expression-node budget, and ordered pending diagnostics. The session owns source IDs, the source-ordered definition environment, and a typed view of compiler hash entries. Parser and semantic work can publish declarations to that view between calls to the token stream. It is not the final semantic symbol table.
- `Generated` contains deterministic source facts produced only after pinned checksum and table-shape validation.
- `Sema` currently exposes primitive type identity, compiler-option metadata, and the audited function-flag model. Declaration resolution, conversions, call checking, and aggregate layout have not entered this layer yet.
- `Ir` currently exposes the exhaustive intermediate-code identity and source metadata table. Instructions, control flow, verification, lowering, and execution have not entered this layer yet.
- `Backend` currently exposes the audited TempleOS BIN header, patch-record, relocation, loader-action, and AOT-adjustment specification. It does not contain a serializer, loader, or code emitter yet.

`holyc_lib` exposes the supported high-level entry points. `preprocess_detailed` returns tokens, every diagnostic, and per-stream help metadata so callers can keep nonfatal warnings and compiler annotations together. The older `preprocess` convenience result remains available when only tokens and fatal errors matter. The command-line program calls the same detailed path exercised by the CLI golden tests.

Later frontend work will route nonconstant preprocessor expressions through semantic analysis, verified IR, and the compile-time VM. It will also add predefined values, `#exe`, and general generated source to the existing frame stream. Parsing, semantic analysis, IR instructions and verification, interpretation, optimization, assembly, hosted code generation, and TempleOS module emission follow after that. A stage enters the supported list only after its verifier boundary and focused tests exist.

## Source authority

TempleOS source at `c26482bb6ad3f80106d28504ec5db3c6a360732c` is authoritative when prose and implementation disagree. `reference/traceability.toml` links each claimed behavior to the pinned function or table, implementation files, and tests.

## Determinism

Byte offsets and source order are preserved. Reports sort data only when source order has no meaning. Version output always names the pinned reference commit. Tests override data that could depend on clocks, locale, process IDs, or temporary directories.
