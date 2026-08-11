# holyc-ocaml architecture

The compiler is split into explicit stages. Each stage consumes immutable inputs and returns either a value or structured diagnostics. Sessions own source IDs and configuration; compiler modules do not rely on hidden global state.

The current slice has four layers:

- `Common` owns source files, byte positions, spans, diagnostics, deterministic rendering, and target-width integer helpers.
- `Frontend` owns token definitions, the audited keyword table, streaming lexical state, and stable token dumps.
- `Generated` contains deterministic source facts produced only after pinned checksum and table-shape validation.
- `Sema` currently exposes primitive type identity and representation metadata. Declaration resolution, conversions, and aggregate layout have not entered this layer yet.

`holyc_lib` exposes the supported high-level entry points. The command-line program calls the same library functions used by tests.

Later stages will add preprocessing, parsing, semantic analysis, canonical IR, interpretation, optimization, assembly, hosted code generation, and TempleOS module emission. A stage enters the supported list only after its verifier boundary and focused tests exist.

## Source authority

TempleOS source at `c26482bb6ad3f80106d28504ec5db3c6a360732c` is authoritative when prose and implementation disagree. `reference/traceability.toml` links each claimed behavior to the pinned function or table, implementation files, and tests.

## Determinism

Byte offsets and source order are preserved. Reports sort data only when source order has no meaning. Version output always names the pinned reference commit. Tests override data that could depend on clocks, locale, process IDs, or temporary directories.
