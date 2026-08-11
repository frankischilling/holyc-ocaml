# holyc-ocaml security policy

Report suspected vulnerabilities through GitHub private vulnerability reporting when it is available. Do not include exploit details in a public issue.

## Untrusted source files

The compiler treats input as untrusted. Relevant risks include include path traversal, recursive definition expansion, generated-text exhaustion, expression exhaustion, parser exhaustion, malformed binary lengths, relocation overflow, output path writes, and executable-memory permissions. Hosted includes are confined to canonical configured roots. `#help_file` resolves a lexical metadata path inside the same configured roots but never opens, creates, or reads the named help file. Definition expansion rejects active cycles and applies separate nesting and generated-byte budgets. Conditional nesting has its own limit; after it is exhausted, the rest of the stream remains inactive so later directives cannot mutate state during error recovery. Constant `#if` and `#assert` evaluation share a separate node budget and do not read the host environment, filesystem, process state, or clock. Inactive branches are scanned without loading includes, expanding ordinary identifiers, evaluating symbol predicates or assertions, or changing help metadata. Ordinary hosted lexing still rejects an embedded NUL so hidden suffix bytes cannot change later tooling behavior. The corpus command permits NUL termination only after it verifies a clean checkout at the exact reference commit. Its report records the terminator and all trailing bytes; it never executes the payload. The default symbol-visibility environment contains only entries derived from pinned compiler tables. It does not expose host process symbols or environment variables. Host metadata uses checked sizes and conversions. HolyC arithmetic wraps only where target semantics require it.

## Compile-time execution

HolyC `#exe` is arbitrary compile-time code. The current build never executes user-supplied `#exe` blocks. The six standard predefined values use a dedicated expander for their audited pinned definitions; this path cannot access the network, launch a process, or read another file. Their generated token text shares the configured nesting and byte limits with ordinary definition expansion.

Planned general hosted support will deny network access, process creation, unrestricted file access, and unbounded execution by default. A deterministic instruction and memory budget will apply even when source generation is nested.

Native compile-time execution will not be the default. Any future unsafe mode must be named clearly in command output and compatibility reports.

## Supported versions

The project is pre-release. Security fixes apply to the current default branch until the first versioned support policy is published.
