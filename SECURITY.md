# Security policy

Report suspected vulnerabilities through GitHub private vulnerability reporting when it is available. Do not include exploit details in a public issue.

## Untrusted source files

The compiler treats input as untrusted. Relevant risks include include path traversal, parser exhaustion, malformed binary lengths, relocation overflow, output path writes, and executable-memory permissions. Host metadata uses checked sizes and conversions. HolyC arithmetic wraps only where target semantics require it.

## Compile-time execution

HolyC `#exe` is arbitrary compile-time code. The current build never executes it. Planned hosted support will deny network access, process creation, unrestricted file access, and unbounded execution by default. A deterministic instruction and memory budget will apply even when source generation is nested.

Native compile-time execution will not be the default. Any future unsafe mode must be named clearly in command output and compatibility reports.

## Supported versions

The project is pre-release. Security fixes apply to the current default branch until the first versioned support policy is published.
