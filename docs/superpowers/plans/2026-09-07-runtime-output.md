# Checked captured runtime output (#621)

Connect the existing semantic output passes to the integer execution path. The
four baseline tests reproduce HCRUN0001 for Print/PutChars declarations in both
modes. The maintained fixture must capture `34320a` and return I64 42.

## Design

`Ir.Runtime_call_context` seals selected semantic call records against the final
entry and exact function bodies. Its opaque calls retain declaration/header and
symbol identity, physical argument order and types, count producers, call and
cleanup opcodes, result identities, and implicit statement discard origins.
Provider approval is per selected declaration snapshot. Source definitions keep
their own bodies. Existing graph-only lowering cannot silently lose output
authority. Checked output adapters reuse canonical direct-call lowering.

`Ir.Integer_output` privately implements ordinary bytes, `%%`, `%d`, `%s`, and
`%c`, using a VM byte-reader callback so runtime references stay private. Print
formats a bounded draft and publishes only when that call succeeds. PutChars
streams nonzero packed bytes in native order and retains its emitted prefix if a
later resource check fails. Unused variadic arguments are permitted and evaluated.

Every format or string byte fetch (including terminators and failed reads),
packed byte inspection, and candidate output byte costs one work unit. An
exhausted charge leaves the count at the limit. Append work precedes capacity
checks. Defaults are 1,048,576 output bytes and work units, independently bounded.
Print("42\\n") costs seven work units; PutChars('42\\n') costs six. Runtime
providers consume one active call depth and eight bytes per ABI argument slot,
including Print's hidden count. No source body or local frame is fabricated.

VM reports always retain immutable output bytes and charged work alongside the
outcome. Existing execution convenience APIs project that outcome. Configuration
and preflight failures have empty capture. Driver reports preserve compiler
diagnostics and the existing runtime diagnostic identities and phase evidence.
Implicit output preserves the last ordinary expression; explicit U0 calls clear
it according to the established execution contract.

CLI run defaults to a versioned v2 report on both execution success and failure,
with lossless output_hex and output_byte_length separate from the final word.
Human output renders captured bytes explicitly as hex. `--report-version=1`
preserves the previous result/diagnostic contract. Evaluation and IR dumps retain
their established interfaces. Positive output/work limits are configurable.

## Implementation and verification

1. Frontend owner adds the context, checked call adapters, complete lowering
   metadata, four semantic output passes, and driver compilation/context getter.
2. VM owner adds private formatting, context validation and hosted invocation,
   resource charging, report execution, and legacy outcome projection.
3. Test owner extends the four RED groups with capture, grammar, identity,
   declaration snapshots, ordinary-expression reporting, faults and exact limits.
4. Root adds driver reports and shared diagnostic conversion, public aliases,
   CLI v2/legacy path and CLI fixtures; documents the supported hosted boundary.
5. Freeze production/test edits before serialized builds. Run focused tests,
   full suite and CLI, formatting/generated/build/install gates, pinned checksums,
   provenance and exact lexer/parser corpus comparisons. Obtain independent
   production review and resolve findings before commit.
6. Update traceability and the external requirements checkpoint. Push the final
   source, open/update draft PR, wait for all five required source checks, then
   use normal protected merge. Verify merged identity and the public fixture.

Output capacity, work, format and argument errors use HCIRVM0022 through 0025.
Existing configuration, call-preflight and owned-reference codes remain intact.
Full formatting, arbitrary runtime services, stateful compilation, native
backends, BIN/loader acceptance and bootstrap remain outside this increment and
remain required by the full compiler goal.
