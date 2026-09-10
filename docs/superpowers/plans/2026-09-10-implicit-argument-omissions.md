# Implicit argument omissions

Continue #635 using TempleOS c26482bb6ad3f80106d28504ec5db3c6a360732c,
Compiler/PrsExp.HC:438-503. Each fixed parameter processes its own separator;
parenthesis-free defaults leave the current token unconsumed. A later required
parameter must then process its own separator before parsing its value.

Preserve the existing selected header and source statement identities. Extend
the AST with ordered omission evidence containing the formal position, optional
consumed comma and unconsumed lookahead location. Keep supplied expressions in
their original order. Both body and top-level binding use this source evidence
to select saved defaults without reevaluating their declaration expressions.

- [x] Reproduce separator-only defaults and defaults before required arguments
      through the production JIT/AOT pipeline, including function bodies.
- [x] Retain original omission evidence in the parser and AST dumps, and bind
      provided values around omitted fixed slots in both semantic paths.
- [x] Test saved side effects, separator locations, malformed inputs, parser
      effect timing and exact source ownership.
- [ ] Run focused and full tests, format/build/install, reference/provenance
      and consumer checks; review, commit and push the checked checkpoint.

This step handles fixed slots after the initial supplied output argument.
Parenthesized multi-argument forms, omitted initial arguments, general values
and conversions remain required. It does not complete #635 or the full native,
BIN/loader and bootstrap compiler mission.

Local validation: all 2,517 compiler tests pass in 52.138 seconds, plus the
fourteen standalone authority tests and both CLI suites. Formatting,
generated sources, build/install, 82 pinned checksums and all 104 provenance
scenarios pass. Full corpus reports match the pinned baselines. Review found
no remaining blocker after strengthening the top-level result assertions.
The new example returns 42 in JIT/AOT with empty capture; maintained CLI tests
verify exact and one-below runtime and preparation limits. Committed-revision
consumer verification and publication follow this source checkpoint.
