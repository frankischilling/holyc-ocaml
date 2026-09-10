# Retained implicit output

Continue the full compiler objective by removing the retained implicit-output
boundary in incremental task inputs. Pinned PrsExp.HC:396-405 selects Print or
PutChars with a function-only hash lookup before argument parsing; PrsStmt.HC:1201
dispatches literal statements through that call path.

Preserve an original parser selection distinct from ordinary identifier reads.
The receipt captures the exact command, environment, function entry and marker
before lookahead, and retains the completed implicit statement when available.
The task journal carries that selection through source activation. Semantic
target resolution consumes the original statement's selection, including
explicit absence and unfinished headers. Runtime lowering binds the exact
retained metadata and classified record through the existing task function link.

- [x] Add a failing incremental Print/PutChars regression covering separate
      inputs and function bodies.
- [x] Capture and validate original implicit target selections in the parser.
- [x] Preserve selections through task source observation, activation and seals.
- [x] Bind semantic implicit targets to selected source or retained headers.
- [x] Lower retained output through exact task function and record ownership.
- [x] Test shadowing during lookahead, absent/partial targets, function-kind
      filtering, generated source, frozen function bodies and resource limits.
- [ ] Review and run complete checks, verify consumers at the committed revision,
      then publish without closing unfinished compiler/native requirements.

Full suite: 2,505 compiler tests pass in 46.243 seconds, with the separate
authority and CLI suites. Review led to exact AST and call-batch checks,
complete argument-group validation and rejection of duplicate source
statements. The duplicate regressions failed before the fix and pass after it.
The built and installed implicit-output example emits AB and returns 42 in
both modes, including exact and one-below instruction limits. Publication and
committed-revision consumer verification remain part of the final checkpoint.
