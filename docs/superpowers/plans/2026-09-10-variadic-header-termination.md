# Variadic header termination implementation plan

> Use superpowers:subagent-driven-development for the independent frontend and
> semantic adaptations, then independent review. Root owns every Dune run,
> runtime integration, publication and final verification.

**Goal:** Preserve native variadic termination when ellipsis is not followed by
a closing parenthesis, through parsing, source-backed header typing and execution.

**Architecture:** Represent a function signature's closing parenthesis as an
optional original source location. The ellipsis parser consumes an actual `)`
when present and otherwise returns without consuming the following token. Its
caller owns the next token. Semantic signature origins preserve the same absence;
nonvariadic signatures still require a closing origin. Never synthesize punctuation
or borrow the body token's location as closing-parenthesis evidence.

**Tech stack:** Existing OCaml frontend, semantic adapters, retained task driver
and bounded integer execution, with unchanged package and CLI identities.

**Spec:** Latest verified checkpoint in `C:/Users/imike/Desktop/holyc-ocaml.txt`,
issue #635, TempleOS `c26482bb6ad3f80106d28504ec5db3c6a360732c`:
PrsVar.HC:373-405 conditionally consumes `)`; PrsVarLst exits immediately;
PrsStmt.HC:114,151-159 passes the remaining token to the body statement parser.
PrsVar.HC:350-356 applies the same signature parser to function pointers.

## Constraints

- Preserve original source tokens, parameter children, default receipts and
  completed-header callback/replay authority. Absence cannot impersonate presence.
- Keep ordinary present-parenthesis AST dumps and CLI report contracts stable.
- Preserve original calling signatures, argument counts, defaults, variadic
  tails, frame/depth limits and nested header/executable version selection.
- Do not invent a delimiter whitelist after ellipsis; subsequent syntax belongs
  to the existing prototype, callback, definition or statement caller.
- Keep full compiler/native/BIN/loader/bootstrap scope and #635 criteria open.

## Frontend

- [x] Add failing definition, prototype, recursive callback and directive-boundary
  tests before implementing termination changes.
- [x] Change only function-signature closing fields to `location option` in
  Ast/Parser records and constructors; keep other parenthesis roles unchanged.
- [x] Preserve exact present token locations, represent missing closes explicitly
  in dumps and keep the token after ellipsis available to its caller.
- [x] Adapt existing source fixtures and malformed cases based on the actual
  caller grammar; retain recovery/visibility assertions.

## Semantic and retained adapters

- [x] Preserve optional closing origin in the semantic signature. Keep supplied
  `~closing_origin` calls source compatible through an optional maker argument,
  and reject absent origin on nonvariadic signatures.
- [x] Adapt Driver function typing, collection, task declaration replay and
  original-child comparisons to optional physical identity.
- [x] Cover nested callback parity and rejection of copied, omitted or supplied
  closing-token substitutions without allocating locals or consuming evidence.

## Runtime and publication

- [x] Run the source-derived 42 gate in both outer modes; cover prototypes,
  explicit closes, defaulted fixed arguments, pending/direct saved callers,
  nested directives and exact/one-below resource budgets.
- [x] Run full tests, quoted format/generated/all/install aliases, reference
  checksums, provenance and full corpus comparisons. Review any corpus change
  against source evidence before changing an expectation.
- [ ] Obtain independent review, update documentation and desktop requirements,
  commit/push to draft #636, rebuild normally and verify exact committed built
  and installed consumers. Record hosted checks and clean synchronized state.

## Local verification

All 2,643 compiler tests pass in 54.191 seconds, plus fourteen authority tests
and both CLI suites. All 104 provenance scenarios and 82 reference checksums
pass. Complete lexer/parser JSON and normalized parser JSON text match the
existing baselines. Independent review found no remaining implementation blocker.

The new visibility regression initially assumed locals ended before post-body
lookahead. Pinned source review established that PrsStmt reads past `}` before
PrsFun clears locals. The corrected test covers immediate local visibility and
later global restoration. Memory exhaustion interrupted an earlier build and
three BIN source tests; single-job build and full-suite retries passed.

Publication evidence is recorded in PR #636 and the desktop prompt after the
committed build and hosted checks. Full #635 acceptance remains open.
