# Scalar global initializer implementation plan

> For agentic workers: use superpowers:executing-plans and independent review to implement these connected tasks in order.

**Goal:** Execute #603's initialized accumulator and observable scalar initializer expressions through the public compiler and bounded VM in JIT and AOT modes.

**Architecture:** Extend the existing module expression groups with an opt-in, declaration-owned initializer kind. The existing expression tree, identifier, typing, call classification and lowering paths then handle initializer values without synthetic statements or a new semantic session. Classify the value fragment before adding its destination store. Materialize supported constant initial images; retain nonconstant initializer regions with their owner and JIT compilation or AOT load phase while composing them in source order.

**Tech stack:** OCaml 5.1+, Dune, Alcotest, existing generated IC metadata and verified integer VM.

**Spec:** [#603](https://github.com/frankischilling/holyc-ocaml/issues/603), desktop specification sections 0 and 41-48, pinned TempleOS `c26482bb6ad3f80106d28504ec5db3c6a360732c`.

**Constraints:** Keep the full compiler mission open. Preserve exact symbols, source origins, publication/declarator order, mode/options, failure atomicity, graph/x87 checks, bounded eval and existing no-initializer program results. Continue on the focused feature branch in this checkout. Keep unsupported storage/type forms and unresolved optimizer-sensitive arithmetic explicit. Do not add arbitrary host pointers or execute native code.

## 1. Observe the source regression

Files: `examples/integer-global-initializers.hc`, `test/test_integer_global_initializers.ml`, `test/dune`, `test/test_main.ml`.

- [x] Add the initialized accumulator, with `I64 Total=0;` and no separate assignment.
- [x] Add a real source/library test that expects 42 in both modes. Add source-order tests `I64 G=1;G=7;I64 H=G;H;` -> 7 and `I64 G=1;I64 Next(){G=G+1;return G;}I64 H=Next();(H*10+G);` -> 22.
- [x] Run `opam exec -- dune exec test/test_main.exe -- test 'source global initializers'`; verify failure is the existing HCRUN0001 initializer boundary.

## 2. Retain declaration-owned roots through the existing module engine

Files: `src/sema/top_level_expression_binding.ml/.mli`, `src/sema/top_level_expression_tree.ml/.mli`, `src/sema/function_call_expression_result.ml/.mli`; `src/driver/top_level_expression_binding.ml/.mli`, `src/driver/top_level_expression_tree.ml`, `src/driver/integer_source.ml/.mli`.

- [x] Add an explicit initializer owner to opaque module expression inputs/groups. Its constructor receives the exact `Global_initializer_binding.t` and `resolved_global`; reject foreign owners or mismatched records, roots and publication positions. Ordinary statement constructors keep their previous contract.
- [x] Add opt-in `?initializers` to the driver binding traversal. Read the actual global declarator's initializer expression, preserving commas, source locations and initializer shape. Reuse existing identifier/query traversal. Admit the owning publication and earlier declarators at that source point; compare every ordinary occurrence with the checked initializer binding.
- [x] Add `Global_initializer` as a root role retaining its owner. Traverse its actual AST scalar expression with the shared expression/call builder. Keep exact root/source identity and target type; an initializer group must contain precisely its owned root. The existing typed engine derives values and call results, with no unused-expression annotation on initializer roots.
- [x] Have `Integer_source.prepare_unit` retain the initializer batch and opt into those groups for source programs. Keep the ordinary expression-only API's contract unchanged.
- [x] Test self-publication, earlier/later declarations within a comma group, root ownership, same-spelling foreign sessions, typed I64/U64 values and initializer calls. Existing top-level and function semantic tests must retain their default behavior.

## 3. Lower and classify initializer values and initial images

Files: `src/ir/integer_globals.ml/.mli`, `src/ir/global_address_lowering.ml/.mli`, `src/ir/expression_lowering.ml/.mli`, new `src/ir/global_initialization.ml/.mli`, `src/driver/integer_program.ml/.mli`.

- [x] Join each checked initializer root to the exact scalar global slot and declaration. Extend checked storage creation to accept these roots; reject missing, duplicate, foreign or unsupported initializers, including unused declarations.
- [x] Lower the value through the existing expression and direct-call engine. Classify its original value fragment using `(Opcode.info opcode).prevents_constant_folding` before adding any destination address or `IC_ASSIGN`.
- [x] Check the accepted optimizer domain independently of constantness, including transitive initializer callees. Calls must not bypass constant-divisor or shift boundaries in a callee body. Preserve #574/#585 boundaries for unresolved constant shifts, division transformations and faults; never choose raw runtime arithmetic solely because the fragment is constant.
- [x] Evaluate accepted constant fragments through verified IR and the bounded integer VM. Copy the resulting bits into the declared I64/U64 initial image. Retain the root, type, conversion, classification and evaluation-step evidence. A positive initializer-step limit bounds total constant preparation across declarations.
- [x] Prepare nonconstant initializer stores with the checked destination address and shared expression emitter. Preserve the explicit JIT compile-initializer or AOT load-initializer phase, owner and source position. The AOT image remains zero for those objects; uninitialized JIT objects remain unknown.
- [x] Test deterministic initial images, signedness crossings, unary/grouped/arithmetic values, constantness before destination insertion, positive/exact preparation budgets and explicit optimizer boundaries.

## 4. Compose and execute source-ordered initializer regions

Files: `src/ir/integer_program_lowering.ml/.mli`, `src/ir/global_initialization.ml/.mli`, `src/ir/integer_interpreter.ml/.mli`, `src/driver/integer_program.ml`, `src/holyc_lib.ml/.mli`.

- [x] Compose distinct global-initializer operations among the original top-level items, preserving declarator order. Record the emitted instruction region, owner and phase in an opaque context bound to the exact graph and global storage. Ordinary expression results continue to determine the reported final value; declaration initialization alone supplies none.
- [x] Require the matching initialization context whenever the storage/graph needs it. Preflight all regions, canonical stores, entry and called definitions before mutable effects. Reject a graph/context mismatch and graph-only APIs that would discard required initialization metadata.
- [x] Keep shared global words across initializer calls and later source statements. Propagate initializer owner/phase through nested calls and reached faults while preserving the active function's own identity. Existing runtime step/frame/depth/global bounds also cover scheduled initializer execution.
- [x] Test JIT/AOT source-order interactions, initializer calls and recursion, caller temporaries, comma groups, early returns, self-reads, unknown JIT reads, AOT zero reads, unused bad bodies and repeated executions from an unchanged initial image.

## 5. Public reports, review and integration

Files: `bin/holyc.ml`, public interfaces, README, CHANGELOG, compatibility/IR/program/global/testing/source-map guides, `reference/traceability.toml`.

- [x] Expose the checked initialization context and bounded constant-preparation statistics through the compiled-program API. Add a positive initializer-step option to compilation/run and CLI reporting. Keep compile-time preparation counts distinct from runtime instruction counts and label initializer diagnostics with their phase and owner.
- [x] Extend CLI tests for the fixture, limits, deterministic dumps and JSON/human reporting. Update existing unsupported-initializer regressions to test the remaining boundary instead of rejecting newly supported forms.
- [x] Run focused regressions, then sequential quoted formatting/generated/build/install checks and the full suite. Verify the 82 pinned reference files and exact lexer/parser baselines.
- [ ] Obtain independent code review and resolve findings. Commit with the configured identity, incrementally rebuild, verify exact source provenance and all fixtures, push a draft PR linked to #603, wait for all five CI checks and merge normally against the tested head.
- [ ] Verify the merged tree and binary, synchronize main, and update issue/project/epic/prompt evidence without declaring the full compiler complete.

## Verification before integration

The original source gate failed at HCRUN0001 before the connection. Constant
images and both external-call-scope regressions were also observed failing
before their implementations. Final local verification passed all 1,720 tests
(WXR82NSH), the 12 focused initializer groups, CLI checks, quoted formatting,
generated/build/install checks, 82 reference checksums and exact corpus baselines.
Independent read-only review identified region ordering, operand/call closure,
public image mutation and documentation issues; all were addressed and the final
review reported no remaining findings. GitHub integration evidence is tracked
in issue #603; the full compiler mission remains open.
