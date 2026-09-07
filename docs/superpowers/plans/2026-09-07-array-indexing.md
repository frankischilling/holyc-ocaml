# Automatic Array Indexing Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox syntax for tracking.

**Goal:** Execute automatic scalar arrays and indexed aliases through public run and the CLI.

**Architecture:** Retain checked semantic index children, derive exact layout strides in the shared planner, and extend the existing VM with element cells and object-relative bounded references. Preserve public integer results and canonical scalar execution.

**Tech Stack:** OCaml 5.1/5.3, Dune, Alcotest, existing dependencies, PowerShell.

**Spec:** `docs/superpowers/specs/2026-09-07-array-indexing-design.md`.

## Global constraints

- Pinned source c26482bb6ad3f80106d28504ec5db3c6a360732c; exact checked identity and provenance at every join.
- Full goal remains active. Original scalar values/counts, initializer phases/guards and public result signatures are preserved.
- Only one Dune process; no built executable while Dune may relink it. No source/tests edits during a build. Git/GitHub through the configured human workflow; protected merge after all five checks.
- Read progress in this plan and actual branch before resuming. Work is isolated on feat/613-array-indexing from ffe1931.

## Task 1: Semantic evidence and source red gates

Files: `src/sema/function_call_expression_result.ml/.mli`, `src/ir/integer_program_lowering.ml`, recursive initializer/call consumers found by `rg result_binary_operands src`, `test/test_function_call_expression_result.ml`, new `test/test_integer_arrays.ml`, `test/test_main.ml`, `test/dune`.

Interface:
```ocaml
val result_index_operands : expression_result ->
  (expression_result * expression_result) option
(* type_index retains Some (base, index_value); other constructors None. *)
```

- [x] Add eight source cases from #613 to `source integer arrays`; each mode runs `G.run ~mode text |> F.expect 42L`. Run that group and observe HCRUN0003 before implementation.
- [x] Add semantic retention tests for nested arrays, pointer indexing, source identity/origin, requested integer conversion and unchanged rank/type/category. Preserve negative index-base and callback/member tests.
- [x] Retain `index_operands` in make_result/type_index, expose the accessor, and extend recursive consumers that currently visit operand/binary children. Do not re-resolve sources.
- [x] Run semantic focus and inspect retained exact children before source lowering work.

## Task 2: Shared indexed-address lowering and execution

Files: `src/ir/expression_lowering.ml/.mli`, `src/ir/integer_interpreter.ml/.mli`, relevant source-driver declaration joins only if actual evidence requires it.

Internal interfaces:
```ocaml
(* Indexed-address planning retains the complete typed base and ordered indices. *)
type index_step = { index_value : Semantic_result.expression_result;
                    index_stride : int64; index_span : Common.Span.t }
(* Runtime address coordinates are independent of the current callee frame. *)
(* pointer_base/count select one declared object; pointer_offset is byte-relative. *)
```

- [x] Add checked array-root and full index-chain preparation using exact frame location/dimensions. Reject wrong children, rank, type, index conversion or shape with existing metadata diagnostics. Pointer indices use exact one-level I64/U64 values and stride8.
- [x] Add indexed address nodes/tasks shared by load, assignment, compound/prefix/postfix update and address-of. Emit source-order stride constant/index/IC_MUL/IC_ADD operations. Preserve destination-before-RHS and delayed word reads.
- [x] Add any-rank and partial-array address materialization, source-correct grouping/dereference and explicit address-of typing for local pointer initializers/assignments and fixed arguments without changing semantic array type/rank. `Set(a)` and `Set(&a[0])` must address the same object.
- [x] Flatten automatic element cells in frame_context, retain root object base/count/strides and exact offset lookup, and preserve parameter positions and allocation limits.
- [x] Add narrow preflight classifications for scale offsets and indexed addresses. Check each ADD's expected stride/rank against its root/intermediate/pointer base. Prohibit offset metadata from arithmetic, stores, pushes and public returns.
- [x] Extend runtime addresses with base/count/int64 byte offset. Checked multiply/add may produce intermediate out-of-object offsets; final materialization permits one-past and memory access requires aligned in-object offsets. Keep object/lifetime/pointee identity through copies and calls. Reject overflow and neighboring-object access before host indexing.
- [x] Run eight source cases green in both modes; inspect IR and previous #611 counts.

## Task 3: Ownership, integration and protected delivery

Files: source/VM tests above, `examples/integer-arrays.hc`, `test/test_integer_program_cli.ml`, related array/pointer/program/function/IR/testing/compatibility/source-map docs, README, CHANGELOG, `reference/traceability.toml`.

- [x] Add effect tests: `i=0;a[i++]=i;return 40+a[0]+i;`, `i=0;a[i++][i++]=40;return a[0][1]+i;`, indexed compound RHS42, recursion, U64, bare array fixed arguments and initialization through element addresses.
- [x] Add cross-row `a[0][3]`, returning intermediate `a[2][-1]`, interior negative pointer indices, one-past address copies and dereference failures, neighboring locals, index/offset overflow, unwritten elements, fresh images and exact limits. Add malformed canonical stride/rank and semantic operand joins.
- [x] Add caller-element CLI fixture, observe exact counts and lock step/frame/depth limits plus deterministic dumps in both modes. Update public documentation and explicit remaining boundaries.
- [x] Resolve independent implementation/final review; run focused/full tests, CLI, fmt/generated/all/install, reference checksums and exact corpora. Preserve all earlier executable proofs.
- [ ] Commit/push, open a linked draft PR, verify five exact-head checks and threads, then merge normally with source-head guard.
- [ ] Verify identical source/merge trees and rebuilt executable proofs, update issue/project/epic/desktop, and leave clean synchronized main with the full goal active.

Ruling: execute the tightly connected planner/VM tasks inline, with independent
design/code review. The existing user authorization covers implementation and
normal protected delivery; no additional permission checkpoint is required.

Progress: eight source gates red MM1BF3FR; exact index-child semantic focus green RG7KK3H0. Grouping/callback red HLME5NHV, green SULTMYUX; initial VM gates green 8ZW5CYPO. Address-of remaining-rank arrays red Y1BU9IM2 and corrected. Current array/pointer/semantic focus: 124 tests, 43BHP37N, 0.457s. Independent implementation review reports no remaining production blocker. CLI caller-element fixture observed 42/51+0 with 24 bytes/depth2; full verification and protected delivery remain.

Final local verification on 2026-09-07: OCaml 5.4.1, Dune 3.24.2; 100 focused tests and all 1,792 registered tests passed, including CLI checks. Formatting, generated source, all/install builds, 82 reference checksums and all 11 incremental provenance scenarios passed. Lexer acceptance remains 528/528; parser acceptance remains 25/528 standalone and 126/528 with the prelude, with the exact report preserved after Windows newline normalization. Independent review approved the final code.

The full run exposed four obsolete array boundary assertions and a grouped outer-array provenance regression. Updated frame/function/update tests preserve positive behavior and exact limits while rejecting remaining unsupported shapes. The top-level grouping test first failed on its missing outer occurrence after checking the new pointer shape; forwarding the same five provenance fields as ordinary grouping fixed it. These follow-ups are in test/test_ir_integer_frames.ml, test/test_integer_functions.ml, test/test_integer_updates.ml and test/test_top_level_expression_result.ml.

Protected delivery and rebuilt merge identity are recorded in issue #613 and its linked pull request after this source snapshot. The full compiler goal remains open under #396.
