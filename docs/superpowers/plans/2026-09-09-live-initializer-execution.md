# Live initializer execution

Continue the approved #635 design in
`docs/superpowers/specs/2026-09-08-stateful-exe-design.md` from `0964626`.
Use the executing-plans workflow for this continuation.

The original scalar/copy leaf must complete before the parser reads the next
initializer leaf. Its destination is the declaration's existing partial object.
Completing the declaration must reuse both storage and reached initializer work.
The reference remains c26482bb6ad3f80106d28504ec5db3c6a360732c; preserve package
names, source selection timing, native numeric classes and the public report
contracts. The full compiler/native/BIN/loader/bootstrap goal remains active.

- [x] Reproduce scalar-before-directive, earlier-array-leaf, copied-row and
  effectful initializer timing using the real parser/task/StreamPrint adapter.
  Keep the failures visible until their production integration works.
- [x] Retain explicit checked initializer-fragment source evidence in the
  source ledger; this grants typing, not execution authority. The fragment
  owns the original leaf, declaration, namespace, references and
  queries. Never construct a completed AST statement or global around it.
- [x] Extend top-level binding/tree construction with a fragment group and root
  that reuse the existing expression traversal and type analyzer. Construct a
  legitimate empty semantic task context for declarations not present in the
  fragment; retained symbols come from the exact task snapshot.
- [x] Lower the typed fragment through the existing integer expression/call IR.
  Bind its destination to the declared object's checked scalar or array position.
  Execute through retained task cells with cumulative budgets and original leaf
  order. Copied strings use the checked owned-prefix and terminator policy.
- [x] Share complete and incremental fixed-array layout. Observe original
  initializer delimiters synchronously before requesting the following token;
  reject invalid layout before later directives can run. Completion retains
  exact leaf destinations and every original trailing delimiter. Layout is
  pure source metadata and grants no runtime admission authority.
- [x] Make completed-command compilation and admission reuse fragment execution
  and preparation receipts without repeating effects or charges. Test mixed
  prepared/scheduled leaves, failure, limits, foreign owners and replay.
- [ ] Obtain independent review, run focused/full/CLI and build/reference/
  provenance/corpus checks, push a reviewed checkpoint and update the prompt.

The runtime now consumes source-owned leaf attempts. Fragment storage is marked
and rejected by ordinary command admission. A started live initializer cannot
join its completed declaration until every leaf and trailing delimiter passes
the runtime layout cursor. Review found both admission gaps; failing regressions
confirmed them before these checks were added.

The original scalar, array, retained-call and copied-row timing gates now pass.
The copied-row positive generates two characters plus its terminator for a
three-byte row; the original shorter source remains a negative owned-bound test.
Nested calls with explicit arguments execute. The new omitted-default acceptance
case remains failing at shared call lowering (`HCRUN0003`), which still rejects
the existing declared-default result type. Complete declaration-time default
preparation and its later call materialization; do not replace defaults with
synthetic expression results or literal-only substitutions.

The public outer JIT facade still needs separate invocation projection and
provider installation. Partial function timing, effectful dimensions, implicit
providers/defaults/later reads and extern joins remain required. This plan does
not replace those requirements with a constant-only initializer path.
