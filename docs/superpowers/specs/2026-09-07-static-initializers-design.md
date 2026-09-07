# Phase-aware scalar static initializers

Issue #609 advances the full compiler goal after #607/#608. The executable gates are definition-time effects in unused functions (7), early versus late snapshots of a global (1 versus 7), and an earlier callee supplying a persistent counter (42 after two calls). All four currently fail with HCRUN0006 in JIT and AOT at `81723f2461bf4fd7b213ed94bcd62535d95ac918`.

## Source contract

At pinned TempleOS `c26482bb6ad3f80106d28504ec5db3c6a360732c`, `Kernel/KTask.HC:338-351` compiles and executes each ordinary JIT statement before the next. Static initialization occurs while parsing the containing body (`Compiler/PrsVar.HC:53-112,215-244`). For normal AOT loading, its IET_MAIN enters the queue before the enclosing ordinary statement main (`Compiler/CMain.HC:82-90`); serialization and loading retain that order (`CMain.HC:513-531`, `Kernel/KLoad.HC:158-165`). Unused functions, false branches and declarations after return still initialize.

The containing function is published only after body parsing (`PrsStmt.HC:160-184`). JIT static initializer calls may reach earlier completed definitions, including recursive earlier callees, but not the containing or later functions. Check this transitively before effects. AOT load-time can use completed bodies, including the containing function. The globals-on-data-heap option suppresses normal AOT deferral despite static allocation staying on the code heap; retain HCRUN0006 for this nonconstant AOT combination until its compile-time path is connected. Actual reads of containing parameters or automatic locals remain unsupported; metadata such as sizeof(parameter) is distinct.

## Shared region architecture

Extend the existing initialization checker with distinct static descriptions/regions and a common read-only region view. Existing global `region_description`, `regions`, `find`, `root`, `symbol` and `phase` remain global-only. Add optional static descriptions to `create` and common lookup/owner/phase/bounds accessors for the VM. The checker merges pending global/static owners by semantic item/declarator order and verifies exact roots, slots, nonoverlapping instruction bounds, canonical address/store/end structure, closed operands and balanced calls in the exact entry graph. No fabricated global declaration or raw image setter is added.

Static region authority permits addresses and load/store/update consumers belonging to its declaring frame. Ordinary entry instructions retain no static authority. Check both address production and storage consumption so reusing a region's pointer value after its end cannot bypass ownership. Callee bodies continue using their own exact frames; initializer authority does not grant an invocation frame or leak into called bodies.

The VM uses common region lookup for fault identity and final-value suppression. It checks JIT static initializer callees transitively against exact checked function-frame source indices, using the actual supplied definitions before execution. Validation is not cached against different function bodies. Static pending roots are discharged by the matching regions; a nonconstant root no longer needs constant-image bits, but missing or foreign initialization evidence still fails before effects.

## Source lowering and preparation

Add a static initializer statement to the existing program lowerer. Its destination is a checked persistent static address; its RHS uses the declaring frame solely for semantic lowering. Reuse the existing store-initializer expression planner. Refactor the direct-call helper to take its exact frame context, choose function versus top-level targets accordingly, and recurse in that same context. Table-local expression IDs must not select an unrelated top-level call.

One shared program-lowering core returns the entry plus global/static descriptions. The current global-only entrypoint remains a compatibility projection that rejects static regions rather than dropping them. The source driver uses the complete result. This is an end-to-end extension of existing lowering, not a detached initializer executor.

The preparation driver receives the checked function call targets already built by the source driver. It keeps constant image evaluation and the existing aggregate budget unchanged (literal static preparation remains four instructions). Nonconstant static values become scheduled, after the existing arithmetic guard and explicit frame-read/option checks. The source driver consumes every static AST initializer without a per-invocation store and inserts pending static regions at the function-definition item, in lexical declaration order alongside global initializers and ordinary statements.

## Verification and remaining work

Begin with the four public source gates failing on current main. Then cover global/static declaration ordering, same-name static owners, eager unused/unreachable effects and faults, multiple static declarations, same-frame reads/updates, earlier recursive callees, JIT self/transitive publication rejection versus AOT availability, actual frame-read rejection, data-heap option boundaries, canonical regions, missing/foreign evidence, pointer-value escape after a region, static-only replay and exact preparation/runtime/storage/frame/depth limits. Assert source/operator/function/initializer phase provenance, final-value preservation, deterministic reports and the checked-in CLI fixture. Retain the constant counter 42/21+4 and all earlier fixture proofs.

Use the configured human Git identity and issue/branch/commit/push/draft-PR/review/CI/protected-merge workflow. Run focused/full tests, format/generated/build/install checks, 82 reference checksums and exact corpus comparisons. Full native JIT parsing/effect/fault order remains stateful-compilation work; general memory/runtime, complete optimizer parity, native/BIN/loader execution and bootstrap remain required. No native execution capture is claimed.
