# 1.0 release contract

This document defines the compatibility evidence required for the first 1.0
release. It is the maintained human-readable contract for issue
[#683](https://github.com/frankischilling/holyc-ocaml/issues/683). The machine
readable source-set facts belong in `reference/manifest.json`; this document does
not maintain a second input registry.

Closing #683 means the target matrix, source sets, gates, observables and owners
below are defined. It does not satisfy those gates. The complete release remains
tracked by [#682](https://github.com/frankischilling/holyc-ocaml/issues/682), and
publication remains blocked by [#736](https://github.com/frankischilling/holyc-ocaml/issues/736)
until every required row passes at the release candidate revision.

## Status terms

Release evidence uses these terms consistently:

| Status | Meaning |
| --- | --- |
| Verified | The named producer and consumer ran against the stated revision and input set, and the required observable result passed. |
| Implemented, unqualified | Relevant code exists, but the release cell lacks the required platform, artifact, consumer or exact-revision evidence. |
| Pending integration | The implementation exists on the named development revision but has not yet completed the required protected integration and exact-head verification. |
| Progress metric | The measurement is useful for tracking work but is not itself a release pass. |
| Unimplemented, blocking | A required producer, consumer or command does not exist yet. The owning issue remains a release blocker. |
| Unqualified host | The project does not promise this host for 1.0. Adding it requires an owning issue plus CI, ABI, memory-safety and artifact evidence. |

An intentional hosted restriction may describe a supported hosted mode. It
cannot turn a missing required language, target, loader or bootstrap capability
into a 1.0 pass.

## Required target matrix

The 1.0 hosted promise contains two compiler hosts. Frontend processing, checked
IR interpretation, hosted native execution and own-encoder host artifacts are
required on both.

| Compiler host | CPU | Native ABI | Frontend and interpreter | Hosted native execution | Own-encoder artifact | 1.0 classification |
| --- | --- | --- | --- | --- | --- | --- |
| Linux | x86-64 | System V x64 | Required | Required | ELF relocatable plus standalone executable | Required |
| Windows | x86-64 | Windows x64 | Required | Required | COFF relocatable plus PE executable | Required |

The native execution boundary currently admits only 64-bit x86-64 Windows and
Linux. Windows images execute with the Windows x64 status ABI and Linux images
execute with the System V x64 status ABI. The backend may encode the other ABI
for inspection, but a foreign ABI image is not executable evidence for that
host. See [native programs](native-programs.md),
[native expressions](native-expressions.md), and
[the security policy](../SECURITY.md).

macOS x86-64, macOS ARM64, Linux ARM64, Windows ARM64, ARM64EC and other host
combinations are unqualified for 1.0. The OCaml package's portability does not
qualify a compiler host. Any future promise for one of these cells needs its own
issue and maintained CI, ABI, executable-memory, unwind, artifact and installed
consumer evidence.

TempleOS target compatibility is a separate required part of 1.0. The release
must produce and exercise TempleOS x86-64 assembly, BIN and JIT forms in the
controlled oracle environment. Actual loader execution and compiler bootstrap
are required results. They are not optional because hosted Linux and Windows
paths pass.

Each required compiler host must emit its matching host artifacts and the
TempleOS target forms. Host execution consumes the matching native ABI;
TempleOS artifacts execute in the controlled TempleOS runtime. Cross-compiling
ELF from Windows or PE from Linux is not an additional 1.0 promise.

JIT and AOT source modes require independent semantic evidence on both hosts.
Where a target form has a native mode restriction, such as AOT BIN writing, the
gate records that source-backed restriction and verifies rejection of the other
mode. It does not omit a mode merely because its implementation is unfinished.

## Source modes and execution engines

JIT and AOT are HolyC source/preprocessing and linkage modes. They do not name
host artifact formats. In particular, selecting AOT on the current hosted
executor does not produce an ELF, COFF, PE or TempleOS BIN file.

The current public execution entry points are:

| Entry point | Engine | Current role |
| --- | --- | --- |
| `holyc eval FILE` | Checked integer IR interpreter | Bounded expression execution. |
| `holyc run --target=ir FILE` | Checked integer program interpreter | Bounded supported source execution, including supported compile-time task behavior. |
| `holyc eval-native FILE` | In-process x86-64 own-encoder execution | Bounded integer expression native execution on the matching Linux or Windows x86-64 host. |
| `holyc run --target=host-jit FILE` | In-process x86-64 own-encoder execution | Bounded structured native program execution on the matching Linux or Windows x86-64 host. |

At published revision `c359100`, the hosted native program slice includes the
checked `PutChars` capture path described in [native output](native-output.md).
This remains a bounded hosted capability. General host object/executable output,
TempleOS BIN production, actual loader compatibility and bootstrap are still
unimplemented release requirements.

## Canonical source coverage

The whole-tree denominator is the complete set of 528 canonical Git blobs at
TempleOS commit `c26482bb6ad3f80106d28504ec5db3c6a360732c` whose paths end in
`.HC`, `.HH` or `.PRJ`. `reference/manifest.json` records the tree identity,
checksums and reviewed corpus metadata. Release reports must keep this denominator
visible. They may not remove a file because a later phase fails.

Standalone parsing is a diagnostic measurement. HolyC source depends on project
order, prelude state, includes and previously published declarations, so the
release compilation gate uses the actual project context and source order. The
reviewed standalone/project-prelude parser comparison in
`reference/parser-corpus-aot.json` remains a progress baseline until the complete
project-context gate passes.

The compiler and kernel project roots are:

* `Compiler/Compiler.PRJ`, as the canonical compiler project root.
* `Kernel/Kernel.PRJ`, as the canonical kernel project root.

The Adam release set contains every canonical Git blob with a case-sensitive
`Adam/` prefix and a `.HC`, `.HH` or `.PRJ` suffix. The Demo release set uses the
same rule with the case-sensitive `Demo/` prefix. The machine-readable source
inventory counts and digests belong in the `release_contract` section of
`reference/manifest.json`.

The pinned inventories contain 34 Compiler, 77 Kernel, 132 Adam and 203 Demo
source blobs. They are subsets of the whole-tree set, not separate additions to
its 528-file denominator. Compiler and Kernel source closures can cross those
directory prefixes.

The manifest also pins the complete canonical Git-blob dependency input universe,
including non-source assets and compressed inputs. Inventory hashes use ordinal
path order to prove membership; they do not encode semantic include order. Every
Compiler, Kernel, Adam and Demo release run must record its actual source-authored
project/include sequence, transitive inputs consumed, source mode, options and
generated-input provenance. Dynamic `#exe` or generated-source consumption is
recorded from the run rather than guessed statically. A missing or damaged
dependency blocks the run. An input outside the pinned dependency universe is
allowed only when the controlled oracle fixture manifest names it explicitly.

The Adam and Demo scope cannot be narrowed to examples that already pass. Every
execution case must name its entry point, required setup, expected observable
behavior and oracle evidence. A case that cannot execute must record its first
failing phase and blocking issue. Hardware-dependent programs remain in scope;
they require a controlled target setup or an explicit blocking result instead of
a silent skip.

## Current public commands

The current `holyc` CLI exposes `version`, `lex`, `preprocess`, `parse`,
`dump-ast`, `dump-symbols`, `dump-layout`, `eval`, `eval-native`, `run`,
`dump-ir`, and `corpus` with `lex` and `parse` subcommands. The README documents
their current use.

`dump-ir` prints the verified bounded IR that the current source path can lower.
It is not the complete 1.0 `verify-ir` workflow.

The following release workflows do not currently have operational public
commands: full `check`, full `compile`, `verify-ir`, host object/executable
writing, `dump-relocs`, `verify-bin`, integrated assembler/disassembler output,
an independent BIN loader command, an oracle capture runner, and bootstrap
commands. [#726](https://github.com/frankischilling/holyc-ocaml/issues/726)
owns the public check/compile/artifact command surface. Individual producers and
consumers remain owned by the issues named below. Documentation must not present
their planned command names as usable before those consumers exist.

`Backend.Bin_spec` and the generated BIN record tables are a source-audited
format specification. The generator command documented in
[TempleOS BIN format](templeos-bin-format.md) regenerates that specification; it
does not produce or validate a TempleOS module.

The repository also contains reviewed TempleOS oracle captures and manual QEMU
reproduction procedures in [oracle fixtures](oracle-fixtures.md). Those captures
are evidence for their named semantic slices. They are not an automated loader or
bootstrap runner. [#720](https://github.com/frankischilling/holyc-ocaml/issues/720)
owns the reproducible oracle runner.

## Release gate registry

Every required row below must identify the exact producer revision, canonical
input set, runtime, target, consumer and observable result in release evidence.
When a command is absent today, the table says so directly.

Run these commands from the selected repository with its opam environment
activated, or prefix tools with `opam exec --`. In a nested linked worktree,
pass `--root .` to Dune. PowerShell scripts below use `pwsh`, as CI does.

| Gate | Required evidence and pass condition | Current command or current boundary | Owner |
| --- | --- | --- | --- |
| Reference and producer identity | Candidate revision and pinned TempleOS revision are exact; audited generated sources and reference checks match their inputs. | `holyc version`; `pwsh -NoProfile -File tools/verify-reference.ps1`; `pwsh -NoProfile -File tools/test-release-contract.ps1`; `dune build '@generated-check'`. | [#731](https://github.com/frankischilling/holyc-ocaml/issues/731), [#733](https://github.com/frankischilling/holyc-ocaml/issues/733) |
| Hosted build and source tests | Formatting, generated checks, build, install target and ordinary tests pass on each required compiler host. | `dune build '@fmt' '@generated-check' '@all' '@install'`; `dune runtest`. | [#733](https://github.com/frankischilling/holyc-ocaml/issues/733) |
| Raw lexing | All 528 canonical source blobs tokenize with no lexer, read or internal error, retaining source byte and embedded-payload accounting. | `holyc corpus lex --reference-root=third_party/TempleOS`. | [#722](https://github.com/frankischilling/holyc-ocaml/issues/722) |
| Language preprocessing | Canonical project streams preprocess in source order with no unresolved required preprocessor capability; first failures retain file, phase and owner. | Single-source `holyc preprocess` exists. No complete whole-tree preprocess report yet. | [#3](https://github.com/frankischilling/holyc-ocaml/issues/3), [#27](https://github.com/frankischilling/holyc-ocaml/issues/27), [#33](https://github.com/frankischilling/holyc-ocaml/issues/33), [#722](https://github.com/frankischilling/holyc-ocaml/issues/722) |
| Parsing | Every required project-context source reaches a successful parse in canonical order. Standalone results remain reported as diagnostics, not the compilation pass condition. | `holyc corpus parse` reports the reviewed comparison; `--require-all` currently gates only that parser phase. | [#46](https://github.com/frankischilling/holyc-ocaml/issues/46), [#47](https://github.com/frankischilling/holyc-ocaml/issues/47), [#48](https://github.com/frankischilling/holyc-ocaml/issues/48), [#722](https://github.com/frankischilling/holyc-ocaml/issues/722) |
| Type, layout and semantic checking | Complete required source obtains checked types, layouts, calls, storage and conversions with no missing required semantic feature. | `dump-layout` exercises a bounded completed layout path. No full public `check` command exists. | [#263](https://github.com/frankischilling/holyc-ocaml/issues/263), [#684](https://github.com/frankischilling/holyc-ocaml/issues/684) through [#695](https://github.com/frankischilling/holyc-ocaml/issues/695), [#726](https://github.com/frankischilling/holyc-ocaml/issues/726) |
| Canonical IR and verification | Complete checked source lowers to the canonical verified IR, including required control, memory, call, exception and inline-assembly boundaries. | `dump-ir` is a bounded subset. No complete `verify-ir` command exists. | [#396](https://github.com/frankischilling/holyc-ocaml/issues/396), [#726](https://github.com/frankischilling/holyc-ocaml/issues/726) |
| Optimizer semantics | Required early and late passes match source-grounded behavior and independent semantic/oracle comparisons. Verified source rewrites that differ from raw IR retain explicit expected-difference cases; equivalence is required only in its supported domain. | Only implemented verified slices may be claimed. No full optimizer pass gate exists. | [#574](https://github.com/frankischilling/holyc-ocaml/issues/574), [#585](https://github.com/frankischilling/holyc-ocaml/issues/585), [#593](https://github.com/frankischilling/holyc-ocaml/issues/593), [#696](https://github.com/frankischilling/holyc-ocaml/issues/696), [#697](https://github.com/frankischilling/holyc-ocaml/issues/697) |
| Interpreter execution | Required nonprivileged source behavior passes the checked interpreter with exact outputs, faults and resource accounting. Hosted runtime substitutions remain within their documented contracts. | `holyc run --target=ir`. Full language/runtime coverage is incomplete. | [#684](https://github.com/frankischilling/holyc-ocaml/issues/684) through [#697](https://github.com/frankischilling/holyc-ocaml/issues/697) |
| Hosted native execution | The complete required hosted language/runtime domain executes through the own encoder on Linux x86-64/System V and Windows x86-64/Windows x64 with independent expected results, ABI checks and exact resource/fault behavior. Current subset tests cannot satisfy this whole row. | `holyc eval-native`; `holyc run --target=host-jit`; `dune build '@native-tests'`. General native support remains incomplete. | [#699](https://github.com/frankischilling/holyc-ocaml/issues/699) through [#710](https://github.com/frankischilling/holyc-ocaml/issues/710) |
| Native runtime output and providers | Required hosted providers preserve source authority, output and failure semantics across both supported native hosts. | `PutChars` is supported in the bounded native path at `c359100`; broader providers remain incomplete. | [#705](https://github.com/frankischilling/holyc-ocaml/issues/705), [#695](https://github.com/frankischilling/holyc-ocaml/issues/695) |
| Linux host artifact | Candidate compiler emits ELF relocatables and linked standalone executables with correct sections, symbols, relocations, startup/runtime behavior and reproducible provenance; independent tools inspect them and clean off-checkout execution passes. | No operational writer command. | [#707](https://github.com/frankischilling/holyc-ocaml/issues/707), [#708](https://github.com/frankischilling/holyc-ocaml/issues/708) |
| Windows host artifact | Candidate compiler emits COFF relocatables and PE executables with correct sections, symbols, relocations, imports, startup, stack/unwind behavior and reproducible provenance; independent tools inspect them and clean off-checkout execution passes. | No operational writer command. | [#707](https://github.com/frankischilling/holyc-ocaml/issues/707), [#709](https://github.com/frankischilling/holyc-ocaml/issues/709) |
| TempleOS assembler and disassembler | Complete source-required operands, directives, labels, layouts, fixups, listings and machine forms agree with pinned source behavior; inspection output is independently checkable. | Parser/database slices exist. No integrated assembler/disassembler command. | [#711](https://github.com/frankischilling/holyc-ocaml/issues/711) through [#716](https://github.com/frankischilling/holyc-ocaml/issues/716) |
| TempleOS BIN writer | Candidate compiler emits source-required BIN headers, code/data, ordered patch records and maps with checked arithmetic and deterministic output. | BIN specification exists. No writer command. | [#717](https://github.com/frankischilling/holyc-ocaml/issues/717) |
| Independent BIN model | A separately checked reader/model validates required records, relocations, loader passes, unresolved symbols, heap actions, flags and failures without executing untrusted native code. | No `verify-bin` or `dump-relocs` command. | [#718](https://github.com/frankischilling/holyc-ocaml/issues/718) |
| TempleOS JIT and fixups | Emitted JIT/AOT code applies source-grounded fixups, symbol publication, retained definitions and callable-entry behavior in the target contract. | Host JIT does not satisfy this target gate. No TempleOS JIT consumer exists. | [#719](https://github.com/frankischilling/holyc-ocaml/issues/719) |
| Actual TempleOS loader | Own-encoder BIN fixtures load in the verified TempleOS environment; imports, exports, patches, heaps, main records and expected failures agree with independent model predictions and observable target behavior. | No generated BIN or automated loader command. Existing oracle docs cover unrelated earlier fixtures. | [#720](https://github.com/frankischilling/holyc-ocaml/issues/720), [#721](https://github.com/frankischilling/holyc-ocaml/issues/721) |
| Whole-tree phase report | The fixed 528-source denominator is reported separately for lex, preprocess, project parse, check, lower/verify, interpretation where applicable, native emission/execution where applicable, BIN emission and target loading. Every failure records first phase and owner; no silent skip changes the denominator. | `holyc corpus lex` and `holyc corpus parse` exist. Later phase reports do not. | [#722](https://github.com/frankischilling/holyc-ocaml/issues/722) |
| Compiler project artifact | Candidate holyc-ocaml processes the complete `Compiler/Compiler.PRJ` source closure through required checking, lowering, assembly and native/BIN output, then the emitted compiler runs real compiler entry points. | No complete compiler artifact command or result. | [#723](https://github.com/frankischilling/holyc-ocaml/issues/723) |
| Compiler bootstrap | The compiler emitted by the previous stage compiles the designated compiler source set again; later generated stages compile conformance inputs and the required next stage with recorded producer lineage. | Not attempted; no bootstrap command. | [#724](https://github.com/frankischilling/holyc-ocaml/issues/724) |
| Kernel, Adam and Demo | Exact project/source sets advance through required phases and execute in the correct runtime. Privileged/hardware cases use controlled target setups or remain explicit blockers. | No full product-corpus execution gate. | [#725](https://github.com/frankischilling/holyc-ocaml/issues/725) |
| Public compiler workflows | Installed CLI/library exposes real check, compile and artifact inspection consumers for every promised mode and target. Help, exits and examples match behavior outside the checkout. | Existing commands remain bounded; required compiler/artifact commands are absent. | [#726](https://github.com/frankischilling/holyc-ocaml/issues/726) |
| Persistent REPL | Interactive inputs reuse the same checked source/session path; definitions, storage, defaults, failed input recovery and limits remain coherent across commands. | Retained session APIs exist; no complete public REPL command. | [#727](https://github.com/frankischilling/holyc-ocaml/issues/727), [#704](https://github.com/frankischilling/holyc-ocaml/issues/704) |
| Diagnostics and public contracts | Supported success and failure reports retain exact source provenance, bytes, types, phase and work; versioned schemas and library contracts pass compatibility tests. | Existing API/CLI report tests cover implemented slices; 1.0 stabilization remains open. | [#728](https://github.com/frankischilling/holyc-ocaml/issues/728) |
| Fuzzing and differential regressions | Reproducible bounded campaigns cover required parser, layout, IR, encoder, binary and runtime boundaries; minimized crashes and oracle differences have maintained regressions and no unresolved P0 failure. | Component properties and regressions exist; the complete campaign is pending. | [#729](https://github.com/frankischilling/holyc-ocaml/issues/729) |
| Input and runtime safety | Required include, generated-input, allocation, pointer, relocation, executable-memory and host-boundary checks pass with exact limits and failure effects. Untrusted compile-time input gains no implicit host filesystem, network or process capability. | The security policy and component tests cover existing paths; the 1.0 audit remains open. | [#730](https://github.com/frankischilling/holyc-ocaml/issues/730) |
| Determinism and reproducibility | Independent clean directories reproduce required compiler tables, IR, diagnostics, host artifacts, BIN/maps and packages or document source-grounded nondeterminism. | Build-identity checks exist; complete artifact reproduction is pending. | [#731](https://github.com/frankischilling/holyc-ocaml/issues/731) |
| Performance and resource budgets | The candidate passes versioned per-phase resource and performance budgets established before qualification, with recorded hardware/toolchain/input baselines and explained changes. | Semantic limits exist; the complete measured regression budget is pending. | [#732](https://github.com/frankischilling/holyc-ocaml/issues/732) |
| Supported-host CI | Exact candidate revision passes required Linux x86-64 and Windows x86-64 source, native, artifact, install and release checks. Trusted oracle jobs stay separate from untrusted pull-request execution. | Current CI covers hosted Linux/Windows source/native tests, not the complete 1.0 artifact matrix. | [#733](https://github.com/frankischilling/holyc-ocaml/issues/733) |
| Packages and provenance | Source and supported-host packages install and run outside the checkout with exact producer/reference identity, checksums, required assets and notices. | Development package exists; 1.0 package qualification is pending. | [#734](https://github.com/frankischilling/holyc-ocaml/issues/734) |
| Documentation | Language, architecture, commands, target classifications, limits and known gaps describe the qualified release without planned commands masquerading as implemented behavior. | Maintained docs exist and continue to evolve. | [#735](https://github.com/frankischilling/holyc-ocaml/issues/735) |
| Release candidate | Every required row above passes at the exact protected candidate revision, with no P0 defect or unfinished required capability. Published artifacts are then reverified. | No 1.0 candidate is qualified. | [#736](https://github.com/frankischilling/holyc-ocaml/issues/736) |

The raw lexer remains its own whole-tree phase and must cover all 528 canonical
source blobs. The reviewed parser baseline is useful for regression comparison,
but its current acceptance counts are baseline measurements rather than a 1.0
pass criterion. Later corpus reports must retain the same canonical source
identity and state their own phase-specific denominator when a phase applies to a
project artifact rather than independently to every file.

## Bootstrap lineage

Bootstrap evidence must identify the producer that generated every artifact. A
successful stage produced by the original TempleOS compiler cannot be attributed
to a compiler emitted by holyc-ocaml.

### Oracle/reference evidence

The original TempleOS compiler running in the verified `final` image is an
independent reference producer. Its fixture inputs, image hashes, commands and
captured outputs belong to the oracle evidence described in
[oracle fixtures](oracle-fixtures.md). This reference evidence does not count as
a holyc-ocaml bootstrap stage.

### Stage 0: compiler artifact

The producer is the exact protected holyc-ocaml candidate, running on a required
compiler host. It consumes `Compiler/Compiler.PRJ` using the manifest's canonical
source/asset universe and records its complete ordered consumption trace. Call
the emitted compiler artifact A0. Its target is the host artifact format or
TempleOS form named by the gate. A0 runs real compiler entry points on maintained
conformance programs in the matching host runtime or verified TempleOS oracle.
The phase report, artifact hashes, symbols/relocations and observable behavior
must identify the same producer and inputs.

### Stage 1: generated compiler recompiles the compiler

The producer is A0, identified by its Stage 0 artifact hash. It recompiles the
same designated canonical compiler project under the declared runtime, mode and
target, producing A1. Any cross-target transition is recorded explicitly.
Conformance programs and production of A1 are the consumers. The original
TempleOS compiler and the OCaml implementation cannot substitute for A0 here.

### Stage 2 and comparison

The producer is A1, which compiles the same project to A2 and runs the required
conformance inputs. Release evidence compares A1 and A2 bytes, symbols and
relocations when those outputs are required to be deterministic, and compares
defined behavior where source- or platform-defined metadata can legitimately
differ. Every permitted difference needs an explicit rule and evidence.

Each bootstrap record carries producer revision and artifact hash, canonical
input hashes and order, runtime or image revision and hash, target format, CPU,
ABI and source mode, the consumer command, observable outputs, artifact
checksums, and the first failing phase if the stage does not complete.

## Product corpus after bootstrap

The qualified generated compiler then processes the required Kernel, Adam and
Demo sets. `Kernel/Kernel.PRJ` defines the kernel project root and source closure.
Adam and Demo use the complete prefix-scoped canonical sets defined above. Their
phase results stay tied to the same source blobs used in whole-tree reporting.

Privileged, interrupt, task, display, device or other hardware-dependent cases
run only in a controlled environment appropriate to their behavior. If the
required environment or compiler support is missing, the release record names
the first failing phase and blocking issue. Hosted approximations may provide
additional tests but cannot replace a required TempleOS target result.

## Release evidence record

Every final gate result should be reproducible from a compact record containing:

* holyc-ocaml producer revision and artifact hash;
* pinned TempleOS revision and canonical input-set identity from
  `reference/manifest.json`;
* exact source/project order and source mode;
* compiler host, CPU and ABI where applicable;
* target format and runtime or oracle image identity;
* command, limits and environment relevant to the observable result;
* consumer identity, output, exit status and artifact hashes;
* first failing phase, diagnostic and owning issue for an incomplete gate.

Hosted predictions, project-owned validator output and native TempleOS oracle
observations remain distinct fields. A project writer and project reader agreeing
with each other does not replace an independent artifact consumer, and an
independent model does not replace required actual TempleOS loader execution.

## #683 completion boundary

Issue #683 is complete when this contract and the corresponding
`reference/manifest.json` input-set fields are reviewed and maintained, every
required matrix cell and gate has an observable pass condition and owner, and no
planned command is described as operational. The implementation issues may still
be open after that point. Their open state is expected to keep #682 and #736
blocked until the required evidence exists.
