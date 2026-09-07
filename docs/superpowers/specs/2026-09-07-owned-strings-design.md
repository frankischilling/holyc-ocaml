# Owned string-literal byte storage

Issue #617 consumes the canonical string IR from #490 and byte storage from
#615. The current source gate fails HCRUN0003 at its U8* initializer in both
modes at `4af56c7db43b12dfb9b6fae659f4eaaa1e49be49`:

```c
I64 Read(U8 *p){return p[0]+p[1];}
I64 F(){U8 *s="*";return Read(s);}
F();
```

The required I64 result is 42. Continuing implementation and protected
integration are authorized by the compiler prompt. Runtime output, stateful
compilation, native backends, TempleOS modules and bootstrap remain required.

## Storage and identity

Use the existing canonical IC_STR_CONST, Bytes payload, instruction/value IDs,
type, flags and span. Plan one persistent literal image for each execution,
including the entry and every supplied function, unreachable blocks and uncalled
definitions. Each owning body/site receives a disjoint byte region containing
the exact payload plus one initialized zero. Owner identity must distinguish
entry/functions and different owners even if a supplied graph is shared.
Identical text at distinct sites remains distinct.

Reuse private runtime_storage and checked byte addresses. A literal region keeps
its own base, count, byte extent, exact pointee and offset. Reference copies,
callee writes and interior indexing preserve all of these. The image stays live
across frame returns, recursion and entry initializer phases; a new execute call
starts a fresh image. No frame bytes are charged for literal storage.

Allocating in invocation frames would lose source lifetime; globally interning
content would merge mutable objects and executions. The per-execution image
fits the existing ownership model without either change. All structural and
capacity checks precede cell expansion and effects. Existing JIT publication
and initializer phase checks remain authoritative.

## Checked source classes

Add Type.compatible_u8_pointer for conversion boundaries: exact Type.equal, or
two one-level primitive U8 pointers whose primitive declaration form is the
checked Internal_type. Permit public/internal spelling forms only for that
case. Use it for plain assignment, local initialization, VM store/formal
matching and runtime coercion. Preserve the exact left/result identity. A
coercion retags only the pointee to the destination type; storage identity,
bounds and offset stay intact. Canonical literal producers remain strictly
Internal_storage U8*. Type.equal, pointer indexing/dereference checks, aggregate
identity and public I64/U64 union distinctions remain unchanged.

Program execution and execute_function gain literal storage. Raw graph-only
execute/eval keep their existing unsupported string/byte contracts. The program
entry needs explicit byte admission even when it has no automatic frame. Call
argument literal producers may carry ICF_PUSH_RES; existing call preparation
must validate/normalize that flag before ordinary literal shape checking.

## Resource and public API contract

Add optional max_literal_bytes, default 1,048,576, to execute_program,
execute_function and run_integer_program, and --literal-byte-limit to run.
Keep package and library names unchanged. Report literal_byte_limit in the
program JSON and literal-byte-limit in human output as additive fields.

Nonpositive limits fail configuration with HCIRVM0001 before source compilation.
HCIRVM0021 reports capacity/host-container exhaustion in preflight with zero
executed steps and the offending site's available source/body/initializer
identity. Count payload-plus-terminator for every site; check arithmetic,
cumulative bytes and Sys.max_array_length before expanding metadata/cells.
Literal allocation consumes no IR steps; a reached IC_STR_CONST consumes its
ordinary step. Frame/global/preparation/depth limits remain separate.

## Source and verification

TempleOS pin: `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
CInit.HC:9, AsmInit.HC:197-206 and PrsExp.HC:693-694 establish U8 class and
literal typing. LexLib.HC:248-274 retains embedded bytes and concatenates
without intermediate terminators. PrsLib.HC:143 and OptPass789A.HC:1098-1105
establish separate records and exact extents. PrsLib.HC:298-308 and
PrsStmt.HC:150-189 establish compiled-storage lifetime. Mutability follows from
ordinary U8 references to allocated compiled storage and native byte stores;
this is a source-derived inference, not a new native execution capture.

Test source/API/CLI in both modes: local and direct literals, assignments and
fixed calls, mutation/narrowing/full assignment results, recursion, repeated and
distinct sites, fresh images, initializer effects, empty/embedded/high bytes,
concatenation, terminator writes, bounds/overflow and exact limits. Exercise
malformed literal shapes/types/flags/owners and retained graph-only contracts.
Run focused/full/CLI, formatting/generated/build/install, reference/provenance
and corpus checks with stable code and serialized builds. Obtain independent
review and all five final-commit checks before protected merge. Next connect
the already typed/resolved implicit-output path to checked runtime calls.
