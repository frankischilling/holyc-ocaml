# Shared integer globals

Reference: `c26482bb6ad3f80106d28504ec5db3c6a360732c`.

The first V3 global-storage fixture uses the same parser, semantic passes,
expression lowerers, verified graphs and integer interpreter as source functions:

```hc
I64 Total;
I64 AddTo(I64 n) {
    Total = Total + n;
    return Total;
}
Total = 0;
AddTo(20);
AddTo(22);
Total;
```

```text
opam exec -- dune exec bin/holyc.exe -- run --target=ir --global-byte-limit=8 --frame-byte-limit=8 --call-depth-limit=1 examples/integer-globals.hc
opam exec -- dune exec bin/holyc.exe -- run --target=ir --mode=aot --format=json examples/integer-globals.hc
opam exec -- dune exec bin/holyc.exe -- dump-ir --program examples/integer-globals.hc
```

Both modes return 42 in 50 instructions. The successful process status remains
0, separate from the final expression value. `--global-byte-limit` defaults to
1048576 and bounds program-owned scalar storage, independently of active frame
bytes. A limit of 8 admits this one-word object; 7 produces `HCIRVM0016` during
preflight with zero executed instructions. The CLI reports `global-byte-limit`
in human output and `global_byte_limit` in JSON. Every resource limit must be
positive.

The accepted declarations are ordinary public I64/U64 scalar objects on the
code heap, without declaration initializers or aliases. Comma groups and
source-visible distinct objects work. Loads and simple assignments compose in
top-level expressions, loop bodies, call arguments, automatic-local initializer
values and returns. Assignments preserve bits and use the destination's declared
class. Public U64 negation remains U64. Local names shadow globals according to
the existing checked binding; source order determines which declaration a
function can see. In the tested repeated-definition case JIT preserves two
objects, while AOT creates an alias and retains the explicit alias boundary.

`Integer_globals` retains immutable declaration and storage evidence. The
compiled-program API exposes it through `integer_program_globals`; pass that
context and `integer_program_functions` to `Ir_integer_interpreter.execute_program`
when using the lower-level entry graph. `run_integer_program` does this join and
accepts optional `max_global_bytes`. `lower_integer_program` and graph-only
`execute` retain their previous storage boundary. Global metadata participates
in deterministic program dumps.

`Global_address_lowering` checks the exact symbol object, publication positions,
type, rank, category and source occurrence. Canonical JIT `IC_IMM_I64` and AOT
`IC_ABS_ADDR` producers carry logical symbols, following
`Compiler/PrsExp.HC:867-902`; they are not fabricated host addresses. Existing
`IC_DEREF` and `IC_ASSIGN` consumers reuse the checked destination type. VM
preflight rejects foreign contexts, wrong address opcodes, classes and flags,
and invented integer pointers. Each execution allocates its own global words
after all bodies and the entry pass preflight. Calls and block transfers
preserve those words while local frames and temporary values keep their
separate lifetimes.

Ordinary AOT code-heap storage starts at zero, following
`Compiler/PrsStmt.HC:350-367`. JIT words start unknown in this hosted executor;
a reached read before assignment produces `HCIRVM0012`, explicitly labeled as
a hosted diagnostic. TempleOS instead uses `MAlloc` and conditionally fills
globals according to `sys_var_init_flag`/`sys_var_init_val` at lines 370-384.
`Kernel/KStart32.HC:17-29` obtains those values from kernel configuration. This
executor does not claim that TempleOS initializes or checks the same JIT read.
Faults retain their execution stage, attempted instruction count, block,
instruction, source span and active function.

Declaration initializers remain a distinct required connection.
`PrsStmt.HC:409-433` publishes an object before initializing it.
`Compiler/PrsVar.HC:51-112` evaluates eligible initializers during compilation
and emits nonconstant AOT code-heap initialization as `IET_MAIN` routines.
`Kernel/KLoad.HC:153-181` executes those entries in record order. AOT data-heap
globals reject `=` at `PrsStmt.HC:336-338`. The source assignment in the first
fixture establishes shared storage without erasing these phase rules.

Static locals, external/import/data-heap storage, aliases, declaration
initializers, arrays, aggregates, pointers, callbacks and narrow/floating
storage remain unsupported, even in unused declarations. Runtime output,
stateful compilation and #exe, optimizer parity, native backends, actual-loader
acceptance and bootstrap remain full-compiler requirements. These tests add
hosted source evidence; they do not add native TempleOS execution captures.
