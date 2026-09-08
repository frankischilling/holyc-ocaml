# Scalar integer global initializers

[Persistent array initializers](integer-persistent-arrays.md) classify ordered
numeric leaves separately, store exact checked destinations and retain direct
owned byte copies. Prepared AOT leaves precede all scheduled load work; JIT
publication receipts preserve declaration and leaf positions in the entry graph.

Initializer expressions can pass [checked scalar addresses](integer-pointers.md)
to fixed pointer parameters. Callees modify the original persistent objects;
declaration phases, publication checks and transitive arithmetic guards remain.
Pointer-valued initial images remain unsupported.

Reference: `c26482bb6ad3f80106d28504ec5db3c6a360732c`. Issue: #603.

The initialized accumulator now runs directly from source:

```hc
I64 Total=0;
I64 AddTo(I64 n) { Total=Total+n; return Total; }
AddTo(20);
AddTo(22);
Total;
```

```text
opam exec -- dune exec bin/holyc.exe -- run --initializer-step-limit=3 --global-byte-limit=8 examples/integer-global-initializers.hc
opam exec -- dune exec bin/holyc.exe -- run --mode=aot --format=json examples/integer-global-initializers.hc
opam exec -- dune exec bin/holyc.exe -- dump-ir --program examples/integer-global-initializers.hc
```

Both modes report 42 after 46 runtime instructions and 3 constant-preparation
instructions. The original explicitly assigned accumulator remains 42 in 50
runtime instructions. Preparation includes the value fragment, its expression
boundary and stream terminator. It does not consume the runtime instruction
budget or supply the final ordinary expression result.

## Declaration values and phases

Ordinary, non-aliased public I64/U64 code-heap globals accept the supported
scalar expression and direct-call domain. Initializer groups retain their exact
declaration owner and use the existing module expression binding, tree, typing,
call classification and lowering passes. They are distinct from ordinary
statement roots. Each object is visible in its own initializer; earlier comma
declarators are visible and later ones are not.

The driver lowers and classifies the original value instructions before adding
any destination address or assignment. It uses the generated
`prevents_constant_folding` metadata, following `OptPass012.HC:66-68` and
`CInit.HC:27-108`. In particular, adding AOT `IC_ABS_ADDR` and `IC_ASSIGN` first
would wrongly make every value nonconstant. Supported pure constant values run
through a verified graph and the bounded integer VM, then supply immutable
initial-image bits in the declared I64/U64 class.

Nonconstant values become source-ordered initializer regions. JIT regions carry
`compile-initializer`; AOT code-heap regions carry `load-initializer` and begin
with zero image words. This follows the distinction in `PrsVar.HC:51-112` and
the ordered `IET_MAIN` execution in `KLoad.HC:153-181`. The hosted graph retains
that intent; it does not emit or load actual BIN records. Ordinary top-level
statements and comma declarators keep their relative order:

```hc
I64 G=1; G=7; I64 H=G; H;             // 7 in both modes
I64 A=1,B=(A=A+1),C=A+B; A*100+B*10+C; // 224
7; I64 G=1; I64 H=G;                  // final ordinary value is 7
```

`I64 G=(G=7)+1;G;` returns 8 without reading the destination before its store.
`I64 G=G+1;G;` returns 1 in AOT; JIT reports the existing hosted unknown-read
diagnostic. Calls, nested calls and recursion share the normal global, frame,
depth and runtime instruction bounds.

## Bounds, ownership and reports

`compile_integer_program` and `run_integer_program` accept positive
`max_initializer_steps`, default 100000. The CLI exposes
`--initializer-step-limit` on `run` and `dump-ir --program`. It bounds total
constant preparation across declarations, including unreachable declarations;
nonconstant regions use the runtime `--step-limit`. Invalid limits fail before
parsing. A failed preparation publishes no compiled program or partial image.

`integer_program_initializer_preparation` exposes immutable classifications,
owned roots, verified value graphs and step evidence.
`integer_program_initialization` exposes the context bound to the exact entry
graph and global image. Pass it with globals and definitions to
`Ir_integer_interpreter.execute_program`; initializer-bearing storage without
the matching context fails preflight with `HCIRVM0017`. Raw image mutation is
internal to the checked compiler path and absent from the public API.

Regions must be complete, ordered and nonoverlapping. Their instruction IDs
increase in physical order, destinations and trailing stores are canonical,
and operands and call scopes cannot enter from outside the region. Every body
and the entry pass VM preflight before mutable words are allocated. Each run
copies the immutable image, preserving repeatability.

Success reports add `initializer_step_limit` and `compiled_initializer_steps`
to JSON, with corresponding human fields. Runtime faults keep the initializer
owner and phase through callees, alongside the active function, source span,
block, instruction and executed-step count. Constant faults have the separate
`constant-preparation` phase and preparation-step evidence.

## Hosted boundary

This path preserves the existing whole-program preflight policy. Supported
total pure values may be prepared early within this scalar domain because
earlier source cannot name that exact future object. Faulting preparation can
still precede earlier scheduled JIT effects; stateful parsing and execution
must be connected before claiming TempleOS's complete fault order.

Initializer arithmetic has an explicit conservative optimizer boundary:
`HCRUN0006` rejects shifts and constant-divisor division/remainder in a
nonconstant value or any transitively called function. A call cannot bypass
this check: `Half(-3)` where `Half(n)` returns `n/2` reaches TempleOS's signed
divide-to-shift rewrite at `OptPass012.HC:419-430`, which differs from raw VM
division. Pure successful constant division such as `(-3)/2` is accepted and
produces -1. Issues #574 and #585 retain the broader optimizer work.

The same guard covers compound shifts and constant-divisor `/=` and `%=`.
`OptPass012.HC:827-854` also rewrites compound arithmetic; a signed remainder
assignment may become an AND mask. Scalar updates remain constant barriers,
so `I64 H=G++;` is scheduled with H's region and phase. See [scalar updates](integer-updates.md).

[Static initializers](integer-static-initializers.md) use this preparation driver
with separate declaration owners, the same aggregate budget and checked
nonconstant declaration regions. Aliases, data-heap/external/import storage, narrow or floating
objects, pointers, arrays, aggregates and callbacks remain outside this path.
General runtime output, stateful `#exe`, native backends, loader acceptance and
bootstrap remain full-compiler requirements. These tests add hosted source
evidence, with no new native TempleOS captures.
