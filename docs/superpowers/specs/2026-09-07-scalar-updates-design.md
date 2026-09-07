# Scalar update expressions

Issue [#605](https://github.com/frankischilling/holyc-ocaml/issues/605) connects
scalar I64/U64 updates to the source integer program executor. At main
`d6c1e9dbb3cb878f885bef7a6598dfa4053659c2`, the compound global accumulator,
postfix return and local postfix loop all fail with HCRUN0003 in both modes.

The shared semantic engine already checks update lvalues and assignment result
classes. Retain the exact typed postfix operand, as prefix and binary operands
are retained. Expression lowering prepares one existing checked frame/global
address, visits the compound RHS, then emits the original update opcode.
Prefix/postfix forms emit an address followed by their unary update opcode.
An expansion that loads the old value before visiting the RHS would change
`G+=(G=2)`. Reusing ordinary assignment and arithmetic would also erase the
source update opcode and its constant-folding barrier.

The VM preflights all update instructions against canonical location identity,
the exact destination type, supported RHS word types, flags and payloads. It
reads the old word when the update instruction executes, after RHS effects.
It computes the new bits using existing bounded word arithmetic, stores only
after successful computation, and publishes either old bits for postfix or
new bits for prefix/compound forms. The destination class determines result and
division/right-shift signedness; no RHS unsigned promotion changes that class.
Uninitialized reads, step/allocation/depth limits and owner/phase diagnostics
use the existing mechanisms. Updates remain one VM instruction after address
and RHS preparation.

Pinned TempleOS source `c26482bb6ad3f80106d28504ec5db3c6a360732c`:

- `Compiler/PrsExp.HC:98-118,201-208`: prefix/postfix opcode choice and
  compound destination dereference removal before RHS parsing;
  `15-66`: pointer compound scaling, outside this scalar connection.
- `Compiler/BackB.HC:304-385`: prefix reads after mutation, postfix before it.
- `Compiler/BackA.HC:372-431,442-595,603-658`: destination read/modify/write and division/right-shift
class selection, with RHS available before mutation.
- `Compiler/CInit.HC:54-59,88-97`: all update forms are constant barriers.
- `Compiler/OptPass012.HC:827-854`: compound multiplication, division and
  remainder strength reductions. The initializer guard must recognize new
  compound shift/divide/remainder ICs in value fragments and transitive callees.

The scope is scalar frame and ordinary code-heap globals only. Pointer,
floating, narrow, aggregate, imported and locked/general-memory updates remain
explicit boundaries. This exposes raw hosted update IC execution, preserving
the existing unresolved optimizer boundary (#574/#585); it does not establish
optimized/native source parity. No new native TempleOS oracle is claimed.

Acceptance covers source accumulator/loops, all operators, old/new values,
aliasing RHS and calls, signedness/wraparound, conditional versus eager
contexts, initializer barriers/guards/provenance, malformed and foreign
contexts, exact budgets and repeat execution in JIT and AOT. Integrate only
after independent review, full local verification and all five CI checks.
