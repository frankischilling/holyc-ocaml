# Native value liveness

The native backend gives values used in another basic block a stack home. It
publishes the producer before a branch and reloads the home when another block
needs the value. These homes survive calls within the same activation.

Homes now share space when their lifetimes occupy disjoint blocks. A backward
worklist computes live-in sets from uses, definitions and successor edges. It
includes loop backedges, not just the order of blocks in the file. Each value
reserves its home throughout every block where it is defined or live. This
whole-block rule also prevents a register still holding an old value from
publishing it over a newer value's home. A deterministic first-fit assignment
chooses the lowest available home.

For example, 1,024 integer values produced and consumed in separate consecutive
block pairs need one 8-byte home, rather than 1,024 permanent homes. Values whose
lifetimes overlap across a loop retain separate homes. Branches, reordered
blocks, exact frame quotas, repeated execution and cleanup after budget faults
have independent expected-result tests. The existing checks still cover fixed
instruction registers, reserved runtime bases, call clobbers and frame unwinding.

This applies to closed native integer graphs and callable scalar values whose
uses are represented directly in the graph. Callable graphs with expanded
reference dependencies or shared pointer values keep permanent homes. Local
spill slots remain separate from shared homes, and semantic storage, active
stack bytes, depth and code quotas remain independent.

TempleOS's `Compiler/OptPass6.HC:32-176` tracks temporary stack values and call
clobbers; `Compiler/OptPass3.HC:511-621` selects source register variables.
Those source variables are distinct from the hosted backend's temporary spill
homes. This change does not widen narrow source values, alter `reg` or `noreg`,
or claim the same machine code as TempleOS. General source register allocation,
floating allocation and requested-register conflicts remain open in issue #703.
