# Classes across retained JIT commands

JIT IR and `host-jit-task` commands import earlier completed classes into their semantic context.
Functions keep the class selected by their original return and parameter
headers. Local storage and member reads use that class's original layout.
Defaults can call a checked class-returning function at declaration time.

```c
I64 Preparations=0;
U16 class Packet { U16 low; U8 guard; };
Packet Make() {
  Preparations++;
  return 0x07002a;
}
I64 Take(Packet packet=Make()) {
  if (packet.guard!=7) return -1;
  return packet;
}
Take();
Take()+Preparations;
```

This returns 43: the second call returns 42 and `Preparations` remains 1.
`Make()` runs when the default is declared. Its full register word becomes the
saved default, including the guard byte above the `U16` prefix. Each omitted
argument receives that word in its own eight-byte slot. Reading `packet`
selects its two-byte prefix. A provided argument still requires the original
default's preparation, including its effects and faults.

Run the complete example with:

```text
holyc run --mode=jit examples/retained-class-defaults.hc
holyc run --mode=jit --target=host-jit-task examples/retained-class-defaults.hc
```

Native fragments use the original checked class frame and word ABI, and their
reports record zero interpreted runtime instructions. Larger programs can
exceed the default 65,536-byte code budget. For example:

```text
holyc run --target=host-jit-task --mode=jit --code-byte-limit=524288 examples/inherited-aggregates.hc
```

Exact cumulative code, instruction, execution,
preparation and saved-word limits remain enforced.

Imports carry opaque proofs of the original completed definitions, publications
and sizes from the owning namespace. The task lifecycle journal requires each
import to precede the consuming command. Duplicate imports and foreign tables
cannot change the declaration index adjustment. Executable commands retain
their original syntax and admission receipts.

The semantic context adds metadata items before the current command. A
completed function header keeps its original source index and identity while
analysis uses its position in that context. Global storage joins translate that
position back to the original command. Imported class wrappers omit attached
global declarators; persistent aggregate storage still needs its own admission.

Member layouts reuse the dimensions and `$$` offsets already prepared by the
original declaration. Importing a class does not execute those expressions
again or charge another preparation. Before publishing member storage, the
driver checks the recomputed size and each admitted inherited base against the
original records. Later declarations with the same name cannot replace a saved
base. Partial, cyclic and forward-only base snapshots retain their existing
storage restrictions.

Class reads in preparation callees require the original member or backing
projection, function identity, frame and source position. Raw numbers, copied
ASTs and another function's projection do not grant that storage. Argument and
return words retain their nominal class separately from the integer carrier.
Runtime checks still require the bound callee's original ABI.

The aggregate suites pass 309 IR groups and 583 native groups: 292 ordinary
native groups and 291 retained task groups. The CLI checks pass 920 IR reports
and 2,137 reports including native execution. The retained checks include
exact and one-below limits plus the remaining nested-member and prototype gates.

The reference is TempleOS commit
`c26482bb6ad3f80106d28504ec5db3c6a360732c`.
`Compiler/PrsStmt.HC:1-60` publishes and completes class metadata in the compiler
hash table, including the selected base. `Compiler/PrsVar.HC:609-674` prepares
fixed defaults immediately and allocates word argument slots.
`Compiler/PrsExp.HC:453-470` materializes a saved default with its original
parameter. `Compiler/BackLib.HC:445-659` distinguishes register words from memory
prefix reads. `Compiler/LexLib.HC:154-177` compares saved defaults during header
replacement, so changing a JIT default retains the argument-list warning.
Expected values come from these source paths. The tests contain no TempleOS
runtime capture or machine-byte comparison.

AOT IR and ordinary native source compilation still restrict defaults containing value
or function references. Retained nested class members still lack an original
member-selection receipt; their whole-source layouts remain supported. Native
calls made through an extern class prototype before its replacement still lack
the completed callee ABI. Ordinary native prototypes, class callback signatures,
F64/pointer backings, persistent objects,
owned string defaults, `lastclass` and general aggregate copies remain open
under #686/#702. Full compiler and release acceptance remain under #682.
