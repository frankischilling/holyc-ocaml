# Class and union member sizes

`sizeof` accepts a named class, union, automatic object or class parameter
followed by dot members. Each member replaces the preceding size with its
selected class size multiplied by its own array count.

```c
class Inner { U16 words[3]; };
class Box { Inner items[2]; U8 tag; };
I64 Measure()
{
  Box object;
  return sizeof(object.items)+sizeof(object.items.words)+sizeof(Box.tag);
}
Measure(); // 12 + 6 + 1 = 19
```

The intermediate `items` array contains two objects, but its count does not
multiply the final `words` field. The same rule applies when the root object
is an array. A terminal pointer or callback field uses eight bytes per element.
Trailing stars after a valid member path produce eight, including more than
four stars: this query suffix does not construct a pointer declaration.

The query emits an immediate `I64` word. It can inspect an uninitialized object
because it reads declaration metadata. It does not load a field or evaluate an
object expression. The original grammar has dot paths and trailing stars;
`sizeof(pointer->field)`, array subscripts, calls and arithmetic inside that
path are rejected.

The [aggregate example](../examples/aggregate-member-sizeof.hc) prints
`1 14 6 4 8` followed by a newline and returns 33. Its queries run through both
IR/JIT, IR/AOT, `host-jit` and `host-jit-task`.

## Builtin integer unions

The six public integer unions retain all 24 fields from `Kernel/KernelA.HH`.
Their generated declarations preserve each field's original type spelling,
array count and source line.

| Selected union | Original members | Size of each terminal field |
| --- | --- | --- |
| `U16`, `I16` | `i8[2]`, `u8[2]` | 2 bytes |
| `U32`, `I32` | `i8[4]`, `u8[4]`, `i16[2]`, `u16[2]` | 4 bytes |
| `U64`, `I64` | `i8[8]`, `u8[8]`, `i16[4]`, `u16[4]`, `i32[2]`, `u32[2]` | 8 bytes |

The `i8` and `u8` fields select internal `I8i` and `U8i` classes. The wider
fields select the original public unions, so a query can continue through
them. `sizeof(U64.u32)` is 8, `sizeof(U64.u32.u16)` is 4, and
`sizeof(U64.u32.u16.u8)` is 2. Each dot replaces the preceding array count.
An internal terminal class rejects a further dot before the next member token
is read.

These queries also work on primitive locals, parameters, static locals and
module globals, including arrays, and on primitive fields of user-defined
classes. A scalar global retains the class selected by its original
declaration. Later classes named `U64` or `U32` cannot replace that class or
the original types inside a builtin union.

The [builtin union example](../examples/builtin-union-sizeof.hc) prints
`8 4 2 2 4` followed by a newline and returns 32. It queries uninitialized local,
static and global objects through declaration metadata in both IR modes and
the two native JIT targets.

## Selected identities and query timing

Each field retains the type selected by its original declaration and the
checked count from its dimensions. A later class with the same spelling cannot
replace that selected identity. A forward completion can change the size read
by a later query without changing the containing object's earlier layout.

Dot validation happens before the lexer produces the member token. Member
lookup then uses the original selected class and runs before subsequent
lookahead. A directive encountered between those phases can complete the
selected class. A directive reached after a member read cannot rewrite that
saved result.

Inherited lookup searches the selected class before its original base chain.
Ordinary duplicate names are rejected by `MemberAdd`. Its `pad`, `reserved`
and `_anon_` exceptions keep the first declaration within each class. For those
exempt names, a matching child member takes precedence over a base member.

An internal type such as `U8`, or a local declared with that type, fails at the
dot before member-token lookahead. A global variable passes this first class
check and then selects its declared class, so its member-token lookahead can
run a directive before the missing-member error.
Class pointer companions have the original class flag and an empty member
tree, so a pointer path fails when its member is looked up. Terminal pointer
field queries remain valid.

Original dimension and `$$` preparations remain attached to the saved query.
Later query reads reuse those results without repeating their output or other
effects.

## Source evidence and verification

`Compiler_record` retains each field's original type, count and placement size
during the member layout phase. The semantic layout adapter binds that saved
size to the exact field input and keeps its nominal class type. This preserves
the containing object's earlier layout even when a forward field class grows.
A query validates its table, namespace and parser
callback, then reads the current record for the exact canonical class. Each
dot and member step belongs to the same root and ordinal. Completion requires
the complete original member path. Expired callbacks and records from another
class cannot supply replacement evidence.

The shared execution fixtures cover named types, automatic objects, class
parameters, nested and inherited members, arrays, callbacks, pointer fields,
permitted duplicate names, declaration lookahead and forward completion.
Builtin fixtures cover all six unions, nested counts, scalar global and static
declarations, selected types after replacement, and internal-member rejection.
The runtime fixture prints `offdim` once and returns 42; its exact and
one-below limits cover preparation and execution. Native checks also cover
generated code and output budgets, require completed machine-code fragments,
and require zero interpreted runtime instructions.

Generator tests check all 24 original field declarations and source lines.
Missing, duplicate, reordered or changed fields, invalid counts and unsupported
declarator shapes fail explicitly.

Parser authority tests run in JIT and AOT modes. They check original member
steps, namespace and table ownership, foreign and stale class records,
omitted steps, expired callbacks and immutable results before and after a
forward member class changes from zero to two bytes. CLI checks verify both
examples' independent output and results.

Builtin seeding requires the frontend entry's explicit primitive payload.
The authority tests reject an ordinary class with the same name and pinned
origin, so that class cannot acquire the generated union fields.

The reference is TempleOS commit
`c26482bb6ad3f80106d28504ec5db3c6a360732c`:

- `Compiler/PrsExp.HC:303-351` implements the complete `PrsSizeOf` path, including
  class checks, member lookup, live size reads and pointer suffixes.
- `Compiler/LexLib.HC:67-85,103-148` implements inherited lookup, duplicate
  exceptions and member insertion order.
- `Compiler/PrsVar.HC:247-283,521-532,660-669` prepares dimensions, publishes
  members and stores their earlier placement sizes.
- `Compiler/PrsLib.HC:40-59` creates the pointer companion records.
- `Kernel/KernelA.HH:67-113` declares the six builtin integer unions and their
  original field types and counts.

Expected results come from these source rules and hosted execution tests.
This increment includes no TempleOS runtime capture or machine-byte comparison.

## Remaining work

Persistent class and union objects still require their own storage admission.
`sizeof(static_obj.field)` and `sizeof(module_global.field)` are valid HolyC
metadata queries that the current hosted storage path still refuses; the
fixtures check those boundaries explicitly. General object copies and the
remaining `offset` and relocation paths keep their existing boundaries.

[Issue #822](https://github.com/frankischilling/holyc-ocaml/issues/822) tracks
the original aggregate query implementation, and
[issue #824](https://github.com/frankischilling/holyc-ocaml/issues/824) tracks
builtin union fields within [aggregate work](https://github.com/frankischilling/holyc-ocaml/issues/686).
The [release contract](release-contract.md) covers full compiler acceptance.
