# Formal members and duplicate register requests

TempleOS keeps a member for every fixed parameter, including parameters without
names. An unnamed parameter is not a discarded argument. `PrsVar.HC:339-344`
assigns it the name `_anon_` and suppresses its unused-member warning. An
explicitly written `_anon_` keeps the ordinary warning rules.

Each fixed parameter occupies an eight-byte argument slot. `U0` and `I0` do not
turn a parameter list into an empty list: `F(U0)` has one parameter, whereas
`F()` has none. Preserving that signature and its offsets does not establish
support for executing zero-sized parameter values.

## First member lookup

`LexLib.HC:67-84,103-147` allows repeated `_anon_` members and resolves a name
to its first inserted member. The later members still have separate declaration
positions, offsets and register requests.
The existing duplicate-name exceptions `pad` and `reserved` follow the same
first-member lookup and initializer-target rules.

For example, the source-derived result of this function is 37:

```c
I64 F(I64)
{
  I64 _anon_=37;
  return _anon_;
}
F(11);
```

The initializer writes the first parameter, not the newly declared local.
`PrsVar.HC:607-614` publishes the new member, restores the declaration's tokens
and parses the textual assignment. That assignment finds the first `_anon_`.
Its conversion therefore uses the first member's type. The new declaration
still owns its own storage and the subsequent use-count reset.
Publishing the duplicate first looks up the existing member and increments its
use count. Initializer left- and right-hand uses are additional events; only
the newly declared member's count is reset afterward.

The assignment target and declaration owner must remain separate throughout
binding, conversion, lowering, register scoring and warning accounting. Source
tokens for an unnamed formal stay unnamed; the derived binding must retain the
original formal and its position rather than fabricate an identifier token.

## Defaults and variadic slots

A simple unnamed scalar or pointer parameter cannot have a default. The
missing-name branch accepts a comma, semicolon or closing parenthesis, not `=`.
An explicitly named `_anon_` can have a default. Anonymous function-pointer
declarators follow a different parsing path and must not inherit a blanket ban.

Anonymous fixed parameters count toward the fixed arity before `...`.
`PrsDotDotDot` appends `argc` and `argv` after those fixed slots. For N fixed
parameters, their offsets are `16 + 8*N` and `24 + 8*N` respectively.

## Last explicit register owner

Duplicate explicit register requests are different from duplicate names.
`OptPass3.HC:29-36` warns when a requested register is already in use, then
replaces its owner with the later member. Earlier members are not put back into
the automatic allocation pool. The register is reserved once.

For example, with two distinct byte parameters requesting R12, assigning 128
to the first parameter writes its byte stack slot and reads back -128. Assigning
128 to the final owner can retain the full register word. An unused later owner
still shadows the earlier request. A local declared after a parameter can also
be the final owner.

`OptPass4.HC:198-215` selects registers by the final offset table.
`OptPass789A.HC:712-716` loads entry arguments only for final owners with positive
parameter offsets. A local owner must not receive an earlier parameter's entry
value merely because both requested the same register.

Conflict diagnostics belong to compilation. Revalidating allocation evidence
must not print the same warning again. The proof must retain the ordered member
list, final owner, shadowed members and unused reservations independently of
which accesses survive optimization.
The hosted `HCREG0001` diagnostic follows the ordinary, uncounted `PrintWarn`
path in `Kernel/StrPrint.HC:914-922`.

The hosted physical-home report is not a copy of TempleOS's register table.
A source-zero unused owner can reserve a register without an emitted home or
entry load. That does not permit an earlier shadowed member to use the register.
This projection preserves the admitted value behavior; it does not claim that
the generated save and load instructions match TempleOS byte for byte.

## Evidence limits

These rules are deductions from TempleOS commit
`c26482bb6ad3f80106d28504ec5db3c6a360732c`, not new TempleOS runtime captures.
They do not establish support for fixed machine registers, overlapping member
offsets, explicit address-taken requests, arbitrary callback or aggregate
allocation, or the complete HolyC calling convention. Issue #703 remains open.
