# Persistent class and union storage

[The example](../examples/persistent-aggregates.hc) prints `AB` and returns 42
through IR, isolated native execution and native source tasks in JIT and AOT
modes. It combines a packed class global array with a function-static object.

Nonempty classes and unions retain their original storage extent and element
stride. Integer and Bool fields, nested aggregates, inherited members, member
arrays and multidimensional root arrays use the same checked member projections
as automatic objects. Ordinary and integer-backed whole values read and write
their selected scalar prefix. A prefix copy preserves the destination's other
bytes. Class parameters and returned register words can supply that prefix.

## Allocation and lifetime

A global allocation captures its selected class record at the original parser
publication. A static allocation retains the original function, local occurrence,
class record and checked dimensions. Later class or global replacements cannot
resize these roots or change the field selected by an earlier function. Runtime
member dimensions and offset expressions retain their original preparation
receipts and execute once.

Attached global declarators remain part of their original aggregate statement.
Source sealing checks each completed declarator, header, selected class entry and
publication order before adding it to the semantic declaration list. Function-local
class definitions can supply static objects through their exact allocation receipts.

Globals and statics have separate roots. A function's static object survives calls
and recursion within one execution. Equal local names in different functions
remain separate. A new execution receives fresh persistent storage.

The source shape distinguishes class elements from physical byte cells. Two
three-byte class elements have an object count of two, six byte cells and a
three-byte stride. Size queries and indexing use source geometry; allocation and
initialization flags cover every physical byte. Static allocation rounds its
storage charge to eight bytes without granting access to the padding.

Owned class pointers, primitive byte views and casts retain the complete original
root. Every reached field window must fit that root, including from an interior
or one-past reference. Unrelated allocations cannot supply a pointer difference.

## Initial state and limits

Uninitialized persistent objects begin with unknown bytes in hosted JIT mode.
A write initializes only its destination window. A partly written union word
still faults with `HCIRVM0012` if the read reaches unknown bytes. AOT objects
begin with every declared byte initialized to zero, including scalar classes
larger than eight bytes. Unknown-byte tracking is hosted policy; it does not
predict the contents of a TempleOS JIT allocation.

The existing global-byte quota counts the complete global extent and padded
static allocation. The quota fixture uses six global bytes and an eight-byte
static allocation: 14 bytes succeeds and 13 rejects. Private native descriptors
and initialization flags retain their separate arena bounds.

The shared fixtures cover packed widths, signed narrowing, unions, inheritance,
padding, arrays, prefix copies, recursive statics, retained replacements, runtime
layouts and metadata reads. Ownership controls reject foreign roots, reused
static locations and changed source extents before execution. Native graph
controls also require the original sealed member projections.

IR data pointer fields also retain their separate ownership shadows over these
byte roots. A stored address cannot keep a retired activation alive. The native
path continues to reject data pointer fields with `HCBACK0002`.

[Aggregate initializers](aggregate-initializers.md) now traverse completed selected
classes, integer leaves, nested fixed arrays and strings with the original source
byte cursor. Selected forward members can complete before their leaves are
consumed. Changes during a leaf and other leaf families remain
open.
Callback fields, native data pointer fields, reference returns, persistent pointer
cells, general aggregate copies and whole-value casts remain unfinished under
[#686](https://github.com/frankischilling/holyc-ocaml/issues/686) and related
issues. This storage path does not close the aggregate issue or establish full
compiler parity. Tests derive expected words from pinned source and hosted
execution; there is no TempleOS runtime capture or machine-code comparison.

## TempleOS source

The reference is commit `c26482bb6ad3f80106d28504ec5db3c6a360732c`.

- Complete `PrsGlblVarLst` at `Compiler/PrsStmt.HC:209-463` uses the selected
  class size and dimensions, chooses global alignment and zeroes AOT storage.
- `PrsVarLst` at `Compiler/PrsVar.HC:405-742` separates automatic and static
  allocations, rounds static storage to eight bytes and zeroes AOT statics.
- `PrsVarInit`, `PrsVarInit2`, `PrsGlblInit` and `PrsStaticInit` at
  `Compiler/PrsVar.HC:1-246` supply the original member-leaf initializer traversal.
- Member access and indexing at `Compiler/PrsExp.HC:960-1115` retain the
  selected class offsets and element size. Scalar prefixes follow
  [backed values](backed-aggregate-values.md) and
  [default class values](default-aggregate-values.md).
