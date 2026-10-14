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

Ordinary JIT class and union initializers activate their original source task at
the initializer start. Earlier declarations and globals remain selected. Each
reached expression executes once during parsing, including a zero-byte forward
member inside a nonempty allocation. That member copies no bytes; the following
direct member keeps the same initializer cursor. The
[ordinary-source example](../examples/source-aggregate-initializers.hc) checks
this timing without a `#exe` block and now runs in both source modes. AOT's
scheduled leaves retain their load-time behavior.

Isolated global and static initializers keep their original layout metadata
through later class completion or replacement. Completed consumers join those
snapshots to the exact source leaves and declared storage, preserving zero-byte
copies and the original byte cursor. The
[retained-layout example](../examples/retained-aot-initializer-layouts.hc) checks
both kinds of storage and a static mutation across calls. Layout snapshots grant
no computed-value or execution authority. Native AOT scalar static leaves can
execute at load time through their original scheduled regions. Each load receipt
joins the exact source leaf, selected destination and declaring frame. Closed
leaves retain their prepared image bytes. The
[static-load example](../examples/native-aot-static-initializers.hc) checks load
order, forward members, original function selection and persistent mutation.
Nonconstant AOT statics with globals-on-data-heap remain open.

Source-task static references retain the original aggregate type and dimensions.
A later initializer can read or update that object's direct, inherited and
nested fields, including fixed member and root arrays. Original member-name
observations retain the reached field layout across replacement. Fragment views
do not allocate storage or grant execution permission; runtime access still joins
the exact private allocation. The static-load example runs in both source modes.

The source shape distinguishes class elements from physical byte cells. Two
three-byte class elements have an object count of two, six byte cells and a
three-byte stride. Size queries and indexing use source geometry; allocation and
initialization flags cover every physical byte. Static allocation rounds its
storage charge to eight bytes without granting access to the padding.

Empty global and static roots have zero data bytes and retain their original
identity and dimensions. Their fixed arrays have positive element counts and
zero strides. Every initializer expression still executes once, including leaves
of zero-width direct members. Native storage retains one owner word per root and
no byte flags, so an arena can contain only private metadata. Reads cannot cross
the empty root into an adjacent nonempty object. The
[empty-root example](../examples/empty-aggregate-initializers.hc) checks source
order, unused-function preparation, repeated calls and class replacement in both
source modes. Automatic zero-byte frame storage remains open.

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

IR and native data pointer fields retain separate ownership shadows over these
byte roots. A stored address cannot keep a retired activation alive. Native
source fragments share those shadows through their original task arena.

[Aggregate initializers](aggregate-initializers.md) now traverse completed selected
classes, integer leaves, nested fixed arrays and strings with the original source
byte cursor. Selected forward members can complete before their leaves are
consumed. Changes during a leaf and other leaf families remain
open.
Callback fields, reference returns, persistent pointer
cells, general aggregate copies and floating whole-value casts remain unfinished under
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
