# Zero-sized arrays

The pinned compiler gives the first empty `[]` a count of zero. It does not infer
an element count from the initializer. Explicit `[0]` has the same size. Closed
fractional bounds are converted to integers first, so `[0.5]` and `[-0.5]` also
produce zero. A negative integer count still fails.

```c
U8 text[]="AB";
I64 words[]={};
I64 Check() {
  static I64 saved[2][0]={{},{}};
  U8 local[];
  return sizeof(text)+sizeof(words)+sizeof(saved)+sizeof(local);
}
Check();
```

This returns zero. The maintained `examples/empty-arrays.hc` adds a neighboring
integer and returns 42 through IR, source-task native and isolated native
execution in both outer modes. No source bytes or elements are allocated for
the empty arrays.
The supported storage families are the existing integer, class and callback-word
arrays. Nested zero counts retain their original dimensions and element widths.
All dimension expressions still run at their original phases, even after an
earlier dimension makes the total count zero. JIT source-task tests cover that
work and declaration-time static preparation. Calling the containing function
does not repeat it.

## Initializers and ownership

An empty braced initializer has no scalar leaves. Its completion uses the
original delimiter transcript, declaration, frame and namespace. It does not
invent an expression or a successful scalar evaluation. A zero-count array
cannot consume an ordinary element initializer: `I64 words[]={Next()};` fails
without calling `Next`. Lexer and directive work retain their normal phases.

A direct string initializer still consumes its original string leaf, but copies
zero bytes into a zero-count destination. It supplies no terminator, padding or
zero-fill. The next member's byte cursor is unchanged. Empty copies retain
once-only completion receipts and charge zero copy work; missing, reordered,
copied and foreign receipts remain invalid. Native execution validates the live
arena and its scope without touching data or initialization flags. A later JIT
read of an otherwise uninitialized neighboring member still fails.

Retained zero-count globals keep their original allocation when a forward class
completes. Static empty declarations are omitted from function-body execution
only after their exact completed source and zero-sized persistent owner match.
Private bookkeeping does not grant a source byte or a readable element. Public
resource limits remain positive; reports can still record zero preparation
work. Tests use an adjacent eight-byte integer to check exact storage limits.

## Address and execution boundaries

Global and static out-of-bounds accesses retain the checked object bounds.
Automatic zero-count array address materialization remains unsupported. The
reference VM raised a general-protection fault at `ECAddresses+0x000c` in an
additional address probe. Its source and screenshot are retained under
`test/oracle/empty-arrays/unqualified-address.*`; it is not a successful value
oracle. The hosted source path rejects that case before emitting an address.

Runtime AOT dimensions still need their separate relocation and callable
authority. General floating storage, wider pointer storage and compiler-record
mutation remain outside this feature. Checked extent and stride overflow limits
still apply. Neither original AOT placement nor cross-object address equality
is established by these captures.

## Source and observations

The reference is `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
`Compiler/PrsVar.HC:247-283` sets the first empty count to zero and rejects
negative evaluated counts. `PrsVarInit2` at lines 123-214 has separate negative
count branches; ordinary `[]` does not select them. `PrsGlblInit`,
`PrsStaticInit`, `PrsGlblVarLst` and `PrsVarLst` retain their original traversal,
allocation and replay phases. No negative inference sentinel is synthesized.

Two complete native JIT captures record eighteen observations twice. They cover
zero sizes, a following packed byte, fractional bounds, once-only dimension
calls and failure before ordinary initializer effects. Maintained tests compare
the supported values through IR and native source tasks, and check failure
paths without treating a rejected compilation as an integer result. The
captures do not establish emitted-byte equality or original AOT execution.
Issues [#686](https://github.com/frankischilling/holyc-ocaml/issues/686) and
[#690](https://github.com/frankischilling/holyc-ocaml/issues/690) remain open.
