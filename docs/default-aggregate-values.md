# Default class and union values

Completed earlier classes and unions without an explicit backing use a signed
eight-byte whole value in automatic storage. TempleOS initializes their raw
type to `RT_PTR`, which aliases `RT_I64`. The value reads or writes the first
eight bytes; allocation and class pointer stride still use the declared size.

```c
class Word { U64 low; U8 guard; };
I64 F() {
  Word o;
  o.guard=9;
  o=42;
  if (o.low!=42 || o.guard!=9 || sizeof(Word)!=9) return -1;
  return o;
}
F();
```

This returns 42. Assignment preserves the ninth byte. `b=a` also copies only
the scalar prefix. The maintained `examples/default-aggregate-values.hc`
checks two objects, their guard bytes and nine-byte pointer stride. It returns
42 in JIT and AOT with either `--target=ir` or `--target=host-jit`.

The whole value is signed even when its first field is `U64`. Comparisons,
division, shifts, conditions and scalar updates use that signed value. Field
reads retain the field's own type. A derived class has the default value unless
it declares a backing of its own; inheritance does not inherit the base's
backing width. A backing chain ending at an ordinary class also selects the
default signed word. A completed empty terminal class can supply this raw
type to a nonempty outer object; the terminal contributes no storage. Later
same-name definitions cannot alter either the
original selected layout or value class.

Locals, nested members, multidimensional array elements and owned class
pointers share the existing scalar projection and root bytes. The immutable
selection retains the original completed classes, compiler table, namespace,
function identity, scope and source position. Borrowed, missing,
other-function and changed-offset projections fail before IR execution.
Native execution requires the original sealed source graph. Callback storage
keeps its physical pointer type even when the declared return type is a class.

Ordinary assignments capture their destination before evaluating the RHS.
Fused `*p++=rhs` and `*p--=rhs` stores read the binding's current pointer after
the RHS, write the word, then advance by the original class size. Scalar
results can supply supported primitive integer parameters and variadic
output arguments. Class value parameters and returns need separate ABI work.

The root's actual byte extent bounds every access. A one-byte standalone
class cannot hold its default whole value. An eight-byte window beginning at
a one-byte nested class can fit inside its larger root, including adjacent
fields or array elements. Frame allocation padding grants no extra bytes.
Reads require all eight reached bytes to be initialized, and fresh activations
have fresh storage. Unknown and one-past pointers retain the existing faults.
These ownership and initialization checks are hosted policies; they do not
claim TempleOS behavior for invalid memory.

Twenty-three independent value groups cover signed operations, prefix copies,
unions, inheritance, original backing chains and redefinitions, nested arrays,
byte aliases, pointer strides, conditions, argument producers and RHS effects.
Seven fault cases check eight-byte initialization, short roots and frame
padding, a late array element, unknown and one-past pointers, and fresh
activation bytes. Shared IR and native suites also check exact and one-below
instruction, frame, stack and image limits, both native ABIs and proof
ownership. The checks use expected words and hosted execution; they contain
no TempleOS runtime capture or machine-byte comparison.

Whole-value postfix casts, aggregate initializers, class value parameter and
return ABI paths, persistent objects, function-local and zero-sized layouts,
pointer and callback backings, F64 execution, native task objects, retained
JIT imports and general aggregate copies remain open under
[#686](https://github.com/frankischilling/holyc-ocaml/issues/686) and related
value and ABI issues. Full compiler and release acceptance remain under
[#682](https://github.com/frankischilling/holyc-ocaml/issues/682).

The audit uses TempleOS commit
`c26482bb6ad3f80106d28504ec5db3c6a360732c`. Complete `PrsClassNew` at
`Compiler/PrsLib.HC:40-60` initializes each class record to `RT_PTR` and resets
the nominal class size. Complete `PrsClass` at `Compiler/PrsStmt.HC:1-60`
adds inherited and declared storage without changing the raw type.
`Kernel/KernelA.HH:1572-1573` defines the signed raw alias. Complete `PrsType`
at `Compiler/PrsVar.HC:285-371` installs explicit backing relations; complete
`OptClassFwd`, `CmpRawType` and `CmpRawTypePointed` at
`Compiler/OptLib.HC:9-15,508-525` resolve the value class.
The address, dereference, update and assignment producers and scalar backend
consumers are the same complete functions audited for
[integer-backed aggregate values](backed-aggregate-values.md). They retain
nominal addresses and consume the selected raw width, including the fused
postfix store's original class stride.
