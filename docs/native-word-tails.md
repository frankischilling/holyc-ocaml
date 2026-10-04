# Native integer word tails

`run --target=host-jit` executes source-defined integer/U0 variadic functions and
calls through matching one-star callback signatures in JIT and AOT source modes.
Fixed parameters keep their declared scalar widths. Each supplied tail argument
occupies a complete eight-byte word; `argv` reads that word as I64 without
narrowing it through a fixed parameter's type.

```sh
opam exec -- dune exec --root . -- bin/holyc.exe run --target=host-jit --format=json examples/native-word-tails.hc
```

The example returns I64 42. It copies the original `Sum` address from a callback
array into a fixed callback parameter of the variadic `Apply` function. `Apply`
passes two tail words to `Sum` and uses its callback declaration's saved default
of 10. The two anonymous defaults prepare once, use six preparation steps and
retain sixteen saved bytes. Fresh public JIT source execution also charges one
step for the global array dimension; the isolated native batch does not.

## Argument storage and calls

The compiler retains the original checked `argc` and `argv` synthetic bindings,
types and frame displacements. The incoming hidden count follows the fixed
parameters; `argv` begins immediately after it. The compiler-placeholder array
dimension does not determine the tail's bounds.

Each activation saves its original count separately from the mutable `argc`
object. Indexed loads, stores, updates and typed pointer aliases use that saved
extent. Changing `argc`, including through an alias, cannot enlarge or shrink
the allocated tail. Empty tails can be addressed at their one-past position;
reading, writing or updating an element faults. One-past addresses of nonempty
tails can move back to an element before dereferencing.

Pointer conversion at a checked copy boundary admits the synthetic internal I64
storage through an ordinary public I64 pointer. Original producers and object
ownership remain checked separately. The existing reference descriptors retain
the tail's origin and actual extent. Their private allocation is bounded by the
largest original source-call tail in the compiled bundle.

Call receipts retain every fixed argument, hidden count and tail producer.
Native calls capture callback addresses and owners before arguments, then
evaluate supplied arguments in the pinned reverse order. Fixed callback
parameters retain their owner words after the complete physical argument area,
including the hidden count and actual tail. Recursive calls and different tail
lengths keep those owners and bounds separate.

Reached numeric/null or signature/cleanup faults occur after argument effects.
Uninitialized callback elements and callee-index bounds faults occur before
those effects. Indirect dispatch checks the original return type, fixed types,
variadic shape and cleanup policy. `argpop`, `noargpop` and `haserrcode` retain
their existing source rules, including the distinction between global staged
flags and local anonymous headers. The private adapter uses fixed RSP and plain
`RET`; this is not an exported HolyC ABI implementation.

## Quotas and validation

Logical activation bytes include semantic local storage, fixed eight-byte slots,
the hidden count and the actual tail. Compiler-private count snapshots,
reference descriptors and callback owner words count toward the compiled frame
and physical active-stack limits. Depth, logical-frame, physical-stack and step
guards retain their existing fault order and unwind before another execution of
the same image. Saved defaults share the existing preparation and byte quotas.

Tests compare public source IR results and an independently executed checked
batch with native execution. They cover all eight integer widths, zero and
nonzero tails, mutable counts, aliases, updates, callback storage and parameters,
ordinary flags, saved defaults, argument effects, reached faults, recursive exact
and one-below quotas, and recovery. Compilation tests admit both image ABIs and
reject an equal foreign variadic frame. Maintained CLI coverage checks the
example's values, preparation and exact runtime step boundary.

The source audit uses `Compiler/PrsExp.HC:435-584`, `Compiler/PrsVar.HC` and
`Compiler/PrsStmt.HC` at TempleOS commit
`c26482bb6ad3f80106d28504ec5db3c6a360732c`. These are hosted comparisons and a
pinned source audit; they add no TempleOS execution capture.

General pointer or owned-code tail values, F64 and aggregate arguments, callback
member/initializer/update consumers, effectful or owned-code defaults, ordinary
code-word storage/returns and retained replacement/linking remain unfinished.
The full compiler, exported ABI, assembler/object/BIN output, TempleOS loader,
whole-tree compilation and bootstrap remain open under #801, #688 and #682.
