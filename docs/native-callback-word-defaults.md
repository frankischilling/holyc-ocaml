# Native callback-word defaults

`run --target=host-jit` prepares closed integer defaults for original one-star
callback-valued fixed parameters, including parameters inside an indirect
signature. JIT and AOT source modes retain the same saved-word contract.

```sh
opam exec -- dune exec --root . -- bin/holyc.exe run --target=host-jit --format=json examples/native-callback-word-defaults.hc
```

The example returns I64 42. `invoke`'s original declaration supplies the numeric
word 17 when its callback argument is omitted. `Check` retains its own default
12, which that indirect call does not select. Both values prepare once, use six
preparation steps and retain sixteen saved bytes.

A callback parameter's return class does not determine its default's storage
width. The original one-star declarator selects a complete integer word, even
when the callback returns a narrow integer, F64, U0 or a pointer. Preparation
evaluates the integer expression with an internal zero-depth I64 destination;
the later argument producer keeps the parameter's physical RT_PTR type. The
native proof checks that distinction against the original declaration, saved
object, charged completion and entire callable bundle. An ordinary object
pointer cannot use this admission path.

Every original default requires successful preparation, including unused
functions, unused callback declarations and explicit-only calls. Omission loads
the selected header's saved word; forwarding and copying an executable address
do not move defaults between headers. All 64 bits survive
preparation and callback storage. Numeric and null defaults stage zero executable
owners. They can be compared as words, copied through callback cells and passed
onward, but cannot select executable code. Invoking them reports `HCIRVM0024`
after the reached argument effects. An explicit owned-code argument can override
the saved numeric default and invoke its original body.

Callback equality accepts scalar integer operands. Two numeric values compare
their full bits; owned code compared with a nonzero numeric operand reports a
reached `HCIRVM0024`. Object pointers remain outside that comparison path.
An effective `noreg` selects the existing stack parameter path for scalar and
callback parameters and their defaults. Allocatable or explicit register
selection remains outside the native adapter.

Named, anonymous and initializer preparation share their existing work budget.
Each successfully saved default charges eight bytes. Failed preparation retains
reached work and earlier completed payloads, then prevents native entry.
Compiled code/frame/IR/block limits and runtime frame/depth/physical-stack/step
limits still check exact and one-below boundaries and recover on the same image.

Tests compare independent values, fresh public IR, separately executed checked
batch IR and native execution in both source modes. They cover full 64-bit words,
callback return classes, defaults from different headers, multiple lanes, sparse
omission, explicit owned overrides, forwarding, storage copies, word tails,
effective `noreg`, reached faults and quota recovery. Both image ABIs compile;
reconstructed or foreign saved values, missing or duplicate completions, and
equal foreign bundles cannot supply proof. The maintained CLI example checks
values, preparation and exact runtime work.

The pinned audit follows `Compiler/PrsVar.HC:350-357,619-657` for callback storage,
fixed slots and saved declaration defaults, and `Compiler/PrsExp.HC:435-586` for
materialization, reverse arguments and indirect cleanup, at TempleOS commit
`c26482bb6ad3f80106d28504ec5db3c6a360732c`. Private executable ownership enforces
the hosted runtime contract. This adds no TempleOS capture or exported ABI proof.

Effectful, owned-code, string, `lastclass`, floating-value and ordinary
object-pointer defaults remain unfinished, as do multistar consumers, member
storage, owned-code/effectful callback initializers and callback updates, ordinary owned-code storage/returns,
replacement/linking, general floating/aggregate execution and the full compiler.
