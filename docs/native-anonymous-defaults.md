# Live anonymous callback defaults

`run --target=host-jit-task --mode=jit` executes expression defaults in an
original callback declaration, including declarations in a function body or
another callback parameter. Each expression runs once while the parser reads
that header. Later omitted arguments use its saved value, even when the reached
function has a different default.

```c
I64 Counter=40;
I64 Seed(){return ++Counter;}
I64 Answer(I64 n){return n+1;}
I64 (*answer)(I64 n=Seed())=&Answer;
Counter=100;
answer();
```

This returns 42. Explicit arguments, unused declarations and repeated calls
keep the same declaration-time effects. Narrow parameters convert the saved
word at the original argument producer. Fixed arrays, word tails and the
anonymous header's cleanup retain their existing behavior.

Nested callback parameters can save an original function owner too:

```c
I64 Original(){return 42;}
I64 Call(I64 (*q)()){return q();}
I64 (*invoke)(I64 (*q)()=&Original)=&Call;
I64 Original(){return 17;}
invoke();
```

This also returns 42. Reads from callback cells, fully indexed arrays and
assignment expressions preserve the owner captured at the header. A captured
unresolved extern retains its unresolved entry after a later body installation.
Numeric callback defaults retain their full word and zero executable authority.
Reached calls check ownership and the original signature after the ordinary
reverse argument effects.

Run the combined maintained example with:

```sh
opam exec -- dune exec --root . -- bin/holyc.exe run --target=host-jit-task --mode=jit --format=json examples/native-source-anonymous-defaults.hc
```

It saves two defaults, retains sixteen payload bytes and returns 42 through
native source-task execution and independent IR execution. The isolated
`host-jit` target retains its reference-bearing default diagnostic.

Each default image retains its exact parser receipt, namespace, typed root and
sealed call graph. Completion publishes the actual native value into the same
original anonymous header. Later command, initializer, static-initializer and
default images require that saved object and successful native completion.
Equal bits, copied saved objects or a foreign request cannot substitute for
them. Saved source values contain no native PC. The existing owner mapping
checks any live PC before capturing an original retained function value.

Callback calls inside named and anonymous defaults retain their original typed
call records. Scheduled integer static initializers also retain the original
function callback source, exact initializer root and complete entry region.
These joins keep the ordinary IR execution path available for comparison.

Actual default instructions charge both the native step allowance and the
remaining initializer allowance. Each completed value charges eight saved
bytes. Failed defaults retain reached writes, output and work, then stop header
publication. Code, IR, frame, depth and output quotas apply through the existing
native execution path. Native source-task success records no VM instructions.

Tests cover original header shapes, callback calls inside defaults, historical
owners, once-only effects, result latches, reached faults and exact/one-below
limits. Authority controls compile both private ABIs, execute the host ABI,
reject copied or changed values, and check domains, replay, collection and expiry.
Copied owned-default and static-initializer instruction records lose their
complete sealed graph authority before execution.

The pinned source contract comes from `Compiler/PrsVar.HC:619-656`, which
compiles and calls each default and stores its full word in the parameter member,
and `Compiler/PrsExp.HC:455-469,553-586`, which supplies saved arguments and
performs indirect-call cleanup. The reference commit is
`c26482bb6ad3f80106d28504ec5db3c6a360732c`. Hosted ownership and quotas add no
TempleOS runtime capture or exported ABI evidence.

Static callback storage in the streaming native task still needs its original
allocation connection. Direct automatic callback initializers retain the pinned
`HCPARSE0137` rejection; static initialization has a separate source branch.
AOT reference defaults still require output relocation and callable authority.
String, ordinary object-pointer, F64, aggregate and `lastclass` defaults, provider
callback entries, member callbacks and the broader compiler remain required work.
