# Global callbacks

One-star global callback cells and fully indexed global callback arrays execute
through the IR source runner in JIT and AOT modes. Calls inside checked functions
use the selected storage header's saved integer defaults, fixed arguments, word
variadic tail and calling flags. Top-level assignments can install or copy an
owned address for those functions to use.

```sh
holyc run --target ir --mode jit examples/global-callbacks.hc
holyc run --target ir --mode aot examples/global-callbacks.hc
```

The example returns I64 42. Its scalar cell occupies eight bytes and its [2][3]
array occupies 48 bytes. Each call uses the default belonging to the selected
callback declaration. Both headers retain `noargpop` from their original global
declaration.

`Compiler/PrsStmt.HC:223-224` passes the declaration's `fsp_flags` to `PrsType`.
`Compiler/PrsVar.HC:285-366` creates the anonymous header with those flags,
selects internal `RT_PTR` storage and then parses dimensions.
`Compiler/PrsStmt.HC:403-407` stores that header separately from the global's
physical class. A callback returning F64, U0 or an aggregate still occupies an
eight-byte element; `sizeof` uses that size and the original checked dimensions.
Storing or comparing such an address does not make its return domain executable.

The global address lowerer checks the exact publication, selected callback
header, original occurrence, declared return metadata and physical storage type.
For retained globals it also checks the original outer binding and storage
reference. Completing a previously admitted callback declaration publishes its
checked header with the existing allocation and reference. It neither allocates
another cell nor changes an earlier snapshot.

`PrsFunCall` captures the callee before executing arguments right to left. Cleanup
comes from the selected callback declaration: RET1 or `argpop` requests callee
pop unless `noargpop` is present. The reached executable must agree with that
policy. Its own flags do not choose the caller's cleanup. Opaque values retain
their original prepared body across copying, parameter transfer and JIT
same-name replacement. Null and numeric words acquire no executable authority.
Copied or foreign graphs fail before execution.

The hosted JIT runner treats an uninitialized cell as unknown. Loading it fails
before argument effects. AOT zero and an explicitly stored null fail at reached
invocation after arguments. Signature and cleanup mismatches also preserve
argument effects. Array bounds and offset overflow fail before callee loading
and argument execution. These are the hosted storage contracts; the existing
TempleOS fixture supplies separate basic global and array observations.

The maintained tests cover storage, defaults, callee snapshots, reverse argument
order, word tails, original cleanup flags, replacement, source-owned graph
receipts, faults and exact resource limits. Global callback initializers,
top-level indirect invocation, member storage, updates, owned-code defaults,
multistar and dereferenced calls, live task linking/expiry and hosted native
callbacks remain under issue #801. General F64, aggregate and mixed-value
execution remains under issue #688. No new TempleOS capture is claimed here.
