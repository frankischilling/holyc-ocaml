# Extern call publication

An integer call compiled against an unresolved extern can execute the source
body later joined to that declaration. This works for forward calls, mutual
recursion and retained callers across separate task inputs:

```c
extern I64 F(I64 n=40);
I64 Use(){return F();}
I64 F(I64 n=99){return n+2;}
Use();
```

The result is 42 in JIT and AOT. `Use` keeps the earlier header's saved 40;
publishing the body does not rebind its arguments to the later default.
[Integer defaults](integer-defaults.md) describes declaration-time evaluation.

## Publication and ownership

JIT exposes a local body only after its definition's source item is published.
Thus `extern I64 F();F();I64 F(){return 42;}` reaches an unresolved call in JIT.
AOT links the image's checked source definitions before entry, so the same
program returns 42. Calls inside an earlier compiled body use the publication
boundary of the invocation that reaches them.

Retained task callers can acquire a body published by a later command.
Resolution requires the exact canonical callable symbol and strict ancestry
through the checked declaration predecessor chain. Matching names, numeric IDs,
copied declarations or reconstructed headers cannot authorize a join. Once a
JIT definition is complete, a new same-name definition gets a new identity;
older calls keep their original joined executable.

The call retains its original header, supplied arguments, saved defaults,
variadic count and cleanup protocol. Invocation checks that the published
body's return type, fixed parameter types, variadic shape and cleanup agree.
Renamed parameters do not change these types. The definition owns its frame,
locals, statics and literal storage. [Joined definitions](integer-joined-definitions.md)
describes those ownership checks.

Supported [variadic bodies](task-implicit-output.md) receive integer tails in
their invocation's owned `argc` and `argv` storage. The
[stateful extern example](../examples/stateful-exe-extern-calls.hc) declares an
extern in `#exe`, retains a caller before the variadic definition is published,
and uses `StreamPrint` to insert the resulting 42 into the outer source.
AOT directives run in their separate JIT task; this does not link that task's
namespace into the outer AOT image. See [incremental task execution](integer-task.md).

The example returns 42 with empty ordinary output, 50 runtime instructions
in JIT and 52 in AOT, and six preparation instructions in either mode.
The CLI regression checks exact limits together and either limit one lower.

## Reached calls and providers

A reached extern without an available joined body or approved hosted provider
reports HCIRVM0030 at execution. An unused function body or skipped branch may
contain such a call. Other unsupported instructions and malformed call evidence
still fail preflight. Earlier writes and output survive a reached failure;
a task can subsequently publish the body and invoke its retained caller.

A checked hosted provider remains available until source publication. After
publication, a compatible source body replaces it even for a retained caller.
For `extern U0 PutChars(U64 ch);PutChars('A');U0 PutChars(U64 ch){}`,
JIT captures A and AOT captures nothing. A published body incompatible with
the captured signature reports runtime HCIRVM0014 without provider fallback.
This is a bounded hosted diagnostic, not native header-mismatch ABI emulation.
Calls retain the existing instruction, frame, depth and storage limits.

## Pinned evidence and remaining boundary

The audited TempleOS revision is `c26482bb6ad3f80106d28504ec5db3c6a360732c`:

- [Compiler/PrsStmt.HC:62-137](../third_party/TempleOS/Compiler/PrsStmt.HC)
  selects reusable function records, initializes a new JIT record's `exe_addr`
  to `UndefinedExtern`, and replaces its header. Lines 151-190 compile and
  publish the body before clearing extern state.
- [Compiler/PrsExp.HC:545-582](../third_party/TempleOS/Compiler/PrsExp.HC)
  emits the argument protocol, JIT `IC_CALL_INDIRECT2` through `exe_addr`,
  AOT `IC_CALL_EXTERN` or `IC_CALL_IMPORT`, and cleanup.
- [Compiler/CExcept.HC:98-102](../third_party/TempleOS/Compiler/CExcept.HC)
  implements `UndefinedExtern` by reporting the call and throwing `UndefExt`.

The hosted implementation follows source-backed publication using checked
declarations and executable bodies. It does not read machine addresses or
implement native imports, mutable native extern slots, general runtime linking,
or full ABI/header-mismatch behavior. Native code generation, TempleOS BIN and
loader acceptance, and bootstrap remain unfinished. These claims use pinned
source audit and hosted regression tests; there is no new native capture.

[Completed function headers](completed-function-headers.md) can be typed from
their original source evidence before a body exists. Runtime admission of such
pending definitions preserves argument evaluation before a reached undefined
call. A directive reached while looking past the body's closing brace cannot
acquire that body's executable before publication. Nested replacements retain
separate selected headers and executable versions.

[Extern regression tests](../test/test_integer_extern_calls.ml) cover forward
and retained calls, publication order, saved defaults, variadic bodies,
provider replacement, failure recovery, initializers and resource bounds.
