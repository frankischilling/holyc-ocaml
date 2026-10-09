# Live internal bindings

Ordinary JIT source tasks evaluate the original `_intern` target before reading
its type and header. Both the interpreter and `host-jit-task` accept integer
targets built from literals, parentheses, arithmetic, original task storage,
retained direct calls, checked function-pointer calls and supported internal
calls. Evaluation happens once, including when the declared function is unused.

```c
I64 Count=0;
I64 Target() { Count++; return 0x1e; }
_intern Target() I64 Convert(U8 ch);
Convert(97)+Count;
```

This returns 66. Changing the source cell used by the target later does not
change the saved operation. A later type or header error retains reached
effects, output and work. The original completed header installs that saved
operation, sets the internal flag and clears the extern flag. Historical calls
keep the header and operation selected at their own source occurrence.

`examples/native-internal-bindings.hc` prints `bind` once and returns 42. Its
target calls Print, increments a task global and selects StrLen. A later
function uses StrLen through an independently copied string-pointer default.

```text
holyc run --target=ir --mode=jit --format=json examples/native-internal-bindings.hc
holyc run --target=host-jit-task --mode=jit --format=json examples/native-internal-bindings.hc
```

The native adapter lowers the original typed target and executes its actual
machine entry in the shared task arena. It does not evaluate the target in the
interpreter. The native report labels this fragment `internal-binding`.
Supported calls use the same checked intrinsic instructions as closed programs:
ToBool, ToUpper, AbsI64, SignI64, integer squares and min/max, Bsf/Bsr, pointed
Bt/Bts/Btr/Btc, Swap and ModU64, and StrLen. Pointed operations retain the original
owned object, view, extent and byte-initialization checks.

Successful native target execution creates an opaque result in the C bridge.
That result retains the original program and arena across collection and image
release, together with the actual returned bits and execution work. Completing
the current source attempt requires that exact program and work, a live arena
and a single consumption. Equal source text, graph metadata, a foreign arena,
another domain or a replayed result cannot authorize the header.

Target evaluation consumes the shared execution and initializer allowances.
Its code and IR count toward cumulative compilation limits, and its calls share
storage, frame, depth, literal and output limits. Reached faults retain their
original effects and charges. Exact-limit and one-below tests exercise these
boundaries through the API and maintained CLI example.

This delivery covers guarded hosted integer bindings. Floating targets,
arbitrary host functions, executable addresses for installed intrinsic entries,
native stream services, native `#exe` and AOT source sessions remain open.
Internal globals still require their separate storage/address implementation.
The exported HolyC ABI, object/BIN output, actual TempleOS loader acceptance,
whole-tree compilation and bootstrap remain separate requirements. No new
TempleOS runtime capture is claimed.
