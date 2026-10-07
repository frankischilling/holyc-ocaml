# Native source tasks

`holyc run --target=host-jit-task --mode=jit` executes integer scalar and fixed
array initializer leaves during their original parser callbacks. Later source
commands use the same native storage. The scalar example is:

```c
I64 A=41;
I64 B=A+1;
B;
```

```text
opam exec -- dune exec --root . -- bin/holyc.exe run --target=host-jit-task --mode=jit --format=json examples/native-source-initializers.hc
```

The initializer for `A` writes its original native cell. At the original live
leaf for `B`, generated code reads that cell and stores 42 into `B`. The resumed
`B;` expression reads the same arena. Initializer side effects run once; later
declaration completion retains the original allocation and reference.

Fixed arrays retain their original checked dimensions and strides. Each leaf
writes its own element, so a later initializer can read an earlier element:

```c
I64 A[2]={41,1};
I64 B=A[0]+A[1];
B;
```

This is `examples/native-source-arrays.hc`. Its two array leaves, scalar leaf
and final command execute as four native fragments. Indexed reads, assignments
and numeric updates support signed and unsigned 8-, 16-, 32- and 64-bit storage,
including Bool's signed byte storage. Multidimensional indexing retains flat
offsets within the original object. An unwritten element remains uninitialized;
writing another element does not authorize its load.

Named direct calls use the exact function selected by the original source:

```c
I64 A=41;
I64 F(){return A+1;}
F();
```

This is `examples/native-source-functions.hc`. The declaration retains its
original checked body, frame and call context. A later call compiles that body
into the caller's native fragment and reads `A` from the same task arena.
Initializer calls such as `I64 B=F();` execute at that original live leaf.
The example returns 42 in three native fragments with nineteen runtime steps
and three separately counted preparation steps. Its eight-byte global and one
initialization flag occupy the same nine-byte arena throughout.
Supported integer and checked scalar-pointer arguments, integer word tails,
and integer or U0 results use the existing native calling path. Automatic local
storage and nested calls retain their original frame and argument owners.
Arguments retain their right-to-left evaluation order; modifying `argc` does
not change the original variadic extent.

Retained functions can call the checked Print and PutChars providers. The
maintained `examples/native-source-output.hc` prints `A42;` and returns 42.
Its format bytes come from an original U8 array in the task arena. The example
uses six native fragments, 62 runtime steps, thirteen preparation steps and
nine output-work units. Formats can also use checked automatic arrays or original
task string literals.

`examples/native-source-literals.hc` prints `42;` and returns 42 using a retained
`Print("%d;",42)` call. The original function declaration admits four literal
bytes, including the NUL, and 160 private reference-table bytes. Its later
caller uses those same 164 arena bytes. Each original producer owns a distinct
region, even when two literals have equal text. Repeated or recursive calls
reuse the producer's region and preserve mutations after later globals, arrays
or functions append storage. Literal admission validates the physical graph,
original runtime context and sealed instructions; a copied graph grants no
authority. Logical literal bytes and private metadata remain separately bounded.

The provider call must retain its original sealed occurrence, admitted function
context and extern link. A source function named Print or PutChars remains an
ordinary function. When its original extern slot acquires a joined source body,
the native call executes that body. Another function with the same spelling
does not replace the original slot or disable its provider.

Ordinary output survives later faults and source fragments. Print publishes a
complete draft, so a format, output-byte or work fault adds no partial bytes.
PutChars retains its reached prefix. Both consume the remaining task allowance,
including zero, and preserve argument order and initializer effects once.
Each caller still pays for its recompiled body and formatting code; repeated
Print calls across fragments can exhaust the cumulative code limit.

Self-recursion uses the original current body and physical frame. Its JIT
unresolved call form may bind only to that same body's checked definition and
declaration ancestry. Named JIT extern calls retain the original address-slot
identity and selected caller header:

```c
extern I64 Answer(I64 value=41);
I64 Old(){return Answer();}
I64 Answer(I64 value){return value+1;}
I64 Answer(I64 value){return 100;}
Old();
```

This is `examples/native-source-extern-slots.hc`. `Old()` uses its saved default
and the body joined to its original slot, returning 42 after the later separate
same-name definition. The source resolver requires the original task publication,
physical call occurrence, sealed owner context and joined declaration ancestry.
Its opaque proof belongs to the current source request and native publication
generation. Equal names, copied contexts, foreign tasks and stale proofs cannot
select a body.

An unresolved slot is legal in an uncalled function. If execution reaches that
call before installation, native instructions report `HCIRVM0030` after its
arguments have run. An incompatible joined signature likewise produces the
existing hosted `HCIRVM0014` guard at the call, preserving argument effects.
That signature guard is hosted policy: the pinned compiler warns about header
mismatches. Integer and U0 bodies, supported scalar pointers and integer word
tails retain the original caller's defaults, argument order and cleanup.

Unlike a callback call, a named extern slot does not capture its target before
arguments. The transient native image stages its selected body address at the
call instruction and makes the indirect call there. Source publication cannot
change during these supported ordinary native fragments; changes between parser
callbacks affect the next fragment. This path grants no persistent executable
address. Joined bodies are charged as part of the bounded caller closure, with
cycles and shared dependencies included once per image. Initializer and named
default fragments use the same source resolver.

Named and anonymous integer and one-star callback parameter defaults execute once at the original live header
callback. `examples/native-source-defaults.hc` saves 41 from `Seed()`, returns
42 from `Answer()`, changes the source counter and returns 42 again. Each
omitted argument loads the saved value from its original header. Explicit
arguments and unused functions still require the declaration-time default to
execute. Historical callers retain their selected original header and saved
value after same-name replacement.

The default expression has its own real native image, reported as `default` in
`native.fragments`. Its original parser receipt, typed root, namespace and
checked call graph remain joined through execution and header completion.
Every selected/source header and original call header in the retained closure
must use its exact published saved object and completed native execution.
Equal bits, copied saved objects and another source snapshot grant no authority.
Closed preparation and interpreter execution do not supply these native values.

Defaults can read and update admitted integer or callback storage and call retained
integer functions, including functions with earlier saved defaults. Actual expression
instructions consume both the shared native step allowance and the remaining
initializer allowance. Each successfully captured default retains eight bytes,
reported by `Native_source_execution.default_bytes` and JSON
`prepared_default_bytes`, under `max_default_bytes`. Code, output, frame and
call-depth limits keep their existing cumulative or per-activation meanings.
Reached faults preserve earlier native writes, output and work; later defaults
and body publication stop. Named scalar-pointer, string, F64 and
aggregate defaults and `lastclass` remain separate
work. The isolated `host-jit` target keeps its closed-default restriction.

The task publishes function source records when its original declaration
request claims admission. Rejection before that claim leaves the registry
unchanged. An admitted declaration keeps its source after a later reached fault. Each
retained function link identifies its original definition; a newer declaration
with the same name cannot retarget an earlier call. Historical function bodies
also retain their original global references after a global name is replaced.

Each compiled body uses its own original runtime-call context and source
storage. Retained callee lookup consumes the original emission link while
caller arguments retain their selected header. The backend validates those
records before collecting its direct callees. Every body included in a fragment
counts toward that fragment's IR, block and code limits. Recompiling a retained
body in another fragment charges
that new code again. The shared data arena preserves native writes across
those fragment lifetimes.

The collector checks IR and block limits before admitting each body to its work
queue. These source records contain no executable address. The host releases
each fragment's public entry after execution. The task arena keeps canonical
callback entries and the native mappings they need across source events.

One-star callback globals and fixed arrays now keep their numeric words in the
shared task arena. Each cell has eight data bytes, an independent initialization
flag and an eight-byte executable-owner lane. A scalar occupies seventeen arena
bytes; a four-element array occupies 96, including eight bytes per element for
initialization flags. Only the data extent counts against `max_global_bytes`.
All appended data, flags and owner lanes count against the arena bound.

Function-owned static callback cells and fixed arrays use the same native word
and owner lanes. Their live allocation retains the original partial-header
symbol, anonymous signature and checked dimensions. Function completion joins
that allocation to the exact original frame and location without allocating or
charging again. Return metadata stays separate from physical `RT_PTR` storage.
JIT elements remain unknown until their original initializer leaf or a reached
assignment initializes them.

`examples/native-source-static-callbacks.hc` saves a declaration-time default
once, copies the original executable owner between static array elements and
returns 42 after later counter writes and a same-name function replacement.
Calls retain the selected static cell's header and saved default across
activations. Numeric writes clear only the written element's owner. Static
callback initializers use the same original allocation and anonymous header.
Each scalar leaf executes during parsing, before the containing body is
installed. Copies preserve the original executable owner; numeric leaves have
no executable owner. A later array leaf can read or call an earlier initialized
element. Its callee retains the selected static header and saved default.

`examples/native-source-static-callback-initializers.hc` initializes three
elements with an owned address, a copy and an earlier-element call. It returns
42 after a counter write and replacement of the named target. IR consumes the
same original live JIT events in interpreter cells. Native tasks execute each
leaf in machine code and allocate no interpreter shadow values. Completed
functions join the original cells and successful leaf receipts without charging
or initializing them again. A self-address captured before body installation
keeps UndefinedExtern even after the body is installed.

`examples/native-source-callback-words.hc` copies an original array word into a
callback cell, updates numeric cells and forwards the saved word through a named
function's callback parameter. It returns 42 through nine actual native fragments.
Automatic cells, indexed arrays, fixed callback parameters and saved integer
defaults use the existing native word and owner lanes. Callback return metadata,
including F64 or pointer return metadata, does not change their eight-byte
`RT_PTR` storage. These numeric operations do not execute an F64 callback body.
Prefix and postfix increment/decrement move a one-star callback by eight bytes.
The full 64-bit word survives a bare expression or supported integer return.
An original callback update can also supply a later callback initializer leaf.

Declaration completion keeps the original storage object while completing its
anonymous header. Each task snapshot refreshes that header only when its source
object is physically the same. Earlier offsets and words remain in place;
an unrelated later declaration cannot retarget an older function's storage.
Original named callback parameters can save an integer expression's returned
word, including an effectful retained integer call, through the same original
native default receipt and eight-byte payload allowance.

Numeric callback words have zero executable owners. A reached indirect call
captures its original cell before reverse argument evaluation, preserves those
argument effects and reports `HCIRVM0024` from the native call site. Uncalled
numeric callbacks do not fault. Uninitialized cells and indexed bounds retain
their existing native checks.

Original JIT immediate `&Function` producers now create native executable owners.
`examples/native-source-callbacks.hc` saves an original body, copies its callback
through an array and calls it after an unrelated same-name definition. It returns
42. Globals, arrays, automatic cells and fixed callback parameters copy both the
native address and its private owner. Integer and U0 bodies, scalar pointer
parameters and supported integer word tails use the existing private call ABI.
Global, static and named parameter-default consumers can call these owners.

Each owner reserves sixteen private arena bytes: one canonical entry address and
one current body target. Its eight-byte native leaf entry jumps through the target
cell without changing the stack or arguments. The first entry mapping stays live
until arena release. Later fragments compile the same original body and its
closure, then bind that leaf to the current native body after the live source
request is claimed. The captured callback address is called directly; faults and
quotas belong to the current body's original source sites. The leaf bytes charge
the cumulative code allowance and carry separate Windows unwind ranges. Source
function counts exclude those private leaves. Retained code remains within the
host mapping bound; canonical and current mappings stay rooted and leased during
entry. Public fragment release revokes that entry while its owned leaf remains
mapped. Arena release closes the remaining mappings.

A dynamic owner survives loads, copies, captures and full-word views. A bare owned
expression explicitly clears an earlier captured result. Its negative private
site marker identifies the reached original discard and carries zero bits; an
unexecuted branch or an implicit declaration leaves the earlier result intact.
An owned word returned or passed as an ordinary integer faults at the reached
native site; numeric callback words still
preserve all sixty-four bits. Owned updates and comparisons with a nonzero numeric
word retain their existing native faults. This implements a private hosted entry,
not the exported HolyC ABI.

`examples/native-source-owned-defaults.hc` saves the original selected callback
for a named parameter, replaces the global cell and still returns 42. Immediate
`&Function` values, bare callback cells, fixed-array elements and assignment
effects retain the owner selected during the original header's execution.
Omitted arguments materialize that saved owner; explicit arguments and unused
functions still execute the default once. Historical callers preserve their
original saved header after same-name replacement. Owned default captures leave
an earlier command result intact.

The native default image compares the live callback PC with its owner's canonical
entry before returning checked source metadata. The saved eight-byte payload
retains the original expression and function identity. It grants no entry
permission; later call images require the original completed native default and
the retained body, then materialize the owner's canonical entry. Private owner
cells charge the arena allowance rather than logical global storage.

Original JIT extern-slot addresses retain their complete `IMM_I64` and `DEREF`
pair. The first instruction addresses a private logical function slot; the second
reads its current native entry and captures its owner. Self-addresses inside a
body can call that body, including recursion. A retained function that selects an
extern can capture the original joined body after installation. A later unrelated
same-name definition has a different slot and cannot retarget that capture.

`examples/native-source-slot-addresses.hc` returns 42. `Read` selects the first
Answer record and reads its installed body when called. `Same` saves the shared
UndefinedExtern entry during its original header, before Answer has a body. Its
later comparison returns zero because the saved entry differs from the installed
Answer entry. Copies, fixed-array elements and saved defaults retain the captured
owner. Installing a body changes subsequent slot reads; it does not rewrite an
earlier capture. Calling an unresolved capture reports the reached UndefinedExtern
fault after its arguments finish, even if that body has since been installed.

Each logical slot appends sixteen private arena bytes for its PC and owner. The
task also keeps one shared unresolved entry, with its canonical and current target
cells, a real framed native fault stub and a separate unwind range. These private
entries consume code, arena and physical stack allowances, without increasing the
source function count. The bridge checks every slot range and owner before
publishing its cells. Original graph receipts, task ownership and the current body
generation still govern source selection; names and numeric bits grant no entry.
The IR consumer preserves the same declaration-time capture and source-position
rules. Hosted output-provider callback addresses still require their own checked
entries and receive a diagnostic rather than an unresolved capture.

Live anonymous expression defaults and nested saved callback owners use the
same native completion path. See [anonymous defaults](native-anonymous-defaults.md)
and `examples/native-source-anonymous-defaults.hc`. Callback calls inside defaults
retain their original typed sources; integer static initializers can consume
these saved arguments. Static callback allocations in the streaming native task
still require their separate connection. Direct automatic
callback initializers remain rejected at the pinned `Grid.HC` restriction.

Numeric callback words can supply all ten compound update operators for integer
locals, globals, indexed elements and references. Narrow and unsigned destinations
keep their original conversions. Callback return metadata does not change the
stored word. The original callback word view must be retained through lowering;
an executable owner still faults at the reached update after the original left
initialization and bounds checks. Code, IR, runtime, frame and call-depth limits
include these guards. `examples/native-source-callback-updates.hc` exercises all
four scalar destinations and returns 42 in IR and native source-task execution.

F64 and aggregate callback execution, member cells and the exported ABI remain
outside this supported native slice. Static callback leaves and joined extern
calls are tested natively; the isolated IR path still rejects some of these
source-session consumers. No new TempleOS native oracle capture is claimed.

`Native_source_execution.evaluate` accepts the original session, preprocessor
configuration and source. An internal synchronous dispatch connects source
admission to the hosted native executor. The public task API preserves its
existing interpreter interface; the native source API owns dispatch and entry.
The driver retains source and declaration metadata without allocating
interpreter cells.
Closed expression preparation has its own counter. JIT dimensions share that
cumulative initializer allowance; `dimension_work` also reports their reached
node visits. The separate dimension limit applies to ordinary source compilation.
Native steps come from actual native outcomes. Reports retain detached image
metadata and each checked
completion or fault. Executable owners keep their required mappings until the
source arena closes; reported fragments carry metadata rather than live entries. The JSON `native.fragments` array records their source order, cumulative
native work, storage sizes and compiled function counts; `native.image` is null because
the source task has separate fragments.

Each layout has one opaque native arena. Its data and initialization flags
occupy stable offsets as later globals append. Admission passes a checked
extent and new original literal payloads to the host, which zeros only the new
suffix and copies those payloads once. Compilation, retention and
report metadata do not build a seed copy of the whole arena. Old native writes
remain in that arena without a second interpreter storage copy. Retention
checks the original live request, layout, ABI and limits before admission.
Execution claims that request once, and return or failure closes it. Source
ownership and the originating domain are checked before the arena binds to its
cumulative budget, so a rejected first call cannot claim the allowance.

Array elements use the existing eight-byte spacing between initialization flags.
The flag base points to the last flag, and indexed access subtracts the scaled
element offset. The array example occupies sixteen data bytes and sixteen flag
bytes before `B` appends eight data bytes and one scalar flag. Logical storage
is 24 bytes; the arena extent is 41 bytes. A two-element U8 array needs two
logical bytes and sixteen flag bytes. Arena reservation accounts for that ratio
and remains capped by the hard native arena limit. Logical byte limits and
private metadata limits are checked separately before admission. Reservation
also includes the worst-case literal byte and reference-table ratio, within
the same hard arena cap. Retained task metadata stores original payload chunks,
not a duplicate initialized arena image.

Code and arena leases cover source admission, entry and release. A concurrent
release rejects before changing an active owner. Failed OS cleanup revokes
further activation while preserving the handle needed to retry cleanup.

Code bytes and IR instructions are bounded across all emitted fragments.
Runtime steps, output bytes and output work share the task allowance, including
zero remaining capacity. Reached native faults preserve prior writes and work;
later source does not execute. Uninitialized scalar and element reads retain
their native fault. Declared integer widths govern storage, while full I64/U64 words govern
expression results. The C bridge owns mappings, protection, entry and release;
source admission, lowering, instruction selection and report validation remain
in OCaml.

This source-task path supports integer globals, fixed integer arrays,
integer and one-star callback function statics, retained direct and joined JIT
extern calls, their original literals, numeric and owned callback storage, and
named or anonymous integer and one-star callback defaults. Runtime-dependent
dimensions, wider callback defaults,
hosted-provider callback entries, native `#exe` and AOT source-task execution
remain required work under #704. Full callback domains and the exported ABI remain
required under #801 and the broader compiler acceptance scope.
The compiler retains each successful live JIT static allocation, including
its original function publication, table, namespace and checked dimensions.
The sealed command keeps these witnesses in source order. Compilation joins
them to the declaring frame and exact checked local location; foreign owners
and substituted dimensions fail that join. These records carry no prepared
values or native execution permission. Native task collection inserts each
static's original symbol in the partial function scope during allocation and
reuses that symbol at completion. Its private task record retains the checked
integer or callback shape without publishing a global name. The task charges the padded
extent at declaration; the completed frame joins the same storage owner without
charging it again. Identifier receipts also retain the exact local publication
selected when their token was produced.
Each private reference also retains that exact identifier AST occurrence.
Another token selecting the same allocation cannot replace it in an initializer.

Each allocation appends zeroed data and initialization flags to the original
native task arena. Admission preserves earlier native writes and charges the
padded extent once. Each scalar initializer leaf compiles and executes through
its original live parser request, including direct calls, provider effects,
global references and references to earlier private statics in the same
function. Successful leaf receipts join the exact completed typed roots and
array destinations. Later calls and historical direct-function closures use
that same private allocation without replaying its initialization. These
storage records carry no arena address, interpreter cells or initializer values.

The literal-string branch copies the original fixed byte count directly into
the admitted private allocation. `Compiler/PrsVar.HC:123-145` calls MemCpy
during parsing; `Kernel/KUtils.HC:54-68` implements its byte copy. The hosted
consumer checks the original live leaf, task, allocation, stream offset and
accessible extent, then claims its request and charges the copied bytes before
writing. The native host validates all destination flags before the first
write. A copy may overwrite an element written by an earlier initializer
expression; initialization flags do not substitute for its single-use receipt.
Nested rows, mixed scalar/copy leaves, truncation and an included
terminating zero keep their original stream order. A count beyond the literal
plus terminator remains a hosted rejection. Copy observations use detached
payloads, so mutating an observation cannot replace the retained source bytes.
`Native_source_execution.static_copies` and JSON `native.static_copies` report
these direct writes separately from expression-code fragments. They consume
initializer work without fabricated expression IR or runtime instruction steps.

`examples/native-source-statics.hc` returns 43 and captures `I` once. The native
tests cover narrow stores, fixed arrays, independent function owners, historical
calls, cumulative limits and reached initialization faults. Released arenas,
foreign sources and domains, and repeated or expired requests are rejected.
`examples/native-source-static-copies.hc` returns 69 after a copied byte array
is mutated across two calls and a later allocation.
Automatic or parameter references in static initializers,
partial fixed-array initialization, ordinary pointer/F64/aggregate statics and dynamic
dimensions still return diagnostics. The runtime checks bounds against the
declared accessible extent, excluding static padding.
StreamPrint and StreamExePrint keep their separate generated-source and
outer-context authority requirements.
The existing `host-jit` target keeps
its isolated compilation and AOT load-region contracts. This path adds no
exported HolyC ABI, object or BIN loader, bootstrap, whole-tree compilation or
full-compiler completion claim.

The pinned source is `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
`Compiler/PrsVar.HC:53-107` compiles and calls an initializer expression before
writing its declared-width destination. `PrsVar.HC:123-212` traverses array
dimensions and initializes each fixed-count leaf in order.
`Compiler/PrsExp.HC:1068-1100` scales each subscript by its original remaining
dimension stride and element width. `Compiler/CMain.HC:1-32` compiles the
original statement and returns its final expression. `PrsStmt.HC:62-137`
distinguishes original function declarations and reused extern records;
`PrsStmt.HC:140-207` compiles each original body and installs its executable.
`PrsExp.HC:544-586` retains the argument order, selects the unresolved JIT
address-slot call while `Cf_EXTERN` is set, and applies the selected cleanup.
`PrsStmt.HC:181-191` installs the JIT body before clearing that flag.
The hosted direct-call path retains that source identity while compiling code
for each caller fragment. The arena bounds and
ownership checks are hosted policy. These source audits and host tests add no
new TempleOS oracle capture.
`Kernel/KeyDev.HC:20-27` consumes a packed PutChars word byte by byte.
`Kernel/StrPrint.HC:890-896` builds the complete Print buffer before publishing
it. The checked hosted providers reuse those existing source-backed contracts;
this source-task connection adds no native oracle observation.
`Compiler/PrsExp.HC:692-697` creates the original string object and internal U8
pointer producer. `OptPass789A.HC:296-304` addresses that object's generated
storage. The shared task arena preserves the original mutable bytes under the
hosted ownership and lifetime rules described above.

`Compiler/PrsVar.HC:629-656` compiles and calls the original default expression,
keeps its full returned word in the parameter member, and marks the default
available. `Compiler/PrsExp.HC:455-469` uses that saved word when an argument is
omitted. The native task connection preserves this timing and original header;
its private capability checks and quotas are hosted policy. It adds no new
TempleOS runtime capture.

`OptPass789A.HC:345-357` emits the named address-slot call after argument
instructions; `PrsExp.HC:553-571` distinguishes it from the callback capture
before arguments. `CExcept.HC:98-102` supplies the reached UndefinedExtern
placeholder. The native source path preserves the corresponding call timing
and reports its fault through the existing hosted diagnostic contract.

`PrsExp.HC:621-654` lowers `&Function` through the mutable `exe_addr` slot
while `Cf_EXTERN` is set. `PrsStmt.HC:95-114` installs UndefinedExtern before
parsing a new JIT header, and `PrsStmt.HC:181-191` installs the compiled body.
The slot-address consumer keeps those original producers and capture timing.
Its private ownership cells, host fault stub and resource guards are hosted
policy, with no new TempleOS oracle capture.

Named classes in callback return and parameter metadata retain the class selected
at the original type token. Global, local, static and nested anonymous headers
carry that selection into the sealed source command. A later class with the same
name cannot replace it; completing its original forward declaration keeps the
same canonical class. Comma declarators share their original base type while
retaining each declarator's own return-pointer children.

The callback cell still has physical RT_PTR storage. `Pair (*p)()` can hold a
complete numeric word without allocating a Pair object. Automatic frame positions,
static initializer destinations and `sizeof(p)` use the eight-byte cell, while
the anonymous header retains Pair as return metadata. A callback parameter such
as `Pair (*word)()` can save and forward a numeric callback word through an
integer-returning function. `examples/native-source-named-callback-types.hc`
returns 42 after a later class shadow and counter write; its original header
effect runs once and its static cells occupy 24 logical bytes alongside the
eight-byte counter. Aggregate-return invocation, ordinary aggregate pointer
execution, callback members and the exported ABI remain separate work.

[Numeric callback expressions](callback-expressions.md) consume original reads
and assignment results through signed RT_PTR word views. Parser pointer scaling
and difference division remain separate from numeric computation classes.
Dynamic executable owners are checked at reached integer consumers; callback
copies and calls keep their existing owner path.
