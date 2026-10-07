# Saved data-pointer defaults

JIT source tasks evaluate one-level primitive data-pointer defaults once at the
original named or anonymous header. Omitted arguments reuse that evaluated
object, byte extent, offset and primitive view. An explicit argument or an
unused function still requires its declared default to complete.

The interpreter and `host-jit-task` support Bool and signed/unsigned 8, 16, 32
and 64-bit views. A default such as `I64 *p=&Values[++Counter]` captures its
original element once. Later writes to that object remain visible through the
saved alias. Parameter rebinding changes the invocation's pointer, while the
saved default keeps its original value. Recursive calls, historical functions
and anonymous callback signatures retain their own selected defaults.

`Compiler/PrsVar.HC:629-656` evaluates the original expression, then calls
`StrNew` when that compilation set `CCF_HAS_MISC_DATA`. The hosted path follows
that distinction. A default containing a string producer copies the resulting
NUL-terminated byte sequence into fresh mutable task storage. This also applies
when the resulting pointer addresses another object. A non-string default keeps
an alias to its original object. An embedded NUL ends the copy, and an offset
into a string copies only the remaining terminated suffix.

Copied bytes consume the cumulative literal allowance, including the terminator.
Each attempted scan byte consumes initializer work. Missing terminators,
uninitialized bytes and exhausted limits preserve earlier expression effects
and attempted work. Address formation does not read an unknown pointee; later
access still checks the original object and complete view window. Padding and
bytes after the copied terminator grant no additional access.

Native default fragments save a private four-word descriptor in the task arena.
The descriptor keeps the original data base, initialization flags, offset and
extent. Its 32 bytes count toward arena metadata. The saved parameter value
still occupies eight logical `default_bytes`, and copied string bytes count
separately toward literal storage. Existing objects and descriptors retain their
offsets when later declarations append storage.

An opaque saved value carries source and view identity without exposing a host
address. Native completion also requires the actual successful capture from
that exact image and arena. Equal metadata, a foreign arena or a repeated
completion cannot supply it. Later calls require the original published saved
default and its successful source-header completion.

```text
holyc run --target=ir examples/data-pointer-defaults.hc
holyc run --target=host-jit-task examples/data-pointer-defaults.hc
```

The example increments its counter once while capturing the second array
element, mutates that element through two omitted calls, prints `AB` from its
copied default and returns 42.

AOT reference defaults still require output relocation and callable authority.
Closed `host-jit` images retain their separate default admission. Raw/null
address conversions, persistent pointer variables, pointer returns and escapes,
source variadic pointer ownership, deeper/F64/aggregate pointers, live native
internal bindings and the exported HolyC ABI remain compiler work. These hosted
tests add no TempleOS runtime capture, loader or bootstrap acceptance.
