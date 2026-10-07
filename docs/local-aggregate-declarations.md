# Classes and unions in statements

A class or union declared inside a function publishes its type in the global
namespace when the parser reaches it. Calling the function is unnecessary:

```c
U0 Publish() { class Local { U8 bytes[42]; }; }
sizeof(Local); // 42
```

The parser accepts definitions, extern forwards, modifiers, member metadata,
anonymous unions, and inherited class headers in statement bodies. Commas can
separate declarations inside a function. A bare definition ends at its semicolon
or comma; it does not consume an attached local variable. A backed inline type
uses the local declaration path, as in `I64 class C { I64 value; } local;`.
Parsing and semantic analysis retain that type even where executable aggregate
object storage remains unsupported.

All semantic passes share one lexical declaration order, including types inside
function and statement bodies. Function bodies and later globals therefore keep
their correct indices. Saved queries and defaults retain the type selected at
their original source occurrence, including after another declaration replaces
the same name. An aggregate statement carries the original completed parser
item; rebuilding a module or replaying its completion callback grants no source
authority.

AST consumers should use `Ast.declaration_items` to look up these shared indices.
The forward declaration's `semicolon` is optional because a comma belongs to
the enclosing statement sequence. Semicolon-terminated forwards retain their
existing dump representation; the new comma form reports a null semicolon.

The maintained example returns and prints 42 with IR in either mode and with
native JIT:

```sh
holyc run --mode=jit --target=host-jit-task --code-byte-limit=524288 examples/local-aggregate-declarations.hc
holyc run --mode=aot --target=ir examples/local-aggregate-declarations.hc
```

JIT bounds and `$$` offsets inside these declarations use the existing original
source execution path. Their calls, output, globals, preparation work and native
instruction charges occur once during parsing, including inside an uncalled
function. A malformed following declaration retains effects already reached.
Native tests require actual completed fragments and zero interpreted
instructions.

The pinned `Compiler/PrsStmt.HC:1-60,1143-1159,1208-1218` provides global class
publication and function statement delimiters. `Compiler/PrsVar.HC:286-368`
provides backed inline types through the local declaration path.

[Inherited size metadata](source-inherited-layouts.md) retains the original
selected base and its reached layout. Executable aggregate objects still need
their storage and member-index support. Runtime AOT dimensions and offsets retain
their relocation and callable-authority limits; native AOT source execution and
synchronous native StreamExePrint also remain open. Function `$$` addresses keep
their code-address requirements. These parser and layout changes do not complete
those execution paths or the full compiler.
