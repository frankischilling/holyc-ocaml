# Primitive type names and source visibility

Primitive names use the symbol selected when the parser reads their token.
A function, global, parameter or local can shadow a primitive type spelling.
For example, `I64 F64(){return 42;}F64();` calls the source function, and
`I64 F(){I64 U64=42;return (U64);}F();` returns the grouped local value.
An ordinary `U64++` statement also uses that local.

The parser recognizes a public primitive through its original public union
entry, or an internal primitive through its selected internal-type entry.
A newer source class with the same spelling keeps its own aggregate identity.
Unshadowed declarations and postfix casts retain their existing primitive form.
Replacement input uses the same selection rules.

Function locals remain visible during the lookahead after the body. In live
source execution, `I64 F(){I64 U64=42;return (U64);}U64 value;` therefore fails:
the first following `U64` token selected the local before function teardown.
An intervening semicolon lets the following token select the restored type.
Callback-free parsing retains its separate contract and does not have that live
resume observation. The low-level AST result does not certify source execution.

`examples/primitive-type-shadowing.hc` combines a function named `F64`, a local
named `U64`, a parameter named `U16`, and a global named `I8`. Both source modes
and both execution targets return I64 42 in 42 runtime instructions, with three
initializer instructions and no output. A runtime allowance of 41 or an
initializer allowance of two prevents completion.

The [oracle fixture](../test/oracle/primitive-type-shadowing.json) records eight
native value fields twice and two compilation controls twice. All twenty
accepted commands have source and result capture hashes. The native control
without a separator reports a compiler exception at the following variable;
the hosted live path reports `HCPARSE0001`. Those diagnostics use different
codes and position conventions. The separated native control compiles.

Five API/parser groups and the CLI tests cover all twelve primitive spellings
as function, global, local and parameter names, grouping, updates, replacement
input, restored types, newer aggregate identities and unshadowed casts. Native
CLI tests execute the emitted image on the active host. The wider spelling
matrix is hosted regression evidence; the fixture identifies the independently
observed TempleOS cases. Conditional comparison chains and pending comparison
reductions remain separate work under #593. F64 values, the full ABI, module
loading and bootstrap retain their existing implementation requirements.
