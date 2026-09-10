# Function parameter delimiters

Function signatures accept comma and semicolon delimiters, including a trailing
delimiter. Empty semicolon entries can precede a parameter, follow a comma, or
appear before the closing parenthesis. They occupy no parameter or stack slot.

```c
I64 F(;;I64 n=40,;;I64 m=2,;;) { return n+m; };
F();
```

This source returns I64 42 in both hosted JIT and AOT modes. It has two fixed
parameters, two saved defaults and sixteen bytes of argument storage, just like
`I64 F(I64 n=40,I64 m=2)`. Run it from
`examples/integer-parameter-delimiters.hc`.
The CLI regression uses 22 runtime instructions in JIT and 20 in AOT, with six
preparation instructions in either mode. Exact limits pass; runtime or
preparation allowances one below those values fail at their respective limit.

The same list parser handles prototypes, definitions and recursive callback
signatures. Completed headers and resumed bodies preserve the original parameter
and delimiter nodes. AST dumps keep concrete parameter delimiters and the
positions of empty semicolon entries; generated delimiters retain their macro
invocation and definition origins. Empty entries do not alter default indexes,
variadic cleanup, frame allocation or execution budgets.

A delimiter remains required between concrete parameters and before ellipsis.
A comma cannot start a parameter list or follow another comma without a
parameter. Semicolons do not make a later bare comma valid. A dangling register
qualifier cannot terminate the list or attach to an empty semicolon entry.
These declaration delimiters are separate from
omitted arguments at call sites.

The source reference is TempleOS
`c26482bb6ad3f80106d28504ec5db3c6a360732c`.
[PrsVar.HC](../third_party/TempleOS/Compiler/PrsVar.HC), lines 423-447, skips
semicolons at the start of each list iteration and recognizes the closing
parenthesis before parsing another type. Lines 691-716 advance past a parameter's
comma or semicolon and return to that loop. Parameter allocation occurs in the
FUN_ARG branch at lines 618-657, not when consuming delimiters.

[Tests](../test/test_function_parameter_delimiters.ml) cover ordinary and nested
execution, source ownership, callbacks, macros, variadics, malformed lists and
exact/one-below resource limits. These are hosted results and pinned source
evidence; no native execution capture is claimed. General callback execution,
the full native parameter grammar and complete compiler remain unfinished.
