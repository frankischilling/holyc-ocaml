# Variadic header termination

A variadic signature can end at `...` without a closing parenthesis. The next
token belongs to the declaration or statement that contains the signature.

```c
#exe {
  I64 F(... { return argc; }
  StreamPrint("%d;",F(1)+41);
}
```

This directive injects `42;` in both outer modes. The directive task executes
as JIT even when the enclosing compilation is AOT. Ordinary definitions,
prototypes and recursive callback signatures use the same termination rule.
Nonvariadic signatures still require their closing parenthesis.

The AST stores the closing location as an option. Present parentheses retain
their original locations; absent parentheses have no invented source location.
JSON dumps use `null` for an absent close and human dumps say `absent`.
Existing dumps with present parentheses keep their shape. Semantic signatures
retain this distinction, including callback signatures in parameters, globals,
locals and members.

Completed headers preserve the exact original closing child or its absence.
Adding, removing or reconstructing that child cannot authorize a retained
header. Wrapping the same original child in a new option preserves its identity.
Parameter children, saved defaults, variadic argument counts, frame allocation
and nested executable selection keep their existing contracts.

The maintained [example](../examples/integer-variadic-termination.hc) calls an
unclosed variadic definition through a saved caller. It returns I64 42 with no
ordinary captured output. The [execution tests](../test/test_variadic_header_termination.ml)
compare its resource use with an otherwise identical closed signature and cover
exact and one-below runtime, preparation, frame and call-depth allowances.
Its CLI reports use 58 runtime instructions in JIT and 60 in AOT, with three
preparation instructions in either mode. The two active calls need 32 frame
bytes and call depth two.
Parser and semantic tests also cover prototype and recursive callback contexts,
macro provenance, malformed caller syntax and original-child substitutions.

The source reference is TempleOS
`c26482bb6ad3f80106d28504ec5db3c6a360732c`.
[PrsVar.HC](../third_party/TempleOS/Compiler/PrsVar.HC):373-405 creates `argc`
and `argv`, then consumes `)` only if it is present. `PrsVarLst` returns after
this branch. `PrsFunJoin` does not add a closing-token check; `PrsFun` passes the
remaining token to `PrsStmt` for a definition body. The callback branch at
PrsVar.HC:350-356 uses that same signature parser.

Lookahead retains native local visibility. PrsStmt.HC:929 reads beyond the
body's `}` before PrsFun clears locals at line 206. An immediately following
`#ifdef argc` therefore sees the local variadic parameter and is false, even
when a global `argc` exists. A later conditional after the next ordinary token
sees the restored global. The parser regression checks both boundaries.

These results combine hosted execution and pinned source evidence. They do not
claim a native execution capture. General callback execution, provisional
records, full defaults and conversions, native backends, BIN/loader acceptance
and bootstrap remain required.
