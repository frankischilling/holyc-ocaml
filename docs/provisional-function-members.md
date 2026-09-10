# Provisional function member source

Named headers now publish original member phases while parameter parsing is
still in progress. The task declaration ledger validates and retains those
phases before accepting a completed header. Runtime admission of provisional
calls remains unfinished.

For `I64 F(I64 n=#exe {}40)#exe {}{return n;}`, the first directive sees n's
original type and name before its default has been parsed. The second sees the
original default and completed parameter, while the function header remains
unfinished. Earlier semantic snapshots keep their original phase after parsing
continues.

| Event | Source evidence available |
| --- | --- |
| Function declaration | Original publication and previous lookup |
| Parameter declaration | Index, predecessor, type, name, registers, pointers and callback |
| Default completion | Original parsed default; no claim of successful evaluation |
| Parameter completion | Original final parameter AST |
| Ellipsis start | Original marker before the following lexer read |
| Ellipsis completion | Source phase after synthetic argc/argv creation |
| Header completion | Exact ordered parameter completions and ellipsis receipt |

Recursive callback children stay attached to their original outer member.
They do not publish members into the enclosing named function's transcript.
Empty parameter entries do not create members. The parser preserves existing
lookahead, delimiters and optional variadic closing punctuation.

The semantic record checks namespace, table, publication, callback lifetime,
phase order, predecessors and original children before changing its snapshot.
Rejected, repeated, foreign and expired events leave it unchanged. An original
JIT source activation can replay the phases once. Legacy delayed ledger metadata
replay creates no live provisional source authority.

## Native evidence and remaining runtime work

Reference: TempleOS `c26482bb6ad3f80106d28504ec5db3c6a360732c`.

- PrsVar.HC:519–530 inserts a concrete member after PrsType returns, before
  default parsing/evaluation at 618–657. Type lookahead therefore precedes member
  publication. Parameter completion follows valid delimiter recognition.
- PrsDotDotDot at PrsVar.HC:373–405 sets the function flag before its lexer read,
  then adds synthetic members. Those members do not increment member_cnt.
- PrsFunJoin at PrsStmt.HC:90–115 saves the old argument count for comparison,
  clears reused members and finally sets arg_cnt from the current member_cnt.
  LexLib.HC:209–218's ClassMemberLstDel resets both counts to zero. Reused
  provisional headers do not retain the previous active argument count.
- PrsFunCall at PrsExp.HC:430–491 consumes arg_cnt fixed members before checking
  whether the resulting member cursor denotes a variadic tail. The function's
  ellipsis flag alone does not establish that call shape.

Native shared record state must remain separate from this immutable source
transcript. Nested reuse can reset or complete the same native record while an
outer header is suspended. Runtime integration must preserve active counts,
partial members, successfully saved defaults, flags, return metadata and
executable lineage without creating a fabricated completed source signature.

The following source-derived execution gate is still open in both outer modes:

```c
#exe {
  I64 F(I64 n=40)
  #exe {if(0&&F()) {}}
  {return n;}
  StreamPrint("42;");
}
```

Its expected outer result is I64 42 with empty ordinary output. The conditional
consumer skips the call; an ordinary Boolean value expression `0&&F();` is eager
and is a different requirement. The source transcript does not by itself admit
either form. Reached UndefinedExtern behavior, general partial records and the
complete compiler/native/BIN/loader/bootstrap objective remain required.

Run focused source tests with
`opam exec -- dune exec -j 1 test/test_main.exe -- test 'provisional function'`.
`dune runtest` also runs the private source-activation authority fixture.
These are hosted tests and pinned source evidence, not native execution captures.
