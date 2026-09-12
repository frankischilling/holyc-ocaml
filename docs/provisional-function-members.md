# Provisional function member source

Named headers now publish original member phases while parameter parsing is
still in progress. The task declaration ledger validates and retains those
phases before accepting a completed header. Retained JIT tasks admit those
original native phases for provisional calls. Separate argument and emission
phase binding remains unfinished.

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

The ledger also keeps the immutable native snapshot observed at each original
declaration event. During source activation, a snapshot read returns only that
event's retained phase. A later completed or reused header cannot change an
earlier replayed argument count or member list. Reads before activation starts,
at another declaration's event or after failed activation reject. Reading a
snapshot does not admit a callable or execute code.

Provisional call type projections carry a checked variadic cursor without
allocating body-local `argc` or `argv` symbols. Direct and implicit-output
argument binding and lowering, plus runtime call-shape checks, use the cursor's
count type. A flag alone
does not permit a variadic tail before its original members exist. Fixed
defaults retain their original parameter children, and omitted variadic values
remain errors. These consumers still require the separate call and runtime
authority described below.

Runtime admission checks the original live declaration callback or its exact
activation event, the task's namespace and command order, and the actual current
catalog head. Failed replay cannot reuse a still-live callback. A reused native
allocation cannot become a second semantic identity by omitting its predecessor
or using an ordinary completed-header declaration. Rejections preserve the
catalog. Header completion retains the original ordinary typed header for body
frames and a separate callable projection for native members.

A suspended header may finish after nested code creates a newer same-name
function. Its hidden native head advances separately from visible name lookup;
completion does not move it ahead of the newer function.

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

Direct calls have three distinct source phases: identifier selection before
name lookahead, argument-count/member capture after that lookahead, and emission
after closing-parenthesis lookahead. `PrsExp.HC:810,865,430-434,534-586` reads the
selected record at those boundaries. The parser and ledger retain original
call-start and emission receipts. Opaque captures read the native record during
those original callbacks, and runtime activation consumes the saved captures.
The VM freezes the exact admitted function reference at identifier selection;
copying its metadata does not create another valid reference. Semantic binding
uses the captured argument projection, while lowering and runtime verification
use the separate emission classification, return type and executable. Cleanup uses the current
emission argument count plus the captured variadic count and hidden count slot;
it cannot always be inferred from the number of values originally pushed.

Each call binding belongs to its exact task, namespace, source command and AST.
Failed replay or an exception revokes an uncommitted binding even after its
emission was captured; previously admitted command effects remain retained.
Rejected captures leave the original authentic capture usable. Defaults follow
the original native member and completed parameter, including members installed
by a nested header whose source symbol differs from the suspended publication.

Completed-header runtime admission uses the same journal boundary as provisional
phases. A header already recorded in the activation journal can publish only at
its own active event, even if its original parser callback remains live. Checks
before replay, at another event and after replay failure reject before catalog
publication. Headers observed after the journal was sealed retain the ordinary
live-callback path and its source-order checks.

The following source-derived execution gate passes in both outer modes:

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
and is a different requirement. Reached and eager calls now report UndefinedExtern
(`HCIRVM0030`). Fresh and reused provisional headers, calls during default
preparation, and the existing post-close executable-selection cases pass.

Argument-phase binding now accepts
`extern I64 F();if(0&&F#exe {extern I64 F(I64 n);}(40)){}`. The parser correctly
captures one argument after name lookahead, and semantic binding uses that
original cursor. Moving the directive inside the opening parenthesis preserves
the earlier zero count and rejects the surplus argument. Post-close extern
completion supplies the joined body; a previously resolved call keeps its
original executable when a fresh same-name definition appears. All ten runtime
gates pass in both outer modes, including replaced defaults and differing
argument/emission counts. General partial records and the complete
compiler/native/BIN/loader/bootstrap objective remain required under #635 and
the wider compiler milestones.

Run focused source tests with
`opam exec -- dune exec -j 1 test/test_main.exe -- test 'provisional function'`.
`dune runtest` also runs the private source-activation authority fixture.
These are hosted tests and pinned source evidence, not native execution captures.
