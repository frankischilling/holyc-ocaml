# U0 initializer warnings

JIT source tasks now report `HCSEMA0079: U0 Expression` when a reached initializer
expression has the original U0 result class. Both IR and native source execution
use the same preparation phase. A zero-sized destination alone is not enough to
produce this warning: the checked expression must actually return U0. Callback
values and function addresses keep their physical pointer class even when the
function signature returns U0; selecting those values does not warn.

The warning appears after the original leaf is typed and before its compiled
expression executes. It increments the active compiler control's warning count.
The existing global attempt or static preparation claim prevents replay from
emitting another warning. Closed and foreign parser controls cannot increment
the counter. Calling a function again does not repeat its static declaration's
warning or initializer effects.

The diagnostic retains the initializer expression's source span. Earlier
warnings and callee output survive a later execution failure. A failed leaf does
not receive a successful initializer completion just because its warning was
emitted.

Ordinary standalone U0 calls do not use this path. In the pinned source,
`PrsExpression` appends an `IC_END_EXP` with a null class when `end_exp` is true.
`LexExpression2Bin` instead calls it with `end_exp` false, so the original U0
class is still available to the warning check.

## Evidence and remaining work

The reference is `c26482bb6ad3f80106d28504ec5db3c6a360732c`:

- `Compiler/PrsExp.HC:263-301` checks the last expression class.
- `Compiler/PrsExp.HC:1117-1133` compiles initializer expressions without an end
  marker before adding their return instructions.
- `Compiler/PrsLib.HC:79-97` retains the supplied instruction class.
- `Compiler/PrsVar.HC:1-120` evaluates the initializer before copying its reached
  byte count; global and static traversal are at lines 209-246.
- `Compiler/PrsVar.HC:313-368` assigns callback storage its physical `RT_PTR`
  class separately from the declared return type.
- `Compiler/CExcept.HC:65-79` emits the warning and increments `warning_cnt`.

`test/oracle/void-initializers/warnings.*` retains two complete native JIT
observations. Standalone calls produce no warning; aggregate and static U0
leaves produce one each, and later calls leave the static count unchanged.
A second capture records six callback-word, function-address and integer-leaf
controls twice. Those values produce no U0 warning and do not call the selected
function. Live JIT function-address leaves now use the checked
[zero-byte reference initializer path](zero-byte-reference-initializers.md).
Tests compare supported warning counts and values through both executors,
check source spans, and preserve a warning before a later division failure.
The live-parser test also checks duplicate and expired preparation attempts.

The native fixture also covers plain scalar U0 globals. Their warning count
now has an execution comparison through the
[persistent U0 path](persistent-void-declarations.md). AOT and isolated native warning phases are not implemented by this
change. Original AOT load code has its own assignment-warning path, which must
not be replaced by replaying the JIT warning. Automatic U0 assignments and their
register aliases also remain separate. Issues #686 and #690 stay open.
