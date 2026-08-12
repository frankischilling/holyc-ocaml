# Oracle fixtures

Every result on this page uses TempleOS commit `c26482bb6ad3f80106d28504ec5db3c6a360732c`.

## Local declarations

[`test/oracle/local-declarations.json`](../test/oracle/local-declarations.json) records automatic, static, variadic, nested, pointer, array, register-qualified, and comma-following locals compiled by the native TempleOS compiler. The first two calls to one static-local function returned 42 and 43, confirming retained storage. A local name was visible in its own initializer, and a name declared inside a nested block remained visible later in the function.

The fixture also captures a source-specific statement rule. After `PrsVarLst` returns, `PrsStmt` continues unless the next token is `}`. An unbraced false `if` containing a local declaration therefore consumed the following assignment and returned 0. The braced comparison returned 7. The same rule causes a primitive local declaration in a `for` initializer to fail with `Missing )`. Register-qualified static locals, a qualifier before the local type, and an unbraced `try` local were also rejected. The hosted parser matches these accepted and rejected shapes, but does not claim identical diagnostic wording.

The ten labeled native outputs were:

```text
ORACLE_AUTO=9
ORACLE_STATIC1=42
ORACLE_STATIC2=43
ORACLE_VAR=7
ORACLE_NESTED=9
ORACLE_SELF=1
ORACLE_COMMA=3
ORACLE_IF_LOCAL=0
ORACLE_IF_BLOCK=7
ORACLE_LOCK=8
```

## Function definitions

The native compiler accepted the function-definition fixture recorded in [`test/oracle/function-definitions.json`](../test/oracle/function-definitions.json). The source was passed to `ExePrint` as one buffer so the final signature reached the compiler at EOF and the `#ifdef` directive ran while the preceding function still awaited its body.

The run covered an explicit semicolon body, a compound body, an unbraced body, recursive publication with a default parameter, preprocessor visibility before the first body token, and a signature with no body token at EOF. The observed results were:

```text
ORACLE_EMPTY=1
ORACLE_BLOCK=11
ORACLE_BARE=12
ORACLE_RECURSIVE=13
ORACLE_VISIBLE=1
ORACLE_ABSENT=1
```

`ORACLE_EMPTY=1` was printed only after calling the semicolon-bodied function. `ORACLE_VISIBLE=1` proves that the true `#ifdef OracleVisible` branch supplied the function body; its false branch contains an invalid declaration and would have stopped compilation. `ORACLE_ABSENT=1` confirms that the EOF signature entered the current task's function table.

The run used the same verified ISO and isolated QEMU configuration described below. QEMU ran without a display, networking, or persistent disk. Input arrived through QEMU's loopback-only machine-control channel. A preflight command checked the key mapping, and the accepted source was captured before the result commands ran. The fixture records both capture hashes. A discarded attempt with overlapping modifier events is not compatibility evidence.

## Parenthesis-free calls

[`test/oracle/parenthesis-free-calls.json`](../test/oracle/parenthesis-free-calls.json) records seven direct-call checks from the native TempleOS compiler. The tested functions cover zero fixed parameters, two defaulted parameters, a required parameter between defaults, two required parameters, and a variadic signature. The observed results were:

```text
OracleZero;          => 10
OracleDefaults;      => 46
OracleMixed 7;       => 173
OracleRequired 2 3;  => 23
OracleVar;           => 0
OracleZero+2;        => 12
&OracleZero!=0;      => 1
```

These results confirm that a direct function name starts a call without parentheses, defaults occupy their declared positions, required parameters consume adjacent expressions, and a parenthesis-free variadic call has no extra arguments. The binary check places the call before `+2`; the address check proves that `&Function` takes the function address instead of calling it.

The run used the verified ISO, read-only boot media, and isolated QEMU configuration described below. The preflight expression returned 5 before the fixture ran. The fixture records SHA-256 hashes for the accepted-source and observed-result captures; neither screenshot nor the ISO is committed. This evidence covers parser-visible call shape only. It does not establish the OCaml compiler's future default evaluation, type conversion, lowering, or execution behavior.

## Lexical frame boundaries

The first native compiler oracle run covers the point where an include or definition buffer ends while `Lex` is still scanning one lexical item. The machine-readable record is [`test/oracle/frame-boundaries.json`](../test/oracle/frame-boundaries.json).

The run used the `final` release from the canonical TempleOS repository. That tag resolves to the pinned commit. Before boot, the downloaded `TOS_Distro.ISO` matched the SHA-1 published in the release notes:

```text
SHA-1   1a1ec79990e21fa3d66ac680009da63d3ac512b0
SHA-256 3c68ca395d0b64f779f1ffa330e2685798b17862e3c36ae4e349537fd676b40b
```

The ISO ran read-only under QEMU 10.2.50 with one virtual CPU, 512 MiB of memory, no persistent disk, and networking disabled. Temporary include files were written only to TempleOS's `B:/Tmp` RAM drive. The ISO and screen captures were kept outside the repository.

The observed results were:

```text
FRAME_GE=1
FRAME_NUM=1
FRAME_STRING=left
FRAME_CHAR=65
FRAME_BLOCK=1
FRAME_LINE=1
FRAME_NESTED=1
FRAME_INCLUDE=7
FRAME_CONT=1
GE_KIND=1
GE_TAIL=1:tail
```

The direct lexer case checks more than expression behavior. `Lex` returned `TK_GREATER_EQU` for the `>` byte from a definition followed by the caller's `=` byte. Its next call returned `TK_IDENT` with `cur_str` equal to `tail`. This records the token kind and proves that the caller cursor resumed once, after the fused operator.

The OCaml tests split every multi-byte operator at every possible definition-frame boundary. Separate cases cover numbers, strings, character constants, block and line comments, line-continuation trivia, three nested include frames, an exhausted intermediate definition frame, ordered source segments, and a diagnostic that begins in one source and ends in another. The raw `holyc lex` path still uses a single frame and keeps its previous output.

This fixture validates the native lexer behavior needed by issue #24. It is not a TempleOS loader oracle and says nothing about `.BIN` acceptance. Loader fixtures will use a separate controlled procedure once the module writer exists.

## Reproduction

Download `TOS_Distro.ISO` only from the canonical `final` release and reject it unless the SHA-1 above matches. A suitable isolated boot command is:

```text
qemu-system-x86_64 -m 512 -smp 1 -accel tcg -cdrom TOS_Distro.ISO -boot d -no-reboot -nic none
```

At the TempleOS prompt, run `AutoComplete(OFF);`, then enter the commands listed for each case in the JSON record. Ignore the shell's timing lines and compare the labeled output exactly. The direct lexer case must print both `GE_KIND=1` and `GE_TAIL=1:tail`.

For the function-definition fixture, run its `compile_command` as one command, then run each entry under `checks` in order. The six labeled result lines must match the recorded output exactly. For the local-declaration fixture, run both `compile_commands`, then the accepted and rejected checks in their recorded order.
