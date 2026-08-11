# Oracle fixtures

Every result on this page uses TempleOS commit `c26482bb6ad3f80106d28504ec5db3c6a360732c`.

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
