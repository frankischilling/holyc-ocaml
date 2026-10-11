# TempleOS disassembly

`holyc disasm` inspects a binary stream using the pinned TempleOS `Ui` dialect.
It reports instruction addresses, exact bytes, selected opcode records,
operands and relative targets. The public library exposes the same decoder as
`Holyc_lib.Asm_disassembler`. This command inspects bytes and does not execute
them.

```sh
dune exec holyc -- asm --emit --position=4096 --output=code.bin examples/disassembly.asm
dune exec holyc -- disasm --position=4096 --symbol=entry=4096 code.bin
dune exec holyc -- disasm --position=4096 --format=json --listing=code.lst --map=code.map code.bin
```

`--bits=16`, `32` or `64` selects one mode for the complete stream. The default
is 64. A binary stream has no record of an assembler's `USE` directives; select
each region with `--offset` and `--length` when inspecting mixed modes.
`--position` assigns an address to the first selected byte. It is independent
of the file offset. Repeated `--symbol=NAME=ADDRESS` options name exact
instruction and operand addresses. They supply display context without changing
the bytes or granting a symbol any executable authority.

JSON uses `holyc-disassembly-v1`. Signed 64-bit addresses and values are decimal
strings, byte offsets and lengths are JSON integers, and bytes are uppercase
hexadecimal. Each instruction retains its `OpCodes.DD` source line and entry
index. The listing names supplied instruction labels and prints complete
instruction bytes. The `holyc-disassembly-map-v1` map records supplied symbols
and decoded branch targets in input order. `asm --map` also retains assembler
symbols, imports and fixups; a bare binary cannot recover that source metadata.

## Source behavior and bounds

Table construction follows `UAsmHashLoad`, including original hash-bucket
traversal, `QSortI64` swaps among equal records and `InstEntryFind` binary search.
These details affect signed immediate spellings and canonical names. For
example, native `Ui` displays byte `90` as `XCHG U32 EAX,U32 EAX`, and displays
the explicit word in `48 B8 ...` as `I64`. Source aliases are not decoder
entries. Names such as `RET1`, `FADDP` and `REP_MOVSQ` remain in the original
dialect. `LOCK` remains a separate instruction.

Operand text preserves `Ui`'s signed/unsigned prefixes and hexadecimal field
widths. Relative displacements use the end of their selected field; RIP-relative
memory includes a following immediate when calculating its target. Integer
addresses are checked before addition, including negative relative targets.
Hosted output uses supplied names or hexadecimal addresses in place of
TempleOS's interactive DolDoc links.

Legacy memory rules also follow `Ui`. USE16 retains its original limitations:
ModR/M displacement and SIB handling are not a general 16-bit x86
address decoder, and address overrides retain the original toggling rule.
MOFFS uses the source's address width rather than a general x86-64 address
field. Repeated segment names stop after the original 24-character display
bound. `INT` uses the kernel's initial `ST_INT_NAMES` annotations. `Ui` also
leaves the register text empty for its fixed `BSWAP` record; the report preserves
that source result. These are utility compatibility
behaviors, not claims of a complete Intel disassembler.

The pinned lookup can return a neighboring record when it finds no match.
Hosted decoding requires the returned record's opcode and slash bits to match
the input before reading its operands. It rejects unrelated fallback records,
truncated fields, standalone segment/REX prefixes, nonzero nesting bytes outside
the pinned `ENTER` form, and instructions longer than 15 bytes. No partial
report is returned. Failed validation leaves requested listing and map files
untouched; output paths must be distinct from each other, the input path and
its resolved target. Unix CLI tests cover symbolic links to the input.

The independent encoder fixture census contains 924 records. Its empty opcode
has no bytes to decode. Of the 923 byte specimens, 904 decode completely;
`FCLEX` appears as separate `FWAIT` and `FNCLEX` instructions. Nineteen fail the
checked `Ui` path: seven standalone REX/segment prefixes, three `INS`/`INSB`
specimens, `INC` at line 659, six x87 memory specimens at lines 1197, 1202,
1203, 1212, 1219 and 1231, and `FINIT`/`FNINIT` at lines 1258 and 1259.
The tests account for every specimen, including these source lookup boundaries.

The complete input is bounded by `--byte-limit` (default 16 MiB), before reading
its selected range. `--instruction-limit` independently bounds decoded
instructions (default 1,000,000). Both limits must be positive. Exact and
one-below tests cover both allowances and signed address endpoints. The tests
also inspect 13,056 deterministic byte streams across all three modes, checking
that every successful report consumes the input with bounded instructions and
every rejection identifies an input byte.

## Native evidence

The fixture in `test/oracle/source-disassembly.json` contains 89 distinct native
`Ui` observations: 37 manually specified byte vectors and 52 instructions from
the compiled `Ui`, `IsDbgMode` and `DrvChk` functions. All 89 complete records
were captured twice in one boot, and both sequences are byte-identical.
Tests compare instruction lengths, scalar and memory operand text, source
spellings and relative targets. Interactive source links stay in the original
capture and are compared through their numeric branch destinations.

The capture used QEMU on Windows with one CPU, 512 MiB RAM, TCG and no network
or persistent disk. The original final ISO's SHA-256 is
`3c68ca395d0b64f779f1ffa330e2685798b17862e3c36ae4e349537fd676b40b`.
`tools/prepare-disassembly-oracle.py` verifies that original image and creates
a read-only boot copy with the capture source appended as `T:/ORACLE.HC`.
It changes one unused RedSea directory entry and volume sizes; existing
compiler, kernel and boot code stays intact. The fixture records both image
digests, the payload digest and the raw capture digest.
Eight compiler/kernel source files were expanded from the original ISO and
compared with the pinned Git blobs. They match after CRLF-to-LF and
`CH_SHIFT_SPACE`-to-space normalization; `Lex.HC:255-256` performs the latter
conversion before tokenization. The metadata retains original archive,
expanded-byte and Git-blob digests plus normalization counts.
Tests verify those payload and capture checksums and compare every JSON
expectation with its original debug-console record before checking the decoder.

To repeat the capture, prepare a new image:

```sh
python tools/prepare-disassembly-oracle.py --iso TOS_Distro.ISO --output capture.ISO
qemu-system-x86_64 -m 512 -smp 1 -accel tcg,tb-size=32 -nic none -cdrom capture.ISO -boot d -debugcon file:capture.log -global isa-debugcon.iobase=0xe9
```

Decline installation and the tour, then run `#include "T:/ORACLE.HC"`.
The payload writes two complete
sequences and a final `DONE` line to the debug console. The committed payload
and raw capture make the observations inspectable independently of this
decoder. This evidence covers the named utility paths; native compiler parity,
inline assembly execution and TempleOS BIN loading remain separate work.

The source audit uses TempleOS `c26482bb6ad3f80106d28504ec5db3c6a360732c`:
`Compiler/UAsm.HC`, `AsmInit.HC`, `OpCodes.DD`, `Kernel/KHashA.HC:1-27`,
`Kernel/QSort.HC:1-29` and `Kernel/KDefine.HC:118-124`.
