# Instruction byte encoding

The assembler and native compiler share the byte construction rules in
`src/asm/encoding.ml`. Source assembly reaches them through `Asm.Encoder`;
the native compiler supplies its existing checked operands through
`Backend.X86_64_encoder`. Both paths validate a complete recipe before
allocating its byte buffer.

The instruction database remains the pinned
`Compiler/OpCodes.DD` at
`c26482bb6ad3f80106d28504ec5db3c6a360732c`.
`AsmPrsInsFlags` and `AsmHashLoad` in `Compiler/AsmInit.HC:1-153`
define the record flags, opcode modifiers, argument classes and slash fields.
`PrsAsmInst` in `Compiler/Asm.HC:395-812` defines selection and emission.
`AsmStoreNum` at lines 268-390 supplies little-endian stores and relative
range checks.

## Public command

`asm --emit` produces bytes from a standalone stream of instructions and
ordered [assembler directives](assembly-directives.md):

```sh
dune exec holyc -- asm --emit examples/assembly-encoding.asm
dune exec holyc -- asm --emit --format=json examples/assembly-encoding.asm
dune exec holyc -- asm --emit --position=4096 --code-byte-limit=26 --output=assembly.bin examples/assembly-encoding.asm
```

The example changes modes, loads a full-width immediate, uses x87 registers
and emits a `LOCK ADD` sequence. Its bytes are:

```text
b83412b87856341249b88877665544332211d9c1dec1f0490101
```

The report schema is `holyc-assembly-encoding-v1`. It includes the reference
commit, opcode-source checksum, start and end positions, complete byte string
and instruction boundaries. Each instruction retains its source spelling,
assembler mode and the database index and source line of its actual encoded
form. Ordered `entries` also describe directive ranges, listing state and
source locations. Separate fields retain origin, labels, imports, relocation
records, binary-input provenance and resource accounting. The embedded
`assembly` field keeps the parser's `holyc-assembly-v1` representation,
original selections and source locations.

`--position` is the starting output coordinate used by relative fields.
The command requires a nonnegative signed 64-bit coordinate and checks every
instruction end. `--code-byte-limit` limits the complete output, including
all prefixes, displacement and immediate fields, and the final zero byte
of `ENTER`, together with directive data, alignment and included binary bytes.
Its accepted range is 1 through 16 MiB, subject to the host string limit.
`--output` writes the raw byte string after successful preflight.

`DU*` data, repetition and `ALIGN` advance the raw coordinate. `ORG` records
an independent origin. `LIST` and `NOLIST` change listing metadata, and
`IMPORT` retains external declarations. Closed instruction expressions
evaluate at the current raw position before form selection and byte
reservation. Same-unit forward labels can supply data fields or a reserved
instruction whose form and size remain valid. An imported value or a form
that needs another layout pass produces an explicit diagnostic. General
label/import fixups and branch layout remain under #714. Loading and
executing source assembly, live compiler expressions and compiler-owned
`$IB` pointers remain under #715; bounded [Ui disassembly](disassembly.md) has public CLI and library reports. The
intact DolDoc oracle boundary remains under #375.

`BINFILE` requires explicit input-root configuration and reports both raw and
decoded file provenance. `--directive-work-limit` and
`--binary-input-byte-limit` bound cumulative work and raw file bytes, with
`HCASM0007` and `HCASM0008` diagnostics. See the
[directive limits](assembly-directives.md#limits-and-reports) for the API
inputs, defaults and binary-expansion checks.

The ordinary `asm` command continues to expose syntax and selected-form
metadata without requesting emission.

## Selection and planning

The ordinary selector considers source records in order, uses the original
comparison cost, and keeps the first form when costs tie. The comparison
cost excludes operand-size, address-size and segment prefixes and the `ENTER`
ending byte. The emitted size includes them.

`Instruction.select_form` selects an original record by its opcode-local
`entry_index`. It applies the same operand and mode checks. The explicit
index lets regression tests reach longer forms and duplicate encodings
that ordinary shortest-form selection would not choose; it cannot authorize
an invented record.

The public library exposes `Holyc_lib.Asm_encoder` and
`Holyc_lib.Assembly_encoding`. At the instruction layer:

| Operation | Result |
| --- | --- |
| `plan ~position selection` | A validated instruction plan with an exact size |
| `size plan` | Its complete byte length |
| `emit plan` | A fresh immutable byte string |
| `encode ~position selection` | Bytes, size and the retained source selection |
| `encode_all ~max_bytes plans` | One concatenated string after aggregate preflight |

The instruction API accepts signed 64-bit coordinates. Both end-address
addition and relative-target subtraction must remain representable.
The command and assembly-unit API impose the additional nonnegative
position rule.

A relative immediate is measured from the end of its field. A RIP-relative
memory displacement is measured from the end of the displacement and any
following immediate. `ENTER` appends its zero after the immediate store.
These bases follow `Asm.HC:796-808`; they are independent of output
buffer allocation.

Known values retain their field category. Base displacements and relative
differences use signed ranges. Immediate classes use their signed or
unsigned range. Absolute address and moffs fields accept either signed or
unsigned spelling of the selected bit pattern. Eight-byte fields retain
every `int64` bit pattern. Narrow instruction fields cannot truncate a value.
The original `AsmStoreNum` checks relative ranges but writes nonrelative
low bytes directly; rejecting other instruction-field narrowing is an explicit
hosted rule. `DU8`, `DU16`, `DU32` and `DU64` use the original low-byte data
stores. For example, `DU8 0x123;` emits `23`.

## Checked operand boundaries

Instruction planning requires every expression value and address category
to be known. Assembly-unit planning evaluates closed values before reserving
bytes and can later resolve forward labels while retaining that reserved
form and byte count. An unresolved import, address category or live expression
remains a diagnostic; its provisional numeric slot cannot supply a zero value.

The parser preserves the original address grammar. Emission additionally
rejects scales outside 1, 2, 4 and 8, empty memory addresses, and an actual
stack-pointer index whose SIB code would discard the index. The parser's
synthetic no-index representation for `[RSP]` and `[R12]` remains
encodable. A stack pointer used as a base and `R12` used as a real index
remain distinct cases.

Extended registers and integer qword data operands require USE64. A qword
x87 operand or state-block operand retains its own instruction class;
`FLD U64 [EAX]` and `FILD U64 [EAX]` therefore remain available in
USE16 and USE32. Ordinary address-register spelling still follows the
source's mode rules.

AH, CH, DH and BH cannot share an emitted REX prefix. The low-byte RSP,
RBP, RSI and RDI spellings require one. MMX and XMM tokens cannot masquerade
as the generated table's general-register forms. These checks belong to
emission and preserve the parser's source representation.

## Original encoding details

The byte writer retains the selected source recipe. The database itself
says that some forms are nonstandard and that the original assembler does
not reject every invalid form (`OpCodes.DD:12-19`). Producing bytes is
therefore separate from establishing that every source record is a valid
hardware instruction.

| Source detail | Retained encoding |
| --- | --- |
| `Asm.HC:775-789` | Segment, operand-size, address-size and REX prefixes precede the stored opcode bytes. A repeat prefix already in the opcode stays there. |
| `Asm.HC:470-480,580-600` | A single x87 `+I` operand adds its stack index to the final opcode byte; two x87 operands contribute through bitwise OR. |
| `Asm.HC:738-760` and `OpCodes.DD:274,283` | The UIMM32 move forms with the backtick flag can omit REX.W. |
| `Asm.HC:695-700` | USE16 base displacements retain the original two-byte wide field even when the address-size prefix is present. |
| `OpCodes.DD:733-734` | BSWAP uses `/R` without the plus flag. Its opcode register bits stay at the original EAX/RAX encoding. |
| `OpCodes.DD:812-814` with `Asm.HC:606-633` | The explicit INS operand forms retain the additional ModRM byte produced by the source rule. |
| `OpCodes.DD:926-930` | REP_LODS forms contain F2 in the stored opcode. |
| `OpCodes.DD:1182-1183,1239,1262` | FSAVE, FSTSW and FSTCW retain the stored forms without an added WAIT prefix. |
| `OpCodes.DD:920` | REP_OUTSB's initial comma creates an empty opcode record. Selection rejects that record explicitly. |

The fixed inventory also retains the original `CHPXCHG8B` spelling and
its `0F C7 /0` bytes at `OpCodes.DD:795`. The independent comparison
does not replace such source bytes with an external assembler's preferred
encoding.

## Fixed regression inventory

`test/fixtures/assembly-encoder-forms.tsv` accounts for all 924 original
records: 923 have fixed byte specimens and the empty REP_OUTSB record has
an explicit rejection result. Every row identifies its canonical opcode,
opcode-local index, original source line, mode, argument classes, concrete
operands and expected hexadecimal bytes.

The specimens use distinct ordinary register roles, simple base addresses,
nonzero x87 indices, signed and unsigned immediate values, and known
relative coordinates. The USE16 indirect CALL/JMP records forbid an
address-size prefix, so their specimens use DX directly, following
`Asm.HC:484-486,555-557,762-764`. Expected bytes were derived from the original
database and the cited source emission rules. The construction aid
`test/fixtures/assembly-encoder-forms.py` reads that original database;
it does not import or execute the production encoder. CI reads the
committed expected bytes.

The independent Clang 21.1.5 comparison in
`test/fixtures/assembly-encoder-forms.clang.json` records 821 equal byte
strings, 75 different encodings and 27 rejected translated specimens. The
empty original record is listed separately. The target is
`x86_64-pc-windows-msvc`, using `.code16`, `.code32` or `.code64` for each
specimen. Rejection describes the submitted Clang spelling; it does not
establish that every rejected source record is invalid on hardware.

External assemblers can choose a different immediate form, prefix order
or implicit operand size. Some original records have no equivalent legal
spelling. The comparison preserves those differences, and the fixture
continues to use the source's expected bytes. This evidence is separate
from a live TempleOS compiler capture.

With Clang available, the optional comparison can be repeated from the
repository root. It writes intermediate assembly and COFF files under
`_build/encoder-audit-clang` and refreshes the separate comparison report:

```sh
python -B test/fixtures/assembly-encoder-forms.clang.py --clang clang
```

This audit reads the committed TSV expectations. Clang is not a dependency
of the ordinary OCaml test suite.

`test/test_assembly_encoder.ml` checks the inventory against the complete
generated form list and compares emitted bytes with the fixed specimens.
Additional groups cover exact output limits, aggregate preflight, signed
and unsigned field endpoints, relative and address overflow, segment and
size prefixes, SIB and REX roles, x87 patches, aliases, unresolved inputs
and the checked operand boundaries above. Shared-writer and existing
native execution tests verify the native consumers separately.
The [directive regression coverage](assembly-directives.md#limits-and-reports)
adds mixed instruction/data output, relocation metadata, binary provenance
and command-output preservation after preflight failures.
