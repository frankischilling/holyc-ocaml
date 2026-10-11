# TempleOS assembler

The assembler follows TempleOS commit
`c26482bb6ad3f80106d28504ec5db3c6a360732c`. Its grammar and form-selection
rules come from `Compiler/Asm.HC`, `AsmLib.HC` and `AsmInit.HC`; the instruction
records come from the checked `Compiler/OpCodes.DD` database.

## Public commands

HolyC `asm { ... }` blocks and function-local direct instructions share the
same operand parser and instruction-form selector. The `asm` command reads a
standalone assembly stream, with USE64 as its default initial mode:

```sh
dune exec holyc -- parse examples/assembly-operands.hc
dune exec holyc -- asm test/cli/assembly-operations.hc
dune exec holyc -- asm --bits=16 --format=json test/cli/assembly-operations.hc
dune exec holyc -- asm --emit examples/assembly-encoding.asm
dune exec holyc -- asm --emit --base=0x1003 --output=assembly-layout.bin examples/assembly-layout.asm
dune exec holyc -- asm --emit --map=assembly-layout.map --listing=assembly-layout.lst examples/assembly-layout.asm
dune exec holyc -- disasm --position=4096 --format=json code.bin
```

The command accepts the same include roots, TempleOS path mapping, preprocessing
mode and source-expansion limits as `parse`. The `--bits` option selects the
initial assembler mode; `USE16`, `USE32` and `USE64` change it as the stream is
read. HolyC compilation mode (`--mode=jit` or `--mode=aot`) is a separate setting.

Human and JSON dumps expose each instruction's source spelling, canonical
opcode, selected database entry and source line, operand fields, size prefixes,
mode, matching-form count and warnings. The standalone JSON schema is
`holyc-assembly-v1`. HolyC AST dumps retain `holyc-ast-v1` and add the ordered
`items` of each assembly block. Source-line groups remain available for display.

The ordinary command checks assembly syntax and selects instruction forms.
`--emit` processes instructions and directives in source order. It writes
data and binary-file bytes, applies alignment, resolves same-unit labels,
and records source locations and import patches. `--base` requests a linked
image using that additive load bias. Each `--symbol NAME=ADDRESS` supplies
an external value to encoding and linking; a symbol option without `--base`
links at base zero. `--output` writes the complete selected image after
preflight. A used import must have a supplied binding before file output.
The [instruction encoding
guide](instruction-encoder.md) covers checked instruction fields;
[assembler directives](assembly-directives.md) covers data, files and ordered
output metadata.

External values arrive after parsing. If a supplied name conflicts with an
active opcode, register, size, type, query or `DUP` role retained by the parser,
emission produces a diagnostic. Unused name collisions and explicit `IMPORT`
symbol uses are accepted.

The parsed source keeps symbolic expressions and their original selections.
Emission evaluates closed instruction expressions at the current raw position
before selecting the form and reserving its bytes. Forward references retain
those fields through resolution. A base displacement that was unresolved
keeps its full width even when its final value is zero. Branches keep their
selected short or near form and fail when the displacement cannot fit; the
assembler performs no branch relaxation. The report retains the actual
encoded selection separately from the parsed operation. Live compiler
expressions, source assembly loading and execution remain under #715.
The [disassembler](disassembly.md) inspects binary streams through the pinned
`Ui` forms and produces versioned JSON, listings and branch maps. Its source
lookup, legacy-mode boundaries and native captures are documented separately.

`--map` and `--listing` write inspection reports at raw source coordinates.
The listing uses final unrelocated bytes and obeys `LIST` and `NOLIST`.
These two reports remain in raw coordinates when `--base` selects linked
JSON, human output or a linked binary file.

## Operation boundaries and source identity

`Compiler/Asm.HC:904-1114` parses a stream of operations. A physical newline
does not end an instruction. Instructions, directives and labels can share one
line, and one operand can span several lines.

The original `PrsAsmBlk` consults the first opcode record to determine whether
to read zero, one or two operands. That is only the grammar's operand count:
`PrsAsmInst` subsequently checks the complete form list. The OCaml parser keeps
this distinction. Two-operand instructions require their separating comma;
an unexpected additional operand produces a diagnostic.

`LOCK` is an ordinary zero-operand record and remains a separate operation in
the dump. Repeat-string instructions use the pinned combined names such as
`REP_MOVSB`. Instruction names and aliases keep their exact spelling; for
example, the original dialect uses `RET1 8` and `IMUL2 RAX,10`.

Blocks and standalone streams retain ordinary `name:`, exported `name::` and
local `@@name:` labels at their actual operation positions. Function label
collection consumes these ordered items, so a label after an instruction on the
same line remains visible to language `goto` resolution. Standalone emission
records label positions and accepts repeated definitions at the same position.
A newly defined exported label resolves the preceding local references and
starts a new `@@` scope. An ordinary label and a same-position repeated `::`
definition preserve the current scope. Repeating an ordinary or supplied
external binding with `::` does not promote it to an export. Required labels
must resolve within their original scope. Function-assembly address validation
through the semantic label pass remains under #174.

Every item and operand retains its source span, component spans, include source,
definition site and expansion site. Classification does not replace a source
spelling with its canonical name. Visible compiler symbols can shadow opcode,
register and directive names.

## Operands

`PrsAsmArg` at `Compiler/Asm.HC:35-175` supplies the operand grammar:

| Form | Retained meaning |
| --- | --- |
| Register | Original register family, spelling and number |
| Integer size prefix | I8/U8, I16/U16, I32/U32 or I64/U64, with byte width |
| Segment prefix | A segment register followed by a colon, such as `FS:` |
| Immediate expression | Original expression tokens and an exact integer value when available |
| Memory operand | Brackets, register slots, scale and optional displacement expression |
| Displacement before brackets | Expressions such as `8[RBP]` and `&value[RBP]` |

Only the eight integer size spellings take the source's assembler-keyword
fallback when ordinary lookup finds a class or internal type. This preserves
`U64` prefixes after the kernel publishes its intrinsic-backed union.
A local or unrelated newer symbol still shadows that spelling.

x87 stack, MMX and XMM registers retain the original parser's eight-byte size
field; their register families remain distinct during matching. A segment
register used alone is distinct from a segment override. The mode and operand
masks determine which generated forms can match.

The implementation preserves the pinned address parser's register-slot rules.
An initial stack-pointer-family register creates the original special register
pair. A scale can swap the register slots. A later address register replaces
the second slot rather than adding a third slot. These are source behaviors,
not a normalized Intel-syntax address tree.

The original parser accepts an integer scale token. Its encoding planner sets
SIB scale bits only for 1, 2, 4 and 8; other values leave those bits zero.
The parsed scale remains visible in the dump. Repeated size prefixes overwrite
the previous size, and a direct register then supplies its own size.
Missing brackets, missing scale tokens and duplicate immediate components
produce diagnostics.

The checked emitter rejects a scale that cannot be represented by the source's
SIB bits. It also checks register families, REX/high-byte interactions and
integer operand widths before producing instruction bytes. These checks preserve
the original parsed operands and their form-selection metadata.

Expressions retain the compiler's source grammar and operator precedence.
Symbol, call, query and address expressions that need a later compiler phase
remain visibly unresolved. The operand model does not turn an unresolved
expression into a known zero.

Visible globals and static locals permit expression indexing, so `values[1]`
stays one typed expression. An export label followed by brackets starts an
assembly address. Automatic locals require `&name`, as in `&value[RBP]`.
Known class member offsets keep their identity separately from export labels.
Postfix casts retain pointer, array and function-pointer syntax. Literal,
query, label and typed-value terms follow the modifier states in `PrsExp.HC`;
for example, a literal followed by `()` is not a function call.

Known function declarations supply their required, defaulted and variadic
parameter shapes. Calls without parentheses consume required arguments using
the original grammar. A known scalar variable cannot be called; a declared
function pointer retains its callable shape. Values whose type or address needs
later resolution remain explicit in the operand model. An immediate's
`absolute_known` field distinguishes confirmed address-count parity from an
address whose export metadata still needs resolution.

Inline class and union definitions in postfix casts use the ordinary aggregate
parser, including its member checks and declaration visibility. Instruction
operands and directive arguments retain these definitions alongside the
original source tokens. JSON dumps include a `type_definitions` array when
such a definition occurs.

## Form selection

`AsmMakeArgMask` at `Compiler/Asm.HC:177-276` classifies operands.
`PrsAsmInst` at lines 395-812 checks the generated form records, mode flags,
operand masks, immediate widths, address shape and prefix requirements.

The original selection rule is specific:

1. Consider matching forms in database order.
2. Prefer the form with the smallest original comparison byte count.
3. Keep the earlier form when counts tie.
4. Preserve the warning for an unsized operand when a second possible form is
   reached after a best candidate without `IEF_DFT`.

A blanket rejection of every ambiguous unsized operand would disagree with
`PrsAsmInst`. The warning and deterministic selected form are both exposed.

The source's comparison count excludes operand/address/segment prefixes and
the final zero emitted by `ENTER`. Dumps therefore distinguish
`comparison_bytes` from `estimated_bytes`. Neither field is a loaded address.
Emission checks a known relative target against the supplied output position.
Forward fields are patched after the ordered raw layout is known. Declared
imports retain source-width records for the explicit linking pass.

Each HolyC assembly block or direct run begins in USE64, matching line 914.
An optional semicolon after a direct opcode, `USE` or listing directive ends
that run; a later direct run begins in USE64 again. A mandatory semicolon
consumed by `DU*`, `IMPORT` or `BINFILE` belongs to that directive and permits
the current run to continue. Brace blocks continue across separators.

## Directives

The shared operation stream retains `IMPORT`, `DU8`, `DU16`, `DU32`, `DU64`,
postfix `DUP`, `BINFILE`, `LIST`, `NOLIST` and the three `USE` directives.
`ORG` and `ALIGN` have their original context restrictions.
`IMPORT` names and data values can use the optional commas accepted by
`PrsAsmBlk`. `DUP` cannot start an operation.

The parser records directive arguments and separators without reading a
`BINFILE` path. `IMPORT` publishes unresolved external names in source
lookup, and USE directives change parser mode. `Assembly_encoding.encode`
consumes the retained items for standalone emission, using their original
expression boundaries and source locations.

Reading an unresolved assembler import as an ordinary HolyC value produces
`HCPARSE0180`, following `Compiler/PrsExp.HC:837-839`. Assembly expressions
retain the selected import for deferred linkage.

`DU8`, `DU16`, `DU32` and `DU64` store the low bytes of a scalar in
little-endian order. Quoted strings contribute their exact bytes at every
width, with no implicit terminator or padding. `count DUP(value)` repeats
one scalar word; counts at or below zero emit nothing but still require a
valid value. Closed scalar expressions, same-unit data labels and `$$` use
the original numeric rules. Instruction operands retain checked field ranges.

`ORG` records an origin at raw position zero without adding a hole or
relocating the returned bytes. `$$` observes the current raw coordinate.
Absolute-address occurrence parity produces `AAT_ADD_U*` relocation records
for affected fields. Relative uses of supplied external addresses produce
`AAT_SUB_U*` records. Imports remain ordered declarations and produce a patch
for each used field. `Assembly_encoding.link` applies these records to a fresh
image. `LIST` and `NOLIST` control the listing flag on subsequent output ranges.

`ALIGN` checks the original signed `I64` value. Positive JIT alignments are
limited to 8; the single high-bit value also passes the source's power-of-two
and signed-limit checks. Emission fills to the next boundary of the raw
coordinate and records `max_align_bits`. The high-bit boundary records 63
and follows the original signed loop's zero-fill behavior.

`BINFILE` requires an explicit preprocessing configuration in the library
API. It resolves regular files within the working directory, include roots
and TempleOS root mapping. Raw bytes and supported `.Z` archives retain
resolved/display paths, raw and decoded sizes, SHA-256 hashes and compression
kind. Malformed archives produce a diagnostic. Their bytes never become a
raw fallback. The detailed [binary input rules](assembly-directives.md#binary-files-and-inserted-binary-tokens)
describe default extensions and compressed-file selection.

Ordered `entries` report each item's raw coordinate, byte range, listing
state and source location. The report also includes `origin`, `imports`,
`symbols`, `fixups`, `relocations` and `binary_inputs`. `initial_bytes` is a
normalized pre-fixup image: deferred absolute fields contain zero, and deferred
relative fields contain the negated relative base. It is not a byte-perfect
TempleOS runtime listing or capture. Symbol records have stable identities
and every same-position definition site. `--directive-work-limit` applies
independently to encoding and linking. It bounds layout, expression,
field-patch and byte-copy work in encoding, and image copying, relocation
records and symbol lookup work in linking;
`--binary-input-byte-limit` bounds cumulative raw
binary input. Repeated file directives count their input again. Included
output must fit the remaining output and work allowances before expansion.

Saved DolDoc `$IS` tokens retain their integer size and source record
metadata. `$IB` yields a compiler-owned pointer and remains under #715;
it does not act as a data string. The intact DolDoc oracle requirement in
#375 remains separate from hosted synthetic-token and archive tests.

## Checked opcode database

The generator validates the pinned source checksum and records source lines.
CI rejects stale generated output. The database contains 106 register names,
325 canonical opcodes, 49 aliases and 924 instruction records. Aliases share
their canonical form list while retaining alias identity.

The source loader accepts up to four opcode bytes and up to 32 forms per
canonical opcode. It preserves opcode modifiers, operand-size flags,
slash selectors and all eight `IEF_*` flags. The checked reader rejects
malformed records, unknown operand classes, out-of-range bytes, duplicate
spellings and reordered sections.

`REP_OUTSB` has an empty first instruction record because its source starts
with a comma at `OpCodes.DD:920`. That entry remains in generated data.
Selection diagnoses the empty record with its source location instead of
indexing before its opcode bytes or silently replacing it.

```sh
dune exec tools/opcode_table_gen.exe -- --source third_party/TempleOS/Compiler/OpCodes.DD --manifest reference/manifest.json --output src/generated/opcode_keywords.ml
dune build @generated-check
```

The source assembler and native compiler share the checked byte writer in
`Asm.Encoding`. Each consumer supplies its own selected forms and operands.
Emitted source assembly remains data; loading and execution belong to #715.

## Verification

The public command uses the shared parser diagnostics:

| Code | Meaning |
| --- | --- |
| HCPARSE0175 | No compatible instruction form, or the selected original form has no opcode bytes |
| HCPARSE0176 | Original warning for an unsized operand with multiple matching forms |
| HCPARSE0177 | Invalid operation start, conflicting label identity or unexpected standalone closing brace |
| HCPARSE0178 | A local `@@label` cannot use the exported `::` delimiter |
| HCPARSE0179 | Invalid ALIGN power, JIT alignment limit or missing fill separator |
| HCPARSE0180 | An ordinary HolyC expression reads an unresolved assembler import |

Byte emission adds these diagnostics:

| Code | Meaning |
| --- | --- |
| HCASM0001 | An operand, field value or address shape cannot be emitted |
| HCASM0002 | The requested or aggregate output exceeds the byte limit |
| HCASM0003 | The output position is negative or outside the signed 64-bit range |
| HCASM0004 | Unsupported scalar evaluation, invalid directive context, missing required label, conflicting definition or unsupported import expression |
| HCASM0005 | Operation, operand, mode or directive metadata does not belong to the retained selection |
| HCASM0006 | The raw output file could not be written |
| HCASM0007 | The directive work allowance is invalid or exhausted |
| HCASM0008 | Invalid binary-input allowance, missing BINFILE root authority, or a file-resolution, reading, size or decompression failure |
| HCASM0009 | Invalid link input, missing import binding, load-coordinate overflow, relative import range or link-work failure |

The tests use original Compiler, Kernel and Demo instruction patterns, cover
all three modes, check aliases and x87 operands, and distinguish source quirks
from malformed syntax. Public parser and command fixtures check ordered items,
deterministic dumps, same-line labels, include and macro provenance, and
source-expansion limits. Exact expected masks and form choices are derived from
the pinned source.

`test/test_assembly_directives.ml` covers ordered directive behavior and
source metadata. `test/test_assembly_binary.ml` checks independent archive
packets, malformed inputs, exact limits and file-root boundaries. Public
checks in `test/test_assembly_directives_cli.ml` compare reported bytes with
the output file and check that a failed emission preserves an existing file.
`test/test_assembly_layout.ml` adds literal Compiler and Kernel blocks,
forward fields, scoped labels, selected branch boundaries, final map/listing
offsets and explicit import linking. These tests cover standalone raw and
linked image construction. They do not establish TempleOS loader execution
or a fresh runtime capture.

The byte inventory in `test/fixtures/assembly-encoder-forms.tsv` includes all
924 original records. It fixes expected bytes for 923 nonempty forms and
records the malformed empty REP_OUTSB form as an explicit rejection. The
[encoding guide](instruction-encoder.md#fixed-regression-inventory) describes
its independent source derivation and external assembler comparison.

The checked raw-token inventory in `reference/assembly-blocks.json` still
records 45 brace blocks in 40 files under Compiler, Kernel, Adam and Demo:
39 at brace depth zero and six nested in another source block. That inventory
measures discovery separately from instruction validation. These are hosted
source-derived checks; they do not constitute a fresh TempleOS execution capture.
