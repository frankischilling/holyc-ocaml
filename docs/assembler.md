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

This phase checks assembly syntax and selects instruction forms. Symbolic
expressions keep their original tokens and are marked unresolved; their selected
forms and byte counts remain provisional until expression evaluation and layout.
The command does not produce a machine-code file. Instruction emission, data and
file directives, symbol relocation, disassembly and execution have separate
requirements under issues #712 through #716.

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
same line remains visible to language `goto` resolution. Output addresses,
repeated-definition equality and local assembler fixup scopes remain part of
the layout work in #174 and #714.

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
Relative target distances and unresolved symbol values require later layout
before a final encoding can be certified.

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
`BINFILE` path or publishing imported symbols. USE directives change parser
mode; data emission and output-position changes belong to #713 and #714.

`ALIGN` checks the original signed `I64` value. Positive JIT alignments are
limited to 8; the single high-bit value also passes the source's power-of-two
and signed-limit checks. The parser preserves that syntax without advancing
an output position.

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

The separate checked native compiler encoder continues to serve its existing
source-program consumers. The assembler parser's form selections do not grant
those consumers permission to execute arbitrary source assembly.

## Verification

The public command uses the shared parser diagnostics:

| Code | Meaning |
| --- | --- |
| HCPARSE0175 | No compatible instruction form, or the selected original form has no opcode bytes |
| HCPARSE0176 | Original warning for an unsized operand with multiple matching forms |
| HCPARSE0177 | Invalid operation start, conflicting label identity or unexpected standalone closing brace |
| HCPARSE0178 | A local `@@label` cannot use the exported `::` delimiter |
| HCPARSE0179 | Invalid ALIGN power, JIT alignment limit or missing fill separator |

The tests use original Compiler, Kernel and Demo instruction patterns, cover
all three modes, check aliases and x87 operands, and distinguish source quirks
from malformed syntax. Public parser and command fixtures check ordered items,
deterministic dumps, same-line labels, include and macro provenance, and
source-expansion limits. Exact expected masks and form choices are derived from
the pinned source.

The checked raw-token inventory in `reference/assembly-blocks.json` still
records 45 brace blocks in 40 files under Compiler, Kernel, Adam and Demo:
39 at brace depth zero and six nested in another source block. That inventory
measures discovery separately from instruction validation. These are hosted
source-derived checks; they do not constitute a fresh TempleOS execution capture.
