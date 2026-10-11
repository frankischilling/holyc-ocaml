# Native sections, symbols and relocations

`Holyc_lib.Native_sections` owns the object model used by native emission,
assembly linking and TempleOS BIN output. An object retains ordered code and
data sections, sparse symbol IDs, source identities, scoped names, bindings,
relocation fields and source maps. Its coordinates describe sections and byte
offsets. A load address is supplied separately when linking.

Native branches, calls and RIP-relative function addresses use this model to
resolve forward machine labels. Their skeleton bytes come from the project's
own x86 encoder. Function records retain the semantic symbol ID, name, scope
and source span. Maps retain machine plan item offsets, whole source function
ranges and private data slot ranges. Ordinary global storage is represented by its initial private
arena and source symbols. A native task's separately retained storage remains
owned by that task; its object does not copy the live arena or grant access to it.

`X86_64_program.native_sections` and `X86_64_expression.native_sections` expose
these objects. `run --target=host-jit --format=json` includes the program object
under `native.image.native_sections`. Assembly reports include the object under
`native_sections`, and `Assembly_encoding.native_sections` exposes the same
projection through the API. Assembly expression evaluation retains its existing
source semantics; resolved compiler fields and pending loader fields remain
separate records.

## Source rules

The specification is the pinned TempleOS commit
`c26482bb6ad3f80106d28504ec5db3c6a360732c`:

- `Compiler/BackLib.HC` writes width-specific machine fields. `ICModr2` bases
  RIP-relative displacements on the instruction's remaining fields;
  `ICLea` records AOT U32 address adjustments for pushed code addresses.
- `Compiler/AsmResolve.HC` resolves scoped forward labels, preserves each
  selected field width, records imports and publishes exports.
- `Compiler/CMain.HC` separates compiler AAT adjustments from BIN IET records.
  Its loaded relative imports use the end of the patched field, including when
  an instruction has later bytes.
- `Kernel/KLoad.HC` consumes the BIN patch table and supplies deferred imports,
  allocation metadata and ordered MAIN entries.

The model records an explicit relative anchor. Native compiler fields use their
instruction end; loader imports use their field end. These coordinates can
differ. Compiler fields resolve at raw coordinates first. Load fields use
biased coordinates. Load adjustments then run in their
supplied order, followed by import replacements in their supplied order.
Overlapping fields observe earlier patches without modifying the input object.

## Checked layout and linking

Sections retain input order. Section alignment inserts zero padding. The image
alignment metadata separately retains the assembler's original high-bit ALIGN
exponent 63, which does not pad its already laid-out bytes; BIN output rejects
that exponent. Local root `@@` bindings retain a separate namespace from global
imports with the same spelling. Section and symbol
IDs must be nonnegative and unique; they never determine an allocation size.
Names must be unique within a scope. Imports are undefined root symbols;
exports are defined root symbols. A definition may point to a section's end,
while a patched field must fit entirely inside its section.

Linking accepts a nonnegative load bias and named external values. Duplicate
external names fail even when unused. Address ends, addends and relative
subtraction use checked signed 64-bit arithmetic except in explicit modulo
fields. One-, two-, four- and eight-byte
fields carry an explicit signed, unsigned, bit-pattern or modulo policy.
Checked policies reject narrowing. Modulo stores and AAT adjustments preserve
the original low-byte behavior.

`compile_only` requires zero bias and retains load records. A normal link rejects
missing imports unless `defer_imports` is set. Deferred fields remain pending in
source order. A later link can supply those values against the original object;
linking does not publish symbols to a live host process or execute code.

The default byte/work limits are 16 MiB and 100,000,000. The hard byte limit
is 100,000,000 and the hard work limit is 1,010,000,000. The legacy relocation
API still checks its original allowance before using a larger internal budget
for the shared engine's metadata and field-byte charges. Construction work counts section bytes, alignment padding, one per
section, symbol, relocation and map, and section/symbol/scope name bytes.
Link work counts that construction cost, one per examined relocation, each
written field byte, and one plus the name length per supplied external value.
Limits and identities are checked before copying input bytes. Field arithmetic
is checked before allocating the final output. Public byte and name views are
fresh copies. A failed link leaves the object and earlier results intact.

## BIN consumer

`Templeos_bin.of_sections` accepts an AOT object and optional MAIN symbol IDs.
It serializes source-ordered exports and first-use import groups. AAT add fields
become the original reversed `ABS_ADDR` table, whose wire fields are U32 even
when the compiler record had another width. The writer checks those U32 body
ranges separately. Subtract adjustments do not become BIN loader records.
Imports must have zero addends and field-end relative anchors.

Hosted and JIT objects, private runtime data and unsupported load replacements
fail before output. A hosted object's byte layout does not replace its sealed
runtime context, private arena, unwind metadata or executable ownership.
General HolyC compiler BIN emission remains under #723; inline assembly execution
remains under #715, and runtime entry/fixup integration remains under #719.

## Verification

`test/test_native_sections.ml` checks literal forward references across aligned
code/data sections at independent bases, all field widths, deferred imports,
overlapping patches, duplicate identities and names, arithmetic overflow,
exact/one-below limits, and copied views. Real HolyC compilation checks recursive
forward calls, function/global source identities, deterministic objects and
independent rebases in JIT and AOT source modes. CLI tests check assembly maps
and compare native API/report objects after real source execution. Existing native execution tests
exercise the emitter after shared resolution.

The original compiler's captured `test/oracle/bin-validator/ASM.BIN` supplies
independent body bytes for the source assembly fixture. Literal expected rebases
check its import and absolute address fields. This evidence is separate from
assembler/object/BIN agreement checks and the existing original-loader fixtures.
The fixture's capture identity and reproduction instructions remain in
[BIN validation](bin-validator.md).

```sh
opam exec -- dune exec test/test_main.exe -- test 'shared native sections'
```
