# BIN validation and loader predictions

`holyc verify-bin` reads complete uncompressed TempleOS modules with a parser
independent of the BIN writer. `holyc dump-relocs` predicts their patches,
exports, heap allocations, pending imports and MAIN addresses. The public OCaml
API is `Holyc_lib.Bin_validator`.

```sh
dune exec holyc -- verify-bin module.BIN --format=json
dune exec holyc -- dump-relocs module.BIN --base=0x10000000 --bind=Counter=0x40000000 --format=json --map module.MAP
dune exec holyc -- dump-relocs consumer.BIN provider.BIN --base=0x10000000 --base=0x10001000 --require-resolved --format=json
```

Supply one `--base` per input or omit all bases. Bases identify the byte after
the 32-byte header. An explicit base overrides `org`, matching the original
loader's supplied header address. Automatic placement uses `org` as the header
address when present; otherwise it aligns the body to at least 16 bytes and the
module's requested alignment. The virtual module allocator starts at
`0x10000000`, code heaps at `0x20000000`, and data heaps at `0x30000000`.
The API accepts different allocator bases. These addresses are deterministic
inputs to the prediction, rather than estimates of TempleOS's allocator.

Initial `--bind NAME=VALUE` arguments supply effective symbol values. They can
represent an export value, a function's `exe_addr`, or a global's `data_addr`.
The API's `bind` operation also publishes a later symbol and resolves earlier
imports. Every address is an integer; the model allocates no executable memory,
looks up no host symbols and calls no MAIN routine.

## Reader checks

The reader checks the jump, signature, reserved byte, alignment exponent,
origin, exact padded file size, patch-table position, every record payload,
terminated name, import-group boundary, body field and final zero padding.
Counts are bounded against remaining bytes before list allocation. Code-heap
sizes are signed I32; data-heap sizes are signed I64. Negative sizes fail.
Unknown IET values and reserved slots fail with the artifact name and byte
offset. A failed read returns no partial artifact.

All twenty defined nonterminal IET types are supported. Names retain their
original bytes, and records retain their order, including duplicates. Relative
exports may identify the byte just after the body; MAIN must identify a body
byte. Patch fields must fit their original width. These checks are hosted
validation policy: the original loader trusts its input and performs fewer
checks. Compressed `.BIN.Z` input must be expanded before using these commands.

## Loader rules and transactions

`Kernel/KLoad.HC` supplies the interpretation. Pass one handles exports,
imports, absolute additions and heap allocations in record order. Pass two
collects MAIN addresses in record order. `--no-abss`, `--just-load` and
`--silent` model `LDF_NO_ABSS`, `LDF_JUST_LOAD` and `LDF_SILENT`. Silent loading
retains pending groups while suppressing unresolved-reference messages.

Relative imports store `symbol - field_address - width`; immediate imports
store the symbol value. Both use the original modular field stores, including
narrowing and I64 wrapping. Zero-width imports still select or defer a symbol.
Exports, including the two types named "64", carry U32 values in the file.
The newest same-name export wins. Ordinary exports replay pending groups,
newest first. Named heap allocations publish a symbol but leave earlier groups
pending until an ordinary export or `bind` resolves them.

Absolute patches and heap patches add to the existing field. Repeated and
overlapping fields observe preceding patches. Code-heap fields are four bytes;
data-heap fields are eight. Each heap allocation reserves an aligned virtual
range of at least 16 bytes. A zeroed allocation reports all-zero contents;
an ordinary allocation reports unknown contents. Requested heap sizes remain
separate from that virtual spacing policy.

The model validates all allocation plans and address arithmetic, then plans
patches in an immutable byte overlay. It creates private patched images only
after the entire transaction succeeds. Failure leaves the previous world,
artifact bytes, pending groups and prior modules unchanged. Success resolving
an older module's imports also preserves every older world snapshot. Complete
artifact ranges and heap ranges cannot overlap in the virtual address space.

| Limit | Default and hard bounds |
| --- | --- |
| File bytes | 16 MiB per artifact; caller can lower it |
| Work | 1,000,000; caller can set up to 100,000,000 |
| Image bytes | 64 MiB cumulatively; caller can lower it |
| Requested heap bytes | 64 MiB cumulatively; caller can lower it |
| Modules | 4,096; the API can lower it |

Reader work charges file bytes, artifact-label bytes, record count, name bytes
and field count. Artifact labels have a hard bound of 65,536 bytes.
Model work charges those reader units, each examined record or pending group,
each patch plus its width, and prior image bytes reserved for copying. Binding
charges its name bytes plus one. World work is cumulative, so repeated binding
or loading cannot evade the metadata bound. Reports include both transaction
and total work. The CLI additionally bounds all retained input files to 64 MiB.
Exact-limit and one-below tests cover the reader and model budgets.

## Reports and evidence

Verification JSON uses `holyc-bin-verification-v1`; each artifact contains a
`holyc-bin-validation-v1` report. Model JSON uses `holyc-bin-loader-v1`.
Reports retain body bytes, record offsets, artifact-relative field offsets,
before/after field values, symbols, heaps, unresolved groups and ordered entry
addresses. JSON name fields escape bytes above ASCII as `\xNN` and include
their exact hexadecimal bytes. Maps escape names and show patches in their
application order.
`--require-resolved` fails if any group remains pending. `--output` writes the
report and `--map` writes the map; input aliases are protected and validation
failure preserves existing outputs.

`test/oracle/bin-validator` contains two artifacts produced by the original
`Cmp` in the verified final TempleOS ISO, with zero compilation errors. The
assembly artifact has an import, exports and an absolute patch. The HolyC
artifact has a global, a function and a top-level MAIN entry. Their exact
sources, capture payload, raw debug-console capture and digests are retained.
API and CLI tests validate and predict both artifacts at two virtual bases.
They also regenerate the five public-writer modules in
`test/oracle/bin-modules`, check their exact bytes, and compare modeled fields
and deferred resolution with the earlier original-loader observations.
Handcrafted tests cover every IET type, malformed inputs, flags, duplicate
patches, ownership and failed transactions.

To repeat the original-compiler capture:

```sh
python tools/native-bin-oracle.py --payload ORACLE.HC
python tools/prepare-disassembly-oracle.py --iso TOS_Distro.ISO --payload ORACLE.HC --output capture.ISO
qemu-system-x86_64 -m 512 -smp 1 -accel tcg,tb-size=32 -nic none -cdrom capture.ISO -boot d -debugcon file:capture.log -global isa-debugcon.iobase=0xe9
python tools/native-bin-oracle.py --capture capture.log --boot-iso capture.ISO
```

Decline installation and the tour, then run `#include "T:/ORACLE.HC"` before
the final recording command. The preparation helper checks the original ISO
SHA-256 `3c68ca395d0b64f779f1ffa330e2685798b17862e3c36ae4e349537fd676b40b`
and preserves it. The pinned reference commit is
`c26482bb6ad3f80106d28504ec5db3c6a360732c`. The earlier loader capture also
checks four expanded ISO source files against that pinned source tree.

Issue #718 covers this reader and model. General HolyC-to-BIN integration,
host loading, the full target ABI and broader executable acceptance remain
under #723, #719, #720, #702 and #721. The model predicts MAIN addresses and
patches; native execution evidence applies to the named capture fixtures.
