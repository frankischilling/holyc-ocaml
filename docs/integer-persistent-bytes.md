# Persistent scalar U8 storage

Issue [#625](https://github.com/frankischilling/holyc-ocaml/issues/625) extends
the existing byte cells to scalar U8 globals and static locals. The maintained
example combines a global byte, a persistent static counter and a pointer call:

```c
U8 Total=0;
U0 Save(U8 *p,I64 value) { *p=value; }
I64 Next() {
    static U8 count=40;
    count=count+1;
    Save(&Total,count);
    return Total;
}
Next();
Next();
```

```text
holyc run --mode=jit --format=json --step-limit=77 --initializer-step-limit=7 --global-byte-limit=9 --frame-byte-limit=16 --call-depth-limit=2 examples/integer-persistent-bytes.hc
holyc run --mode=aot --format=json --step-limit=77 --initializer-step-limit=7 --global-byte-limit=9 --frame-byte-limit=16 --call-depth-limit=2 examples/integer-persistent-bytes.hc
```

Both modes return I64 42 in 77 runtime instructions and seven preparation
instructions, with empty captured output and zero output work. Nine persistent
bytes cover Total and the padded static allocation. Save needs 16 active ABI
bytes and the nested call reaches depth two. Each one-below resource limit
fails with its existing diagnostic. CLI v2 and explicit v1 reporting remain
unchanged.

## Stored bytes and expression values

U8 initial images and reached stores retain the low eight bits; reads
zero-extend them. `U8 G=298;G;` reports U64 42. An I64 function returning that
read reports I64 42, preserving the existing byte-expression and return rules.
Negative and large unsigned initializer words take the same narrowing path.
Constant and zero images use the same word representation as reached byte
stores; they no longer depend on a word-only persistent type classifier.

The register result of assignment retains the original RHS bits. For example,
`U8 A=0;I64 B=(A=554);B;` returns 554 while A stores 42. Initializing another
U8 instead narrows that destination independently. U8-valued expressions can
initialize supported byte and word globals/statics. Constant preparation
describes its evaluated expression bits; the stored initial-image bits can
differ after narrowing. The existing preparation budget covers evaluation.

Globals keep exact checked declaration records and symbols. Statics keep
their checked definition frame, location, initializer and compiler options.
Joined extern/definition calls retain #623's association. The shared scalar
width helper admits only public zero-pointer-depth I64/U64/U8 storage; it does
not grant execution to arbitrary primitive classes or pointer-valued storage.

## Lifetime, initialization and bounds

Globals and statics share one fresh persistent image per execution. Repeated
and recursive calls retain updates; another execution of the compiled program
starts from its immutable prepared image. Byte and word objects retain separate
indices and exact type/extent metadata through pointer copies and callees.

Static initialization occurs at its declaration position, including declarations
in uncalled functions and unreachable statements. It does not run per invocation
or on first use. Constant preparation and scheduled JIT compile/AOT load
initializers retain their existing source order, exact expression subtree and
fault phase. Actual callees retain transitive arithmetic/publication guards.
Prior completed output survives a later initializer fault. AOT uninitialized
objects start at zero; reached unknown JIT reads retain HCIRVM0012.

A scalar U8 has one accessible byte. Indexed aliases and Print scans cannot
reach a neighboring object or static allocation padding. Empty strings can be
read from a zero byte; a nonzero scalar without an in-object terminator hits
the existing bounds diagnostic. Pointer escape, exact pointee and image
lifetime checks remain unchanged.

## Storage accounting and dumps

The hosted persistent-byte quota counts declared global widths (one for U8,
eight for I64/U64) plus each static allocation rounded up to eight bytes.
Both supported static widths therefore charge eight bytes. This follows the
per-object requests in the pinned source and excludes AOT inter-object alignment
gaps and host allocator/bookkeeping overhead. It is not a native module-size
measurement. Unused declarations count; statics use no invocation-frame slots.

Checked addition protects the total quota, and slot counts are bounded by
integer arithmetic and `Sys.max_array_length` before the VM builds its image.
These guards do not promise that arbitrary allocations fit available physical
memory. Padding remains separate from declared element size and pointer extent.
The static frame record's `location_allocated_size` retains its declared size;
its alignment records the static allocation rule.

Existing `holyc-integer-globals-v1` and `holyc-integer-statics-v1` dumps keep
their schemas. Their byte totals now reflect the respective declared and padded
charges, and constant image payloads show narrowed values. The original word-only
dump forms retain their previous values.

## Source evidence and verification

Reference: `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
`Compiler/CInit.HC:3-14` defines U8. `PrsStmt.HC:285-296,334-385,390-435`
sets global size, allocation/alignment, zero/fill policy and initialization.
`PrsVar.HC:101-107,200-204` copies scalar initializer results at destination
width; lines 530-588 round static allocations and publish initialized bytes.
`BackC.HC:159-204` and `BackLib.HC:453-572` distinguish destination narrowing
from register values and implement byte moves. This is source audit and hosted
execution, with no new native capture.

All six initial gates failed HCRUN0001 on merge
`74ccadceede12bfc3b42f9cd69b7370981f837ea`. The 15 new groups cover those gates,
initial/reached narrowing, byte-valued initializer sources, full RHS word
destinations, aliases, recursive/fresh images, initializer effects/faults,
quota/padding, scalar bounds, output scans and malformed metadata. An unchanged
rebuild is the control for exact load-type and static-option guards; the foreign
initializer-slot case has its own valid context control. CLI tests cover all
six gates, measured fixture limits, v1/v2 and deterministic dumps. Local OCaml
5.4.1 / Dune 3.24.2 verification passed all 1,885 tests in 38.547 seconds plus
CLI checks. All 34 automatic/persistent byte groups passed in 0.262 seconds
after the final guard assertions. Formatting, generated-source and build/install
checks passed, along with 82 pinned checksums and all 11 incremental provenance
scenarios. Lexer JSON matches the preceding capture exactly: 528/528 with zero
errors. Parser JSON and normalized text match the committed AOT baseline:
25/528 standalone and 126/528 with the prelude. Final source CI and merge
evidence are recorded in #625.

Persistent arrays, pointer-valued globals/statics, narrow updates and other
primitive storage remain explicit boundaries. Full formatting/linking, broader
memory, stateful compilation/#exe, optimizer parity, native backends,
TempleOS BIN/loader acceptance and bootstrap remain required work.
