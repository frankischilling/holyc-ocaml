# Native stack probing

TempleOS remains the source for semantic frame layout and function entry.
`Compiler/OptPass789A.HC:698-719` saves RBP, establishes the frame and subtracts
the local allocation. Its leave path at lines 387-405 restores saved registers
and the frame. The hosted adapter keeps its existing fixed-RSP calling
convention, parameter slots and checked local displacements. Windows guard pages
and unwind records are host requirements, not TempleOS language semantics.

The encoder admits spill frames through 524,280 bytes and callable frames
through 524,272 bytes. Their alignment is unchanged. The private-frame default
stays at 4,088 bytes; simultaneous physical stack use still defaults to 65,536
bytes, with a separate opt-in ceiling of 524,288. These are independent limits.

Allocations through 65,528 bytes keep their original bytes. Starting at 4,096
bytes, that path emits one qword TEST per page and a final partial-page touch.
Larger allocations use a compact loop: RAX starts at -4096, each TEST reads
`[RSP+RAX]`, and the cursor moves downward by 4096 until the last full page.
A separate TEST touches the final allocation address when needed. The loop
uses the pinned MOV, TEST, SUB, CMP and JGE forms from OpCodes.DD. It changes
RAX and flags, so RAX must be free at these allocation-entry sites. Argument,
context and nonvolatile registers remain intact. All probes preserve RSP and
memory, and RSP moves once after probing. Probing never jumps directly to the
bottom of an uncommitted stack range.

The compact allocation occupies 34 bytes for a page multiple and 42 bytes with
a partial tail. Callable prologues add four bytes for PUSH RBP and MOV RBP,RSP,
so Windows unwind offsets remain below 256. The ceiling fits the existing
scaled UWOP_ALLOC_LARGE field; the unscaled variant is not claimed.

[Microsoft's x64 prologue rules](https://learn.microsoft.com/en-us/cpp/build/prolog-and-epilog)
require checking large allocations before changing RSP and preserving argument
registers. This adapter uses inline probes rather than a call to `__chkstk`.
[LLVM's Windows CoreCLR lowerer](https://github.com/llvm/llvm-project/blob/main/llvm/lib/Target/X86/X86FrameLowering.cpp)
also supports inline probes before the RSP adjustment.
The hosted adapter's [version 1 unwind records](https://learn.microsoft.com/en-us/cpp/build/exception-handling-x64)
place the allocation operation after the probes. No unwind operation is needed
for a probe because it changes neither RSP nor a nonvolatile register. The
unchanged epilogues add the allocation back, restore RBP where present and return.

The native bridge checks the probe bytes, distances, loop bounds/back edge,
partial-page touch, allocation and unwind endpoint before admitting callable or retained closed images. The Windows test
probe checks `RtlVirtualUnwind` at each probe, before allocation, in the body and
in the epilogue. Linux runs the same emitted allocation paths. Byte tests cover
page boundaries, final partial pages, the maximum frame and exact code quotas.
Source execution tests cover large arrays, recursive calls, repeated calls,
uninitialized reads, arithmetic faults and separate semantic/physical limits.

`examples/large-native-frame.hc` returns 42. Run it with `--target=host-jit`
or `--target=host-jit-task` and `--stack-byte-limit=32768`. The default quota
rejects this example. Its 8,192-byte array needs a 16,528-byte native frame
because initialization flags and private runtime storage also occupy space.

This change does not implement general integer or floating register allocation,
requested-register conflicts or native exception handlers. Issue #703 stays open.

`examples/compact-native-frame.hc` also returns 42. It uses a 40,000-byte array
and an 80,144-byte callable frame. Run it with `--stack-byte-limit=131072`,
`--code-byte-limit=262144` and `--active-stack-byte-limit=262144` on either
native target. Its exact simultaneous physical requirement is 80,320 bytes,
including the entry frame and callee return address/saved RBP. Both private
and active-stack defaults remain too small for it. Enlarging the private-frame
quota does not silently enlarge either the code or active-stack quota.
