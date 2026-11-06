# Native stack probing

TempleOS remains the source for semantic frame layout and function entry.
`Compiler/OptPass789A.HC:698-719` saves RBP, establishes the frame and subtracts
the local allocation. Its leave path at lines 387-405 restores saved registers
and the frame. The hosted adapter keeps its existing fixed-RSP calling
convention, parameter slots and checked local displacements. Windows guard pages
and unwind records are host requirements, not TempleOS language semantics.

The encoder admits spill frames through 65,528 bytes and callable frames through
65,520 bytes. Their alignment is unchanged. The default quota stays at 4,088.
For an allocation of at least 4,096 bytes, the prologue emits a qword TEST from
`RSP-4096`, then each successive page, then the final allocation address if it
is not a page multiple. All reads fall inside the future frame. TEST preserves
registers and memory; only flags change. RSP moves once, after the probes.
The largest probe sequence occupies 128 bytes, so the complete callable
prologue fits the one-byte Windows unwind offset.

[Microsoft's x64 prologue rules](https://learn.microsoft.com/en-us/cpp/build/prolog-and-epilog)
require checking large allocations before changing RSP and preserving argument
registers. This adapter uses inline probes rather than a call to `__chkstk`.
[LLVM's Windows CoreCLR lowerer](https://github.com/llvm/llvm-project/blob/main/llvm/lib/Target/X86/X86FrameLowering.cpp)
also supports inline probes before the RSP adjustment.
The hosted adapter's [version 1 unwind records](https://learn.microsoft.com/en-us/cpp/build/exception-handling-x64)
place the allocation operation after the probes. No unwind operation is needed
for a probe because it changes neither RSP nor a nonvolatile register. The
unchanged epilogues add the allocation back, restore RBP where present and return.

The native bridge checks the probe bytes, distances, allocation and unwind
endpoint before admitting callable or retained closed images. The Windows test
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
