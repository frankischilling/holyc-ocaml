/* The only host/guest FP transitions. No compiler-generated C executes
   between restoring an image and crossing its matching call boundary.

   TempleOS c26482bb: KStart64.HC initializes x87 with FNINIT before FXSAVE;
   KTask.HC allocates aligned 512-byte CFPU images, Sched.HC saves/restores whole images.
   MXCSR is not initialized by FNINIT. This x87-only guest must not use it.
   FXSAVE64 does not preserve AVX upper halves; no such guest instructions are
   admitted. These thunks preserve x87, MXCSR and the sixteen legacy XMM regs.

   Slot 36 is private C-owned storage, never a generated-accessible offset.
   Every entry, including legacy expressions, owns 37 words physically. Each
   image is stack-private and 16-byte aligned, so nested callbacks/tasks do
   not share mutable state. C callback bodies must catch OCaml exceptions;
   foreign unwinding through a running guest remains unsupported. */
#ifndef HOLYC_NATIVE_FPU_BOUNDARY_H
#define HOLYC_NATIVE_FPU_BOUNDARY_H

#define NATIVE_FPU_CONTEXT_WORDS 37

#if defined(__GNUC__) || defined(__clang__)
#define HOLYC_NATIVE_X87_SUPPORTED 1
#define NATIVE_FPU_BODY __attribute__((used, noinline))
uint64_t native_fpu_enter(uint64_t (*entry)(uint64_t *), uint64_t *context);
uint64_t native_reference_service(uint64_t *context);
uint64_t native_source_callback(uint64_t *context);
uint64_t native_option_callback(uint64_t *context, uint64_t index, uint64_t value);

#if defined(_WIN32)
#define FP_BEGIN(name, size) \
  ".text\n.p2align 4\n.globl " name "\n.seh_proc " name "\n" name ":\n" \
  "subq $" #size ", %rsp\n.seh_stackalloc " #size "\n"
/* FXSAVE's XMM6 starts at 160 + 6*16 within the image at rsp+32. */
#define FP_SAVED \
  ".seh_savexmm %xmm6, 288\n.seh_savexmm %xmm7, 304\n" \
  ".seh_savexmm %xmm8, 320\n.seh_savexmm %xmm9, 336\n" \
  ".seh_savexmm %xmm10, 352\n.seh_savexmm %xmm11, 368\n" \
  ".seh_savexmm %xmm12, 384\n.seh_savexmm %xmm13, 400\n" \
  ".seh_savexmm %xmm14, 416\n.seh_savexmm %xmm15, 432\n.seh_endprologue\n"
#define FP_END(name, size) "addq $" #size ", %rsp\nret\n.seh_endproc\n"
#define FP_CONTEXT "%rcx"
#define FP_ENTER_ARGS \
  "movq %rcx, %r11\nmovq %rdx, %rcx\n"
#else
#define FP_BEGIN(name, size) \
  ".text\n.p2align 4\n.globl " name "\n.type " name ", @function\n" name ":\n" \
  ".cfi_startproc\nsubq $" #size ", %rsp\n.cfi_def_cfa_offset " #size "+8\n"
#define FP_SAVED ""
#define FP_END(name, size) \
  "addq $" #size ", %rsp\n.cfi_def_cfa_offset 8\nret\n.cfi_endproc\n" \
  ".size " name ", .-" name "\n"
#define FP_CONTEXT "%rdi"
#define FP_ENTER_ARGS "movq %rdi, %r11\nmovq %rsi, %rdi\n"
#endif

__asm__(
  FP_BEGIN("native_fpu_enter", 568)
  "fxsave64 32(%rsp)\n"
  FP_SAVED
  FP_ENTER_ARGS
  "movq " FP_CONTEXT ", 544(%rsp)\n"
  "movq 288(" FP_CONTEXT "), %r10\nmovq %r10, 552(%rsp)\n"
  "leaq 32(%rsp), %r10\nmovq %r10, 288(" FP_CONTEXT ")\n"
  "fninit\ncall *%r11\nfxrstor64 32(%rsp)\n"
  "movq 544(%rsp), %r10\nmovq 552(%rsp), %r11\nmovq %r11, 288(%r10)\n"
  FP_END("native_fpu_enter", 568)
);

#define FP_CALLBACK(name) \
  FP_BEGIN(#name, 552) \
  "fxsave64 32(%rsp)\n" FP_SAVED \
  "movq 288(" FP_CONTEXT "), %r10\nfxrstor64 (%r10)\n" \
  "call " #name "_body\nfxrstor64 32(%rsp)\n" \
  FP_END(#name, 552)
__asm__(FP_CALLBACK(native_reference_service)
        FP_CALLBACK(native_source_callback)
        FP_CALLBACK(native_option_callback));

#undef FP_CALLBACK
#undef FP_BEGIN
#undef FP_SAVED
#undef FP_END
#undef FP_CONTEXT
#undef FP_ENTER_ARGS
#else
/* Preserve the existing integer-only execution path for compilers without
   GNU-style assembly (notably MSVC cl.exe). The opaque image capability gate
   rejects any x87-containing program before mapping on these builds. */
#define HOLYC_NATIVE_X87_SUPPORTED 0
#define NATIVE_FPU_BODY
#define native_reference_service native_reference_service_body
#define native_source_callback native_source_callback_body
#define native_option_callback native_option_callback_body
static uint64_t native_reference_service_body(uint64_t *context);
static uint64_t native_source_callback_body(uint64_t *context);
static uint64_t native_option_callback_body(uint64_t *context, uint64_t index, uint64_t value);
static uint64_t native_fpu_enter(uint64_t (*entry)(uint64_t *), uint64_t *context)
{
  return entry(context);
}
#endif
#endif
