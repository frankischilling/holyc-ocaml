/* Standalone architectural boundary test. Build with the host C compiler, not
   the OCaml runtime. It executes freshly emitted RX bytes through the exact
   production thunk, with hostile x87 state installed immediately around it. */
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#if defined(__x86_64__) && (defined(__GNUC__) || defined(__clang__)) && \
    (defined(_WIN32) || defined(__linux__))
#if defined(_WIN32)
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#else
#include <sys/mman.h>
#endif
#include "../src/native_fpu_boundary.h"

struct image { _Alignas(16) unsigned char bytes[512]; };
static struct image host_before, host_after, callback_host, nested_before, nested_after;
static struct image guest_initial, guest_before, guest_after;
static unsigned callbacks;
static uint64_t (*nested_entry)(uint64_t *);
static void save_image(struct image *image)
{
  __asm__ volatile("fxsave64 %0" : "=m" (*image));
}
static int relevant_equal(const struct image *a, const struct image *b)
{
  /* Reserved bytes and the padding of each80-bit register are unspecified. */
  if (memcmp(a->bytes, b->bytes, 5) || memcmp(a->bytes + 6, b->bytes + 6, 26)) return 0;
  for (unsigned i = 0; i < 8; ++i)
    if (memcmp(a->bytes + 32 + 16*i, b->bytes + 32 + 16*i, 10)) return 0;
  return 1;
}
static void require(int condition, const char *message)
{
  if (!condition) { fprintf(stderr, "native FPU boundary: %s\n", message); exit(1); }
}
static NATIVE_FPU_BODY uint64_t native_reference_service_body(uint64_t *context)
{
  save_image(&callback_host);
  ++callbacks;
  /* An ordinary C callback can reenter a separate native task. */
  uint64_t nested_context[NATIVE_FPU_CONTEXT_WORDS] = {0};
  save_image(&nested_before);
  uint64_t bits = native_fpu_enter(nested_entry, nested_context);
  save_image(&nested_after);
  if (bits != 17 || !relevant_equal(&nested_before, &nested_after)) abort();
  context[6] = 1;
  return 19;
}
static NATIVE_FPU_BODY uint64_t native_source_callback_body(uint64_t *context)
{
  /* Model the existing captured-exception convention: a status return, not
     a C longjmp through generated frames. End-to-end OCaml tests cover capture. */
  uint64_t result = native_reference_service_body(context);
  context[0] = 29;
  return result;
}
static NATIVE_FPU_BODY uint64_t native_option_callback_body(uint64_t *context,
                                                           uint64_t index, uint64_t value)
{
  if (index != 7 || value != 2) abort();
  return native_reference_service_body(context);
}

uint64_t probe(uint64_t (*entry)(uint64_t *), uint64_t *context,
               struct image *before, struct image *after);
/* This test-only harness uses no C while hostile state is installed. It saves
   its surrounding FP image, installs rounding-down with24-bit precision, sticky invalid and three
   nonempty x87 stack entries, invokes production entry, snapshots, restores. */
#if defined(_WIN32)
#define P_ARGS "movq %rcx,544(%rsp)\nmovq %rdx,552(%rsp)\nmovq %r8,560(%rsp)\nmovq %r9,568(%rsp)\n"
#define P_CALL "movq 544(%rsp),%rcx\nmovq 552(%rsp),%rdx\n"
#else
#define P_ARGS "movq %rdi,544(%rsp)\nmovq %rsi,552(%rsp)\nmovq %rdx,560(%rsp)\nmovq %rcx,568(%rsp)\n"
#define P_CALL "movq 544(%rsp),%rdi\nmovq 552(%rsp),%rsi\n"
#endif
__asm__(".text\n.globl probe\nprobe:\nsubq $600,%rsp\nfxsave64 32(%rsp)\n"
  P_ARGS
  "fninit\nfldz\nfldz\nfdivp\nfld1\nfldpi\n"
  "movw $0x047f,576(%rsp)\nfldcw 576(%rsp)\n"
  "movq 560(%rsp),%r10\nfxsave64 (%r10)\n"
  P_CALL "call native_fpu_enter\n"
  "movq 568(%rsp),%r10\nfxsave64 (%r10)\nfxrstor64 32(%rsp)\n"
  "addq $600,%rsp\nret\n");

static uint64_t (*executable(const unsigned char *bytes, size_t size))(uint64_t *)
{
  void *mapping;
#if defined(_WIN32)
  DWORD old;
  mapping = VirtualAlloc(NULL, 4096, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
  require(mapping != NULL, "allocation");
  memcpy(mapping, bytes, size);
  require(VirtualProtect(mapping, 4096, PAGE_EXECUTE_READ, &old), "RX protection");
  require(FlushInstructionCache(GetCurrentProcess(), mapping, size), "instruction cache");
#else
  mapping = mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  require(mapping != MAP_FAILED, "allocation");
  memcpy(mapping, bytes, size);
  require(mprotect(mapping, 4096, PROT_READ | PROT_EXEC) == 0, "RX protection");
#endif
  uint64_t (*entry)(uint64_t *);
  memcpy(&entry, &mapping, sizeof(entry));
  return entry;
}
static void release(uint64_t (*entry)(uint64_t *))
{
  void *mapping;
  memcpy(&mapping, &entry, sizeof(mapping));
#if defined(_WIN32)
  require(VirtualFree(mapping, 0, MEM_RELEASE), "release");
#else
  require(munmap(mapping, 4096) == 0, "release");
#endif
}
static size_t emit(unsigned char *code, size_t at, const unsigned char *bytes, size_t size)
{
  memcpy(code + at, bytes, size); return at + size;
}
#define EMIT(...) do { const unsigned char bytes[] = {__VA_ARGS__}; n = emit(code,n,bytes,sizeof bytes); } while (0)
static uint64_t (*guest(uintptr_t callback))(uint64_t *)
{
  unsigned char code[128]; size_t n = 0;
  /* Keep the context across the callback outside its32-byte shadow space. */
  EMIT(0x48,0x83,0xec,0x28);
#if defined(_WIN32)
  EMIT(0x48,0x89,0x4c,0x24,0x20); /* mov [rsp+32],rcx */
#else
  EMIT(0x48,0x89,0x7c,0x24,0x20); /* mov [rsp+32],rdi */
#endif
  /* mov rax,[context+8]; fxsave64 [rax] */
#if defined(_WIN32)
  EMIT(0x48,0x8b,0x41,0x08);
#else
  EMIT(0x48,0x8b,0x47,0x08);
#endif
  EMIT(0x48,0x0f,0xae,0x00,0xd9,0xeb,0xd9,0xe8); /* fldpi; fld1 */
#if defined(_WIN32)
  EMIT(0x48,0x8b,0x41,0x10);
#else
  EMIT(0x48,0x8b,0x47,0x10);
#endif
  EMIT(0x48,0x0f,0xae,0x00);
  EMIT(0x48,0xb8); uint64_t address = (uint64_t)(uintptr_t)callback;
  n = emit(code,n,(unsigned char *)&address,8);
#if defined(_WIN32)
  EMIT(0xba,7,0,0,0,0x41,0xb8,2,0,0,0);
#else
  EMIT(0xbe,7,0,0,0,0xba,2,0,0,0);
#endif
  EMIT(0xff,0xd0); /* call rax */
#if defined(_WIN32)
  EMIT(0x48,0x8b,0x4c,0x24,0x20,0x48,0x8b,0x41,0x18);
#else
  EMIT(0x48,0x8b,0x7c,0x24,0x20,0x48,0x8b,0x47,0x18);
#endif
  EMIT(0x48,0x0f,0xae,0x00,0xb8,23,0,0,0,0x48,0x83,0xc4,0x28,0xc3);
  return executable(code,n);
}
static int native_fpu_boundary_run(void)
{
  const unsigned char child[] = {0xd9,0xee,0xd9,0xe8,0xb8,17,0,0,0,0xc3};
  nested_entry = executable(child,sizeof child);
  uintptr_t bridges[] = {(uintptr_t)native_reference_service,(uintptr_t)native_source_callback,
    (uintptr_t)native_option_callback};
  for (unsigned i = 0; i < 3; ++i) {
    uint64_t context[NATIVE_FPU_CONTEXT_WORDS] = {0};
    context[1]=(uintptr_t)&guest_initial; context[2]=(uintptr_t)&guest_before;
    context[3]=(uintptr_t)&guest_after;
    uint64_t (*entry)(uint64_t *) = guest(bridges[i]);
    require(probe(entry,context,&host_before,&host_after)==23,"guest return bits");
    require(relevant_equal(&host_before,&host_after),"host state restoration");
    require(memcmp(host_before.bytes+160,host_after.bytes+160,256)==0,"host XMM restoration");
    require(relevant_equal(&host_before,&callback_host),"callback sees host state");
    require(relevant_equal(&guest_before,&guest_after),"extended guest state across callback");
    require(memcmp(guest_before.bytes+160,guest_after.bytes+160,256)==0,"guest XMM restoration");
    require(guest_initial.bytes[0]==0x7f && guest_initial.bytes[1]==3 &&
      guest_initial.bytes[2]==0 && guest_initial.bytes[3]==0 && guest_initial.bytes[4]==0,
      "guest FNINIT baseline independent of hostile host");
    const unsigned char nearest_pi[10] = {0x35,0xc2,0x68,0x21,0xa2,0xda,0x0f,0xc9,0x00,0x40};
    require(memcmp(guest_before.bytes+48,nearest_pi,10)==0,"guest FLDPI ignores hostile rounding");
    require(context[36]==0,"entry scope slot restored");
    require(context[6]==1 && (i!=1 || context[0]==29),"callback/fault status");
    release(entry);
  }
  require(callbacks==3,"all callback boundaries");
  release(nested_entry);
  puts("native FPU boundary: success, fault return, three bridges and nested entry passed");
  return 0;
}

#else
static int native_fpu_boundary_run(void) { puts("native FPU boundary: unsupported test host, skipped"); return 0; }
#endif

#ifdef HOLYC_OCAML_TEST
#include <caml/mlvalues.h>
CAMLprim value holyc_test_native_fpu_boundary(value unit)
{
  (void)unit;
  (void)native_fpu_boundary_run();
  return Val_unit;
}
#else
int main(void) { return native_fpu_boundary_run(); }
#endif
