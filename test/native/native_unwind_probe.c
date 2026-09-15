#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#if (defined(__x86_64__) || defined(_M_X64)) && \
    !defined(_M_ARM64EC) && UINTPTR_MAX == UINT64_MAX && defined(_WIN32)
#define HOLYC_TEST_WINDOWS_X64 1
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#else
#define HOLYC_TEST_WINDOWS_X64 0
#endif

#if HOLYC_TEST_WINDOWS_X64
static void probe_fail(void *code_mapping, void *stack_mapping,
                       const char *message)
{
  if (stack_mapping != NULL)
    VirtualFree(stack_mapping, 0, MEM_RELEASE);
  if (code_mapping != NULL)
    VirtualFree(code_mapping, 0, MEM_RELEASE);
  caml_failwith(message);
}

static uint32_t read_u32(const unsigned char *bytes)
{
  return (uint32_t)bytes[0]
       | ((uint32_t)bytes[1] << 8)
       | ((uint32_t)bytes[2] << 16)
       | ((uint32_t)bytes[3] << 24);
}

static void check_fixed_image(const unsigned char *code, size_t length,
                              const unsigned char *unwind, size_t unwind_length,
                              uint32_t frame_bytes)
{
  unsigned char expected_op;
  uint16_t scaled;

  if (frame_bytes < 8 || frame_bytes > 4088 || (frame_bytes & 15u) != 8u)
    caml_invalid_argument("unwind probe frame is outside 8..4088 / 8 mod 16");
  if (length < 15 || length > UINT32_MAX)
    caml_invalid_argument("unwind probe code length is outside the checked range");
  if (unwind_length != 8)
    caml_invalid_argument("unwind probe requires the fixed eight-byte blob");
  if (code[0] != 0x48 || code[1] != 0x81 || code[2] != 0xec ||
      read_u32(code + 3) != frame_bytes)
    caml_invalid_argument("unwind probe image has an unexpected stack prologue");
  if (code[length - 8] != 0x48 || code[length - 7] != 0x81 ||
      code[length - 6] != 0xc4 || read_u32(code + length - 5) != frame_bytes ||
      code[length - 1] != 0xc3)
    caml_invalid_argument("unwind probe image has an unexpected stack epilogue");
  if (unwind[0] != 0x01 || unwind[1] != 0x07 || unwind[3] != 0x00 ||
      unwind[4] != 0x07)
    caml_invalid_argument("unwind probe metadata has an unexpected header");

  if (frame_bytes <= 128) {
    expected_op = (unsigned char)((((frame_bytes - 8u) / 8u) << 4) | 0x02u);
    if (unwind[2] != 0x01 || unwind[5] != expected_op ||
        unwind[6] != 0x00 || unwind[7] != 0x00)
      caml_invalid_argument("unwind probe metadata is not UWOP_ALLOC_SMALL");
  } else {
    scaled = (uint16_t)(frame_bytes / 8u);
    if (unwind[2] != 0x02 || unwind[5] != 0x01 ||
        unwind[6] != (unsigned char)(scaled & 0xffu) ||
        unwind[7] != (unsigned char)(scaled >> 8))
      caml_invalid_argument("unwind probe metadata is not scaled UWOP_ALLOC_LARGE");
  }
}

static void initialize_context(CONTEXT *context, DWORD64 rip, DWORD64 rsp)
{
  memset(context, 0, sizeof(*context));
  context->ContextFlags = CONTEXT_FULL;
  context->Rip = rip;
  context->Rsp = rsp;
  context->Rbx = UINT64_C(0x1111222233334444);
  context->Rbp = UINT64_C(0x2222333344445555);
  context->Rsi = UINT64_C(0x3333444455556666);
  context->Rdi = UINT64_C(0x4444555566667777);
  context->R12 = UINT64_C(0x5555666677778888);
  context->R13 = UINT64_C(0x6666777788889999);
  context->R14 = UINT64_C(0x777788889999aaaa);
  context->R15 = UINT64_C(0x88889999aaaabbbb);
}

static int nonvolatile_registers_preserved(const CONTEXT *context)
{
  return context->Rbx == UINT64_C(0x1111222233334444)
      && context->Rbp == UINT64_C(0x2222333344445555)
      && context->Rsi == UINT64_C(0x3333444455556666)
      && context->Rdi == UINT64_C(0x4444555566667777)
      && context->R12 == UINT64_C(0x5555666677778888)
      && context->R13 == UINT64_C(0x6666777788889999)
      && context->R14 == UINT64_C(0x777788889999aaaa)
      && context->R15 == UINT64_C(0x88889999aaaabbbb);
}

static void unwind_at(void *code_mapping, void *stack_mapping,
                      PRUNTIME_FUNCTION function_entry, size_t code_length,
                      uint32_t frame_bytes, size_t offset, DWORD64 rsp,
                      DWORD64 entry_rsp, DWORD64 return_rip)
{
  CONTEXT context;
  KNONVOLATILE_CONTEXT_POINTERS pointers;
  PVOID handler_data = NULL;
  DWORD64 establisher_frame = 0;
  PEXCEPTION_ROUTINE handler;
  const DWORD64 image_base = (DWORD64)(uintptr_t)code_mapping;

  if (offset >= code_length)
    probe_fail(code_mapping, stack_mapping, "unwind probe PC is outside the image");
  initialize_context(&context, image_base + (DWORD64)offset, rsp);
  memset(&pointers, 0, sizeof(pointers));
  handler = RtlVirtualUnwind((DWORD)0, image_base, context.Rip,
                             function_entry, &context, &handler_data,
                             &establisher_frame, &pointers);
  if (handler != NULL)
    probe_fail(code_mapping, stack_mapping,
               "unwind probe unexpectedly resolved an exception handler");
  if (context.Rip != return_rip || context.Rsp != entry_rsp + 8u)
    probe_fail(code_mapping, stack_mapping,
               "RtlVirtualUnwind did not restore the caller RIP/RSP");
  if (!nonvolatile_registers_preserved(&context))
    probe_fail(code_mapping, stack_mapping,
               "RtlVirtualUnwind changed a nonvolatile register");
  (void)frame_bytes;
}
#endif

CAMLprim value holyc_test_native_unwind(value code_value, value unwind_value,
                                        value frame_value)
{
  CAMLparam3(code_value, unwind_value, frame_value);
#if HOLYC_TEST_WINDOWS_X64
  const mlsize_t code_length_ml = caml_string_length(code_value);
  const mlsize_t unwind_length_ml = caml_string_length(unwind_value);
  const intnat frame_signed = Long_val(frame_value);
  const size_t code_length = (size_t)code_length_ml;
  const size_t unwind_length = (size_t)unwind_length_ml;
  const uint32_t frame_bytes = (uint32_t)frame_signed;
  const unsigned char *code = (const unsigned char *)String_val(code_value);
  const unsigned char *unwind = (const unsigned char *)String_val(unwind_value);
  const size_t unwind_offset = (code_length + 3u) & ~(size_t)3u;
  const size_t mapping_length = unwind_offset + unwind_length;
  const size_t stack_length = 16u * 1024u;
  unsigned char *code_mapping = NULL;
  unsigned char *stack_mapping = NULL;
  unsigned char *entry_stack;
  DWORD64 entry_rsp;
  DWORD64 return_rip = UINT64_C(0x0000123456789abc);
  RUNTIME_FUNCTION function_entry;
  size_t epilogue_offset;

  if (frame_signed < 0)
    caml_invalid_argument("unwind probe frame cannot be negative");
  check_fixed_image(code, code_length, unwind, unwind_length, frame_bytes);
  if (unwind_offset > UINT32_MAX || mapping_length < unwind_offset)
    caml_invalid_argument("unwind probe metadata offset is outside RUNTIME_FUNCTION");

  /* The mapping remains writable and non-executable for the entire probe. The
     copied instructions are inspected by RtlVirtualUnwind only; they are never
     invoked as machine code. */
  code_mapping = (unsigned char *)VirtualAlloc(NULL, mapping_length,
                                                MEM_RESERVE | MEM_COMMIT,
                                                PAGE_READWRITE);
  if (code_mapping == NULL)
    caml_failwith("unwind probe could not allocate the RW image mapping");
  memcpy(code_mapping, code, code_length);
  memcpy(code_mapping + unwind_offset, unwind, unwind_length);

  stack_mapping = (unsigned char *)VirtualAlloc(NULL, stack_length,
                                                 MEM_RESERVE | MEM_COMMIT,
                                                 PAGE_READWRITE);
  if (stack_mapping == NULL)
    probe_fail(code_mapping, NULL,
               "unwind probe could not allocate the synthetic stack");
  /* VirtualAlloc is page aligned. +8192+8 has the Windows entry alignment and
     leaves more than the maximum 4088-byte frame mapped below the body RSP. */
  entry_stack = stack_mapping + (8u * 1024u) + 8u;
  entry_rsp = (DWORD64)(uintptr_t)entry_stack;
  memcpy(entry_stack, &return_rip, sizeof(return_rip));

  function_entry.BeginAddress = 0;
  function_entry.EndAddress = (DWORD)code_length;
  function_entry.UnwindData = (DWORD)unwind_offset;
  epilogue_offset = code_length - 8u;

  /* At the function start the SUB has not executed. Offset 7 is the first body
     byte after the real prologue. At the epilogue start the ADD has not executed;
     at RET it has. Each state must unwind to the same synthetic caller. */
  unwind_at(code_mapping, stack_mapping, &function_entry, code_length, frame_bytes,
            0u, entry_rsp, entry_rsp, return_rip);
  unwind_at(code_mapping, stack_mapping, &function_entry, code_length, frame_bytes,
            7u, entry_rsp - frame_bytes, entry_rsp, return_rip);
  unwind_at(code_mapping, stack_mapping, &function_entry, code_length, frame_bytes,
            epilogue_offset, entry_rsp - frame_bytes, entry_rsp, return_rip);
  unwind_at(code_mapping, stack_mapping, &function_entry, code_length, frame_bytes,
            code_length - 1u, entry_rsp, entry_rsp, return_rip);

  if (!VirtualFree(stack_mapping, 0, MEM_RELEASE))
    probe_fail(code_mapping, NULL,
               "unwind probe could not release the synthetic stack");
  stack_mapping = NULL;
  if (!VirtualFree(code_mapping, 0, MEM_RELEASE))
    caml_failwith("unwind probe could not release the RW image mapping");
#else
  (void)code_value;
  (void)unwind_value;
  (void)frame_value;
  caml_failwith("Windows x64 unwind probe called on a non-Windows-x64 host");
#endif
  CAMLreturn(Val_unit);
}
