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

static const char *check_callable_owner(const unsigned char *code,
                                        size_t code_length,
                                        size_t begin, size_t end,
                                        const unsigned char *unwind,
                                        size_t unwind_length,
                                        uint32_t *frame_bytes,
                                        size_t *body_offset,
                                        size_t *epilogue_offset)
{
  uint32_t frame = 0;
  size_t body;
  size_t epilogue;

  if (begin >= end || end > code_length || end - begin < 6u)
    return "callable unwind owner range is invalid";
  if (code[begin] != 0x55u || code[begin + 1u] != 0x48u ||
      code[begin + 2u] != 0x89u || code[begin + 3u] != 0xe5u)
    return "callable owner does not begin with PUSH RBP; MOV RBP,RSP";

  body = begin + 4u;
  if (end - body >= 7u && code[body] == 0x48u && code[body + 1u] == 0x81u &&
      code[body + 2u] == 0xecu) {
    frame = read_u32(code + body + 3u);
    if (frame == 0u || (frame & 15u) != 0u || frame > 4088u)
      return "callable owner has an invalid fixed stack allocation";
    body += 7u;
  }

  if (frame == 0u) {
    if (end - begin < 6u || code[end - 2u] != 0x5du || code[end - 1u] != 0xc3u)
      return "frameless callable owner does not end with POP RBP; RET";
    epilogue = end - 2u;
    if (unwind_length != 8u || unwind[0] != 0x01u || unwind[1] != 0x04u ||
        unwind[2] != 0x01u || unwind[3] != 0x00u || unwind[4] != 0x01u ||
        unwind[5] != 0x50u || unwind[6] != 0x00u || unwind[7] != 0x00u)
      return "frameless callable owner has incorrect PUSH_NONVOL unwind metadata";
  } else {
    unsigned char expected_small =
        (unsigned char)((((frame / 8u) - 1u) << 4) | 0x02u);
    if (end - begin < 13u || code[end - 9u] != 0x48u ||
        code[end - 8u] != 0x81u || code[end - 7u] != 0xc4u ||
        read_u32(code + end - 6u) != frame || code[end - 2u] != 0x5du ||
        code[end - 1u] != 0xc3u)
      return "frameful callable owner has an incorrect epilogue";
    epilogue = end - 9u;
    if (unwind[0] != 0x01u || unwind[1] != 0x0bu || unwind[3] != 0x00u ||
        unwind[4] != 0x0bu)
      return "frameful callable owner has an incorrect unwind header";
    if (frame <= 128u) {
      if (unwind_length != 8u || unwind[2] != 0x02u ||
          unwind[5] != expected_small || unwind[6] != 0x01u ||
          unwind[7] != 0x50u)
        return "small callable frame has incorrect unwind operations";
    } else {
      uint16_t scaled = (uint16_t)(frame / 8u);
      if (unwind_length != 12u || unwind[2] != 0x03u || unwind[5] != 0x01u ||
          unwind[6] != (unsigned char)(scaled & 0xffu) ||
          unwind[7] != (unsigned char)(scaled >> 8) || unwind[8] != 0x01u ||
          unwind[9] != 0x50u || unwind[10] != 0x00u || unwind[11] != 0x00u)
        return "large callable frame has incorrect unwind operations";
    }
  }

  *frame_bytes = frame;
  *body_offset = body;
  *epilogue_offset = epilogue;
  return NULL;
}

static const char *unwind_callable_at(void *code_mapping,
                                      PRUNTIME_FUNCTION function_entry,
                                      DWORD64 rip, DWORD64 rsp, DWORD64 rbp,
                                      DWORD64 entry_rsp, DWORD64 return_rip)
{
  CONTEXT context;
  KNONVOLATILE_CONTEXT_POINTERS pointers;
  PVOID handler_data = NULL;
  DWORD64 establisher_frame = 0;
  PEXCEPTION_ROUTINE handler;
  const DWORD64 image_base = (DWORD64)(uintptr_t)code_mapping;

  initialize_context(&context, rip, rsp);
  context.Rbp = rbp;
  memset(&pointers, 0, sizeof(pointers));
  handler = RtlVirtualUnwind((DWORD)0, image_base, context.Rip, function_entry,
                             &context, &handler_data, &establisher_frame,
                             &pointers);
  if (handler != NULL)
    return "callable virtual unwind unexpectedly resolved an exception handler";
  if (context.Rip != return_rip || context.Rsp != entry_rsp + 8u)
    return "callable virtual unwind did not restore caller RIP/RSP";
  if (!nonvolatile_registers_preserved(&context))
    return "callable virtual unwind did not restore nonvolatile registers";
  return NULL;
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

CAMLprim value holyc_test_native_program_unwind(value code_value,
                                                value functions_value)
{
  CAMLparam2(code_value, functions_value);
#if HOLYC_TEST_WINDOWS_X64
  const mlsize_t code_length_ml = caml_string_length(code_value);
  const size_t code_length = (size_t)code_length_ml;
  const unsigned char *code = (const unsigned char *)String_val(code_value);
  const mlsize_t function_count =
      Is_block(functions_value) ? Wosize_val(functions_value) : 0u;
  size_t metadata_offset = (code_length + 3u) & ~(size_t)3u;
  size_t unwind_end = metadata_offset;
  size_t table_offset;
  size_t mapping_length;
  unsigned char *mapping = NULL;
  unsigned char *stack_mapping = NULL;
  PRUNTIME_FUNCTION table = NULL;
  int registered = 0;
  const char *failure = NULL;
  mlsize_t index;

  if (!Is_block(functions_value) || Tag_val(functions_value) != 0 ||
      function_count == 0u)
    caml_invalid_argument("callable unwind probe requires a nonempty descriptor array");
  if (code_length == 0u || code_length > UINT32_MAX)
    caml_invalid_argument("callable unwind probe code length is invalid");

  for (index = 0; index < function_count; ++index) {
    value descriptor = Field(functions_value, index);
    value begin_value;
    value end_value;
    value unwind_value;
    intnat begin;
    intnat end;
    size_t unwind_length;
    if (!Is_block(descriptor) || Tag_val(descriptor) != 0 ||
        Wosize_val(descriptor) != 3)
      caml_invalid_argument("callable unwind descriptor is malformed");
    begin_value = Field(descriptor, 0);
    end_value = Field(descriptor, 1);
    unwind_value = Field(descriptor, 2);
    if (!Is_long(begin_value) || !Is_long(end_value) || !Is_block(unwind_value) ||
        Tag_val(unwind_value) != String_tag)
      caml_invalid_argument("callable unwind descriptor has invalid fields");
    begin = Long_val(begin_value);
    end = Long_val(end_value);
    if (begin < 0 || end <= begin || (uintnat)end > (uintnat)code_length ||
        (index == 0u && begin != 0) ||
        (index != 0u && begin != Long_val(Field(Field(functions_value, index - 1u), 1))))
      caml_invalid_argument("callable unwind ranges are not contiguous and valid");
    unwind_length = (size_t)caml_string_length(unwind_value);
    if (unwind_length != 8u && unwind_length != 12u)
      caml_invalid_argument("callable unwind metadata has an unexpected size");
    if (unwind_length > SIZE_MAX - unwind_end)
      caml_invalid_argument("callable unwind metadata size overflow");
    unwind_end += unwind_length;
  }
  if ((uintnat)Long_val(Field(Field(functions_value, function_count - 1u), 1)) !=
      (uintnat)code_length)
    caml_invalid_argument("callable unwind ranges do not cover the image tail");

  table_offset = (unwind_end + 3u) & ~(size_t)3u;
  if ((size_t)function_count > (SIZE_MAX - table_offset) / sizeof(RUNTIME_FUNCTION))
    caml_invalid_argument("callable unwind function table size overflow");
  mapping_length = table_offset + ((size_t)function_count * sizeof(RUNTIME_FUNCTION));
  mapping = (unsigned char *)VirtualAlloc(NULL, mapping_length,
                                          MEM_RESERVE | MEM_COMMIT,
                                          PAGE_READWRITE);
  if (mapping == NULL)
    caml_failwith("callable unwind probe could not allocate image mapping");
  memcpy(mapping, code, code_length);
  table = (PRUNTIME_FUNCTION)(mapping + table_offset);
  unwind_end = metadata_offset;
  for (index = 0; index < function_count; ++index) {
    value descriptor = Field(functions_value, index);
    value unwind_value = Field(descriptor, 2);
    size_t unwind_length = (size_t)caml_string_length(unwind_value);
    memcpy(mapping + unwind_end, String_val(unwind_value), unwind_length);
    table[index].BeginAddress = (DWORD)Long_val(Field(descriptor, 0));
    table[index].EndAddress = (DWORD)Long_val(Field(descriptor, 1));
    table[index].UnwindData = (DWORD)unwind_end;
    unwind_end += unwind_length;
  }
  if (!RtlAddFunctionTable(table, (DWORD)function_count,
                           (DWORD64)(uintptr_t)mapping)) {
    VirtualFree(mapping, 0, MEM_RELEASE);
    caml_failwith("callable unwind probe could not register the full function table");
  }
  registered = 1;

  stack_mapping = (unsigned char *)VirtualAlloc(NULL, 16u * 1024u,
                                                MEM_RESERVE | MEM_COMMIT,
                                                PAGE_READWRITE);
  if (stack_mapping == NULL)
    failure = "callable unwind probe could not allocate synthetic stack";

  for (index = 0; failure == NULL && index < function_count; ++index) {
    value descriptor = Field(functions_value, index);
    value unwind_value = Field(descriptor, 2);
    size_t begin = (size_t)Long_val(Field(descriptor, 0));
    size_t end = (size_t)Long_val(Field(descriptor, 1));
    uint32_t frame_bytes = 0;
    size_t body_offset = 0;
    size_t epilogue_offset = 0;
    unsigned char *entry_stack = stack_mapping + (8u * 1024u) + 8u;
    DWORD64 entry_rsp = (DWORD64)(uintptr_t)entry_stack;
    DWORD64 saved_rbp = UINT64_C(0x2222333344445555);
    DWORD64 return_rip = UINT64_C(0x0000123456789abc) + (DWORD64)index;
    DWORD64 image_base = (DWORD64)(uintptr_t)mapping;
    DWORD64 lookup_base = 0;
    PRUNTIME_FUNCTION found;

    failure = check_callable_owner(
        code, code_length, begin, end,
        (const unsigned char *)String_val(unwind_value),
        (size_t)caml_string_length(unwind_value), &frame_bytes, &body_offset,
        &epilogue_offset);
    if (failure != NULL)
      break;
    memcpy(entry_stack - 8u, &saved_rbp, sizeof(saved_rbp));
    memcpy(entry_stack, &return_rip, sizeof(return_rip));
    found = RtlLookupFunctionEntry(image_base + (DWORD64)body_offset,
                                   &lookup_base, NULL);
    if (found == NULL || lookup_base != image_base ||
        found->BeginAddress != table[index].BeginAddress ||
        found->EndAddress != table[index].EndAddress ||
        found->UnwindData != table[index].UnwindData) {
      failure = "RtlLookupFunctionEntry did not return the registered owner";
      break;
    }
    failure = unwind_callable_at(
        mapping, found, image_base + (DWORD64)body_offset,
        entry_rsp - 8u - (DWORD64)frame_bytes, entry_rsp - 8u, entry_rsp,
        return_rip);
    if (failure != NULL)
      break;
    failure = unwind_callable_at(
        mapping, found, image_base + (DWORD64)epilogue_offset,
        entry_rsp - 8u - (DWORD64)frame_bytes, entry_rsp - 8u, entry_rsp,
        return_rip);
  }

  if (registered && !RtlDeleteFunctionTable(table)) {
    registered = 0;
    if (failure == NULL)
      failure = "callable unwind probe could not remove the function table";
    /* Retain the mapping if Windows still owns the table. */
    mapping = NULL;
  } else {
    registered = 0;
  }
  if (stack_mapping != NULL && !VirtualFree(stack_mapping, 0, MEM_RELEASE) &&
      failure == NULL)
    failure = "callable unwind probe could not release the synthetic stack";
  if (mapping != NULL && !VirtualFree(mapping, 0, MEM_RELEASE) && failure == NULL)
    failure = "callable unwind probe could not release the image mapping";
  if (failure != NULL)
    caml_failwith(failure);
#else
  (void)code_value;
  (void)functions_value;
  caml_failwith("callable Windows unwind probe called on a non-Windows-x64 host");
#endif
  CAMLreturn(Val_unit);
}
