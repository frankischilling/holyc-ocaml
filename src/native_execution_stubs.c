/* Only the host execution boundary lives in C. Instruction selection,
   verification, register allocation and byte encoding belong to OCaml. */
#ifndef _DEFAULT_SOURCE
#define _DEFAULT_SOURCE 1
#endif

#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>

#if (defined(__x86_64__) || defined(_M_X64)) && \
    !defined(_M_ARM64EC) && UINTPTR_MAX == UINT64_MAX
#define HOLYC_NATIVE_X64 1
#else
#define HOLYC_NATIVE_X64 0
#endif

#if HOLYC_NATIVE_X64 && defined(_WIN32)
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#define HOLYC_NATIVE_PLATFORM 1
#elif HOLYC_NATIVE_X64 && defined(__linux__)
#include <errno.h>
#include <sys/mman.h>
#include <sys/personality.h>
#define HOLYC_NATIVE_PLATFORM 2
#else
#define HOLYC_NATIVE_PLATFORM 0
#endif

CAMLprim value holyc_native_platform(value unit)
{
  CAMLparam1(unit);
  CAMLreturn(Val_int(HOLYC_NATIVE_PLATFORM));
}

#if HOLYC_NATIVE_PLATFORM != 0
static void native_os_error(const char *operation, unsigned long error)
{
  char message[160];
  snprintf(message, sizeof(message), "native %s failed (OS error %lu)",
           operation, error);
  caml_failwith(message);
}
#endif

#if HOLYC_NATIVE_PLATFORM == 1
static void native_windows_mapping_error(void *mapping, const char *operation,
                                         DWORD error)
{
  if (!VirtualFree(mapping, 0, MEM_RELEASE)) {
    DWORD release_error = GetLastError();
    char message[240];
    snprintf(message, sizeof(message),
             "native %s failed (OS error %lu); release also failed (OS error %lu)",
             operation, (unsigned long)error, (unsigned long)release_error);
    caml_failwith(message);
  }
  native_os_error(operation, (unsigned long)error);
}
#endif

#if HOLYC_NATIVE_PLATFORM != 0
/* Both sealed image kinds use the same mapping and unwind lifetime. The caller
   roots the input strings and owns a stack-local, image-specific context. This
   helper returns only after teardown. Cleanup failure may retain a mapping
   while raising; no successful result is boxed before its mapping is released. */
static uint64_t native_execute_checked_image(value code, value unwind,
                                            intnat abi_code, uint64_t *context)
{
  const mlsize_t length = caml_string_length(code);
  const mlsize_t unwind_length = caml_string_length(unwind);
  void *mapping;
  uint64_t (*entry)(uint64_t *);
  uint64_t bits;

  /* Zero denotes an ABI-neutral image that does not use the argument. Repeat
     the ML-side check here before any executable-memory operation. */
  if (abi_code != 0 && abi_code != HOLYC_NATIVE_PLATFORM)
    caml_invalid_argument("native image status ABI does not match this process");
  /* This is a host allocation bound, independent of the OCaml image's tighter
     caller-selected code quota. No arbitrary byte executor is exported in ML. */
  if (length == 0 || length > 16u * 1024u * 1024u)
    caml_invalid_argument("native image length is outside the host allocation bound");
  if (unwind_length != 0 && unwind_length != 8)
    caml_invalid_argument("native unwind image has an unsupported length");
  if (sizeof(entry) != sizeof(mapping))
    caml_failwith("native function pointers do not match this host's address size");

#if HOLYC_NATIVE_PLATFORM == 1
  DWORD previous_protection;
  PRUNTIME_FUNCTION function_table = NULL;
  const SIZE_T unwind_offset = ((SIZE_T)length + 3u) & ~(SIZE_T)3u;
  const SIZE_T table_offset = unwind_offset + (SIZE_T)unwind_length;
  /* Code is already bounded at 16 MiB; the fixed metadata and alignment add
     at most 23 bytes. Keep the table in the mapping so even a failed removal
     cannot leave Windows pointing at a returned C stack frame. */
  const SIZE_T mapping_length = unwind_length == 0 ? (SIZE_T)length
    : table_offset + sizeof(RUNTIME_FUNCTION);
  mapping = VirtualAlloc(NULL, mapping_length, MEM_RESERVE | MEM_COMMIT,
                         PAGE_READWRITE);
  if (mapping == NULL)
    native_os_error("allocation", (unsigned long)GetLastError());
  memcpy(mapping, String_val(code), (size_t)length);
  if (unwind_length != 0) {
    memcpy((char *)mapping + unwind_offset, String_val(unwind),
           (size_t)unwind_length);
    function_table = (PRUNTIME_FUNCTION)((char *)mapping + table_offset);
    function_table->BeginAddress = 0;
    function_table->EndAddress = (DWORD)length;
    function_table->UnwindData = (DWORD)unwind_offset;
  }
  if (!VirtualProtect(mapping, mapping_length, PAGE_EXECUTE_READ,
                      &previous_protection)) {
    DWORD error = GetLastError();
    native_windows_mapping_error(mapping, "RX protection", error);
  }
  if (!FlushInstructionCache(GetCurrentProcess(), mapping, (SIZE_T)length)) {
    DWORD error = GetLastError();
    native_windows_mapping_error(mapping, "instruction-cache synchronization",
                                 error);
  }
  if (function_table != NULL &&
      !RtlAddFunctionTable(function_table, 1, (DWORD64)(uintptr_t)mapping)) {
    /* RtlAddFunctionTable returns a Boolean, not a documented LastError. */
    if (!VirtualFree(mapping, 0, MEM_RELEASE))
      native_os_error("release after unwind registration failure",
                      (unsigned long)GetLastError());
    caml_failwith("native unwind registration failed");
  }
#else
  /* READ_IMPLIES_EXEC would turn the writable staging mapping executable. */
  int personality_flags = personality(0xffffffffUL);
  if (personality_flags == -1)
    native_os_error("personality query", (unsigned long)errno);
  if ((personality_flags & READ_IMPLIES_EXEC) != 0)
    caml_failwith("native execution requires READ_IMPLIES_EXEC to be disabled");
  mapping = mmap(NULL, (size_t)length, PROT_READ | PROT_WRITE,
                 MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (mapping == MAP_FAILED)
    native_os_error("allocation", (unsigned long)errno);
  memcpy(mapping, String_val(code), (size_t)length);
  if (mprotect(mapping, (size_t)length, PROT_READ | PROT_EXEC) != 0) {
    int error = errno;
    munmap(mapping, (size_t)length);
    native_os_error("RX protection", (unsigned long)error);
  }
  __builtin___clear_cache((char *)mapping, (char *)mapping + length);
#endif

  /* Context-bearing images reserve volatile R11 for their private pointer.
     ABI-neutral expressions ignore the argument and retain their old bytes.
     Programs meter every reached IR instruction, including loop transfers, in
     generated code. Neither image kind calls OCaml or waits on host services.
     Keep the runtime lock so an OCaml callback or blocking-section transition
     cannot raise while the mapping or its registered unwind table is live. */
  memcpy(&entry, &mapping, sizeof(entry));
  bits = entry(context);

#if HOLYC_NATIVE_PLATFORM == 1
  if (function_table != NULL && !RtlDeleteFunctionTable(function_table))
    caml_failwith("native unwind removal failed; registered mapping retained");
  if (!VirtualFree(mapping, 0, MEM_RELEASE))
    native_os_error("release", (unsigned long)GetLastError());
#else
  if (munmap(mapping, (size_t)length) != 0)
    native_os_error("release", (unsigned long)errno);
#endif

  return bits;
}

static value native_box_word(uint64_t word)
{
  int64_t signed_word;
  memcpy(&signed_word, &word, sizeof(signed_word));
  return caml_copy_int64(signed_word);
}
#endif

CAMLprim value holyc_native_execute_image(value code, value unwind, value abi)
{
  CAMLparam3(code, unwind, abi);
#if HOLYC_NATIVE_PLATFORM == 0
  caml_failwith("native execution requires Windows or Linux x86-64 with 64-bit pointers");
#else
  CAMLlocal4(result, boxed_bits, boxed_kind, boxed_site);
  uint64_t status[2] = {0, 0};
  const uint64_t bits =
    native_execute_checked_image(code, unwind, Long_val(abi), status);

  /* Do not narrow through C long, an OCaml int, a JSON number or an exit code. */
  /* The mapping and its function table are gone before the first allocation,
     including on the checked arithmetic-fault path. */
  boxed_bits = native_box_word(bits);
  boxed_kind = native_box_word(status[0]);
  boxed_site = native_box_word(status[1]);
  result = caml_alloc_tuple(3);
  Store_field(result, 0, boxed_bits);
  Store_field(result, 1, boxed_kind);
  Store_field(result, 2, boxed_site);
  CAMLreturn(result);
#endif
  CAMLreturn(Val_unit); /* unreachable on the unsupported host path */
}

CAMLprim value holyc_native_execute_program(value code, value unwind, value abi,
                                           value max_steps)
{
  CAMLparam4(code, unwind, abi, max_steps);
#if HOLYC_NATIVE_PLATFORM == 0
  caml_failwith("native execution requires Windows or Linux x86-64 with 64-bit pointers");
#else
  CAMLlocal5(boxed_kind, boxed_site, boxed_steps, boxed_value_site, boxed_bits);
  CAMLlocal1(result);
  const intnat step_limit = Long_val(max_steps);
  const intnat abi_code = Long_val(abi);
  /* The program ABI is always explicit; an expression's ABI-neutral zero is
     never accepted for a program context. Validate before mapping allocation. */
  if (abi_code != HOLYC_NATIVE_PLATFORM)
    caml_invalid_argument("native program status ABI does not match this process");
  if (step_limit <= 0)
    caml_invalid_argument("native program max_steps must be greater than zero");

  /* kind, fault site, budget, executed steps, last END_EXP site, result bits.
     The checked image owns the type associated with an END_EXP site. C does
     not guess a result type or interpret the program's instructions. */
  uint64_t context[6] = {0, 0, (uint64_t)step_limit, 0, 0, 0};
  (void)native_execute_checked_image(code, unwind, abi_code, context);
  if (context[2] != (uint64_t)step_limit)
    caml_failwith("native program status integrity failure: budget was modified");

  /* All five words are boxed only after executable-memory and unwind teardown,
     including exhausted-loop and guarded arithmetic-fault paths. */
  boxed_kind = native_box_word(context[0]);
  boxed_site = native_box_word(context[1]);
  boxed_steps = native_box_word(context[3]);
  boxed_value_site = native_box_word(context[4]);
  boxed_bits = native_box_word(context[5]);
  result = caml_alloc_tuple(5);
  Store_field(result, 0, boxed_kind);
  Store_field(result, 1, boxed_site);
  Store_field(result, 2, boxed_steps);
  Store_field(result, 3, boxed_value_site);
  Store_field(result, 4, boxed_bits);
  CAMLreturn(result);
#endif
  CAMLreturn(Val_unit);
}
