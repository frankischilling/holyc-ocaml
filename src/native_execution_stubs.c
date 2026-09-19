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

static uint64_t native_windows_execute_mapping(void *mapping,
                                              SIZE_T mapping_length,
                                              SIZE_T code_length,
                                              PRUNTIME_FUNCTION function_table,
                                              DWORD function_count,
                                              uint64_t *context)
{
  DWORD previous_protection;
  uint64_t (*entry)(uint64_t *);
  uint64_t bits;

  if (sizeof(entry) != sizeof(mapping)) {
    if (!VirtualFree(mapping, 0, MEM_RELEASE))
      native_os_error("release after function-pointer check failure",
                      (unsigned long)GetLastError());
    caml_failwith("native function pointers do not match this host's address size");
  }
  if ((function_count == 0) != (function_table == NULL)) {
    if (!VirtualFree(mapping, 0, MEM_RELEASE))
      native_os_error("release after unwind table check failure",
                      (unsigned long)GetLastError());
    caml_failwith("native unwind table metadata is inconsistent");
  }
  if (!VirtualProtect(mapping, mapping_length, PAGE_EXECUTE_READ,
                      &previous_protection)) {
    DWORD error = GetLastError();
    native_windows_mapping_error(mapping, "RX protection", error);
  }
  if (!FlushInstructionCache(GetCurrentProcess(), mapping, code_length)) {
    DWORD error = GetLastError();
    native_windows_mapping_error(mapping, "instruction-cache synchronization",
                                 error);
  }
  if (function_count != 0 &&
      !RtlAddFunctionTable(function_table, function_count,
                           (DWORD64)(uintptr_t)mapping)) {
    /* RtlAddFunctionTable returns a Boolean, not a documented LastError. */
    if (!VirtualFree(mapping, 0, MEM_RELEASE))
      native_os_error("release after unwind registration failure",
                      (unsigned long)GetLastError());
    caml_failwith("native unwind registration failed");
  }

  memcpy(&entry, &mapping, sizeof(entry));
  bits = entry(context);

  if (function_count != 0 && !RtlDeleteFunctionTable(function_table))
    caml_failwith("native unwind removal failed; registered mapping retained");
  if (!VirtualFree(mapping, 0, MEM_RELEASE))
    native_os_error("release", (unsigned long)GetLastError());
  return bits;
}

static void native_windows_storage_mapping_error(void *mapping, void *arena,
                                                 const char *operation,
                                                 DWORD error)
{
  DWORD arena_error = 0;
  DWORD mapping_error = 0;
  char message[320];

  if (arena != NULL && !VirtualFree(arena, 0, MEM_RELEASE))
    arena_error = GetLastError();
  if (mapping != NULL && !VirtualFree(mapping, 0, MEM_RELEASE))
    mapping_error = GetLastError();
  if (arena_error != 0 || mapping_error != 0) {
    snprintf(message, sizeof(message),
             "native %s failed (OS error %lu); arena release error %lu; code release error %lu",
             operation, (unsigned long)error, (unsigned long)arena_error,
             (unsigned long)mapping_error);
    caml_failwith(message);
  }
  native_os_error(operation, (unsigned long)error);
}

static uint64_t native_windows_execute_mapping_storage(
  void *mapping, SIZE_T mapping_length, SIZE_T code_length,
  PRUNTIME_FUNCTION function_table, DWORD function_count, value arena_image,
  uint64_t *context)
{
  const SIZE_T arena_length = (SIZE_T)caml_string_length(arena_image);
  DWORD previous_protection;
  uint64_t (*entry)(uint64_t *);
  uint64_t bits;
  void *arena;
  int pointer_ok;

  if (sizeof(entry) != sizeof(mapping)) {
    if (!VirtualFree(mapping, 0, MEM_RELEASE))
      native_os_error("release after function-pointer check failure",
                      (unsigned long)GetLastError());
    caml_failwith("native function pointers do not match this host's address size");
  }
  if ((function_count == 0) != (function_table == NULL)) {
    if (!VirtualFree(mapping, 0, MEM_RELEASE))
      native_os_error("release after unwind table check failure",
                      (unsigned long)GetLastError());
    caml_failwith("native unwind table metadata is inconsistent");
  }

  arena = VirtualAlloc(NULL, arena_length, MEM_RESERVE | MEM_COMMIT,
                       PAGE_READWRITE);
  if (arena == NULL) {
    DWORD error = GetLastError();
    if (!VirtualFree(mapping, 0, MEM_RELEASE)) {
      DWORD release_error = GetLastError();
      char message[240];
      snprintf(message, sizeof(message),
               "native arena allocation failed (OS error %lu); code release also failed (OS error %lu)",
               (unsigned long)error, (unsigned long)release_error);
      caml_failwith(message);
    }
    native_os_error("arena allocation", (unsigned long)error);
  }
  memcpy(arena, String_val(arena_image), (size_t)arena_length);
  context[9] = (uint64_t)(uintptr_t)arena;

  if (!VirtualProtect(mapping, mapping_length, PAGE_EXECUTE_READ,
                      &previous_protection)) {
    DWORD error = GetLastError();
    native_windows_storage_mapping_error(mapping, arena, "RX protection", error);
  }
  if (!FlushInstructionCache(GetCurrentProcess(), mapping, code_length)) {
    DWORD error = GetLastError();
    native_windows_storage_mapping_error(
      mapping, arena, "instruction-cache synchronization", error);
  }
  if (function_count != 0 &&
      !RtlAddFunctionTable(function_table, function_count,
                           (DWORD64)(uintptr_t)mapping)) {
    DWORD arena_error = 0;
    DWORD mapping_error = 0;
    if (!VirtualFree(arena, 0, MEM_RELEASE))
      arena_error = GetLastError();
    if (!VirtualFree(mapping, 0, MEM_RELEASE))
      mapping_error = GetLastError();
    if (arena_error != 0 || mapping_error != 0) {
      char message[300];
      snprintf(message, sizeof(message),
               "native unwind registration failed; arena release error %lu; code release error %lu",
               (unsigned long)arena_error, (unsigned long)mapping_error);
      caml_failwith(message);
    }
    caml_failwith("native unwind registration failed");
  }

  memcpy(&entry, &mapping, sizeof(entry));
  bits = entry(context);
  pointer_ok = context[9] == (uint64_t)(uintptr_t)arena;

  if (function_count != 0 && !RtlDeleteFunctionTable(function_table)) {
    DWORD arena_error = 0;
    char message[240];
    if (!VirtualFree(arena, 0, MEM_RELEASE))
      arena_error = GetLastError();
    if (arena_error != 0) {
      snprintf(message, sizeof(message),
               "native unwind removal failed; registered mapping retained; arena release also failed (OS error %lu)",
               (unsigned long)arena_error);
      caml_failwith(message);
    }
    caml_failwith("native unwind removal failed; registered mapping retained");
  }

  {
    DWORD arena_error = 0;
    DWORD mapping_error = 0;
    if (!VirtualFree(arena, 0, MEM_RELEASE))
      arena_error = GetLastError();
    if (!VirtualFree(mapping, 0, MEM_RELEASE))
      mapping_error = GetLastError();
    if (arena_error != 0 || mapping_error != 0) {
      char message[280];
      snprintf(message, sizeof(message),
               "native storage teardown failed; arena release error %lu; code release error %lu",
               (unsigned long)arena_error, (unsigned long)mapping_error);
      caml_failwith(message);
    }
  }
  if (!pointer_ok)
    caml_failwith("native program status integrity failure: arena pointer was modified");
  return bits;
}
#elif HOLYC_NATIVE_PLATFORM == 2
static uint64_t native_linux_execute_code(value code, mlsize_t length,
                                         uint64_t *context)
{
  void *mapping;
  uint64_t (*entry)(uint64_t *);
  uint64_t bits;
  int personality_flags;

  if (sizeof(entry) != sizeof(mapping))
    caml_failwith("native function pointers do not match this host's address size");
  /* READ_IMPLIES_EXEC would turn the writable staging mapping executable. */
  personality_flags = personality(0xffffffffUL);
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
  memcpy(&entry, &mapping, sizeof(entry));
  bits = entry(context);
  if (munmap(mapping, (size_t)length) != 0)
    native_os_error("release", (unsigned long)errno);
  return bits;
}

static uint64_t native_linux_execute_code_storage(value code, mlsize_t length,
                                                  value arena_image,
                                                  uint64_t *context)
{
  const size_t arena_length = (size_t)caml_string_length(arena_image);
  void *mapping;
  void *arena;
  uint64_t (*entry)(uint64_t *);
  uint64_t bits;
  int personality_flags;
  int pointer_ok;

  if (sizeof(entry) != sizeof(mapping))
    caml_failwith("native function pointers do not match this host's address size");
  personality_flags = personality(0xffffffffUL);
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

  arena = mmap(NULL, arena_length, PROT_READ | PROT_WRITE,
               MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (arena == MAP_FAILED) {
    int error = errno;
    if (munmap(mapping, (size_t)length) != 0) {
      char message[240];
      snprintf(message, sizeof(message),
               "native arena allocation failed (OS error %lu); code release also failed (OS error %lu)",
               (unsigned long)error, (unsigned long)errno);
      caml_failwith(message);
    }
    native_os_error("arena allocation", (unsigned long)error);
  }
  memcpy(arena, String_val(arena_image), arena_length);
  context[9] = (uint64_t)(uintptr_t)arena;
  memcpy(&entry, &mapping, sizeof(entry));
  bits = entry(context);
  pointer_ok = context[9] == (uint64_t)(uintptr_t)arena;

  {
    int arena_error = 0;
    int mapping_error = 0;
    if (munmap(arena, arena_length) != 0)
      arena_error = errno;
    if (munmap(mapping, (size_t)length) != 0)
      mapping_error = errno;
    if (arena_error != 0 || mapping_error != 0) {
      char message[280];
      snprintf(message, sizeof(message),
               "native storage teardown failed; arena release error %lu; code release error %lu",
               (unsigned long)arena_error, (unsigned long)mapping_error);
      caml_failwith(message);
    }
  }
  if (!pointer_ok)
    caml_failwith("native program status integrity failure: arena pointer was modified");
  return bits;
}
#endif

#if HOLYC_NATIVE_PLATFORM != 0
/* Preserve the established sealed expression/closed-program metadata shape.
   The OS transition and teardown are shared with callable images above. */
static uint64_t native_execute_checked_image(value code, value unwind,
                                            intnat abi_code, uint64_t *context)
{
  const mlsize_t length = caml_string_length(code);
  const mlsize_t unwind_length = caml_string_length(unwind);

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

#if HOLYC_NATIVE_PLATFORM == 1
  void *mapping;
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
  return native_windows_execute_mapping(mapping, mapping_length, (SIZE_T)length,
                                        function_table,
                                        function_table == NULL ? 0u : 1u,
                                        context);
#else
  return native_linux_execute_code(code, length, context);
#endif
}

/* Source-program images carry one checked range/unwind record per generated
   function. Keep this format private to the sealed OCaml image: the bridge only
   accepts sorted image-relative ranges starting at the image entry and the
   narrow UNWIND_INFO forms emitted by this backend. */
#define HOLYC_NATIVE_MAX_FUNCTIONS 100001u
#define HOLYC_NATIVE_MAX_UNWIND_BYTES 64u
#define HOLYC_NATIVE_MAX_GLOBAL_BYTES (16u * 1024u * 1024u)
#define HOLYC_NATIVE_MAX_ARENA_BYTES (32u * 1024u * 1024u)

static unsigned native_validate_program_unwind(value unwind)
{
  const unsigned char *bytes;
  mlsize_t length;
  unsigned count;
  size_t required;
  unsigned index;
  unsigned previous_offset = 256u;
  int saw_push_rbp = 0;
  int saw_allocation = 0;
  unsigned allocation_bytes = 0;

  if (!Is_block(unwind) || Tag_val(unwind) != String_tag)
    caml_invalid_argument("native program unwind entry is not a string");
  length = caml_string_length(unwind);
  if (length < 4 || length > HOLYC_NATIVE_MAX_UNWIND_BYTES ||
      (length & 3u) != 0)
    caml_invalid_argument("native program unwind record has an unsupported length");
  bytes = (const unsigned char *)String_val(unwind);
  if ((bytes[0] & 0x07u) != 1u || (bytes[0] >> 3) != 0u)
    caml_invalid_argument("native program unwind record must be version 1 without handlers");
  count = bytes[2];
  required = (4u + ((size_t)count * 2u) + 3u) & ~(size_t)3u;
  if (required != (size_t)length)
    caml_invalid_argument("native program unwind record length does not match its code count");
  if (bytes[3] != 0u)
    caml_invalid_argument("native program unwind must use the fixed-RSP frame form");

  index = 0;
  while (index < count) {
    const unsigned code_offset = bytes[4u + (2u * index)];
    const unsigned operation = bytes[5u + (2u * index)] & 0x0fu;
    const unsigned info = bytes[5u + (2u * index)] >> 4;
    unsigned slots = 1;

    if (code_offset == 0u || code_offset > bytes[1] ||
        code_offset >= previous_offset)
      caml_invalid_argument("native program unwind codes are not in descending prologue order");
    previous_offset = code_offset;
    switch (operation) {
    case 0: /* UWOP_PUSH_NONVOL */
      if (info != 5u || code_offset != 1u || saw_push_rbp)
        caml_invalid_argument("native program unwind may only save RBP");
      saw_push_rbp = 1;
      break;
    case 1: /* UWOP_ALLOC_LARGE */
      if (info != 0u || code_offset != 11u || saw_allocation)
        caml_invalid_argument("native program large-allocation unwind opcode is malformed");
      slots = 2;
      if (slots > count - index)
        caml_invalid_argument("native program unwind opcode overruns its code array");
      allocation_bytes =
        8u * ((unsigned)bytes[6u + (2u * index)] |
              ((unsigned)bytes[7u + (2u * index)] << 8));
      saw_allocation = 1;
      break;
    case 2: /* UWOP_ALLOC_SMALL */
      if (code_offset != 11u || saw_allocation)
        caml_invalid_argument("native program small-allocation unwind opcode is malformed");
      allocation_bytes = (info * 8u) + 8u;
      saw_allocation = 1;
      break;
    default:
      caml_invalid_argument("native program unwind opcode is outside the sealed ABI");
    }
    if (slots > count - index)
      caml_invalid_argument("native program unwind opcode overruns its code array");
    index += slots;
  }
  if (!saw_push_rbp)
    caml_invalid_argument("native program unwind record does not save RBP");
  if (saw_allocation) {
    if (bytes[1] != 11u || allocation_bytes < 16u || allocation_bytes > 4080u ||
        (allocation_bytes & 15u) != 0u)
      caml_invalid_argument("native program unwind allocation is outside the callable frame bound");
    if ((allocation_bytes <= 128u && count != 2u) ||
        (allocation_bytes > 128u && count != 3u))
      caml_invalid_argument("native program unwind allocation does not use the shortest sealed form");
  } else if (bytes[1] != 4u || count != 1u) {
    caml_invalid_argument("native program frameless callable unwind record is malformed");
  }
  return allocation_bytes;
}

static mlsize_t native_validate_program_functions(value functions,
                                                  mlsize_t code_length,
                                                  unsigned *entry_allocation)
{
  mlsize_t count;
  mlsize_t index;
  uintnat previous_end = 0;

  if (!Is_block(functions) || Tag_val(functions) != 0)
    caml_invalid_argument("native program unwind table is not an array");
  count = Wosize_val(functions);
  if (count == 0 || count > HOLYC_NATIVE_MAX_FUNCTIONS)
    caml_invalid_argument("native program unwind table has an unsupported function count");
  for (index = 0; index < count; ++index) {
    value entry = Field(functions, index);
    value begin_value;
    value end_value;
    intnat begin;
    intnat end;

    if (!Is_block(entry) || Tag_val(entry) != 0 || Wosize_val(entry) != 3)
      caml_invalid_argument("native program unwind table entry is malformed");
    begin_value = Field(entry, 0);
    end_value = Field(entry, 1);
    if (!Is_long(begin_value) || !Is_long(end_value))
      caml_invalid_argument("native program function range is not integral");
    begin = Long_val(begin_value);
    end = Long_val(end_value);
    if (begin < 0 || end <= begin || (uintnat)end > (uintnat)code_length)
      caml_invalid_argument("native program function range is outside the code image");
    if ((index == 0 && begin != 0) ||
        (index != 0 && (uintnat)begin != previous_end))
      caml_invalid_argument("native program function ranges are not ordered and contiguous");
    {
      const unsigned allocation =
        native_validate_program_unwind(Field(entry, 2));
      if (index == 0)
        *entry_allocation = allocation;
    }
    previous_end = (uintnat)end;
  }
  if (previous_end != (uintnat)code_length)
    caml_invalid_argument("native program function ranges do not cover the code image tail");
  return count;
}

static uint64_t native_execute_checked_program_image(value code, value functions,
                                                     intnat abi_code,
                                                     uintnat entry_stack_bytes,
                                                     uint64_t *context)
{
  const mlsize_t length = caml_string_length(code);
  unsigned entry_allocation = 0;
  const mlsize_t function_count = native_validate_program_functions(
    functions, length, &entry_allocation);

  if (abi_code != HOLYC_NATIVE_PLATFORM)
    caml_invalid_argument("native program status ABI does not match this process");
  if (entry_stack_bytes != (uintnat)entry_allocation + 16u)
    caml_invalid_argument("native program entry stack metadata does not match its unwind frame");
  if (length == 0 || length > 16u * 1024u * 1024u)
    caml_invalid_argument("native image length is outside the host allocation bound");

#if HOLYC_NATIVE_PLATFORM == 1
  void *mapping;
  PRUNTIME_FUNCTION function_table;
  SIZE_T metadata_offset = ((SIZE_T)length + 3u) & ~(SIZE_T)3u;
  SIZE_T unwind_offset = metadata_offset;
  SIZE_T table_offset;
  SIZE_T mapping_length;
  mlsize_t index;

  for (index = 0; index < function_count; ++index) {
    const value unwind = Field(Field(functions, index), 2);
    const SIZE_T unwind_length = (SIZE_T)caml_string_length(unwind);
    if (unwind_length > (SIZE_T)-1 - unwind_offset)
      caml_failwith("native program unwind metadata size overflow");
    unwind_offset += unwind_length;
  }
  table_offset = (unwind_offset + 3u) & ~(SIZE_T)3u;
  if ((SIZE_T)function_count > ((SIZE_T)-1 - table_offset) / sizeof(RUNTIME_FUNCTION))
    caml_failwith("native program function table size overflow");
  mapping_length = table_offset + ((SIZE_T)function_count * sizeof(RUNTIME_FUNCTION));
  mapping = VirtualAlloc(NULL, mapping_length, MEM_RESERVE | MEM_COMMIT,
                         PAGE_READWRITE);
  if (mapping == NULL)
    native_os_error("allocation", (unsigned long)GetLastError());
  memcpy(mapping, String_val(code), (size_t)length);

  function_table = (PRUNTIME_FUNCTION)((char *)mapping + table_offset);
  unwind_offset = metadata_offset;
  for (index = 0; index < function_count; ++index) {
    const value descriptor = Field(functions, index);
    const value unwind = Field(descriptor, 2);
    const SIZE_T unwind_length = (SIZE_T)caml_string_length(unwind);
    memcpy((char *)mapping + unwind_offset, String_val(unwind),
           (size_t)unwind_length);
    function_table[index].BeginAddress = (DWORD)Long_val(Field(descriptor, 0));
    function_table[index].EndAddress = (DWORD)Long_val(Field(descriptor, 1));
    function_table[index].UnwindData = (DWORD)unwind_offset;
    unwind_offset += unwind_length;
  }
  return native_windows_execute_mapping(mapping, mapping_length, (SIZE_T)length,
                                        function_table, (DWORD)function_count,
                                        context);
#else
  return native_linux_execute_code(code, length, context);
#endif
}

static uint64_t native_execute_checked_program_storage_image(
  value code, value functions, intnat abi_code, uintnat entry_stack_bytes,
  value arena_image, uint64_t *context)
{
  const mlsize_t length = caml_string_length(code);
  unsigned entry_allocation = 0;
  const mlsize_t function_count = native_validate_program_functions(
    functions, length, &entry_allocation);

  if (abi_code != HOLYC_NATIVE_PLATFORM)
    caml_invalid_argument("native program status ABI does not match this process");
  if (entry_stack_bytes != (uintnat)entry_allocation + 16u)
    caml_invalid_argument("native program entry stack metadata does not match its unwind frame");
  if (length == 0 || length > 16u * 1024u * 1024u)
    caml_invalid_argument("native image length is outside the host allocation bound");

#if HOLYC_NATIVE_PLATFORM == 1
  void *mapping;
  PRUNTIME_FUNCTION function_table;
  SIZE_T metadata_offset = ((SIZE_T)length + 3u) & ~(SIZE_T)3u;
  SIZE_T unwind_offset = metadata_offset;
  SIZE_T table_offset;
  SIZE_T mapping_length;
  mlsize_t index;

  for (index = 0; index < function_count; ++index) {
    const value unwind = Field(Field(functions, index), 2);
    const SIZE_T unwind_length = (SIZE_T)caml_string_length(unwind);
    if (unwind_length > (SIZE_T)-1 - unwind_offset)
      caml_failwith("native program unwind metadata size overflow");
    unwind_offset += unwind_length;
  }
  table_offset = (unwind_offset + 3u) & ~(SIZE_T)3u;
  if ((SIZE_T)function_count > ((SIZE_T)-1 - table_offset) / sizeof(RUNTIME_FUNCTION))
    caml_failwith("native program function table size overflow");
  mapping_length = table_offset + ((SIZE_T)function_count * sizeof(RUNTIME_FUNCTION));
  mapping = VirtualAlloc(NULL, mapping_length, MEM_RESERVE | MEM_COMMIT,
                         PAGE_READWRITE);
  if (mapping == NULL)
    native_os_error("allocation", (unsigned long)GetLastError());
  memcpy(mapping, String_val(code), (size_t)length);

  function_table = (PRUNTIME_FUNCTION)((char *)mapping + table_offset);
  unwind_offset = metadata_offset;
  for (index = 0; index < function_count; ++index) {
    const value descriptor = Field(functions, index);
    const value unwind = Field(descriptor, 2);
    const SIZE_T unwind_length = (SIZE_T)caml_string_length(unwind);
    memcpy((char *)mapping + unwind_offset, String_val(unwind),
           (size_t)unwind_length);
    function_table[index].BeginAddress = (DWORD)Long_val(Field(descriptor, 0));
    function_table[index].EndAddress = (DWORD)Long_val(Field(descriptor, 1));
    function_table[index].UnwindData = (DWORD)unwind_offset;
    unwind_offset += unwind_length;
  }
  return native_windows_execute_mapping_storage(
    mapping, mapping_length, (SIZE_T)length, function_table,
    (DWORD)function_count, arena_image, context);
#else
  return native_linux_execute_code_storage(code, length, arena_image, context);
#endif
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

  /* Preserve the original six-word closed-program ABI exactly. Callable source
     programs use holyc_native_execute_program_functions below. */
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

CAMLprim value holyc_native_execute_program_functions(value code, value functions,
                                                     value abi, value limits)
{
  CAMLparam4(code, functions, abi, limits);
#if HOLYC_NATIVE_PLATFORM == 0
  caml_failwith("native execution requires Windows or Linux x86-64 with 64-bit pointers");
#else
  CAMLlocal5(boxed_kind, boxed_site, boxed_steps, boxed_value_site, boxed_bits);
  CAMLlocal1(result);
  intnat step_limit;
  intnat frame_limit;
  intnat depth_limit;
  intnat active_stack_limit;
  intnat entry_stack_bytes;
  uint64_t remaining_stack;
  intnat abi_code;

  if (!Is_long(abi))
    caml_invalid_argument("native program status ABI is not integral");
  if (!Is_block(limits) || Tag_val(limits) != 0 || Wosize_val(limits) != 5 ||
      !Is_long(Field(limits, 0)) || !Is_long(Field(limits, 1)) ||
      !Is_long(Field(limits, 2)) || !Is_long(Field(limits, 3)) ||
      !Is_long(Field(limits, 4)))
    caml_invalid_argument("native program limits tuple is malformed");
  abi_code = Long_val(abi);
  step_limit = Long_val(Field(limits, 0));
  frame_limit = Long_val(Field(limits, 1));
  depth_limit = Long_val(Field(limits, 2));
  active_stack_limit = Long_val(Field(limits, 3));
  entry_stack_bytes = Long_val(Field(limits, 4));

  if (abi_code != HOLYC_NATIVE_PLATFORM)
    caml_invalid_argument("native program status ABI does not match this process");
  if (step_limit <= 0)
    caml_invalid_argument("native program max_steps must be greater than zero");
  if (frame_limit <= 0)
    caml_invalid_argument("native program max_frame_bytes must be greater than zero");
  if (depth_limit <= 0)
    caml_invalid_argument("native program max_call_depth must be greater than zero");
  if (active_stack_limit <= 0 || active_stack_limit > 65536)
    caml_invalid_argument("native program max_active_stack_bytes must be between 1 and 65536");
  if (entry_stack_bytes <= 0 || entry_stack_bytes > active_stack_limit)
    caml_invalid_argument("native program entry stack exceeds max_active_stack_bytes");
  remaining_stack = (uint64_t)(active_stack_limit - entry_stack_bytes);

  /* kind, fault site, immutable budget, executed steps, last END_EXP site,
     result bits, remaining semantic-frame bytes, remaining call depth, and
     remaining physical native-stack bytes. The root's exact physical cost is
     charged before entry; generated CALL/return and checked-fault paths restore
     all three remaining quotas to these values. */
  uint64_t context[9] = {0, 0, (uint64_t)step_limit, 0, 0, 0,
                         (uint64_t)frame_limit, (uint64_t)depth_limit,
                         remaining_stack};
  (void)native_execute_checked_program_image(code, functions, abi_code,
                                             (uintnat)entry_stack_bytes, context);
  if (context[2] != (uint64_t)step_limit)
    caml_failwith("native program status integrity failure: budget was modified");
  if (context[6] != (uint64_t)frame_limit)
    caml_failwith("native program status integrity failure: frame quota was not restored");
  if (context[7] != (uint64_t)depth_limit)
    caml_failwith("native program status integrity failure: call-depth quota was not restored");
  if (context[8] != remaining_stack)
    caml_failwith("native program status integrity failure: active-stack quota was not restored");

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

CAMLprim value holyc_native_execute_program_storage(value code, value functions,
                                                   value abi, value limits,
                                                   value storage)
{
  CAMLparam5(code, functions, abi, limits, storage);
#if HOLYC_NATIVE_PLATFORM == 0
  caml_failwith("native execution requires Windows or Linux x86-64 with 64-bit pointers");
#else
  CAMLlocal5(boxed_kind, boxed_site, boxed_steps, boxed_value_site, boxed_bits);
  CAMLlocal1(result);
  intnat step_limit;
  intnat frame_limit;
  intnat depth_limit;
  intnat active_stack_limit;
  intnat entry_stack_bytes;
  intnat global_limit;
  intnat logical_global_bytes;
  uint64_t remaining_stack;
  intnat abi_code;
  value arena_image;
  mlsize_t arena_length;

  if (!Is_long(abi))
    caml_invalid_argument("native program status ABI is not integral");
  if (!Is_block(limits) || Tag_val(limits) != 0 || Wosize_val(limits) != 6 ||
      !Is_long(Field(limits, 0)) || !Is_long(Field(limits, 1)) ||
      !Is_long(Field(limits, 2)) || !Is_long(Field(limits, 3)) ||
      !Is_long(Field(limits, 4)) || !Is_long(Field(limits, 5)))
    caml_invalid_argument("native storage program limits tuple is malformed");
  if (!Is_block(storage) || Tag_val(storage) != 0 || Wosize_val(storage) != 2 ||
      !Is_long(Field(storage, 0)))
    caml_invalid_argument("native program storage tuple is malformed");
  arena_image = Field(storage, 1);
  if (!Is_block(arena_image) || Tag_val(arena_image) != String_tag)
    caml_invalid_argument("native program arena image is not a string");

  abi_code = Long_val(abi);
  step_limit = Long_val(Field(limits, 0));
  frame_limit = Long_val(Field(limits, 1));
  depth_limit = Long_val(Field(limits, 2));
  active_stack_limit = Long_val(Field(limits, 3));
  entry_stack_bytes = Long_val(Field(limits, 4));
  global_limit = Long_val(Field(limits, 5));
  logical_global_bytes = Long_val(Field(storage, 0));
  arena_length = caml_string_length(arena_image);

  if (abi_code != HOLYC_NATIVE_PLATFORM)
    caml_invalid_argument("native program status ABI does not match this process");
  if (step_limit <= 0)
    caml_invalid_argument("native program max_steps must be greater than zero");
  if (frame_limit <= 0)
    caml_invalid_argument("native program max_frame_bytes must be greater than zero");
  if (depth_limit <= 0)
    caml_invalid_argument("native program max_call_depth must be greater than zero");
  if (active_stack_limit <= 0 || active_stack_limit > 65536)
    caml_invalid_argument("native program max_active_stack_bytes must be between 1 and 65536");
  if (entry_stack_bytes <= 0 || entry_stack_bytes > active_stack_limit)
    caml_invalid_argument("native program entry stack exceeds max_active_stack_bytes");
  if (global_limit <= 0 || (uintnat)global_limit > HOLYC_NATIVE_MAX_GLOBAL_BYTES)
    caml_invalid_argument("native program max_global_bytes is outside the host bound");
  if (logical_global_bytes <= 0 ||
      (uintnat)logical_global_bytes > HOLYC_NATIVE_MAX_GLOBAL_BYTES ||
      logical_global_bytes > global_limit)
    caml_invalid_argument("native program logical global bytes exceed their bound");
  if ((uintnat)arena_length > HOLYC_NATIVE_MAX_ARENA_BYTES ||
      (uintnat)arena_length < (uintnat)logical_global_bytes ||
      (uintnat)arena_length > 2u * (uintnat)logical_global_bytes)
    caml_invalid_argument("native program arena image is inconsistent with logical globals");

  remaining_stack = (uint64_t)(active_stack_limit - entry_stack_bytes);
  /* The tenth word is an immutable private arena pointer installed by the
     platform helper after fresh RW allocation. Generated code may only read it
     through the sealed R11 context and keeps R9 reserved as the arena base. */
  uint64_t context[10] = {0, 0, (uint64_t)step_limit, 0, 0, 0,
                          (uint64_t)frame_limit, (uint64_t)depth_limit,
                          remaining_stack, 0};
  (void)native_execute_checked_program_storage_image(
    code, functions, abi_code, (uintnat)entry_stack_bytes, arena_image, context);
  if (context[2] != (uint64_t)step_limit)
    caml_failwith("native program status integrity failure: budget was modified");
  if (context[6] != (uint64_t)frame_limit)
    caml_failwith("native program status integrity failure: frame quota was not restored");
  if (context[7] != (uint64_t)depth_limit)
    caml_failwith("native program status integrity failure: call-depth quota was not restored");
  if (context[8] != remaining_stack)
    caml_failwith("native program status integrity failure: active-stack quota was not restored");

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
