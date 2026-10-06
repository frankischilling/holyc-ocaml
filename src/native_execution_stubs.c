/* Only the host execution boundary lives in C. Instruction selection,
   verification, register allocation and byte encoding belong to OCaml. */
#ifndef _DEFAULT_SOURCE
#define _DEFAULT_SOURCE 1
#endif

#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdatomic.h>
#include <caml/custom.h>
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
#include <unistd.h>
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
#define HOLYC_NATIVE_MAX_LITERAL_BYTES (16u * 1024u * 1024u)
#define HOLYC_NATIVE_MAX_ARENA_BYTES (32u * 1024u * 1024u)
#define HOLYC_NATIVE_MAX_OUTPUT_BYTES (16u * 1024u * 1024u)

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
                                                  unsigned *entry_allocation,
                                                  value code, int task_leaves)
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
      value unwind = Field(entry, 2);
      if (task_leaves && index != 0 && Is_block(unwind) && Tag_val(unwind) == String_tag &&
          caml_string_length(unwind) == 4 &&
          memcmp(String_val(unwind), "\001\000\000\000", 4) == 0) {
        const unsigned char *leaf = (const unsigned char *)String_val(code) + begin;
        uint32_t target;
        if (end - begin != 8 || leaf[0] != 0x90 || leaf[1] != 0x41 ||
            leaf[2] != 0xff || leaf[3] != 0xa1)
          caml_invalid_argument("native leaf entry is not the sealed task jump");
        memcpy(&target, leaf + 4, 4);
        if (target > HOLYC_NATIVE_MAX_ARENA_BYTES - 8)
          caml_invalid_argument("native leaf target cell exceeds the task bound");
      } else {
        const unsigned allocation = native_validate_program_unwind(unwind);
        if (index == 0) *entry_allocation = allocation;
      }
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
    functions, length, &entry_allocation, code, 0);

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
    functions, length, &entry_allocation, code, 0);

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


/* Retained mappings are opaque host resources. Their original sealed OCaml
   image stays rooted; no address, arena pointer or foreign mapping is exported. */
struct native_retained_program {
  value identity;
  void *mapping;
  size_t mapping_length;
  void *arena;
  size_t arena_length;
  atomic_int active;
  atomic_int task_readers;
  int closing;
  int closed_entry;
  int task_fragment;
#if HOLYC_NATIVE_PLATFORM == 1
  PRUNTIME_FUNCTION function_table;
  DWORD function_count;
  int registered;
#endif
  char close_error[240];
};

struct native_task_code_owner {
  size_t address, target;
  uint64_t canonical, current_target;
  struct native_retained_program *program, *current_program;
  value canonical_handle, current_handle;
};

struct native_task_arena {
  void *mapping;
  size_t capacity;
  size_t used;
  size_t committed;
  size_t page_size;
  struct native_task_code_owner **owners;
  size_t owner_capacity;
  atomic_int active;
  int closing;
  char close_error[240];
};

static int native_task_arena_close(struct native_task_arena *arena)
{
  arena->closing = 1;
  arena->close_error[0] = 0;
  if (arena->mapping == NULL) return 1;
#if HOLYC_NATIVE_PLATFORM == 1
  if (!VirtualFree(arena->mapping, 0, MEM_RELEASE)) {
    snprintf(arena->close_error, sizeof(arena->close_error),
             "native task arena release failed (OS error %lu)",
             (unsigned long)GetLastError());
    return 0;
  }
#else
  if (munmap(arena->mapping, arena->capacity) != 0) {
    snprintf(arena->close_error, sizeof(arena->close_error),
             "native task arena release failed (OS error %lu)",
             (unsigned long)errno);
    return 0;
  }
#endif
  arena->mapping = NULL;
  for (size_t index = 0; index < arena->owner_capacity; ++index) {
    struct native_task_code_owner *owner = arena->owners[index];
    if (owner != NULL) {
      caml_remove_generational_global_root(&owner->canonical_handle);
      caml_remove_generational_global_root(&owner->current_handle);
      free(owner);
    }
  }
  free(arena->owners);
  arena->owners = NULL;
  arena->owner_capacity = 0;
  return 1;
}

static void native_task_arena_finalize(value handle)
{
  struct native_task_arena *arena =
    *((struct native_task_arena **)Data_custom_val(handle));
  int expected = 0;
  if (arena == NULL) return;
  if (!atomic_compare_exchange_strong(&arena->active, &expected, 1)) return;
  if (native_task_arena_close(arena)) free(arena);
  *((struct native_task_arena **)Data_custom_val(handle)) = NULL;
}

static struct custom_operations native_task_arena_operations = {
  "holyc.native.task-arena.v1",
  native_task_arena_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

static struct native_task_arena *native_task_arena_get(value handle)
{
  struct native_task_arena *arena;
  if (!Is_block(handle) || Tag_val(handle) != Custom_tag ||
      Custom_ops_val(handle) != &native_task_arena_operations)
    caml_invalid_argument("native task arena has another host resource owner");
  arena = *((struct native_task_arena **)Data_custom_val(handle));
  if (arena == NULL)
    caml_invalid_argument("native task arena has been released");
  return arena;
}

static size_t native_task_arena_round_pages(const struct native_task_arena *arena,
                                            size_t length)
{
  size_t pages;
  if (length == 0) return 0;
  pages = (length + arena->page_size - 1u) / arena->page_size;
  return pages * arena->page_size;
}

static unsigned long native_task_arena_commit(struct native_task_arena *arena,
                                              size_t required)
{
  size_t target = native_task_arena_round_pages(arena, required);
  if (target <= arena->committed) return 0;
#if HOLYC_NATIVE_PLATFORM == 1
  if (VirtualAlloc((char *)arena->mapping + arena->committed,
                   target - arena->committed, MEM_COMMIT, PAGE_READWRITE) == NULL)
    return (unsigned long)GetLastError();
#else
  if (mprotect(arena->mapping, target, PROT_READ | PROT_WRITE) != 0)
    return (unsigned long)errno;
#endif
  arena->committed = target;
  return 0;
}

/* Closed entries use their original RSP spill frame, without a saved RBP.
   Keep this admission separate from the callable unwind-table validator. */
static unsigned native_validate_retained_functions(value code, value functions,
                                                    int *closed_entry, int task_leaves)
{
  const mlsize_t code_length = caml_string_length(code);
  unsigned allocation = 0;
  *closed_entry = 0;
  if (Is_block(functions) && Tag_val(functions) == 0 &&
      Wosize_val(functions) == 1) {
    value descriptor = Field(functions, 0);
    if (Is_block(descriptor) && Tag_val(descriptor) == 0 &&
        Wosize_val(descriptor) == 3 && Is_long(Field(descriptor, 0)) &&
        Is_long(Field(descriptor, 1)) &&
        Is_block(Field(descriptor, 2)) &&
        Tag_val(Field(descriptor, 2)) == String_tag) {
      value unwind = Field(descriptor, 2);
      const mlsize_t length = caml_string_length(unwind);
      const unsigned char *bytes = (const unsigned char *)String_val(unwind);
      if (length == 0 || (length == 8 && bytes[1] == 7)) {
        size_t capture_offset = 0;
        const unsigned char *encoded = (const unsigned char *)String_val(code);
        if (Long_val(Field(descriptor, 0)) != 0 ||
            Long_val(Field(descriptor, 1)) < 0 ||
            (uintnat)Long_val(Field(descriptor, 1)) != (uintnat)code_length)
          caml_invalid_argument("retained native closed entry range does not cover its code");
        if (length != 0) {
          if (bytes[0] != 1 || bytes[3] != 0 || bytes[4] != 7)
            caml_invalid_argument("retained native closed entry unwind header is malformed");
          if (bytes[2] == 1 && (bytes[5] & 15u) == 2u &&
              bytes[6] == 0 && bytes[7] == 0) {
            allocation = ((unsigned)(bytes[5] >> 4) + 1u) * 8u;
          } else if (bytes[2] == 2 && bytes[5] == 1) {
            allocation = ((unsigned)bytes[6] | ((unsigned)bytes[7] << 8)) * 8u;
            if (allocation <= 128u)
              caml_invalid_argument("retained native closed entry unwind allocation is not shortest");
          } else {
            caml_invalid_argument("retained native closed entry unwind allocation is malformed");
          }
          if (allocation < 8u || allocation > 4088u ||
              (allocation & 15u) != 8u)
            caml_invalid_argument("retained native closed entry unwind allocation exceeds its frame bound");
          if (code_length < 10 || encoded[0] != 0x48 || encoded[1] != 0x81 ||
              encoded[2] != 0xec || encoded[3] != (allocation & 0xffu) ||
              encoded[4] != ((allocation >> 8) & 0xffu) ||
              encoded[5] != 0 || encoded[6] != 0)
            caml_invalid_argument("retained native closed entry code disagrees with its spill frame");
          capture_offset = 7;
        }
        if (code_length < capture_offset + 3 ||
            encoded[capture_offset] != 0x49 ||
            encoded[capture_offset + 1] != 0x89 ||
            encoded[capture_offset + 2] !=
              (HOLYC_NATIVE_PLATFORM == 1 ? 0xcb : 0xfb))
          caml_invalid_argument("retained native closed entry has another status prologue");
        *closed_entry = 1;
        return allocation + 8u;
      }
    }
  }
  (void)native_validate_program_functions(functions, code_length, &allocation, code, task_leaves);
  return allocation + 16u;
}

static int native_retained_close(struct native_retained_program *program)
{
  if (atomic_load(&program->task_readers) != 0) {
    snprintf(program->close_error, sizeof(program->close_error),
             "native code has active task borrowers");
    return 0;
  }
  /* Even a partial release revokes entry: its arena or unwind registration
     may already be gone. Remaining OS resources are retained only for retry. */
  program->closing = 1;
  program->close_error[0] = 0;
#if HOLYC_NATIVE_PLATFORM == 1
  if (program->registered) {
    if (!RtlDeleteFunctionTable(program->function_table)) {
      snprintf(program->close_error, sizeof(program->close_error),
               "retained native unwind removal failed; registered mapping retained");
      return 0;
    }
    program->registered = 0;
  }
  if (program->arena != NULL) {
    if (!VirtualFree(program->arena, 0, MEM_RELEASE)) {
      snprintf(program->close_error, sizeof(program->close_error),
               "retained native arena release failed (OS error %lu)",
               (unsigned long)GetLastError());
      return 0;
    }
    program->arena = NULL;
  }
  if (program->mapping != NULL) {
    if (!VirtualFree(program->mapping, 0, MEM_RELEASE)) {
      snprintf(program->close_error, sizeof(program->close_error),
               "retained native code release failed (OS error %lu)",
               (unsigned long)GetLastError());
      return 0;
    }
    program->mapping = NULL;
  }
#else
  if (program->arena != NULL) {
    if (munmap(program->arena, program->arena_length) != 0) {
      snprintf(program->close_error, sizeof(program->close_error),
               "retained native arena release failed (OS error %lu)",
               (unsigned long)errno);
      return 0;
    }
    program->arena = NULL;
  }
  if (program->mapping != NULL) {
    if (munmap(program->mapping, program->mapping_length) != 0) {
      snprintf(program->close_error, sizeof(program->close_error),
               "retained native code release failed (OS error %lu)",
               (unsigned long)errno);
      return 0;
    }
    program->mapping = NULL;
  }
#endif
  return 1;
}

static void native_retained_finalize(value handle)
{
  struct native_retained_program *program =
    *((struct native_retained_program **)Data_custom_val(handle));
  int expected = 0;
  if (program == NULL) return;
  if (!atomic_compare_exchange_strong(&program->active, &expected, 1)) return;
  caml_remove_generational_global_root(&program->identity);
  if (native_retained_close(program)) free(program);
  /* A failed OS release retains its allocation and registered unwind table.
     Finalizers cannot raise or let Windows retain a dangling table reference. */
  *((struct native_retained_program **)Data_custom_val(handle)) = NULL;
}

static struct custom_operations native_retained_operations = {
  "holyc.native.retained-program.v1",
  native_retained_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

static struct native_retained_program *native_retained_get(value handle)
{
  struct native_retained_program *program;
  if (!Is_block(handle) || Tag_val(handle) != Custom_tag ||
      Custom_ops_val(handle) != &native_retained_operations)
    caml_invalid_argument("retained native image has another host resource owner");
  program = *((struct native_retained_program **)Data_custom_val(handle));
  if (program == NULL)
    caml_invalid_argument("retained native image has been released");
  return program;
}

static void native_retained_creation_error(struct native_retained_program *program,
                                           const char *operation,
                                           unsigned long error)
{
  char message[480];
  int clean = native_retained_close(program);
  snprintf(message, sizeof(message),
           "retained native %s failed (OS error %lu)%s%s",
           operation, error, clean ? "" : "; ",
           clean ? "" : program->close_error);
  caml_failwith(message);
}

static void native_retained_map(struct native_retained_program *program,
                                value code, value functions, value arena_image,
                                size_t arena_length, int map_arena)
{
  size_t code_length = (size_t)caml_string_length(code);
  uint64_t (*entry)(uint64_t *);
  if (sizeof(entry) != sizeof(program->mapping))
    caml_failwith("native function pointers do not match this host's address size");
  program->arena_length = arena_length;
#if HOLYC_NATIVE_PLATFORM == 1
  size_t count = program->closed_entry &&
    caml_string_length(Field(Field(functions, 0), 2)) == 0
      ? 0 : (size_t)Wosize_val(functions);
  size_t metadata_offset = (code_length + 3u) & ~(size_t)3u;
  size_t unwind_offset = metadata_offset;
  size_t table_offset;
  size_t index;
  DWORD previous_protection;
  for (index = 0; index < count; ++index) {
    size_t length = (size_t)caml_string_length(Field(Field(functions, index), 2));
    if (length > (size_t)-1 - unwind_offset)
      caml_failwith("retained native unwind metadata size overflow");
    unwind_offset += length;
  }
  if (unwind_offset > (size_t)-1 - 3u)
    caml_failwith("retained native unwind table alignment overflow");
  table_offset = (unwind_offset + 3u) & ~(size_t)3u;
  if (count > ((size_t)-1 - table_offset) / sizeof(RUNTIME_FUNCTION))
    caml_failwith("retained native function table size overflow");
  program->mapping_length = table_offset + count * sizeof(RUNTIME_FUNCTION);
  program->mapping = VirtualAlloc(NULL, program->mapping_length,
                                  MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);
  if (program->mapping == NULL)
    native_retained_creation_error(program, "code allocation", GetLastError());
  memcpy(program->mapping, String_val(code), code_length);
  program->function_table =
    (PRUNTIME_FUNCTION)((char *)program->mapping + table_offset);
  program->function_count = (DWORD)count;
  unwind_offset = metadata_offset;
  for (index = 0; index < count; ++index) {
    value descriptor = Field(functions, index);
    value unwind = Field(descriptor, 2);
    size_t length = (size_t)caml_string_length(unwind);
    memcpy((char *)program->mapping + unwind_offset, String_val(unwind), length);
    program->function_table[index].BeginAddress = (DWORD)Long_val(Field(descriptor, 0));
    program->function_table[index].EndAddress = (DWORD)Long_val(Field(descriptor, 1));
    program->function_table[index].UnwindData = (DWORD)unwind_offset;
    unwind_offset += length;
  }
  if (map_arena && arena_length != 0) {
    program->arena = VirtualAlloc(NULL, arena_length, MEM_RESERVE | MEM_COMMIT,
                                  PAGE_READWRITE);
    if (program->arena == NULL)
      native_retained_creation_error(program, "arena allocation", GetLastError());
    memcpy(program->arena, String_val(arena_image), arena_length);
  }
  if (!VirtualProtect(program->mapping, program->mapping_length,
                       PAGE_EXECUTE_READ, &previous_protection))
    native_retained_creation_error(program, "RX protection", GetLastError());
  if (!FlushInstructionCache(GetCurrentProcess(), program->mapping, code_length))
    native_retained_creation_error(program, "instruction-cache synchronization",
                                    GetLastError());
  if (program->function_count != 0) {
    if (!RtlAddFunctionTable(program->function_table, program->function_count,
                             (DWORD64)(uintptr_t)program->mapping))
      native_retained_creation_error(program, "unwind registration", GetLastError());
    program->registered = 1;
  }
#else
  int flags = personality(0xffffffffUL);
  if (flags == -1)
    native_os_error("personality query", (unsigned long)errno);
  if ((flags & READ_IMPLIES_EXEC) != 0)
    caml_failwith("native execution requires READ_IMPLIES_EXEC to be disabled");
  program->mapping_length = code_length;
  program->mapping = mmap(NULL, code_length, PROT_READ | PROT_WRITE,
                           MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (program->mapping == MAP_FAILED) {
    program->mapping = NULL;
    native_retained_creation_error(program, "code allocation", (unsigned long)errno);
  }
  memcpy(program->mapping, String_val(code), code_length);
  if (map_arena && arena_length != 0) {
    program->arena = mmap(NULL, arena_length, PROT_READ | PROT_WRITE,
                           MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (program->arena == MAP_FAILED) {
      program->arena = NULL;
      native_retained_creation_error(program, "arena allocation", (unsigned long)errno);
    }
    memcpy(program->arena, String_val(arena_image), arena_length);
  }
  if (mprotect(program->mapping, code_length, PROT_READ | PROT_EXEC) != 0)
    native_retained_creation_error(program, "RX protection", (unsigned long)errno);
  __builtin___clear_cache((char *)program->mapping,
                          (char *)program->mapping + code_length);
#endif
}

static uint64_t native_retained_run(value handle, uint64_t *context, value entered)
{
  struct native_retained_program *program = native_retained_get(handle);
  int expected = 0;
  uint64_t (*entry)(uint64_t *);
  uint64_t bits;
  uint64_t arena;
  if (program->task_fragment)
    caml_invalid_argument("native task fragment requires its shared task arena");
  if (!atomic_compare_exchange_strong(&program->active, &expected, 1))
    caml_invalid_argument("retained native image is already active");
  if (program->closing || program->mapping == NULL) {
    atomic_store(&program->active, 0);
    caml_invalid_argument("retained native image has been released");
  }
  arena = (uint64_t)(uintptr_t)program->arena;
  context[9] = arena;
  memcpy(&entry, &program->mapping, sizeof(entry));
  if (entered != Val_unit)
    Store_field(entered, 0, Val_true);
  bits = entry(context);
  atomic_store(&program->active, 0);
  if (context[9] != arena)
    caml_failwith("retained native status integrity failure: arena pointer was modified");
  return bits;
}

static int native_task_borrow_code(struct native_retained_program *code,
                                   struct native_retained_program *entry)
{
  int expected = 0;
  if (code == entry) return 1;
  if (!atomic_compare_exchange_strong(&code->active, &expected, 1)) return 0;
  if (code->closing || code->mapping == NULL) {
    atomic_store(&code->active, 0);
    return 0;
  }
  atomic_fetch_add(&code->task_readers, 1);
  atomic_store(&code->active, 0);
  return 1;
}

static void native_task_return_code(struct native_retained_program *code,
                                    struct native_retained_program *entry)
{
  if (code != entry) atomic_fetch_sub(&code->task_readers, 1);
}

static uint64_t native_retained_run_task(value handle, value arena_handle,
                                         uintnat required_arena_bytes,
                                         uint64_t *context, value entered)
{
  struct native_retained_program *program = native_retained_get(handle);
  struct native_task_arena *arena = native_task_arena_get(arena_handle);
  int program_expected = 0;
  int arena_expected = 0;
  uint64_t (*entry)(uint64_t *);
  uint64_t bits;
  uint64_t arena_address;
  int pointer_ok;

  if (!program->task_fragment)
    caml_invalid_argument("ordinary retained native image cannot use a task arena");
  if (!atomic_compare_exchange_strong(&program->active, &program_expected, 1))
    caml_invalid_argument("retained native image is already active");
  if (program->closing || program->mapping == NULL) {
    atomic_store(&program->active, 0);
    caml_invalid_argument("retained native image has been released");
  }
  if (!atomic_compare_exchange_strong(&arena->active, &arena_expected, 1)) {
    atomic_store(&program->active, 0);
    caml_invalid_argument("native task arena is already active");
  }
  if (arena->closing || arena->mapping == NULL) {
    atomic_store(&arena->active, 0);
    atomic_store(&program->active, 0);
    caml_invalid_argument("native task arena has been released");
  }
  if ((size_t)required_arena_bytes > arena->used) {
    atomic_store(&arena->active, 0);
    atomic_store(&program->active, 0);
    caml_invalid_argument("native task fragment requires unadmitted task storage");
  }

  size_t borrowed = 0;
  for (; borrowed < arena->owner_capacity; ++borrowed) {
    struct native_task_code_owner *owner = arena->owners[borrowed];
    if (owner == NULL || owner->canonical == 0) continue;
    uint64_t address, target;
    memcpy(&address, (char *)arena->mapping + owner->address, 8);
    memcpy(&target, (char *)arena->mapping + owner->target, 8);
    if (address != owner->canonical || target != owner->current_target ||
        !native_task_borrow_code(owner->program, program)) break;
    if (!native_task_borrow_code(owner->current_program, program)) {
      native_task_return_code(owner->program, program);
      break;
    }
  }
  if (borrowed != arena->owner_capacity) {
    for (size_t index = 0; index < borrowed; ++index) {
      struct native_task_code_owner *owner = arena->owners[index];
      if (owner != NULL && owner->canonical != 0) {
        native_task_return_code(owner->program, program);
        native_task_return_code(owner->current_program, program);
      }
    }
    atomic_store(&arena->active, 0); atomic_store(&program->active, 0);
    caml_invalid_argument("native task code owner is corrupt, released or active");
  }
  arena_address = (uint64_t)(uintptr_t)arena->mapping;
  context[9] = arena_address;
  memcpy(&entry, &program->mapping, sizeof(entry));
  if (entered != Val_unit)
    Store_field(entered, 0, Val_true);
  bits = entry(context);
  pointer_ok = context[9] == arena_address;
  for (size_t index = 0; index < borrowed; ++index) {
    struct native_task_code_owner *owner = arena->owners[index];
    if (owner != NULL && owner->canonical != 0) {
      native_task_return_code(owner->program, program);
      native_task_return_code(owner->current_program, program);
    }
  }
  atomic_store(&arena->active, 0);
  atomic_store(&program->active, 0);
  if (!pointer_ok)
    caml_failwith("retained native task status integrity failure: arena pointer was modified");
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
  intnat literal_limit;
  intnat logical_global_bytes;
  intnat logical_literal_bytes;
  intnat metadata_bytes;
  uint64_t remaining_stack;
  intnat abi_code;
  value arena_image;
  mlsize_t arena_length;

  if (!Is_long(abi))
    caml_invalid_argument("native program status ABI is not integral");
  if (!Is_block(limits) || Tag_val(limits) != 0 || Wosize_val(limits) != 7 ||
      !Is_long(Field(limits, 0)) || !Is_long(Field(limits, 1)) ||
      !Is_long(Field(limits, 2)) || !Is_long(Field(limits, 3)) ||
      !Is_long(Field(limits, 4)) || !Is_long(Field(limits, 5)) ||
      !Is_long(Field(limits, 6)))
    caml_invalid_argument("native storage program limits tuple is malformed");
  if (!Is_block(storage) || Tag_val(storage) != 0 || Wosize_val(storage) != 4 ||
      !Is_long(Field(storage, 0)) || !Is_long(Field(storage, 1)) ||
      !Is_long(Field(storage, 2)))
    caml_invalid_argument("native program storage tuple is malformed");
  arena_image = Field(storage, 3);
  if (!Is_block(arena_image) || Tag_val(arena_image) != String_tag)
    caml_invalid_argument("native program arena image is not a string");

  abi_code = Long_val(abi);
  step_limit = Long_val(Field(limits, 0));
  frame_limit = Long_val(Field(limits, 1));
  depth_limit = Long_val(Field(limits, 2));
  active_stack_limit = Long_val(Field(limits, 3));
  entry_stack_bytes = Long_val(Field(limits, 4));
  global_limit = Long_val(Field(limits, 5));
  literal_limit = Long_val(Field(limits, 6));
  logical_global_bytes = Long_val(Field(storage, 0));
  logical_literal_bytes = Long_val(Field(storage, 1));
  metadata_bytes = Long_val(Field(storage, 2));
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
  if (literal_limit <= 0 || (uintnat)literal_limit > HOLYC_NATIVE_MAX_LITERAL_BYTES)
    caml_invalid_argument("native program max_literal_bytes is outside the host bound");
  if (logical_global_bytes < 0 ||
      (uintnat)logical_global_bytes > HOLYC_NATIVE_MAX_GLOBAL_BYTES ||
      logical_global_bytes > global_limit)
    caml_invalid_argument("native program logical global bytes exceed their bound");
  if (logical_literal_bytes < 0 ||
      (uintnat)logical_literal_bytes > HOLYC_NATIVE_MAX_LITERAL_BYTES ||
      logical_literal_bytes > literal_limit)
    caml_invalid_argument("native program logical literal bytes exceed their bound");
  if (metadata_bytes < 0 || (uintnat)metadata_bytes > HOLYC_NATIVE_MAX_ARENA_BYTES)
    caml_invalid_argument("native program private metadata bytes exceed their bound");
  if (logical_global_bytes == 0 && logical_literal_bytes == 0)
    caml_invalid_argument("native storage program has no persistent data");
  if ((uintnat)arena_length > HOLYC_NATIVE_MAX_ARENA_BYTES ||
      (uintnat)arena_length != (uintnat)logical_global_bytes +
                               (uintnat)logical_literal_bytes + (uintnat)metadata_bytes)
    caml_invalid_argument("native program arena image is inconsistent with data and metadata");

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

static value native_execute_program_output(value code, value functions,
                                            value abi, value limits,
                                            value storage, value retained,
                                            value consumed, value entered,
                                            value task_arena,
                                            value required_arena_bytes)
{
  CAMLparam5(code, functions, abi, limits, storage);
  CAMLxparam5(retained, consumed, entered, task_arena,
              required_arena_bytes);
#if HOLYC_NATIVE_PLATFORM == 0
  caml_failwith("native execution requires Windows or Linux x86-64 with 64-bit pointers");
#else
  CAMLlocal5(output_buffer, captured, status, result, boxed_kind);
  CAMLlocal5(boxed_site, boxed_steps, boxed_value_site, boxed_bits, arena_image);
  intnat step_limit;
  intnat frame_limit;
  intnat depth_limit;
  intnat active_stack_limit;
  intnat entry_stack_bytes;
  intnat global_limit;
  intnat literal_limit;
  intnat output_limit;
  intnat output_work_limit;
  intnat logical_global_bytes;
  intnat logical_literal_bytes;
  intnat metadata_bytes;
  intnat task_extent;
  intnat abi_code;
  mlsize_t arena_length;
  mlsize_t code_length;
  unsigned entry_allocation = 0;
  uint64_t remaining_stack;
  uint64_t output_address;
  uint64_t written;
  uint64_t work;
  uint64_t consumed_steps = 0;
  uint64_t remaining_steps;
  uint64_t remaining_output;
  uint64_t remaining_work;
  int task_storage;

  if (!Is_long(abi))
    caml_invalid_argument("native program status ABI is not integral");
  if (!Is_block(limits) || Tag_val(limits) != 0 || Wosize_val(limits) != 9 ||
      !Is_long(Field(limits, 0)) || !Is_long(Field(limits, 1)) ||
      !Is_long(Field(limits, 2)) || !Is_long(Field(limits, 3)) ||
      !Is_long(Field(limits, 4)) || !Is_long(Field(limits, 5)) ||
      !Is_long(Field(limits, 6)) || !Is_long(Field(limits, 7)) ||
      !Is_long(Field(limits, 8)))
    caml_invalid_argument("native output program limits tuple is malformed");
  if (!Is_block(storage) || Tag_val(storage) != 0 || Wosize_val(storage) != 4 ||
      !Is_long(Field(storage, 0)) || !Is_long(Field(storage, 1)) ||
      !Is_long(Field(storage, 2)))
    caml_invalid_argument("native output program storage tuple is malformed");
  arena_image = Field(storage, 3);
  if (!Is_block(code) || Tag_val(code) != String_tag)
    caml_invalid_argument("native output program code is not a string");

  abi_code = Long_val(abi);
  step_limit = Long_val(Field(limits, 0));
  frame_limit = Long_val(Field(limits, 1));
  depth_limit = Long_val(Field(limits, 2));
  active_stack_limit = Long_val(Field(limits, 3));
  entry_stack_bytes = Long_val(Field(limits, 4));
  global_limit = Long_val(Field(limits, 5));
  literal_limit = Long_val(Field(limits, 6));
  output_limit = Long_val(Field(limits, 7));
  output_work_limit = Long_val(Field(limits, 8));
  logical_global_bytes = Long_val(Field(storage, 0));
  logical_literal_bytes = Long_val(Field(storage, 1));
  metadata_bytes = Long_val(Field(storage, 2));
  task_storage = task_arena != Val_unit;
  if (task_storage) {
    if (!Is_long(arena_image))
      caml_invalid_argument("native task fragment arena extent is not integral");
    task_extent = Long_val(arena_image);
    if (task_extent < 0 || (uintnat)task_extent > HOLYC_NATIVE_MAX_ARENA_BYTES)
      caml_invalid_argument("native task fragment arena extent is outside the host bound");
    arena_length = (mlsize_t)task_extent;
  } else {
    if (!Is_block(arena_image) || Tag_val(arena_image) != String_tag)
      caml_invalid_argument("native output program arena image is not a string");
    arena_length = caml_string_length(arena_image);
  }

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
  if (literal_limit <= 0 || (uintnat)literal_limit > HOLYC_NATIVE_MAX_LITERAL_BYTES)
    caml_invalid_argument("native program max_literal_bytes is outside the host bound");
  if (output_limit <= 0 || (uintnat)output_limit > HOLYC_NATIVE_MAX_OUTPUT_BYTES)
    caml_invalid_argument("native program max_output_bytes is outside the host bound");
  if (output_work_limit <= 0)
    caml_invalid_argument("native program max_output_work must be greater than zero");
  remaining_steps = (uint64_t)step_limit;
  remaining_output = (uint64_t)output_limit;
  remaining_work = (uint64_t)output_work_limit;
  if (consumed != Val_unit) {
    intnat prior_steps, prior_output, prior_work;
    if (!Is_block(entered) || Tag_val(entered) != 0 ||
        Wosize_val(entered) != 1 || Field(entered, 0) != Val_false)
      caml_invalid_argument("retained native entry marker is malformed or consumed");
    if (retained == Val_unit || !Is_block(consumed) ||
        Tag_val(consumed) != 0 || Wosize_val(consumed) != 3 ||
        !Is_long(Field(consumed, 0)) || !Is_long(Field(consumed, 1)) ||
        !Is_long(Field(consumed, 2)))
      caml_invalid_argument("retained native consumed budget is malformed");
    prior_steps = Long_val(Field(consumed, 0));
    prior_output = Long_val(Field(consumed, 1));
    prior_work = Long_val(Field(consumed, 2));
    if (prior_steps < 0 || prior_steps > step_limit ||
        prior_output < 0 || prior_output > output_limit ||
        prior_work < 0 || prior_work > output_work_limit ||
        prior_output > prior_work)
      caml_invalid_argument("retained native consumed budget exceeds its limits");
    consumed_steps = (uint64_t)prior_steps;
    remaining_steps -= consumed_steps;
    remaining_output -= (uint64_t)prior_output;
    remaining_work -= (uint64_t)prior_work;
  }
  if (task_storage) {
    if (retained == Val_unit || consumed == Val_unit || !Is_long(required_arena_bytes))
      caml_invalid_argument("native task fragment execution state is malformed");
  } else if (required_arena_bytes != Val_unit) {
    caml_invalid_argument("ordinary native execution has task arena metadata");
  }
  if (logical_global_bytes < 0 ||
      (uintnat)logical_global_bytes > HOLYC_NATIVE_MAX_GLOBAL_BYTES ||
      logical_global_bytes > global_limit)
    caml_invalid_argument("native program logical global bytes exceed their bound");
  if (logical_literal_bytes < 0 ||
      (uintnat)logical_literal_bytes > HOLYC_NATIVE_MAX_LITERAL_BYTES ||
      logical_literal_bytes > literal_limit)
    caml_invalid_argument("native program logical literal bytes exceed their bound");
  if (metadata_bytes < 0 || (uintnat)metadata_bytes > HOLYC_NATIVE_MAX_ARENA_BYTES)
    caml_invalid_argument("native program private metadata bytes exceed their bound");
  if ((uintnat)arena_length > HOLYC_NATIVE_MAX_ARENA_BYTES ||
      (uintnat)arena_length != (uintnat)logical_global_bytes +
                               (uintnat)logical_literal_bytes + (uintnat)metadata_bytes)
    caml_invalid_argument("native output program arena image is inconsistent with data and metadata");
  if (logical_global_bytes == 0 && logical_literal_bytes == 0 &&
      metadata_bytes != 0 && !task_storage)
    caml_invalid_argument("native output program private metadata has no persistent data");

  /* Validate the sealed code and unwind table before reserving the capture
     buffer. The checked execution helpers repeat these checks at entry. */
  code_length = caml_string_length(code);
  if (retained != Val_unit) {
    int closed_entry = 0;
    unsigned checked_stack =
      native_validate_retained_functions(code, functions, &closed_entry, task_storage);
    if ((uintnat)entry_stack_bytes != checked_stack)
      caml_invalid_argument("native program entry stack metadata does not match its unwind frame");
    if (closed_entry && arena_length != 0)
      caml_invalid_argument("retained native closed entry cannot own a data arena");
  } else {
    (void)native_validate_program_functions(functions, code_length,
                                            &entry_allocation, code, 0);
    if ((uintnat)entry_stack_bytes != (uintnat)entry_allocation + 16u)
      caml_invalid_argument("native program entry stack metadata does not match its unwind frame");
  }
  if (code_length == 0 || code_length > 16u * 1024u * 1024u)
    caml_invalid_argument("native image length is outside the host allocation bound");

  output_buffer = caml_alloc_string((mlsize_t)remaining_output);
  memset((char *)String_val(output_buffer), 0, (size_t)remaining_output);
  output_address = (uint64_t)(uintptr_t)String_val(output_buffer);
  remaining_stack = (uint64_t)(active_stack_limit - entry_stack_bytes);
  {
    uint64_t context[14] = {
      0, 0, remaining_steps, 0, 0, 0,
      (uint64_t)frame_limit, (uint64_t)depth_limit, remaining_stack, 0,
      output_address, remaining_output, remaining_work, 0
    };

    if (retained != Val_unit) {
      if (task_storage) {
        intnat required = Long_val(required_arena_bytes);
        if (required < 0 || (uintnat)required != (uintnat)arena_length)
          caml_invalid_argument("native task fragment arena extent disagrees with its image");
        (void)native_retained_run_task(retained, task_arena,
                                       (uintnat)required, context, entered);
      } else {
        (void)native_retained_run(retained, context, entered);
      }
    } else if (arena_length == 0) {
      (void)native_execute_checked_program_image(
        code, functions, abi_code, (uintnat)entry_stack_bytes, context);
      if (context[9] != 0)
        caml_failwith("native program status integrity failure: arena pointer was modified");
    } else {
      (void)native_execute_checked_program_storage_image(
        code, functions, abi_code, (uintnat)entry_stack_bytes, arena_image,
        context);
    }

    if (context[2] != remaining_steps)
      caml_failwith("native program status integrity failure: budget was modified");
    if (context[3] > remaining_steps)
      caml_failwith("native program status integrity failure: steps exceed the remaining budget");
    if (consumed != Val_unit &&
        ((context[0] == 3 && context[3] != remaining_steps) ||
         (context[3] == 0 && (context[0] != 3 || remaining_steps != 0))))
      caml_failwith("retained native status integrity failure: activation work disagrees with its fault");
    if (context[6] != (uint64_t)frame_limit)
      caml_failwith("native program status integrity failure: frame quota was not restored");
    if (context[7] != (uint64_t)depth_limit)
      caml_failwith("native program status integrity failure: call-depth quota was not restored");
    if (context[8] != remaining_stack)
      caml_failwith("native program status integrity failure: active-stack quota was not restored");
    if (context[10] != output_address)
      caml_failwith("native program status integrity failure: output pointer was modified");
    if (context[11] > remaining_output ||
        context[12] > remaining_work ||
        context[13] > remaining_output)
      caml_failwith("native program status integrity failure: output counters exceed their bounds");

    written = remaining_output - context[11];
    work = remaining_work - context[12];
    if (context[13] != written)
      caml_failwith("native program status integrity failure: output byte count is inconsistent");

    /* Every value above is validated before the first post-execution allocation.
       Re-read the rooted source pointer after allocating the exact result string. */
    captured = caml_alloc_string((mlsize_t)written);
    if (written != 0)
      memcpy((char *)String_val(captured), String_val(output_buffer),
             (size_t)written);

    boxed_kind = native_box_word(context[0]);
    boxed_site = native_box_word(context[1]);
    boxed_steps = native_box_word(consumed_steps + context[3]);
    boxed_value_site = native_box_word(context[4]);
    boxed_bits = native_box_word(context[5]);
    status = caml_alloc_tuple(5);
    Store_field(status, 0, boxed_kind);
    Store_field(status, 1, boxed_site);
    Store_field(status, 2, boxed_steps);
    Store_field(status, 3, boxed_value_site);
    Store_field(status, 4, boxed_bits);
  }

  result = caml_alloc_tuple(3);
  Store_field(result, 0, status);
  Store_field(result, 1, captured);
  Store_field(result, 2, Val_long((intnat)work));
  CAMLreturn(result);
#endif
  CAMLreturn(Val_unit);
}


CAMLprim value holyc_native_execute_program_output(value code, value functions,
                                                  value abi, value limits,
                                                  value storage)
{
  return native_execute_program_output(code, functions, abi, limits, storage,
                                        Val_unit, Val_unit, Val_unit,
                                        Val_unit, Val_unit);
}

#if HOLYC_NATIVE_PLATFORM != 0
static size_t native_validate_task_bindings(value identity, size_t prefix)
{
  value bindings = Field(identity, 5), functions = Field(identity, 1);
  mlsize_t count, index, function_count;
  size_t maximum = 0, previous_id = 0, previous_end = 0;
  if (!Is_block(bindings) || Tag_val(bindings) != 0)
    caml_invalid_argument("native task entry bindings are not an array");
  count = Wosize_val(bindings);
  function_count = Wosize_val(functions);
  if (count > HOLYC_NATIVE_MAX_FUNCTIONS || count >= function_count)
    caml_invalid_argument("native task entry binding count exceeds its function table");
  for (index = 0; index < function_count - count; ++index)
    if (caml_string_length(Field(Field(functions, index), 2)) == 4)
      caml_invalid_argument("native task function table has an unowned leaf entry");
  for (index = 0; index < count; ++index) {
    value binding = Field(bindings, index), range;
    intnat id, address, target, body, leaf;
    const unsigned char *encoded;
    uint32_t displacement;
    unsigned field;
    if (!Is_block(binding) || Tag_val(binding) != 0 || Wosize_val(binding) != 5)
      caml_invalid_argument("native task entry binding is malformed");
    for (field = 0; field < 5; ++field)
      if (!Is_long(Field(binding, field)))
        caml_invalid_argument("native task entry binding field is not integral");
    id = Long_val(Field(binding, 0)); address = Long_val(Field(binding, 1));
    target = Long_val(Field(binding, 2)); body = Long_val(Field(binding, 3));
    leaf = Long_val(Field(binding, 4));
    if (id <= 0 || (uintnat)id > HOLYC_NATIVE_MAX_FUNCTIONS || (size_t)id <= previous_id ||
        address < 0 || (size_t)address < previous_end || target != address + 8 ||
        (size_t)target > prefix || prefix - (size_t)target < 8 ||
        body <= 0 || (uintnat)body >= function_count - count ||
        leaf != (intnat)(function_count - count + index))
      caml_invalid_argument("native task entry binding leaves its original ranges");
    range = Field(functions, leaf);
    if (Long_val(Field(range, 1)) - Long_val(Field(range, 0)) != 8 ||
        caml_string_length(Field(range, 2)) != 4 ||
        memcmp(String_val(Field(range, 2)), "\001\000\000\000", 4) != 0)
      caml_invalid_argument("native task entry has another leaf unwind range");
    encoded = (const unsigned char *)String_val(Field(identity, 0)) + Long_val(Field(range, 0));
    memcpy(&displacement, encoded + 4, 4);
    if (displacement != (uint32_t)target || encoded[0] != 0x90 || encoded[1] != 0x41 ||
        encoded[2] != 0xff || encoded[3] != 0xa1)
      caml_invalid_argument("native task entry jump has another target cell");
    previous_id = maximum = (size_t)id;
    previous_end = (size_t)target + 8;
  }
  return maximum;
}
#endif

static value native_retain_program_identity(value identity, int task_fragment)
{
  CAMLparam1(identity);
#if HOLYC_NATIVE_PLATFORM == 0
  caml_failwith("native execution requires Windows or Linux x86-64 with 64-bit pointers");
#else
  CAMLlocal1(handle);
  value code, functions, storage, arena_image;
  intnat globals, literals, metadata, task_extent;
  size_t code_length, arena_length;
  unsigned checked_stack = 0;
  int closed_entry = 0;
  struct native_retained_program *program;
  if (!Is_block(identity) || Tag_val(identity) != 0 || (Wosize_val(identity) != 5 && !(task_fragment && Wosize_val(identity) == 6)) ||
      !Is_long(Field(identity, 2)) || !Is_long(Field(identity, 3)))
    caml_invalid_argument("retained native image identity is malformed");
  code = Field(identity, 0);
  functions = Field(identity, 1);
  storage = Field(identity, 4);
  if (!Is_block(code) || Tag_val(code) != String_tag ||
      !Is_block(storage) || Tag_val(storage) != 0 || Wosize_val(storage) != 4 ||
      !Is_long(Field(storage, 0)) || !Is_long(Field(storage, 1)) ||
      !Is_long(Field(storage, 2)))
    caml_invalid_argument("retained native image code or storage is malformed");
  if (Long_val(Field(identity, 2)) != HOLYC_NATIVE_PLATFORM)
    caml_invalid_argument("native program status ABI does not match this process");
  code_length = (size_t)caml_string_length(code);
  arena_image = Field(storage, 3);
  if (task_fragment) {
    if (!Is_long(arena_image))
      caml_invalid_argument("retained native task arena extent is not integral");
    task_extent = Long_val(arena_image);
    if (task_extent < 0 || (uintnat)task_extent > HOLYC_NATIVE_MAX_ARENA_BYTES)
      caml_invalid_argument("retained native task arena extent is outside the host bound");
    arena_length = (size_t)task_extent;
  } else {
    if (!Is_block(arena_image) || Tag_val(arena_image) != String_tag)
      caml_invalid_argument("retained native image arena is not a string");
    arena_length = (size_t)caml_string_length(arena_image);
  }
  if (code_length == 0 || code_length > 16u * 1024u * 1024u)
    caml_invalid_argument("native image length is outside the host allocation bound");
  checked_stack = native_validate_retained_functions(code, functions, &closed_entry, task_fragment);
  if (Long_val(Field(identity, 3)) != (intnat)checked_stack)
    caml_invalid_argument("native program entry stack metadata does not match its unwind frame");
  globals = Long_val(Field(storage, 0));
  literals = Long_val(Field(storage, 1));
  metadata = Long_val(Field(storage, 2));
  if (globals < 0 || (uintnat)globals > HOLYC_NATIVE_MAX_GLOBAL_BYTES ||
      literals < 0 || (uintnat)literals > HOLYC_NATIVE_MAX_LITERAL_BYTES ||
      metadata < 0 || (uintnat)metadata > HOLYC_NATIVE_MAX_ARENA_BYTES ||
      arena_length > HOLYC_NATIVE_MAX_ARENA_BYTES ||
      arena_length != (uintnat)globals + (uintnat)literals + (uintnat)metadata ||
      (globals == 0 && literals == 0 && metadata != 0 && Wosize_val(identity) != 6))
    caml_invalid_argument("retained native arena image is inconsistent with data and metadata");
  if (Wosize_val(identity) != 6) {
    for (mlsize_t index = 0; index < Wosize_val(functions); ++index)
      if (caml_string_length(Field(Field(functions, index), 2)) == 4)
        caml_invalid_argument("native leaf entries require original task owner bindings");
  }
  if (Wosize_val(identity) == 6) {
    size_t maximum = native_validate_task_bindings(identity, arena_length);
    if (globals == 0 && literals == 0 && metadata != 0 && maximum == 0)
      caml_invalid_argument("retained task metadata has no original code owner");
  }
  if (closed_entry && arena_length != 0)
    caml_invalid_argument("retained native closed entry cannot own a data arena");
  handle = caml_alloc_custom_mem(&native_retained_operations, sizeof(program),
                                  code_length + (task_fragment ? 0u : arena_length));
  *((struct native_retained_program **)Data_custom_val(handle)) = NULL;
  program = calloc(1, sizeof(*program));
  if (program == NULL) caml_raise_out_of_memory();
  *((struct native_retained_program **)Data_custom_val(handle)) = program;
  atomic_init(&program->active, 0);
  atomic_init(&program->task_readers, 0);
  program->closed_entry = closed_entry;
  program->task_fragment = task_fragment;
  program->identity = identity;
  caml_register_generational_global_root(&program->identity);
  /* The allocation above can move the original rooted tuple and its children. */
  code = Field(identity, 0);
  functions = Field(identity, 1);
  storage = Field(identity, 4);
  arena_image = task_fragment ? Val_unit : Field(storage, 3);
  native_retained_map(program, code, functions, arena_image, arena_length,
                      !task_fragment);
  CAMLreturn(handle);
#endif
  CAMLreturn(Val_unit);
}

CAMLprim value holyc_native_retain_program(value identity)
{
  return native_retain_program_identity(identity, 0);
}

CAMLprim value holyc_native_retain_task_fragment(value identity)
{
  return native_retain_program_identity(identity, 1);
}

CAMLprim value holyc_native_create_task_arena(value capacity)
{
  CAMLparam1(capacity);
#if HOLYC_NATIVE_PLATFORM == 0
  caml_failwith("native execution requires Windows or Linux x86-64 with 64-bit pointers");
#else
  CAMLlocal1(handle);
  intnat requested;
  struct native_task_arena *arena;
  if (!Is_long(capacity))
    caml_invalid_argument("native task arena capacity is not integral");
  requested = Long_val(capacity);
  if (requested <= 0 || (uintnat)requested > HOLYC_NATIVE_MAX_ARENA_BYTES)
    caml_invalid_argument("native task arena capacity is outside the host bound");
  handle = caml_alloc_custom_mem(&native_task_arena_operations, sizeof(arena),
                                  (uintnat)requested);
  *((struct native_task_arena **)Data_custom_val(handle)) = NULL;
  arena = calloc(1, sizeof(*arena));
  if (arena == NULL) caml_raise_out_of_memory();
  *((struct native_task_arena **)Data_custom_val(handle)) = arena;
  atomic_init(&arena->active, 0);
  arena->capacity = (size_t)requested;
#if HOLYC_NATIVE_PLATFORM == 1
  {
    SYSTEM_INFO info;
    GetSystemInfo(&info);
    arena->page_size = (size_t)info.dwPageSize;
    if (arena->page_size == 0) {
      *((struct native_task_arena **)Data_custom_val(handle)) = NULL;
      free(arena);
      caml_failwith("native task arena has an invalid host page size");
    }
    arena->mapping = VirtualAlloc(NULL, arena->capacity, MEM_RESERVE,
                                  PAGE_NOACCESS);
    if (arena->mapping == NULL) {
      DWORD error = GetLastError();
      *((struct native_task_arena **)Data_custom_val(handle)) = NULL;
      free(arena);
      native_os_error("task arena reservation", (unsigned long)error);
    }
  }
#else
  {
    int flags = personality(0xffffffffUL);
    long page_size;
    if (flags == -1) {
      unsigned long error = (unsigned long)errno;
      *((struct native_task_arena **)Data_custom_val(handle)) = NULL;
      free(arena);
      native_os_error("personality query", error);
    }
    if ((flags & READ_IMPLIES_EXEC) != 0) {
      *((struct native_task_arena **)Data_custom_val(handle)) = NULL;
      free(arena);
      caml_failwith("native task arena requires READ_IMPLIES_EXEC to be disabled");
    }
    page_size = sysconf(_SC_PAGESIZE);
    if (page_size <= 0) {
      *((struct native_task_arena **)Data_custom_val(handle)) = NULL;
      free(arena);
      caml_failwith("native task arena has an invalid host page size");
    }
    arena->page_size = (size_t)page_size;
    arena->mapping = mmap(NULL, arena->capacity, PROT_NONE,
                          MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (arena->mapping == MAP_FAILED) {
      unsigned long error = (unsigned long)errno;
      arena->mapping = NULL;
      *((struct native_task_arena **)Data_custom_val(handle)) = NULL;
      free(arena);
      native_os_error("task arena reservation", error);
    }
  }
#endif
  CAMLreturn(handle);
#endif
  CAMLreturn(Val_unit);
}

CAMLprim value holyc_native_task_arena_admit(value handle, value expected_used,
                                             value required_extent, value literal_chunks)
{
  CAMLparam4(handle, expected_used, required_extent, literal_chunks);
#if HOLYC_NATIVE_PLATFORM == 0
  caml_failwith("native execution requires Windows or Linux x86-64 with 64-bit pointers");
#else
  struct native_task_arena *arena = native_task_arena_get(handle);
  int expected = 0;
  intnat expected_prefix;
  intnat extent;
  size_t target;
  value chunks;
  size_t initialization_count = 0;
  size_t payload_bytes = 0;
  unsigned long commit_error;
  if (!Is_long(expected_used) || !Is_long(required_extent))
    caml_invalid_argument("native task arena admission is malformed");
  expected_prefix = Long_val(expected_used);
  extent = Long_val(required_extent);
  if (expected_prefix < 0 || extent < expected_prefix
      || (uintnat)extent > HOLYC_NATIVE_MAX_ARENA_BYTES)
    caml_invalid_argument("native task arena extent is outside its host bound");
  for (chunks = literal_chunks; chunks != Val_emptylist; chunks = Field(chunks, 1)) {
    value chunk;
    intnat offset;
    mlsize_t length;
    if (!Is_block(chunks) || Tag_val(chunks) != 0 || Wosize_val(chunks) != 2)
      caml_invalid_argument("native task literal initialization list is malformed");
    chunk = Field(chunks, 0);
    if (!Is_block(chunk) || Tag_val(chunk) != 0 || Wosize_val(chunk) != 2
        || !Is_long(Field(chunk, 0)) || !Is_block(Field(chunk, 1))
        || Tag_val(Field(chunk, 1)) != String_tag)
      caml_invalid_argument("native task literal initialization is malformed");
    offset = Long_val(Field(chunk, 0));
    length = caml_string_length(Field(chunk, 1));
    if (++initialization_count > HOLYC_NATIVE_MAX_ARENA_BYTES / 65u
        || length > (uintnat)(extent - expected_prefix) - payload_bytes)
      caml_invalid_argument("native task literal initialization exceeds its suffix bound");
    payload_bytes += length;
    if (offset < expected_prefix || offset >= extent
        || length >= (uintnat)(extent - offset))
      caml_invalid_argument("native task literal initialization leaves the new suffix");
  }
  if (!atomic_compare_exchange_strong(&arena->active, &expected, 1))
    caml_invalid_argument("native task arena is already active");
  if (arena->closing || arena->mapping == NULL) {
    atomic_store(&arena->active, 0);
    caml_invalid_argument("native task arena has been released");
  }
  if ((size_t)expected_prefix != arena->used) {
    atomic_store(&arena->active, 0);
    caml_invalid_argument("native task arena admission does not extend its current prefix");
  }
  target = (size_t)extent;
  if (target < arena->used || target > arena->capacity) {
    atomic_store(&arena->active, 0);
    caml_invalid_argument("native task arena extent is outside its reserved capacity");
  }
  commit_error = native_task_arena_commit(arena, target);
  if (commit_error != 0) {
    atomic_store(&arena->active, 0);
    native_os_error("task arena commit", commit_error);
  }
  if (target > arena->used)
    memset((char *)arena->mapping + arena->used, 0, target - arena->used);
  for (chunks = literal_chunks; chunks != Val_emptylist; chunks = Field(chunks, 1)) {
    value chunk = Field(chunks, 0);
    value payload = Field(chunk, 1);
    memcpy((char *)arena->mapping + Long_val(Field(chunk, 0)),
           String_val(payload), caml_string_length(payload));
  }
  arena->used = target;
  atomic_store(&arena->active, 0);
  CAMLreturn(Val_long((intnat)target));
#endif
  CAMLreturn(Val_unit);
}

CAMLprim value holyc_native_task_static_copy(value handle, value descriptor)
{
  CAMLparam2(handle, descriptor);
#if HOLYC_NATIVE_PLATFORM == 0
  caml_failwith("native execution requires Windows or Linux x86-64 with 64-bit pointers");
#else
  struct native_task_arena *arena = native_task_arena_get(handle);
  int expected = 0;
  intnat prefix, data, flag;
  mlsize_t count;
  size_t lowest_flag, i;
  value payload;
  unsigned char *mapping;
  if (!Is_block(descriptor) || Tag_val(descriptor) != 0
      || Wosize_val(descriptor) != 4
      || !Is_long(Field(descriptor, 0)) || !Is_long(Field(descriptor, 1))
      || !Is_long(Field(descriptor, 2)) || !Is_block(Field(descriptor, 3))
      || Tag_val(Field(descriptor, 3)) != String_tag)
    caml_invalid_argument("native static byte copy descriptor is malformed");
  prefix = Long_val(Field(descriptor, 0));
  data = Long_val(Field(descriptor, 1));
  flag = Long_val(Field(descriptor, 2));
  payload = Field(descriptor, 3);
  count = caml_string_length(payload);
  if (prefix <= 0 || (uintnat)prefix > HOLYC_NATIVE_MAX_ARENA_BYTES
      || data < 0 || data >= prefix || count == 0
      || count > (uintnat)(prefix - data)
      || flag < 0 || flag >= prefix || count - 1 > (uintnat)flag / 8)
    caml_invalid_argument("native static byte copy leaves its admitted extent");
  lowest_flag = (size_t)flag - (count - 1) * 8;
  if (lowest_flag < (size_t)data + count)
    caml_invalid_argument("native static byte copy overlaps its initialization flags");
  if (!atomic_compare_exchange_strong(&arena->active, &expected, 1))
    caml_invalid_argument("native task arena is already active");
  if (arena->closing || arena->mapping == NULL) {
    atomic_store(&arena->active, 0);
    caml_invalid_argument("native task arena has been released");
  }
  if ((size_t)prefix != arena->used || arena->used > arena->capacity) {
    atomic_store(&arena->active, 0);
    caml_invalid_argument("native static byte copy has another admitted arena prefix");
  }
  mapping = arena->mapping;
  /* Earlier original expressions may already have written these elements.
     Validate the flag representation without treating initialization as replay. */
  for (i = 0; i < count; ++i) {
    if (mapping[(size_t)flag - i * 8] > 1) {
      atomic_store(&arena->active, 0);
      caml_invalid_argument("native static byte copy has a malformed initialization flag");
    }
  }
  memcpy(mapping + data, String_val(payload), count);
  for (i = 0; i < count; ++i) mapping[(size_t)flag - i * 8] = 1;
  atomic_store(&arena->active, 0);
  CAMLreturn(Val_long((intnat)count));
#endif
  CAMLreturn(Val_unit);
}

CAMLprim value holyc_native_bind_task_entries(value retained, value descriptor)
{
  CAMLparam2(retained, descriptor);
#if HOLYC_NATIVE_PLATFORM == 0
  caml_failwith("native execution requires Windows or Linux x86-64 with 64-bit pointers");
#else
  struct native_retained_program *program = native_retained_get(retained);
  struct native_task_arena *arena;
  value bindings, functions;
  mlsize_t count, index;
  size_t prefix, maximum = 0;
  int expected = 0, canonical = 0;
  if (!Is_block(descriptor) || Tag_val(descriptor) != 0 || Wosize_val(descriptor) != 2 ||
      !Is_long(Field(descriptor, 1)) || Long_val(Field(descriptor, 1)) < 0)
    caml_invalid_argument("native task entry binding descriptor is malformed");
  arena = native_task_arena_get(Field(descriptor, 0));
  prefix = (size_t)Long_val(Field(descriptor, 1));
  if (!program->task_fragment || Wosize_val(program->identity) != 6 ||
      prefix != (size_t)Long_val(Field(Field(program->identity, 4), 3)))
    caml_invalid_argument("native task entries have another retained arena extent");
  bindings = Field(program->identity, 5);
  functions = Field(program->identity, 1);
  maximum = native_validate_task_bindings(program->identity, prefix);
  count = Wosize_val(bindings);
  if (!atomic_compare_exchange_strong(&arena->active, &expected, 1))
    caml_invalid_argument("native task arena is already active");
  expected = 0;
  if (!atomic_compare_exchange_strong(&program->active, &expected, 1)) {
    atomic_store(&arena->active, 0);
    caml_invalid_argument("retained native image is already active");
  }
#define ENTRY_FAIL(message) do { atomic_store(&program->active, 0); atomic_store(&arena->active, 0); caml_invalid_argument(message); } while (0)
  if (arena->closing || arena->mapping == NULL || program->closing || program->mapping == NULL)
    ENTRY_FAIL("native task entry binding has a released resource");
  if (prefix != arena->used || arena->used > arena->capacity)
    ENTRY_FAIL("native task entry binding has another admitted prefix");
  if (maximum >= arena->owner_capacity) {
    size_t capacity = maximum + 1;
    struct native_task_code_owner **owners = realloc(arena->owners, capacity * sizeof(*owners));
    if (owners == NULL) {
      atomic_store(&program->active, 0); atomic_store(&arena->active, 0);
      caml_raise_out_of_memory();
    }
    memset(owners + arena->owner_capacity, 0, (capacity - arena->owner_capacity) * sizeof(*owners));
    arena->owners = owners; arena->owner_capacity = capacity;
  }
  for (index = 0; index < count; ++index) {
    size_t id = (size_t)Long_val(Field(Field(bindings, index), 0));
    if (arena->owners[id] == NULL) {
      struct native_task_code_owner *owner = calloc(1, sizeof(*owner));
      if (owner == NULL) {
        atomic_store(&program->active, 0); atomic_store(&arena->active, 0);
        caml_raise_out_of_memory();
      }
      owner->canonical_handle = Val_unit; owner->current_handle = Val_unit;
      caml_register_generational_global_root(&owner->canonical_handle);
      caml_register_generational_global_root(&owner->current_handle);
      arena->owners[id] = owner;
    }
  }
  /* Validate every old cell before publishing any mapping or target. */
  for (index = 0; index < count; ++index) {
    value binding = Field(bindings, index);
    struct native_task_code_owner *owner = arena->owners[Long_val(Field(binding, 0))];
    uint64_t address, current_target;
    size_t cell = (size_t)Long_val(Field(binding, 1));
    memcpy(&address, (char *)arena->mapping + cell, 8);
    memcpy(&current_target, (char *)arena->mapping + Long_val(Field(binding, 2)), 8);
    if (owner->canonical == 0) {
      uint64_t target;
      memcpy(&target, (char *)arena->mapping + Long_val(Field(binding, 2)), 8);
      if (address != 0 || target != 0) ENTRY_FAIL("unpublished native task owner cells are not empty");
    } else if (owner->address != cell || owner->target != (size_t)Long_val(Field(binding, 2)) ||
               address != owner->canonical || current_target != owner->current_target ||
               owner->program->closing || owner->program->mapping == NULL ||
               owner->current_program->closing || owner->current_program->mapping == NULL)
      ENTRY_FAIL("native task owner differs from its original mapped entry");
  }
  for (index = 0; index < count; ++index) {
    value binding = Field(bindings, index);
    struct native_task_code_owner *owner = arena->owners[Long_val(Field(binding, 0))];
    value body_range = Field(functions, Long_val(Field(binding, 3)));
    value leaf_range = Field(functions, Long_val(Field(binding, 4)));
    uint64_t target = (uint64_t)(uintptr_t)((char *)program->mapping + Long_val(Field(body_range, 0)));
    if (owner->canonical == 0) {
      owner->address = (size_t)Long_val(Field(binding, 1));
      owner->target = (size_t)Long_val(Field(binding, 2));
      owner->canonical = (uint64_t)(uintptr_t)((char *)program->mapping + Long_val(Field(leaf_range, 0)));
      owner->program = program;
      caml_modify_generational_global_root(&owner->canonical_handle, retained);
      memcpy((char *)arena->mapping + owner->address, &owner->canonical, 8);
      canonical = 1;
    }
    memcpy((char *)arena->mapping + owner->target, &target, 8);
    owner->current_target = target;
    owner->current_program = program;
    caml_modify_generational_global_root(&owner->current_handle, retained);
  }
  atomic_store(&program->active, 0); atomic_store(&arena->active, 0);
#undef ENTRY_FAIL
  CAMLreturn(Val_bool(canonical));
#endif
  CAMLreturn(Val_false);
}

CAMLprim value holyc_native_release_task_arena(value handle)
{
  CAMLparam1(handle);
#if HOLYC_NATIVE_PLATFORM == 0
  caml_failwith("native execution requires Windows or Linux x86-64 with 64-bit pointers");
#else
  struct native_task_arena *arena = native_task_arena_get(handle);
  int expected = 0;
  int success;
  char message[240];
  if (!atomic_compare_exchange_strong(&arena->active, &expected, 1))
    caml_invalid_argument("native task arena is already active");
  success = native_task_arena_close(arena);
  memcpy(message, arena->close_error, sizeof(message));
  atomic_store(&arena->active, 0);
  if (!success) caml_failwith(message);
#endif
  CAMLreturn(Val_unit);
}

CAMLprim value holyc_native_release_program(value handle)
{
  CAMLparam1(handle);
#if HOLYC_NATIVE_PLATFORM == 0
  caml_failwith("native execution requires Windows or Linux x86-64 with 64-bit pointers");
#else
  struct native_retained_program *program = native_retained_get(handle);
  int expected = 0;
  int success;
  char message[240];
  if (!atomic_compare_exchange_strong(&program->active, &expected, 1))
    caml_invalid_argument("retained native image is already active");
  success = native_retained_close(program);
  memcpy(message, program->close_error, sizeof(message));
  atomic_store(&program->active, 0);
  if (!success) caml_failwith(message);
#endif
  CAMLreturn(Val_unit);
}

CAMLprim value holyc_native_execute_retained_program(value handle, value limits)
{
  CAMLparam2(handle, limits);
#if HOLYC_NATIVE_PLATFORM == 0
  caml_failwith("native execution requires Windows or Linux x86-64 with 64-bit pointers");
#else
  CAMLlocal1(identity);
  struct native_retained_program *program = native_retained_get(handle);
  identity = program->identity;
  CAMLreturn(native_execute_program_output(
    Field(identity, 0), Field(identity, 1), Field(identity, 2), limits,
    Field(identity, 4), handle, Val_unit, Val_unit, Val_unit, Val_unit));
#endif
  CAMLreturn(Val_unit);
}

CAMLprim value holyc_native_execute_retained_budget_program(value handle,
                                                           value limits,
                                                           value consumed,
                                                           value entered)
{
  CAMLparam4(handle, limits, consumed, entered);
#if HOLYC_NATIVE_PLATFORM == 0
  caml_failwith("native execution requires Windows or Linux x86-64 with 64-bit pointers");
#else
  CAMLlocal1(identity);
  struct native_retained_program *program = native_retained_get(handle);
  if (consumed == Val_unit)
    caml_invalid_argument("retained native consumed budget is malformed");
  identity = program->identity;
  CAMLreturn(native_execute_program_output(
    Field(identity, 0), Field(identity, 1), Field(identity, 2), limits,
    Field(identity, 4), handle, consumed, entered, Val_unit, Val_unit));
#endif
  CAMLreturn(Val_unit);
}

CAMLprim value holyc_native_execute_retained_budget_task_program(
  value handle, value task, value limits, value consumed, value entered)
{
  CAMLparam5(handle, task, limits, consumed, entered);
#if HOLYC_NATIVE_PLATFORM == 0
  caml_failwith("native execution requires Windows or Linux x86-64 with 64-bit pointers");
#else
  CAMLlocal2(identity, arena_handle);
  struct native_retained_program *program = native_retained_get(handle);
  if (consumed == Val_unit || !Is_block(task) || Tag_val(task) != 0 ||
      Wosize_val(task) != 2 || !Is_long(Field(task, 1)))
    caml_invalid_argument("retained native task execution state is malformed");
  arena_handle = Field(task, 0);
  (void)native_task_arena_get(arena_handle);
  identity = program->identity;
  CAMLreturn(native_execute_program_output(
    Field(identity, 0), Field(identity, 1), Field(identity, 2), limits,
    Field(identity, 4), handle, consumed, entered, arena_handle, Field(task, 1)));
#endif
  CAMLreturn(Val_unit);
}
