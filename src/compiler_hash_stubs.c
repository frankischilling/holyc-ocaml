#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <caml/custom.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>

/* KernelA.HH:639-645. This owns the count-bearing prefix, not a complete
   CHashFun, CHashTable, compiler control or executable allocation. */
struct compiler_hash_prefix {
  uint64_t next;
  uint64_t str;
  uint32_t type;
  uint32_t use_cnt;
};

_Static_assert(sizeof(struct compiler_hash_prefix) == 24,
               "CHash prefix size differs");
_Static_assert(offsetof(struct compiler_hash_prefix, type) == 16,
               "CHash type offset differs");
_Static_assert(offsetof(struct compiler_hash_prefix, use_cnt) == 20,
               "CHash use_cnt offset differs");

static void compiler_hash_finalize(value handle) {
  struct compiler_hash_prefix *record =
      *(struct compiler_hash_prefix **)Data_custom_val(handle);
  if (record != NULL) {
    free((void *)(uintptr_t)record->str);
    free(record);
    *(struct compiler_hash_prefix **)Data_custom_val(handle) = NULL;
  }
}

static struct custom_operations compiler_hash_operations = {
  "holyc.compiler.hash-prefix",
  compiler_hash_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

static struct compiler_hash_prefix *compiler_hash_original(value handle) {
  if (!Is_block(handle) || Tag_val(handle) != Custom_tag ||
      Custom_ops_val(handle) != &compiler_hash_operations)
    caml_invalid_argument("foreign compiler hash storage");
  struct compiler_hash_prefix *record =
      *(struct compiler_hash_prefix **)Data_custom_val(handle);
  if (record == NULL)
    caml_invalid_argument("expired compiler hash storage");
  return record;
}

static void compiler_hash_increment(struct compiler_hash_prefix *record) {
  record->use_cnt++;
}

CAMLprim value holyc_compiler_hash_create_function(value name) {
  CAMLparam1(name);
  CAMLlocal1(handle);
  mlsize_t length = caml_string_length(name);
  if (length == 0 || memchr(String_val(name), 0, length) != NULL)
    caml_invalid_argument("compiler hash name is empty or contains NUL");
  handle = caml_alloc_custom(&compiler_hash_operations,
                            sizeof(struct compiler_hash_prefix *),
                            sizeof(struct compiler_hash_prefix) + length + 1,
                            1024 * 1024);
  *(struct compiler_hash_prefix **)Data_custom_val(handle) = NULL;
  struct compiler_hash_prefix *record = calloc(1, sizeof(*record));
  if (record == NULL) caml_raise_out_of_memory();
  *(struct compiler_hash_prefix **)Data_custom_val(handle) = record;
  char *copy = malloc(length + 1);
  if (copy == NULL) caml_raise_out_of_memory();
  memcpy(copy, String_val(name), length);
  copy[length] = 0;
  record->str = (uint64_t)(uintptr_t)copy;
  record->type = UINT32_C(0x40);
  CAMLreturn(handle);
}

CAMLprim value holyc_compiler_hash_use_count(value handle) {
  CAMLparam1(handle);
  uint32_t count = compiler_hash_original(handle)->use_cnt;
  CAMLreturn(caml_copy_int64((int64_t)count));
}

CAMLprim value holyc_compiler_hash_increment(value handle) {
  CAMLparam1(handle);
  compiler_hash_increment(compiler_hash_original(handle));
  CAMLreturn(Val_unit);
}

CAMLprim value holyc_compiler_hash_reset(value handle) {
  CAMLparam1(handle);
  compiler_hash_original(handle)->use_cnt = 0;
  CAMLreturn(Val_unit);
}

CAMLprim value holyc_compiler_hash_matches_function(value handle, value name) {
  CAMLparam2(handle, name);
  struct compiler_hash_prefix *record = compiler_hash_original(handle);
  size_t length = caml_string_length(name);
  const char *original = (const char *)(uintptr_t)record->str;
  CAMLreturn(Val_bool(record->type == UINT32_C(0x40) &&
                     strlen(original) == length &&
                     memcmp(original, String_val(name), length) == 0));
}

CAMLprim value holyc_compiler_hash_verify_storage(value unit) {
  CAMLparam1(unit);
#if (defined(__x86_64__) || defined(_M_X64)) && defined(__GNUC__)
  const uint32_t counts[] = { 0, 1, 2, UINT32_MAX - 1, UINT32_MAX };
  for (size_t i = 0; i < sizeof(counts) / sizeof(counts[0]); i++) {
    struct compiler_hash_prefix actual = { 7, 11, 0x40, counts[i] };
    struct compiler_hash_prefix reference = actual;
    compiler_hash_increment(&actual);
    /* KHashA.HC:69 selects this field and uses INC U32. */
    __asm__ volatile("incl %0" : "+m"(reference.use_cnt) : : "cc");
    if (actual.use_cnt != reference.use_cnt || actual.next != 7 ||
        actual.str != 11 || actual.type != 0x40)
      CAMLreturn(Val_false);
  }
  CAMLreturn(Val_true);
#else
  CAMLreturn(Val_false);
#endif
}
