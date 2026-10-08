#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <stdatomic.h>
#include <caml/custom.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>

/* KernelA.HH:639-651. Payloads following CHash remain separately owned by
   the compiler. Neither these allocations nor their addresses export an ABI. */
struct compiler_hash_prefix {
  uint64_t next;
  uint64_t str;
  uint32_t type;
  uint32_t use_cnt;
};

struct compiler_hash_table {
  uint64_t next;
  int64_t mask;
  int64_t locked_flags;
  uint64_t body;
};

struct compiler_table_owner;
struct compiler_record_owner {
  struct compiler_hash_prefix prefix;
  atomic_size_t references;
  _Atomic(struct compiler_table_owner *) table;
};
struct compiler_table_member {
  struct compiler_record_owner *record;
  struct compiler_table_member *next;
};
struct compiler_table_owner {
  struct compiler_hash_table table;
  atomic_size_t references;
  struct compiler_table_member *members;
};

_Static_assert(sizeof(struct compiler_hash_prefix) == 24,
               "CHash prefix size differs");
_Static_assert(offsetof(struct compiler_hash_prefix, type) == 16,
               "CHash type offset differs");
_Static_assert(offsetof(struct compiler_hash_prefix, use_cnt) == 20,
               "CHash use_cnt offset differs");
_Static_assert(sizeof(struct compiler_hash_table) == 32,
               "CHashTable size differs");
_Static_assert(offsetof(struct compiler_hash_table, mask) == 8,
               "CHashTable mask offset differs");
_Static_assert(offsetof(struct compiler_hash_table, locked_flags) == 16,
               "CHashTable locked_flags offset differs");
_Static_assert(offsetof(struct compiler_hash_table, body) == 24,
               "CHashTable body offset differs");
_Static_assert(sizeof(void *) == 8, "compiler hash storage requires 64 bits");

/* KHashA.HC:1-27: SHL/ADC preserves the outgoing bit in each addition;
   the final SHR/ADC adds bit 15, not the carry of the subsequent addition. */
static uint64_t compiler_hash_str(const unsigned char *str) {
  uint64_t hash = 0;
  if (str == NULL) return 0;
  while (*str != 0) {
    uint64_t carry = hash >> 63;
    hash = (hash << 1) + *str++ + carry;
  }
  return hash + (hash >> 16) + ((hash >> 15) & 1);
}

static void compiler_record_release(struct compiler_record_owner *record) {
  if (atomic_fetch_sub(&record->references, 1) == 1) {
    free((void *)(uintptr_t)record->prefix.str);
    free(record);
  }
}

static void compiler_hash_finalize(value handle) {
  struct compiler_hash_prefix *record =
      *(struct compiler_hash_prefix **)Data_custom_val(handle);
  if (record != NULL) {
    compiler_record_release((struct compiler_record_owner *)record);
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
                            sizeof(struct compiler_record_owner) + length + 1,
                            1024 * 1024);
  *(struct compiler_hash_prefix **)Data_custom_val(handle) = NULL;
  struct compiler_record_owner *owner = calloc(1, sizeof(*owner));
  if (owner == NULL) caml_raise_out_of_memory();
  atomic_init(&owner->references, 1);
  atomic_init(&owner->table, NULL);
  struct compiler_hash_prefix *record = &owner->prefix;
  *(struct compiler_hash_prefix **)Data_custom_val(handle) = record;
  char *copy = malloc(length + 1);
  if (copy == NULL) caml_raise_out_of_memory();
  memcpy(copy, String_val(name), length);
  copy[length] = 0;
  record->str = (uint64_t)(uintptr_t)copy;
  record->type = UINT32_C(0x40);
  CAMLreturn(handle);
}

CAMLprim value holyc_compiler_hash_create_record(value name, value type) {
  CAMLparam2(name, type);
  CAMLlocal1(handle);
  handle = holyc_compiler_hash_create_function(name);
  compiler_hash_original(handle)->type = (uint32_t)Int32_val(type);
  CAMLreturn(handle);
}

CAMLprim value holyc_compiler_hash_str(value text) {
  CAMLparam1(text);
  CAMLreturn(caml_copy_int64((int64_t)compiler_hash_str(
      (const unsigned char *)String_val(text))));
}

static void compiler_table_release(struct compiler_table_owner *owner) {
  /* Finalizers can run in another domain. Only the last native lease tears
     down storage; clear links before releasing record membership. Iterate to
     avoid growing the C stack for a long owned successor chain. */
  while (owner != NULL && atomic_fetch_sub(&owner->references, 1) == 1) {
    struct compiler_table_owner *next =
        (struct compiler_table_owner *)(uintptr_t)owner->table.next;
    struct compiler_table_member *member = owner->members;
    while (member != NULL) {
      struct compiler_table_member *following = member->next;
      member->record->prefix.next = 0;
      atomic_store(&member->record->table, NULL);
      compiler_record_release(member->record);
      free(member);
      member = following;
    }
    free((void *)(uintptr_t)owner->table.body);
    free(owner);
    owner = next;
  }
}

static void compiler_table_finalize(value handle) {
  struct compiler_table_owner *owner =
      *(struct compiler_table_owner **)Data_custom_val(handle);
  if (owner != NULL) {
    compiler_table_release(owner);
    *(struct compiler_table_owner **)Data_custom_val(handle) = NULL;
  }
}

static struct custom_operations compiler_table_operations = {
  "holyc.compiler.hash-table",
  compiler_table_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

static struct compiler_table_owner *compiler_table_original(value handle) {
  if (!Is_block(handle) || Tag_val(handle) != Custom_tag ||
      Custom_ops_val(handle) != &compiler_table_operations)
    caml_invalid_argument("foreign compiler hash table");
  struct compiler_table_owner *owner =
      *(struct compiler_table_owner **)Data_custom_val(handle);
  if (owner == NULL) caml_invalid_argument("expired compiler hash table");
  return owner;
}

CAMLprim value holyc_compiler_table_create(value size) {
  CAMLparam1(size);
  CAMLlocal1(handle);
  intnat count = Long_val(size);
  if (count <= 0 || ((uintnat)count & ((uintnat)count - 1)) != 0 ||
      (uintnat)count > (SIZE_MAX - sizeof(struct compiler_table_owner)) / 8)
    caml_invalid_argument("compiler hash table size must be a positive power of two");
  handle = caml_alloc_custom(&compiler_table_operations,
                            sizeof(struct compiler_table_owner *),
                            sizeof(struct compiler_table_owner) + (size_t)count * 8,
                            1024 * 1024);
  *(struct compiler_table_owner **)Data_custom_val(handle) = NULL;
  struct compiler_table_owner *owner = calloc(1, sizeof(*owner));
  if (owner == NULL) caml_raise_out_of_memory();
  atomic_init(&owner->references, 1);
  *(struct compiler_table_owner **)Data_custom_val(handle) = owner;
  uint64_t *body = calloc((size_t)count, sizeof(*body));
  if (body == NULL) caml_raise_out_of_memory();
  owner->table.mask = count - 1;
  owner->table.body = (uint64_t)(uintptr_t)body;
  CAMLreturn(handle);
}

CAMLprim value holyc_compiler_table_add(value table, value handle) {
  CAMLparam2(table, handle);
  struct compiler_table_owner *owner = compiler_table_original(table);
  struct compiler_record_owner *record =
      (struct compiler_record_owner *)compiler_hash_original(handle);
  if (atomic_load(&record->table) != NULL)
    caml_invalid_argument("compiler hash record already belongs to a table");
  if (atomic_load(&record->references) == SIZE_MAX) caml_raise_out_of_memory();
  struct compiler_table_member *member = malloc(sizeof(*member));
  if (member == NULL) caml_raise_out_of_memory();
  uint64_t bucket = compiler_hash_str(
      (const unsigned char *)(uintptr_t)record->prefix.str) & owner->table.mask;
  uint64_t *body = (uint64_t *)(uintptr_t)owner->table.body;
  member->record = record;
  member->next = owner->members;
  owner->members = member;
  atomic_fetch_add(&record->references, 1);
  atomic_store(&record->table, owner);
  record->prefix.next = body[bucket];
  body[bucket] = (uint64_t)(uintptr_t)&record->prefix;
  CAMLreturn(Val_unit);
}

CAMLprim value holyc_compiler_table_link(value table, value successor) {
  CAMLparam2(table, successor);
  struct compiler_table_owner *owner = compiler_table_original(table);
  struct compiler_table_owner *next = compiler_table_original(successor);
  for (struct compiler_table_owner *cursor = next; cursor != NULL;
       cursor = (struct compiler_table_owner *)(uintptr_t)cursor->table.next)
    if (cursor == owner) caml_invalid_argument("cyclic compiler hash table chain");
  if (atomic_load(&next->references) == SIZE_MAX) caml_raise_out_of_memory();
  struct compiler_table_owner *previous =
      (struct compiler_table_owner *)(uintptr_t)owner->table.next;
  atomic_fetch_add(&next->references, 1);
  owner->table.next = (uint64_t)(uintptr_t)next;
  if (previous != NULL) compiler_table_release(previous);
  CAMLreturn(Val_unit);
}

/* Preserve the remaining selected instance across tables. Unselected matches
   receive no count. The caller checks the actual selected pointer before INC. */
static struct compiler_hash_prefix *compiler_table_select(
    struct compiler_hash_table *table, const char *name, uint32_t mask,
    uint64_t instance, int chain) {
  if (instance == 0) return NULL;
  uint64_t hash = compiler_hash_str((const unsigned char *)name);
  while (table != NULL) {
    uint64_t *body = (uint64_t *)(uintptr_t)table->body;
    struct compiler_hash_prefix *record =
        (struct compiler_hash_prefix *)(uintptr_t)body[hash & table->mask];
    while (record != NULL) {
      if ((record->type & mask) != 0 &&
          strcmp((const char *)(uintptr_t)record->str, name) == 0 &&
          --instance == 0) return record;
      record = (struct compiler_hash_prefix *)(uintptr_t)record->next;
    }
    table = chain ? (struct compiler_hash_table *)(uintptr_t)table->next : NULL;
  }
  return NULL;
}

CAMLprim value holyc_compiler_table_find_checked(value table, value expected,
                                                value query) {
  CAMLparam3(table, expected, query);
  struct compiler_table_owner *owner = compiler_table_original(table);
  struct compiler_hash_prefix *original = compiler_hash_original(expected);
  struct compiler_hash_prefix *selected = compiler_table_select(
      &owner->table, String_val(Field(query, 0)),
      (uint32_t)Int32_val(Field(query, 1)),
      (uint64_t)Int64_val(Field(query, 2)), Bool_val(Field(query, 3)));
  if (selected != original) CAMLreturn(Val_false);
  if (Bool_val(Field(query, 4))) compiler_hash_increment(selected);
  CAMLreturn(Val_true);
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

#if (defined(__x86_64__) || defined(_M_X64)) && defined(__GNUC__)
/* Independent instruction oracle from KHashA.HC. It uses native offsets,
   SHL/ADC, SHR/ADC, bucket links, a low-32-bit type test and INC U32 rather
   than invoking any of the production arithmetic or selection helpers. */
static uint64_t compiler_hash_instruction_oracle(const unsigned char *str) {
  uint64_t result, state;
  __asm__ volatile(
      "xor %%rax, %%rax\n\t"
      "test %%rsi, %%rsi\n\t"
      "jz 3f\n\t"
      "xor %%rbx, %%rbx\n\t"
      "jmp 2f\n\t"
      "1: shl $1, %%rbx\n\t"
      "adc %%rax, %%rbx\n\t"
      "2: lodsb\n\t"
      "test %%al, %%al\n\t"
      "jnz 1b\n\t"
      "mov %%rbx, %%rax\n\t"
      "shr $16, %%rbx\n\t"
      "adc %%rbx, %%rax\n\t"
      "3:"
      : "=&a"(result), "=&b"(state), "+S"(str)
      : : "cc", "memory");
  return result;
}

static struct compiler_hash_prefix *compiler_single_instruction_oracle(
    struct compiler_hash_table *table, const char *name, uint64_t hash,
    uint32_t mask, uint64_t *remaining) {
  uint64_t instance = *remaining;
  uint64_t pointer;
  __asm__ volatile(
      "test %%rcx, %%rcx\n\t"
      "jz 5f\n\t"
      "and 8(%%rdi), %%rax\n\t"
      "mov 24(%%rdi), %%rdx\n\t"
      "lea (%%rdx,%%rax,8), %%rdx\n\t"
      "1: mov (%%rdx), %%rax\n\t"
      "test %%rax, %%rax\n\t"
      "jz 6f\n\t"
      "test %%ebx, 16(%%rax)\n\t"
      "jz 4f\n\t"
      "mov %%rsi, %%r8\n\t"
      "mov 8(%%rax), %%r9\n\t"
      "2: movzbl (%%r8), %%r10d\n\t"
      "cmp %%r10b, (%%r9)\n\t"
      "jne 4f\n\t"
      "inc %%r8\n\t"
      "inc %%r9\n\t"
      "test %%r10b, %%r10b\n\t"
      "jnz 2b\n\t"
      "loop 4f\n\t"
      "incl 20(%%rax)\n\t"
      "jmp 6f\n\t"
      "4: lea (%%rax), %%rdx\n\t"
      "jmp 1b\n\t"
      "5: xor %%rax, %%rax\n\t"
      "6:"
      : "+a"(hash), "=&d"(pointer), "+c"(instance)
      : "D"(table), "S"(name), "b"((uint64_t)mask)
      : "r8", "r9", "r10", "cc", "memory");
  *remaining = instance;
  return (struct compiler_hash_prefix *)(uintptr_t)hash;
}

static struct compiler_hash_prefix *compiler_find_instruction_oracle(
    struct compiler_hash_table *table, const char *name, uint32_t mask,
    uint64_t instance, int chain) {
  uint64_t hash = compiler_hash_instruction_oracle((const unsigned char *)name);
  while (table != NULL) {
    struct compiler_hash_prefix *result = compiler_single_instruction_oracle(
        table, name, hash, mask, &instance);
    if (result != NULL) return result;
    table = chain ? (struct compiler_hash_table *)(uintptr_t)table->next : NULL;
  }
  return NULL;
}
#endif

CAMLprim value holyc_compiler_table_verify_storage(value unit) {
  CAMLparam1(unit);
#if (defined(__x86_64__) || defined(_M_X64)) && defined(__GNUC__)
  unsigned char bytes[513];
  bytes[512] = 0;
  if (compiler_hash_str(NULL) != compiler_hash_instruction_oracle(NULL))
    CAMLreturn(Val_false);
  for (size_t length = 0; length <= 512; length++) {
    for (size_t i = 0; i < length; i++)
      bytes[i] = (unsigned char)(1 + ((i * 131 + length * 17) % 255));
    bytes[length] = 0;
    if (compiler_hash_str(bytes) != compiler_hash_instruction_oracle(bytes))
      CAMLreturn(Val_false);
  }
  const char *names[] = { "A", "C", "A", "A", "B", "A", "A" };
  uint32_t types[] = { 0x40, 0x40, 0x10, 0x80000040, 0x40, 0x40, 0x40 };
  uint32_t masks[] = { 0, 0x40, 0x10, 0x50, 0x80000000, UINT32_MAX };
  uint32_t counts[] = { 0, 1, UINT32_MAX - 1, UINT32_MAX };
  for (size_t width = 1; width <= 8; width *= 2) {
    for (size_t query = 0; query < 4; query++) {
      const char *name = query == 0 ? "A" : query == 1 ? "B" :
                         query == 2 ? "C" : "missing";
      for (size_t m = 0; m < sizeof(masks) / sizeof(masks[0]); m++) {
        for (uint64_t instance = 0; instance <= 8; instance++) {
          for (int chain = 0; chain <= 1; chain++) {
            for (size_t c = 0; c < sizeof(counts) / sizeof(counts[0]); c++) {
              struct compiler_hash_prefix actual[7], reference[7];
              uint64_t actual_buckets[2][8] = {{0}}, reference_buckets[2][8] = {{0}};
              struct compiler_hash_table actual_tables[2] = {{0}},
                                         reference_tables[2] = {{0}};
              for (size_t t = 0; t < 2; t++) {
                actual_tables[t].mask = reference_tables[t].mask = width - 1;
                actual_tables[t].body = (uint64_t)(uintptr_t)actual_buckets[t];
                reference_tables[t].body = (uint64_t)(uintptr_t)reference_buckets[t];
              }
              actual_tables[0].next = (uint64_t)(uintptr_t)&actual_tables[1];
              reference_tables[0].next = (uint64_t)(uintptr_t)&reference_tables[1];
              for (size_t i = 0; i < 7; i++) {
                size_t t = i < 4 ? 0 : 1;
                uint64_t bucket = compiler_hash_instruction_oracle(
                    (const unsigned char *)names[i]) & (width - 1);
                actual[i] = (struct compiler_hash_prefix){ actual_buckets[t][bucket],
                    (uint64_t)(uintptr_t)names[i], types[i], counts[c] };
                reference[i] = actual[i];
                reference[i].next = reference_buckets[t][bucket];
                actual_buckets[t][bucket] = (uint64_t)(uintptr_t)&actual[i];
                reference_buckets[t][bucket] = (uint64_t)(uintptr_t)&reference[i];
              }
              struct compiler_hash_prefix *selected = compiler_table_select(
                  actual_tables, name, masks[m], instance, chain);
              if (selected != NULL) compiler_hash_increment(selected);
              struct compiler_hash_prefix *expected = compiler_find_instruction_oracle(
                  reference_tables, name, masks[m], instance, chain);
              if ((selected == NULL) != (expected == NULL) ||
                  (selected != NULL && selected - actual != expected - reference))
                CAMLreturn(Val_false);
              for (size_t i = 0; i < 7; i++)
                if (actual[i].use_cnt != reference[i].use_cnt ||
                    actual[i].str != reference[i].str || actual[i].type != reference[i].type)
                  CAMLreturn(Val_false);
            }
          }
        }
      }
    }
  }
  CAMLreturn(Val_true);
#else
  CAMLreturn(Val_false);
#endif
}
