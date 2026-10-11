#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#if (defined(__x86_64__) || defined(_M_X64)) && \
    !defined(_M_ARM64EC) && UINTPTR_MAX == UINT64_MAX && \
    (defined(_WIN32) || defined(__linux__))
#define HOLYC_LOCK_TEST_NATIVE 1
#include <stdatomic.h>
#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#else
#include <pthread.h>
#include <sched.h>
#include <sys/mman.h>
#endif
#else
#define HOLYC_LOCK_TEST_NATIVE 0
#endif

#if HOLYC_LOCK_TEST_NATIVE
#define HOLYC_LOCK_TEST_THREADS 8

typedef uint64_t (*lock_test_entry)(volatile uint64_t *);

struct lock_test_shared {
  _Alignas(64) volatile uint64_t word;
  atomic_int ready;
  atomic_int start;
};

struct lock_test_worker {
  struct lock_test_shared *shared;
  lock_test_entry entry;
  uint64_t iterations;
  uint64_t returned_sum;
};

static void lock_test_yield(void)
{
#ifdef _WIN32
  SwitchToThread();
#else
  sched_yield();
#endif
}

static void lock_test_work(struct lock_test_worker *worker)
{
  int start;
  atomic_fetch_add_explicit(&worker->shared->ready, 1, memory_order_release);
  do {
    start = atomic_load_explicit(&worker->shared->start, memory_order_acquire);
    if (start == 0)
      lock_test_yield();
  } while (start == 0);
  if (start < 0)
    return;
  for (uint64_t i = 0; i < worker->iterations; ++i)
    worker->returned_sum += worker->entry(&worker->shared->word);
}

#ifdef _WIN32
static DWORD WINAPI lock_test_thread(LPVOID argument)
{
  lock_test_work((struct lock_test_worker *)argument);
  return 0;
}
#else
static void *lock_test_thread(void *argument)
{
  lock_test_work((struct lock_test_worker *)argument);
  return NULL;
}
#endif

static void *lock_test_map(const char *code, size_t length)
{
  void *mapping;
#ifdef _WIN32
  DWORD previous;
  mapping = VirtualAlloc(NULL, length, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
  if (mapping == NULL)
    return NULL;
  memcpy(mapping, code, length);
  if (!VirtualProtect(mapping, length, PAGE_EXECUTE_READ, &previous) ||
      !FlushInstructionCache(GetCurrentProcess(), mapping, length)) {
    VirtualFree(mapping, 0, MEM_RELEASE);
    return NULL;
  }
#else
  mapping = mmap(NULL, length, PROT_READ | PROT_WRITE,
                 MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (mapping == MAP_FAILED)
    return NULL;
  memcpy(mapping, code, length);
  if (mprotect(mapping, length, PROT_READ | PROT_EXEC) != 0) {
    munmap(mapping, length);
    return NULL;
  }
  __builtin___clear_cache((char *)mapping, (char *)mapping + length);
#endif
  return mapping;
}

static void lock_test_unmap(void *mapping, size_t length)
{
  if (mapping == NULL)
    return;
#ifdef _WIN32
  (void)length;
  VirtualFree(mapping, 0, MEM_RELEASE);
#else
  munmap(mapping, length);
#endif
}
#endif

CAMLprim value holyc_test_native_lock_contend(value codes, value rounds,
                                              value initial)
{
  CAMLparam3(codes, rounds, initial);
  CAMLlocal3(result, final_word, returned_sum);
#if HOLYC_LOCK_TEST_NATIVE
  mlsize_t count = Wosize_val(codes);
  intnat iterations = Long_val(rounds);
  struct lock_test_shared shared;
  struct lock_test_worker workers[HOLYC_LOCK_TEST_THREADS];
  void *mappings[HOLYC_LOCK_TEST_THREADS] = { 0 };
  size_t lengths[HOLYC_LOCK_TEST_THREADS] = { 0 };
#ifdef _WIN32
  HANDLE threads[HOLYC_LOCK_TEST_THREADS];
#else
  pthread_t threads[HOLYC_LOCK_TEST_THREADS];
#endif
  mlsize_t started = 0;
  uint64_t sum = 0;
  const char *failure = NULL;

  if (count < 2 || count > HOLYC_LOCK_TEST_THREADS ||
      iterations < 1 || iterations > 1000000)
    caml_invalid_argument("lock contention requires 2..8 threads and 1..1000000 rounds");
  for (mlsize_t i = 0; i < count; ++i) {
    lengths[i] = caml_string_length(Field(codes, i));
    if (lengths[i] < 4 || lengths[i] > 256 ||
        (unsigned char)String_val(Field(codes, i))[lengths[i] - 1] != 0xc3)
      caml_invalid_argument("lock test requires a bounded frameless return image");
  }

  memset(workers, 0, sizeof(workers));
  shared.word = (uint64_t)Int64_val(initial);
  atomic_init(&shared.ready, 0);
  atomic_init(&shared.start, 0);
  for (mlsize_t i = 0; i < count; ++i) {
    mappings[i] = lock_test_map(String_val(Field(codes, i)), lengths[i]);
    if (mappings[i] == NULL) {
      failure = "lock contention could not map the native test image";
      goto cleanup;
    }
    workers[i].shared = &shared;
    workers[i].iterations = (uint64_t)iterations;
    workers[i].entry = (lock_test_entry)(uintptr_t)mappings[i];
  }

  for (mlsize_t i = 0; i < count; ++i) {
#ifdef _WIN32
    threads[i] = CreateThread(NULL, 0, lock_test_thread, &workers[i], 0, NULL);
    if (threads[i] == NULL) {
#else
    if (pthread_create(&threads[i], NULL, lock_test_thread, &workers[i]) != 0) {
#endif
      failure = "lock contention could not create its worker thread";
      break;
    }
    ++started;
  }
  if (failure != NULL)
    atomic_store_explicit(&shared.start, -1, memory_order_release);
  else {
    while (atomic_load_explicit(&shared.ready, memory_order_acquire) != (int)count)
      lock_test_yield();
    atomic_store_explicit(&shared.start, 1, memory_order_release);
  }
  for (mlsize_t i = 0; i < started; ++i) {
#ifdef _WIN32
    if (WaitForSingleObject(threads[i], INFINITE) != WAIT_OBJECT_0)
      failure = "lock contention could not join its worker thread";
    CloseHandle(threads[i]);
#else
    if (pthread_join(threads[i], NULL) != 0)
      failure = "lock contention could not join its worker thread";
#endif
    sum += workers[i].returned_sum;
  }

cleanup:
  for (mlsize_t i = 0; i < count; ++i)
    lock_test_unmap(mappings[i], lengths[i]);
  if (failure != NULL)
    caml_failwith(failure);
  final_word = caml_copy_int64((int64_t)shared.word);
  returned_sum = caml_copy_int64((int64_t)sum);
  result = caml_alloc_tuple(2);
  Store_field(result, 0, final_word);
  Store_field(result, 1, returned_sum);
  CAMLreturn(result);
#else
  caml_failwith("native lock contention requires Windows or Linux x86-64");
#endif
}
