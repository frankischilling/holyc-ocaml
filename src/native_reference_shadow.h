/* Hosted native references keep their source-visible address bytes separate
   from ownership. Generated code supplies only checked roots and descriptors.
   This service performs no parsing, IR evaluation or executable dispatch. */
#ifndef HOLYC_NATIVE_REFERENCE_SHADOW_H
#define HOLYC_NATIVE_REFERENCE_SHADOW_H

#define NATIVE_REFERENCE_SHADOW_BYTES (64u * 1024u * 1024u)
#define NATIVE_REFERENCE_BUCKETS 4096u
#define NATIVE_REFERENCE_CODE_TYPE UINT64_C(0x8000000000000000)

struct native_reference_descriptor {
  uint64_t data, flags, offset, extent, epoch;
};

struct native_reference_entry;
struct native_reference_scope;

struct native_reference_frame {
  struct native_reference_frame *previous;
  uint64_t epoch;
  struct native_reference_entry *entries;
};

_Static_assert(sizeof(struct native_reference_descriptor) == 40,
               "native reference descriptors must retain all five qwords");
_Static_assert(sizeof(struct native_reference_frame) == 24,
               "native activation records must fit their generated frame homes");

struct native_reference_state {
  struct native_reference_entry **buckets;
  size_t count;
};

struct native_reference_scope {
  struct native_reference_state *state;
  struct native_reference_scope *previous;
  struct native_reference_frame *frames;
  int closing;
};

struct native_reference_entry {
  uintptr_t address;
  uint64_t owner_epoch, type;
  struct native_reference_descriptor target;
  struct native_reference_state *state;
  struct native_reference_scope *scope;
  struct native_reference_frame *frame;
  struct native_reference_entry *hash_next, *frame_next, **frame_link;
};

static atomic_uint_fast64_t native_reference_next_epoch = 1;
static _Thread_local struct native_reference_scope *native_reference_active;

static uint64_t native_reference_epoch(void)
{
  uint_fast64_t epoch = atomic_load(&native_reference_next_epoch);
  do {
    if (epoch == UINT64_MAX) return 0;
  } while (!atomic_compare_exchange_weak(&native_reference_next_epoch, &epoch, epoch + 1));
  return (uint64_t)epoch;
}

static size_t native_reference_hash(uintptr_t address)
{
  uint64_t hash = (uint64_t)address;
  hash ^= hash >> 33;
  hash *= UINT64_C(0xff51afd7ed558ccd);
  hash ^= hash >> 33;
  return (size_t)(hash & (NATIVE_REFERENCE_BUCKETS - 1));
}

static struct native_reference_entry **native_reference_find(
  struct native_reference_state *state, uintptr_t address)
{
  struct native_reference_entry **entry = &state->buckets[native_reference_hash(address)];
  while (*entry != NULL && (*entry)->address != address) entry = &(*entry)->hash_next;
  return entry;
}

static struct native_reference_entry *native_reference_detach(struct native_reference_entry **link)
{
  struct native_reference_entry *entry = *link;
  *link = entry->hash_next;
  if (entry->frame_link != NULL) {
    /* An abnormal native return may have released its machine stack. The
       closing scope removes those rows without reading that stack again. */
    if (!entry->scope->closing) {
      *entry->frame_link = entry->frame_next;
      if (entry->frame_next != NULL) entry->frame_next->frame_link = entry->frame_link;
    }
  }
  --entry->state->count;
  return entry;
}

static void native_reference_remove(struct native_reference_entry **link)
{
  struct native_reference_entry *entry = native_reference_detach(link);
  free(entry);
}

static void native_reference_state_clear(struct native_reference_state *state)
{
  if (state->buckets == NULL) return;
  for (size_t bucket = 0; bucket < NATIVE_REFERENCE_BUCKETS; ++bucket)
    while (state->buckets[bucket] != NULL) native_reference_remove(&state->buckets[bucket]);
  free(state->buckets);
  state->buckets = NULL;
}

static struct native_reference_frame *native_reference_live_frame(
  uint64_t epoch, struct native_reference_scope **owner)
{
  for (struct native_reference_scope *scope = native_reference_active;
       scope != NULL; scope = scope->previous)
    for (struct native_reference_frame *frame = scope->frames;
         frame != NULL; frame = frame->previous)
      if (frame->epoch == epoch) {
        if (owner != NULL) *owner = scope;
        return frame;
      }
  return NULL;
}

static int native_reference_live(const struct native_reference_descriptor *reference)
{
  return reference->epoch == 0 || native_reference_live_frame(reference->epoch, NULL) != NULL;
}

static uint64_t native_reference_window(
  const struct native_reference_descriptor *reference, size_t width,
  uintptr_t *address)
{
  if (!native_reference_live(reference)) return 34;
  if (reference->offset > reference->extent || width > reference->extent - reference->offset)
    return 10;
  if (reference->data > UINTPTR_MAX - reference->offset) return 10;
  *address = (uintptr_t)(reference->data + reference->offset);
  return 0;
}

static void native_reference_invalidate(
  struct native_reference_state *state, uintptr_t address, size_t width)
{
  if (state->count == 0 || width == 0) return;
  uintptr_t first = address > 7 ? address - 7 : 0;
  uintptr_t last = address + width - 1;
  for (uintptr_t candidate = first;; ++candidate) {
    struct native_reference_entry **link = native_reference_find(state, candidate);
    if (*link != NULL) native_reference_remove(link);
    if (candidate == last) break;
  }
}

static void native_reference_scope_finish(struct native_reference_scope *scope)
{
  scope->closing = 1;
  if (scope->state->count != 0) {
    for (size_t bucket = 0; bucket < NATIVE_REFERENCE_BUCKETS; ++bucket) {
      struct native_reference_entry **link = &scope->state->buckets[bucket];
      while (*link != NULL) {
        if ((*link)->scope == scope) native_reference_remove(link);
        else link = &(*link)->hash_next;
      }
    }
  }
  if (native_reference_active == scope) native_reference_active = scope->previous;
  scope->frames = NULL;
}

/* Request lanes: operation, first, second, third at context words 26..29.
   The first 24 context words retain their existing native/source ABI. */
static uint64_t native_reference_service(uint64_t *context)
{
  struct native_reference_scope *scope = (struct native_reference_scope *)(uintptr_t)context[25];
  struct native_reference_state *state = scope->state;
  uint64_t operation = context[26], fault = 0;
  uintptr_t first = (uintptr_t)context[27], second = (uintptr_t)context[28];
  uint64_t third = context[29];
  if (operation == 0) {
    struct native_reference_frame *frame = (struct native_reference_frame *)first;
    uint64_t epoch = native_reference_epoch();
    frame->epoch = 0;
    frame->entries = NULL;
    if (epoch == 0) fault = 35;
    else {
      if (scope->frames == NULL) {
        scope->previous = native_reference_active;
        native_reference_active = scope;
      }
      frame->previous = scope->frames;
      frame->epoch = epoch;
      scope->frames = frame;
    }
  } else if (operation == 1) {
    struct native_reference_frame *frame = (struct native_reference_frame *)first;
    if (scope->frames == frame) {
      while (frame->entries != NULL) {
        struct native_reference_entry *entry = frame->entries;
        native_reference_remove(native_reference_find(entry->state, entry->address));
      }
      scope->frames = frame->previous;
      if (scope->frames == NULL) native_reference_active = scope->previous;
    }
  } else if (operation == 2) {
    if (!native_reference_live((struct native_reference_descriptor *)first)) fault = 34;
  } else if (operation == 3) {
    native_reference_invalidate(state, first, (size_t)second);
  } else if (operation >= 4 && operation <= 7) {
    const struct native_reference_descriptor *field = (struct native_reference_descriptor *)first;
    uintptr_t address;
    fault = native_reference_window(field, 8, &address);
    if (fault == 0 && (operation == 4 || operation == 6)) {
      /* A load checks known bytes before consulting ownership. */
      if (field->flags != 0) {
        const uint8_t *flags = (const uint8_t *)(uintptr_t)(field->flags - field->offset);
        for (size_t byte = 0; byte < 8; ++byte)
          if (flags[-(intptr_t)byte] == 0) { fault = 7; break; }
      }
      if (fault == 0) {
        struct native_reference_entry *entry = state->count == 0 ? NULL : *native_reference_find(state, address);
        uint64_t bits;
        memcpy(&bits, (void *)address, 8);
        if (operation == 6) {
          uint64_t pair[2] = {bits, 0};
          if (entry != NULL && entry->owner_epoch == field->epoch &&
              entry->type == NATIVE_REFERENCE_CODE_TYPE && bits == entry->target.data)
            pair[1] = entry->target.flags;
          memcpy((void *)second, pair, sizeof(pair));
        } else if (entry == NULL || entry->owner_epoch != field->epoch || entry->type != third ||
            bits != entry->target.data + entry->target.offset) fault = 34;
        else memcpy((void *)second, &entry->target, sizeof(entry->target));
      }
    } else if (fault == 0) {
      struct native_reference_descriptor target;
      if (operation == 7) {
        uint64_t pair[2];
        memcpy(pair, (void *)second, sizeof(pair));
        target = (struct native_reference_descriptor){ .data = pair[0], .flags = pair[1] };
        third = NATIVE_REFERENCE_CODE_TYPE;
        if (pair[1] == 0) {
          native_reference_invalidate(state, address, 8);
          memcpy((void *)address, &pair[0], 8);
          if (field->flags != 0) {
            uint8_t *flags = (uint8_t *)(uintptr_t)(field->flags - field->offset);
            for (size_t byte = 0; byte < 8; ++byte) flags[-(intptr_t)byte] = 1;
          }
          return 0;
        }
      } else {
        uintptr_t target_address;
        memcpy(&target, (void *)second, sizeof(target));
        fault = native_reference_window(&target, 0, &target_address);
        if (fault != 0) { context[0] = fault; return fault; }
      }
      struct native_reference_entry *entry = NULL;
      if (state->count != 0) {
        uintptr_t start = address > 7 ? address - 7 : 0;
        for (uintptr_t candidate = start;; ++candidate) {
          struct native_reference_entry **link = native_reference_find(state, candidate);
          if (*link != NULL) { entry = native_reference_detach(link); break; }
          if (candidate == address + 7) break;
        }
      }
      if (entry == NULL && state->count >=
          (NATIVE_REFERENCE_SHADOW_BYTES - NATIVE_REFERENCE_BUCKETS * sizeof(*state->buckets)) / sizeof(*entry)) fault = 35;
      else if (state->buckets == NULL &&
               (state->buckets = calloc(NATIVE_REFERENCE_BUCKETS, sizeof(*state->buckets))) == NULL) fault = 35;
      else if (entry == NULL && (entry = malloc(sizeof(*entry))) == NULL) fault = 35;
      if (fault == 0) {
        struct native_reference_scope *owner = NULL;
        struct native_reference_frame *frame = field->epoch == 0 ? NULL : native_reference_live_frame(field->epoch, &owner);
        native_reference_invalidate(state, address, 8);
        *entry = (struct native_reference_entry){
          .address = address, .owner_epoch = field->epoch, .type = third, .target = target,
          .state = state, .scope = owner, .frame = frame,
          .hash_next = state->buckets[native_reference_hash(address)]
        };
        state->buckets[native_reference_hash(address)] = entry;
        if (frame != NULL) {
          entry->frame_next = frame->entries;
          entry->frame_link = &frame->entries;
          if (entry->frame_next != NULL) entry->frame_next->frame_link = &entry->frame_next;
          frame->entries = entry;
        }
        ++state->count;
        uint64_t bits = target.data + target.offset;
        memcpy((void *)address, &bits, 8);
        if (field->flags != 0) {
          uint8_t *flags = (uint8_t *)(uintptr_t)(field->flags - field->offset);
          for (size_t byte = 0; byte < 8; ++byte) flags[-(intptr_t)byte] = 1;
        }
      }
    }
  } else fault = 34;
  if (fault != 0) context[0] = fault;
  return fault;
}

static void native_reference_context_init(uint64_t *context,
  struct native_reference_scope *scope, struct native_reference_state *state)
{
  *scope = (struct native_reference_scope){ .state = state };
  context[24] = (uint64_t)(uintptr_t)&native_reference_service;
  context[25] = (uint64_t)(uintptr_t)scope;
}

#endif
