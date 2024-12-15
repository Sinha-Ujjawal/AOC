#ifndef ARENA_H_
#define ARENA_H_

#define ARENA_TYPE_SIMPLE      0
#define ARENA_TYPE_SCAN_LINEAR 1
#define ARENA_TYPE_SCAN_HEAP   2
#define ARENA_TYPE_DEFAULT     ARENA_TYPE_SCAN_HEAP

#ifndef ARENA_DEFAULT_REGION_CAPACITY
#define ARENA_DEFAULT_REGION_CAPACITY 8*1024
#endif // ARENA_DEFAULT_REGION_CAPACITY

typedef struct arena arena;

void *arena_alloc(arena *arena, size_t bytes);
void *arena_realloc(arena *arena, void *old_ptr, size_t old_bytes, size_t new_bytes);
void arena_reset(arena *arena);
void arena_free(arena *arena);
void arena_debug(arena *arena);

#ifdef ARENA_IMPLEMENTATION

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#ifndef ARENA_MALLOC
#define ARENA_MALLOC malloc
#endif // ARENA_MALLOC

#ifndef ARENA_REALLOC
#define ARENA_REALLOC realloc
#endif // ARENA_REALLOC

#ifndef ARENA_FREE
#define ARENA_FREE free
#endif // ARENA_FREE

typedef void *arena_bytes;

typedef struct arena_region arena_region;
struct arena_region {
  arena_region *next;
  size_t       count;
  size_t       capacity;
  arena_bytes  bytes;
};

arena_region *arena_new_region(size_t capacity) {
  arena_region *r = ARENA_MALLOC(sizeof(arena_region));
  r->count    = 0;
  r->capacity = capacity;
  r->bytes    = ARENA_MALLOC(capacity);
  return r;
}

#define ARENA_REALLOC_IMPL(arena, old_ptr, old_bytes, new_bytes) { \
  if (new_bytes <= old_bytes) return old_ptr; \
  void *new_ptr = arena_alloc(arena, new_bytes); \
  char *old_ptr_char = old_ptr; \
  char *new_ptr_char = new_ptr; \
  for (size_t i = 0; i < old_bytes; i++) { \
    new_ptr_char[i] = old_ptr_char[i]; \
  } \
  return new_ptr; \
} \

#define ARENA_RESET_IMPL(arena) { \
  arena_region *r = arena->begin; \
  while (r != NULL) { \
    r->count = 0; \
    r = r->next; \
  } \
  arena->end = arena->begin; \
} \

#define ARENA_FREE_IMPL(arena) { \
  arena_region *r = arena->begin; \
  while (r != NULL) { \
    arena_region *temp = r->next; \
    ARENA_FREE(r); \
    r = temp; \
  } \
  arena->begin = NULL; \
  arena->end = NULL; \
} \

#define ARENA_DEBUG_IMPL(arena, arena_type) { \
  arena_region *r = arena->begin; \
  size_t region_idx = 0; \
  printf(""arena_type"\n"); \
  while (r != NULL) { \
    printf("region: %zu, Count: %zu, Capacity: %zu, Left: %zu\n", region_idx++, r->count, r->capacity, r->capacity - r->count); \
    printf("bytes(hex): "); \
    char *bytes = r->bytes; \
    for (size_t i = 0; i < r->capacity; i++) { \
      printf("%x", bytes[i]); \
    }\
    printf("\n"); \
    r = r->next; \
  } \
} \

#ifndef ARENA_TYPE
#define ARENA_TYPE ARENA_TYPE_DEFAULT
#endif

#if ARENA_TYPE == ARENA_TYPE_SIMPLE
struct arena {
  arena_region *begin;
  arena_region *end;
};

void *arena_alloc(arena *arena, size_t bytes) {
  // when arena is empty
  if (arena->end == NULL) {
    assert(arena->begin == NULL);
    size_t capacity = ARENA_DEFAULT_REGION_CAPACITY;
    if (capacity < bytes) {
      capacity = bytes;
    }
    arena->end = arena_new_region(capacity);
    arena->end->count = bytes;
    arena->begin = arena->end;
    return arena->end->bytes;
  }
  // update arena->end so that it points to a region with sufficient capacity, or the last region
  while (arena->end->count + bytes > arena->end->capacity && arena->end->next != NULL) {
    arena->end = arena->end->next;
  }
  // check if arena->end has enough capacity, if not then allocate new region
  if (arena->end->count + bytes > arena->end->capacity) {
    assert(arena->end->next == NULL);
    size_t capacity = ARENA_DEFAULT_REGION_CAPACITY;
    if (capacity < bytes) {
      capacity = bytes;
    }
    arena->end->next = arena_new_region(capacity);
    arena->end = arena->end->next;
  }
  void *ret = &arena->end->bytes[arena->end->count];
  arena->end->count += bytes;
  return ret;
}

void *arena_realloc(arena *arena, void *old_ptr, size_t old_bytes, size_t new_bytes)
  ARENA_REALLOC_IMPL(arena, old_ptr, old_bytes, new_bytes)
void arena_reset(arena *arena) ARENA_RESET_IMPL(arena)
void arena_free(arena *arena) ARENA_FREE_IMPL(arena)
void arena_debug(arena *arena) ARENA_DEBUG_IMPL(arena, "ARENA_TYPE_SIMPLE")
#endif // ARENA_TYPE_SIMPLE

#if ARENA_TYPE == ARENA_TYPE_SCAN_LINEAR || ARENA_TYPE == ARENA_TYPE_SCAN_HEAP
#define ARENA_TYPE_SCAN_DA_CAPACITY 64

typedef struct arena_da_regions arena_da_regions;
struct arena_da_regions {
  arena_region **data;
  size_t       count;
  size_t       capacity;
};

#define arena_da_append(da, item) { \
    if ((da)->data == NULL) { \
      (da)->count    = 0; \
      (da)->capacity = ARENA_TYPE_SCAN_DA_CAPACITY; \
      (da)->data     = ARENA_REALLOC((da)->data, (da)->capacity); \
    } \
    if ((da)->count >= (da)->capacity) { \
        (da)->capacity = (da)->capacity << 1; \
        (da)->data     = ARENA_REALLOC((da)->data, (da)->capacity); \
    } \
    (da)->data[(da)->count] = (item); \
    (da)->count += 1; \
} \

struct arena {
  arena_region     *begin;
  arena_region     *end;
  arena_da_regions regions;
};
#endif // ARENA_TYPE_SCAN_* common

#if ARENA_TYPE == ARENA_TYPE_SCAN_LINEAR
void *arena_alloc(arena *arena, size_t bytes) {
  // when arena is empty
  if (arena->end == NULL) {
    assert(arena->begin == NULL);
    size_t capacity = ARENA_DEFAULT_REGION_CAPACITY;
    if (capacity < bytes) {
      capacity = bytes;
    }
    arena->end = arena_new_region(capacity);
    arena_da_append(&arena->regions, arena->end);
    arena->end->count = bytes;
    arena->begin = arena->end;
    return arena->end->bytes;
  }
  // find a region with appropriate size using linear scan
  arena_region *r = arena->begin;
  while (r->count + bytes > r->capacity && r->next != NULL) {
    r = r->next;
  }
  if (r->count + bytes <= r->capacity) {
    void *ret = &r->bytes[r->count];
    r->count += bytes;
    return ret;
  }
  // update arena->end so that it points to a region with sufficient capacity, or the last region
  while (arena->end->count + bytes > arena->end->capacity && arena->end->next != NULL) {
    arena->end = arena->end->next;
  }
  // check if arena->end has enough capacity, if not then allocate new region
  if (arena->end->count + bytes > arena->end->capacity) {
    assert(arena->end->next == NULL);
    size_t capacity = ARENA_DEFAULT_REGION_CAPACITY;
    if (capacity < bytes) {
      capacity = bytes;
    }
    arena->end->next = arena_new_region(capacity);
    arena_da_append(&arena->regions, arena->end->next);
    arena->end = arena->end->next;
  }
  void *ret = &arena->end->bytes[arena->end->count];
  arena->end->count += bytes;
  return ret;
}

void *arena_realloc(arena *arena, void *old_ptr, size_t old_bytes, size_t new_bytes)
  ARENA_REALLOC_IMPL(arena, old_ptr, old_bytes, new_bytes)
void arena_reset(arena *arena) ARENA_RESET_IMPL(arena)
void arena_free(arena *arena) ARENA_FREE_IMPL(arena)
void arena_debug(arena *arena) ARENA_DEBUG_IMPL(arena, "ARENA_TYPE_SCAN_LINEAR")
#endif // ARENA_TYPE_SCAN_LINEAR

#if ARENA_TYPE == ARENA_TYPE_SCAN_HEAP
#define arena_region_remaining_cap(region) (region)->capacity - (region)->count

void arena_max_heapify(arena_da_regions *regions) {
 // Reference: https://en.wikipedia.org/wiki/Binary_heap
 size_t i = 0;
 while (1) {
   size_t left = (i << 1) + 1;
   size_t right = left + 1;
   size_t largest = i;
   if (left < regions->count && arena_region_remaining_cap(regions->data[largest]) < arena_region_remaining_cap(regions->data[left])) {
     largest = left;
   }
   if (right < regions->count && arena_region_remaining_cap(regions->data[largest]) < arena_region_remaining_cap(regions->data[right])) {
     largest = right;
   }
   if (largest == i) {
     break;
   }
   // swap A[largest] and A[i]
   arena_region *temp     = regions->data[i];
   regions->data[i]       = regions->data[largest];
   regions->data[largest] = temp;
   i = largest;
 }
}

void arena_da_heappush(arena_da_regions *regions, arena_region *region) {
  arena_da_append(regions, region);
  arena_max_heapify(regions);
}

void *arena_alloc(arena *arena, size_t bytes) {
  // when arena is empty
  if (arena->end == NULL) {
    assert(arena->begin == NULL);
    size_t capacity = ARENA_DEFAULT_REGION_CAPACITY;
    if (capacity < bytes) {
      capacity = bytes;
    }
    arena->end = arena_new_region(capacity);
    arena_da_heappush(&arena->regions, arena->end);
    arena->end->count = bytes;
    arena->begin = arena->end;
    return arena->end->bytes;
  }
  // check if region with max capacity has enough remaining capacity
  arena_region *r = arena->regions.data[0];
  if (r->count + bytes <= r->capacity) {
    void *ret = &r->bytes[r->count];
    r->count += bytes;
    arena_max_heapify(&arena->regions);
    return ret;
  }
  // update arena->end so that it points to a region with sufficient capacity, or the last region
  while (arena->end->count + bytes > arena->end->capacity && arena->end->next != NULL) {
    arena->end = arena->end->next;
  }
  // check if arena->end has enough capacity, if not then allocate new region
  if (arena->end->count + bytes > arena->end->capacity) {
    assert(arena->end->next == NULL);
    size_t capacity = ARENA_DEFAULT_REGION_CAPACITY;
    if (capacity < bytes) {
      capacity = bytes;
    }
    arena->end->next = arena_new_region(capacity);
    arena_da_heappush(&arena->regions, arena->end->next);
    arena->end = arena->end->next;
  }
  void *ret = &arena->end->bytes[arena->end->count];
  arena->end->count += bytes;
  return ret;
}

void *arena_realloc(arena *arena, void *old_ptr, size_t old_bytes, size_t new_bytes)
  ARENA_REALLOC_IMPL(arena, old_ptr, old_bytes, new_bytes)
void arena_reset(arena *arena) ARENA_RESET_IMPL(arena)
void arena_free(arena *arena) ARENA_FREE_IMPL(arena)
void arena_debug(arena *arena) ARENA_DEBUG_IMPL(arena, "ARENA_TYPE_SCAN_HEAP")
#endif // ARENA_TYPE_SCAN_HEAP

#endif // ARENA_IMPLEMENTATION

#endif // ARENA_H_
