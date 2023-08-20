#ifdef DA_IMPLEMENTATION
#include <assert.h>
#include <math.h>
#include <stdlib.h>

#ifndef DA_INITIAL_CAP
#define DA_INITIAL_CAP 50
#endif

#ifndef DA_GROWTH_FACTOR
#define DA_GROWTH_FACTOR 2
#endif

#ifndef DA_SHRINK_THRESHOLD_FACTOR
#define DA_SHRINK_THRESHOLD_FACTOR 0.25
#endif

#ifndef DA_SHRINK_FACTOR
#define DA_SHRINK_FACTOR 0.5
#endif

#define DA_ASSERT_GLOBAL_PARAMS {                                                                                                                      \
  assert(DA_INITIAL_CAP > 0 && "Initial capacity of a dynamic array must be > 0, please set appropriate values for DA_INITAL_CAP");                    \
  assert(DA_GROWTH_FACTOR > 1 && "Growth factor of a dynamic array must be > 1, please set appropriate values for DA_GROWTH_FACTOR");                  \
  assert(                                                                                                                                              \
    (DA_SHRINK_THRESHOLD_FACTOR > 0)                                                                                                                   \
    && (DA_SHRINK_THRESHOLD_FACTOR < DA_SHRINK_FACTOR)                                                                                                 \
    && "Shrink factor must be between 0 - DA_SHRINK_THRESHOLD_FACTOR of a dynamic array, please set appropriate values for DA_SHRINK_THRESHOLD_FACTOR" \
  );                                                                                                                                                   \
  assert(                                                                                                                                              \
    (DA_SHRINK_FACTOR > 0)                                                                                                                             \
    && (DA_SHRINK_FACTOR < 1)                                                                                                                          \
    && "Shrink factor must be between [0, 1) of a dynamic array, please set appropriate values for DA_SHRINK_FACTOR"                                   \
  );                                                                                                                                                   \
}                                                                                                                                                      \

#ifndef DA_MALLOC
#define DA_MALLOC(sz) malloc(sz);
#endif

#ifndef DA_FREE
#define DA_FREE(ptr) free(ptr);
#endif

#define da_typedef(t, name) typedef struct name { \
  unsigned int length;                            \
  unsigned int capacity;                          \
  t      *data;                                   \
} name;                                           \

#define da_allocate_data_arr(da) DA_MALLOC(sizeof(*(da).data) * (da).capacity)

#define da_init_2(da, cap) {                \
  DA_ASSERT_GLOBAL_PARAMS                   \
  (da).length   = 0;                        \
  (da).capacity = cap;                      \
  (da).data     = da_allocate_data_arr(da); \
}                                           \

#define da_init(da) da_init_2(da, DA_INITIAL_CAP)

#define da_grow(da) {                                        \
  (da).capacity = ceil((da).capacity * DA_GROWTH_FACTOR);    \
  typeof((da).data) new_data_arr = da_allocate_data_arr(da); \
  for (unsigned int i = 0; i < (da).length; i++) {           \
    new_data_arr[i] = (da).data[i];                          \
  }                                                          \
  DA_FREE((da).data);                                        \
  (da).data = new_data_arr;                                  \
}                                                            \

#define da_push(da, item) {                        \
  if ((da).length == (da).capacity) {              \
    da_grow(da);                                   \
  }                                                \
  (da).data[(da).length++] = item;                 \
}                                                  \

#define da_shrink(da) {                                      \
  (da).capacity = ceil((da).capacity * DA_SHRINK_FACTOR);    \
  typeof((da).data) new_data_arr = da_allocate_data_arr(da); \
  for (unsigned int i = 0; i < (da).length; i++) {           \
    new_data_arr[i] = (da).data[i];                          \
  }                                                          \
  DA_FREE((da).data);                                        \
  (da).data = new_data_arr;                                  \
}                                                            \

#define da_pop(da, item) {                                          \
  if (((da).data == NULL) || ((da).length == 0)) {                  \
    item = NULL;                                                    \
  } else {                                                          \
    item = (da).data[(da).length-- - 1];                            \
    if ((da).length == 0) {                                         \
      (da).data = NULL;                                             \
    }                                                               \
    if ((da).length < DA_SHRINK_THRESHOLD_FACTOR * (da).capacity) { \
      da_shrink(da);                                                \
    }                                                               \
  }                                                                 \
}                                                                   \

#define da_free(da, deref) {                         \
  if ((da).data != NULL) {                           \
    for (unsigned int i = 0; i < (da).length; i++) { \
      DA_FREE(deref((da).data[i]));                  \
    }                                                \
    DA_FREE((da).data);                              \
    (da).data = NULL;                                \
    (da).length = 0;                                 \
    (da).capacity = 0;                               \
  }                                                  \
}                                                    \

#endif
