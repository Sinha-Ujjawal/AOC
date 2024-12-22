// Copyright 2024 <Sinha-Ujjawal>
//
// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use,
// copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following
// conditions:
//
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
// OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
// HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
// WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
// OTHER DEALINGS IN THE SOFTWARE.

#ifndef DA_H
#define DA_H

#ifdef DA_IMPLEMENTATION
#include <stdlib.h>
#include <assert.h>

#ifndef DA_INIT_CAP
#define DA_INIT_CAP          64
#endif //DA_INIT_CAP

#ifndef DA_GROWTH_FACTOR
#define DA_GROWTH_FACTOR     2
#endif //DA_GROWTH_FACTOR

#ifndef DA_SHRINK_FACTOR
#define DA_SHRINK_FACTOR     0.5
#endif //DA_SHRINK_FACTOR

#ifndef DA_SHRINK_THRESHOLD
#define DA_SHRINK_THRESHOLD  0.33
#endif //DA_SHRINK_THRESHOLD

/* `da` is a structure matching below signature-
    `t` is a type parameter
    typedef struct {
        size_t count;
        size_t capacity;
        t*     data;
        ...
    };
*/

#ifndef DA_REALLOC
#define DA_REALLOC(old_ptr, old_bytes, new_bytes) realloc((old_ptr), (new_bytes))
#endif //DA_REALLOC

#define da_type(name, t) typedef struct {\
    size_t count;\
    size_t capacity;\
    t*     data;\
} name;\
void da_init_##name(name *da) {\
    da_init(da);\
}\
void da_push_##name(name *da, t item) {\
    da_push(da, item);\
}\
void da_ensure_capacity_##name(name *da, size_t min_capacity) {\
    da_ensure_capacity(da, min_capacity);\
}\
void da_grow_##name(name *da) {\
    da_grow(da);\
}\
void da_pop_##name(name *da, t *result) {\
    da_pop(da, result);\
}\
void da_shrink_##name(name *da) {\
    da_shrink(da);\
}\
void da_get_at_##name(name da, size_t idx, t *result) {\
    da_get_at(da, idx, result);\
}\
void da_put_at_##name(name *da, size_t idx, t item) {\
    da_put_at(da, idx, item);\
}\
void da_insert_at_##name(name *da, size_t idx, t item) {\
    da_insert_at(da, idx, item);\
}\
void da_delete_at_##name(name *da, size_t idx) {\
    da_delete_at(da, idx);\
}\
void da_pop_at_##name(name *da, size_t idx, t *result) {\
    da_pop_at(da, idx, result);\
}\

#define _da_resize(da, new_capacity) {\
    (da)->data     = DA_REALLOC((da)->data, sizeof(*(da)->data) * (da)->capacity, sizeof(*(da)->data) * new_capacity);\
    (da)->capacity = (new_capacity);\
}\

#define da_init(da) {\
    _da_resize(da, DA_INIT_CAP);\
    (da)->count = 0;\
}\

#define _da_grow(da) {\
    size_t new_capacity = (da)->capacity * DA_GROWTH_FACTOR;\
    _da_resize(da, new_capacity);\
}\

#define _da_shrink(da) {\
    size_t new_capacity = (da)->capacity * DA_SHRINK_FACTOR;\
    _da_resize(da, new_capacity);\
}\

#define da_ensure_capacity(da, min_capacity) {\
    if ((da)->capacity < (min_capacity)) {\
        _da_resize(da, min_capacity);\
    }\
}\

#define da_grow(da) {\
    if ((da)->count >= (da)->capacity) {\
        _da_grow(da);\
    }\
    (da)->count += 1;\
}\

#define da_push(da, item) {\
    if ((da)->count >= (da)->capacity) {\
        _da_grow(da);\
    }\
    (da)->data[(da)->count] = (item);\
    (da)->count += 1;\
}\

#define da_shrink(da) {\
    if ((da)->count > 0) {\
        (da)->count -= 1;\
        if (((da)->count > DA_INIT_CAP) && ((da)->count <= (da)->capacity * DA_SHRINK_THRESHOLD)) {\
            _da_shrink(da);\
        }\
    }\
}\

#define da_pop(da, result) {\
    if ((da)->count > 0) {\
        (da)->count -= 1;\
        *(result) = (da)->data[(da)->count];\
        if (((da)->count > DA_INIT_CAP) && ((da)->count <= (da)->capacity * DA_SHRINK_THRESHOLD)) {\
            _da_shrink(da);\
        }\
    }\
}\

#define da_index(da, idx) (da)->data[(idx)]

#define da_first(da) da_index(da, 0)
#define da_last(da)  da_index(da, (da)->count - 1)

#define da_get_at(da, idx, result) {\
    assert((((idx) >= 0) && ((idx) < (da).count)) && "Index out of bounds!");\
    *(result) = (da).data[(idx)];\
}\

#define da_put_at(da, idx, item) {\
    assert((((idx) >= 0) && ((idx) < (da)->count)) && "Index out of bounds!");\
    (da)->data[(idx)] = (item);\
}\

#define _da_move(da, start, end, off) {\
    if ((off) > 0) {\
        for (size_t j = end; (j <= end) && (j >= start); j--) {\
            (da)->data[(j + off) % (da)->capacity] = (da)->data[j % (da)->capacity];\
        }\
    }\
    else if ((off) < 0) {\
        for (int j = start; j <= end; j++) {\
            int idx = (j + off) % (int)(da)->capacity;\
            if (idx < 0) {\
               idx += (int)(da)->capacity;\
            }\
            (da)->data[idx] = (da)->data[j % (da)->capacity];\
        }\
    }\
}\

#define _da_make_room(da, idx) {\
    if ((da)->count >= (da)->capacity) {\
        _da_grow(da);\
    }\
    _da_move(da, idx, (da)->count - 1, 1);\
}\

#define da_insert_at(da, idx, item) {\
    assert((((idx) >= 0) && ((idx) <= (da)->count)) && "Index out of bounds!");\
    if ((idx) == (da)->count) {\
        da_push(da, item);\
    }\
    else {\
        _da_make_room(da, idx);\
        (da)->data[(idx)] = (item);\
        (da)->count += 1;\
    }\
}\

#define da_delete_at(da, idx) {\
    assert((((idx) >= 0) && ((idx) < (da)->count)) && "Index out of bounds!");\
    if ((idx) != (da)->count - 1) {\
        _da_move(da, (idx) + 1, (da)->count - 1, -1);\
    }\
    (da)->count -= 1;\
    if (((da)->count > DA_INIT_CAP) && ((da)->count <= (da)->capacity * DA_SHRINK_THRESHOLD)) {\
        _da_shrink(da);\
    }\
}\

#define da_pop_at(da, idx, res) {\
    assert((((idx) >= 0) && ((idx) < (da)->count)) && "Index out of bounds!");\
    *(res) = (da)->data[(idx)];\
    da_delete_at(da, idx);\
}\

#define da_debug(da, fmt) {\
    printf("Count   : %zu\n", (da).count);\
    printf("Capacity: %zu\n", (da).capacity);\
    if ((da).count > 0) {\
        typeof(*da.data) res;\
        for (size_t i = 0; i < (da).count; i++) {\
            da_get_at(da, i, &res);\
            printf(fmt, res);\
        }\
        printf("\n");\
        printf("[");\
        for (size_t i = 0; i < (da).capacity; i++) {\
            if (i < (da).count) {\
                printf(fmt, (da).data[i]);\
            }\
            else {\
                printf(" ");\
            }\
        }\
        printf("]");\
        printf("\n");\
        printf("[");\
        for (size_t i = 0; i < (da).capacity; i++) {\
            if (i == 0) {\
                printf("^");\
            }\
            else if (i == (da).count - 1) {\
                printf("$");\
            }\
            else {\
                printf(" ");\
            }\
        }\
        printf("]");\
        printf("\n");\
    }\
}\

#endif // DA_IMPLEMENTATION

#endif // DA_H
