// Copyright 2024 <Sinha-Ujjawal>
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to
// deal in the Software without restriction, including without limitation the
// rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
// sell copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
// IN THE SOFTWARE.

#ifndef HT_H_
#define HT_H_

#ifdef HT_IMPLEMENTATION
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stdint.h>

#ifndef HT_INITIAL_CAP
#define HT_INITIAL_CAP 64
#endif // HT_INITIAL_CAP

#ifndef HT_GROWTH_FACTOR
#define HT_GROWTH_FACTOR 2
#endif // HT_GROWTH_FACTOR

#ifndef HT_SHRINK_THRESHOLD_FACTOR
#define HT_SHRINK_THRESHOLD_FACTOR 0.33
#endif // HT_SHRINK_THRESHOLD_FACTOR

#ifndef HT_SHRINK_FACTOR
#define HT_SHRINK_FACTOR 0.5
#endif // HT_SHRINK_FACTOR

#ifndef HT_MALLOC
#define HT_MALLOC malloc
#endif // HT_MALLOC

#ifndef HT_FREE
#define HT_FREE free
#endif // HT_FREE

#define ht_pmod(a, b) (a % b + b) % b

// References:
//  - https://en.wikipedia.org/wiki/Open_addressing
//  - https://stackoverflow.com/a/12996028 (Thomam Mueller Hash Function)

#define ht_type(NAME, KEY, VALUE, HASH_FN, IS_KEY_EQL)\
typedef struct {\
    KEY key;\
    VALUE value;\
    bool is_occupied;\
} ht_slot_##NAME;\
\
typedef struct {\
    ht_slot_##NAME *data;\
    size_t count;\
    size_t capacity;\
} NAME;\
\
ht_slot_##NAME *ht_insert_key_##NAME(NAME *ht, KEY key, bool *is_new);\
\
void ht_resize_##NAME(NAME *ht, size_t new_capacity) {\
    ht_slot_##NAME *old_data = ht->data;\
    size_t old_capacity = ht->capacity;\
    ht->data = HT_MALLOC(sizeof(ht_slot_##NAME) * new_capacity);\
    ht->count = 0;\
    ht->capacity = new_capacity;\
    for (size_t i = 0; i < old_capacity; i++) {\
        ht_slot_##NAME slot = old_data[i];\
        if (slot.is_occupied) {\
            ht_slot_##NAME *new_slot = ht_insert_key_##NAME(ht, slot.key, NULL);\
            new_slot->value = slot.value;\
        }\
    }\
    HT_FREE(old_data);\
}\
\
void ht_init_##NAME(NAME *ht) {\
    ht->count = 0;\
    ht->capacity = 0;\
    ht->data = NULL;\
    ht_resize_##NAME(ht, HT_INITIAL_CAP);\
}\
\
void ht_reset_##NAME(NAME *ht) {\
    for (size_t i = 0; i < ht->capacity; i++) {\
        ht->data[i].is_occupied = false;\
    }\
    ht->count = 0;\
}\
\
size_t ht_find_slot_##NAME(NAME ht, KEY key) {\
    size_t i = ht_pmod(HASH_FN(key), ht.capacity);\
    while (ht.data[i].is_occupied && !(IS_KEY_EQL(ht.data[i].key, key))) {\
        i = (i + 1) % ht.capacity;\
    }\
    return i;\
}\
\
bool ht_get_##NAME(NAME ht, KEY key, VALUE *result) {\
    if (ht.count == 0) return false;\
    size_t i = ht_find_slot_##NAME(ht, key);\
    if (ht.data[i].is_occupied) { /* key is in the table */ \
        if (result) *result = ht.data[i].value;\
        return true;\
    }\
    return false;\
}\
\
ht_slot_##NAME *ht_insert_key_##NAME(NAME *ht, KEY key, bool *is_new) {\
    size_t i;\
    i = ht_find_slot_##NAME(*ht, key);\
    if (ht->data[i].is_occupied) { /* key is in the table */ \
        if (is_new) *is_new = false;\
        return &ht->data[i];\
    }\
    ht->count++;\
    ht->data[i].is_occupied = true;\
    ht->data[i].key = key;\
    if (ht->count == ht->capacity) {\
        ht_resize_##NAME(ht, ht->capacity * HT_GROWTH_FACTOR);\
        i = ht_find_slot_##NAME(*ht, key);\
    }\
    if (is_new) *is_new = true;\
    return &ht->data[i];\
}\
\
void ht_delete_##NAME(NAME *ht, KEY key, VALUE *result) {\
    if (ht->count == 0) return;\
    size_t i = ht_find_slot_##NAME(*ht, key);\
    if (!ht->data[i].is_occupied) {\
        return;\
    }\
    ht->data[i].is_occupied = false;\
    size_t j = i;\
    while (true) {\
        j = (j + 1) % ht->capacity;\
        if (!ht->data[i].is_occupied) break;\
        size_t k = ht_pmod(HASH_FN(ht->data[i].key), ht->capacity);\
        /* determine if k lies cyclically in (i,j] */\
        /* i ≤ j: |    i..k..j    |                */\
        /* i > j:    |.k..j     i....|             */\
        /*        or |....j     i..k.|             */\
        if (i <= j && i < k && k <= j) continue;\
        if (k <= j || i < k) continue;\
        ht->data[i].is_occupied = true;\
        ht->data[i].key = ht->data[j].key;\
        ht->data[i].value = ht->data[j].value;\
        ht->data[j].is_occupied = false;\
        i = j;\
    }\
    ht->count -= 1;\
    if (ht->capacity > HT_INITIAL_CAP && ht->count <= ht->capacity * HT_SHRINK_THRESHOLD_FACTOR) {\
        ht_resize_##NAME(ht, ht->capacity * HT_SHRINK_FACTOR);\
    }\
}\
\
void ht_debug_##NAME(NAME ht, const char *key_fmt, const char *value_fmt) {\
    printf("hashtable<"#KEY", "#VALUE">\n");\
    for (size_t i = 0; i < ht.capacity; i++) {\
        ht_slot_##NAME slot = ht.data[i];\
        if (slot.is_occupied) {\
            printf("  slot %zu:\n", i);\
            printf("    key: ")  ; printf(key_fmt, slot.key); printf("\n");\
            printf("    value: "); printf(value_fmt, slot.value); printf("\n");\
        }\
    }\
}\

#define hash_id(x) x
#define is_eql(x, y) x == y

// few hash function implementations

// https://stackoverflow.com/a/12996028
unsigned int hash_uint_thomas_mueller(unsigned int x) {
    x = ((x >> 16) ^ x) * 0x45d9f3b;
    x = ((x >> 16) ^ x) * 0x45d9f3b;
    x = (x >> 16) ^ x;
    return x;
}

int hash_int_thomas_mueller(int x) {
    if (x < 0) {
        x *= -1;
        x <<= 1;
        x++;
        return hash_uint_thomas_mueller(x);
    }
    return hash_uint_thomas_mueller(x <<= 1);
}

#define hash_uint hash_uint_thomas_mueller
#define hash_int hash_int_thomas_mueller

unsigned int hash_char(char c) {
    return hash_int((int) c);
}

unsigned int hash_long(long x) {
    x = (x ^ (x >> 30)) * 0xbf58476d1ce4e5b9L;
    x = (x ^ (x >> 27)) * 0x94d049bb133111ebL;
    x = x ^ (x >> 31);
    return (unsigned int)x;
}

unsigned int hash_float(float x) {
    uint32_t bits;
    memcpy(&bits, &x, sizeof(float)); // Interpret the float as an integer
    return hash_uint(bits); // Reuse the unsigned int hash function
}

// djb2 hash
unsigned int hash_struct(const void *data, size_t size) {
    const unsigned char *bytes = (const unsigned char *)data;
    unsigned long hash = 5381;
    for (size_t i = 0; i < size; i++) {
        hash = ((hash << 5) + hash) + bytes[i]; // hash * 33 + current byte
    }
    return (unsigned int)hash;
}

unsigned int hash_cstr(char *data, size_t size) {
    return hash_struct(data, size);
}

#endif // HT_IMPLEMENTATION
#endif // HT_H_
