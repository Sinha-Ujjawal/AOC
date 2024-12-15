// Copyright 2024 <Sinha-Ujjawal>
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

#ifndef BITARRAY_H_
#define BITARRAY_H_
#include <stdbool.h>
#include <assert.h>

typedef struct bitarray bitarray;

void bitarray_reset(bitarray *bits, size_t capacity);
bool bitarray_set(bitarray *bits, size_t posn);
bool bitarray_unset(bitarray *bits, size_t posn);
bool bitarray_isset(bitarray bits, size_t posn);
void bitarray_print(bitarray bits);

#ifdef BITARRAY_IMPLEMENTATION
#include <math.h>
#include <stdlib.h>
#include <string.h>

#define BITARRAY_REALLOC realloc
#define BITARRAY_MEMSET  memset

struct bitarray {
    char   *buckets;
    size_t num_buckets;
    size_t count;
    size_t capacity;
};

void bitarray_reset(bitarray *bits, size_t capacity) {
    bits->num_buckets = ceil(capacity / 8.0);
    bits->buckets = BITARRAY_REALLOC(bits->buckets, bits->num_buckets);
    BITARRAY_MEMSET(bits->buckets, 0, bits->num_buckets);
    bits->count = 0;
    bits->capacity = capacity;
}

bool bitarray_set(bitarray *bits, size_t posn) {
    size_t bucket_posn = posn / 8;
    if (bucket_posn >= bits->num_buckets) {
        return false; // out of bounds
    }
    size_t posn_in_bucket = posn % 8;
    if (bits->buckets[bucket_posn] & (1 << posn_in_bucket)) {
        return false; // already set
    }
    bits->count += 1;
    bits->buckets[bucket_posn] |= 1 << posn_in_bucket;
    return true;
}

bool bitarray_unset(bitarray *bits, size_t posn) {
    size_t bucket_posn = posn / 8;
    if (bucket_posn >= bits->capacity) {
        return false; // out of bounds
    }
    size_t posn_in_bucket = posn % 8;
    if (!(bits->buckets[bucket_posn] & (1 << posn_in_bucket))) {
        return false; // already unset
    }
    assert(bits->count > 0);
    bits->count -= 1;
    bits->buckets[bucket_posn] ^= 1 << posn_in_bucket;
    return true;
}

bool bitarray_isset(bitarray bits, size_t posn) {
    size_t bucket_posn = posn / 8;
    if (bucket_posn >= bits.capacity) {
        return false; // out of bounds
    }
    size_t posn_in_bucket = posn % 8;
    return bits.buckets[bucket_posn] & (1 << posn_in_bucket);
}

void bitarray_print(bitarray bits) {
    printf("bitarray<");
    for (size_t i = 0; i < ceil(bits.capacity / 8.0); i++) {
        if (i > 0) {
            printf(" ");
        }
        printf("%x", bits.buckets[i]);
    }
    printf(">\n");
}

#endif // BITARRAY_IMPLEMENTATION
#endif // BITARRAY_H_
