// Copyright 2024 <Sinha-Ujjawal>
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

#ifndef ALGO_H_
#define ALGO_H_

#ifdef ALGO_IMPLEMENTATION

// start binary_search
#define bisect_left(target, low, high, fn_key) {\
    while (*low < *high) {\
        typeof(*low) mid = *low + ((*high - *low) >> 1);\
        if (fn_key(mid) < target) {\
            *low = mid + 1;\
        } else {\
            *high = mid;\
        }\
    }\
}\
// end binary_search

// start heap
#define heapify(arr, n, fn_compare, root) {\
    typeof((arr)) items = (arr);\
    size_t count = (n);\
    size_t i = (root);\
    while(1) {\
        size_t r     = i;\
        size_t left  = r << 1;\
        size_t right = left + 1;\
        if (left < count  && (fn_compare(items[left] , items[r]))) {\
            r = left;\
        }\
        if (right < count && (fn_compare(items[right], items[r]))) {\
            r = right;\
        }\
        if (i == r) {\
            break;\
        }\
        typeof(*items) temp = items[r];\
        items[r]            = items[i];\
        items[i]            = temp;\
        i = r;\
    }\
}\

#define build_heap(arr, n, fn_compare) {\
    size_t count = (n);\
    if (count > 0) {\
        int i = (count >> 1) - 1;\
        while (i >= 0) {\
            size_t root = i;\
            heapify(arr, n, fn_compare, root);\
            i--;\
        }\
    }\
}\

#define heapsort(arr, n, fn_compare) {\
    build_heap(arr, n, fn_compare);\
    typeof((arr)) items = (arr);\
    size_t end = (n);\
    while (end > 1) {\
        end -= 1;\
        typeof(*items) temp = items[0];\
        items[0]            = items[end];\
        items[end]          = temp;\
        heapify(arr, end, fn_compare, 0);\
    }\
}\
// end heap

#endif // ALGO_IMPLEMENTATION
#endif // ALGO_H_
