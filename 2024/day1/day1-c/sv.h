// Copyright 2024 <Sinha-Ujjawal>
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

#ifndef SV_H
#define SV_H
#include <stdbool.h>

typedef struct {
    char *data;
    size_t count;
} sv;

typedef struct {
  sv fst;
  sv snd;
} sv_pair;

sv sv_from_ptr(char *data, size_t count);
sv sv_from_cstr(const char *data);
sv sv_take_n(sv s, size_t n);
sv sv_drop_n(sv s, size_t n);
sv sv_trim_left(sv s);
sv_pair sv_split_at(sv s, size_t idx);
sv_pair sv_split_by_char(sv s, char c);
sv sv_substring(sv s, size_t begin, size_t count);
bool sv_starts_with(sv self, sv other);
bool sv_starts_with_cstr(sv self, const char *other);
int sv_find(sv self, sv other);
sv_pair sv_split_by_sv(sv self, sv other);
sv_pair sv_split_by_cstr(sv self, const char *other);

#ifdef SV_IMPLEMENTATION
#include <string.h>
#include <ctype.h>
#include <assert.h>

#define SV_FMT "%.*s"
#define SV_DATA(sv) (int)(sv).count, (sv).data

sv sv_from_ptr(char *data, size_t count) {
    sv ret = {0};
    ret.data = data;
    ret.count = count;
    return ret;
}

sv sv_from_cstr(const char *data) {
    return sv_from_ptr((char *) data, strlen(data));
}

sv sv_take_n(sv s, size_t n) {
    if (n > s.count) {
        n = s.count;
    }
    return sv_from_ptr(s.data, n);
}

sv sv_drop_n(sv s, size_t n) {
    if (n > s.count) {
        n = s.count;
    }
    return sv_from_ptr(s.data + n, s.count - n);
}

sv sv_trim_left(sv s) {
    size_t idx = 0;
    while (idx < s.count && isspace(s.data[idx])) {
        idx++;
    }
    return sv_drop_n(s, idx);
}

sv_pair sv_split_at(sv s, size_t idx) {
    sv_pair ret = {0};
    if (s.count == 0) {
        return ret;
    }
    if (idx > s.count) {
        idx = s.count - 1;
        ret.fst = sv_from_ptr(s.data, idx);
    } else {
        ret.fst = sv_from_ptr(s.data, idx);
        ret.snd = sv_from_ptr(s.data + idx, s.count - idx);
    }
    return ret;
}

sv_pair sv_split_by_char(sv s, char c) {
    sv_pair ret = {0};
    if (s.count == 0) {
        return ret;
    }
    size_t idx = 0;
    while (idx < s.count && s.data[idx] != c) {
        idx++;
    }
    if (idx >= s.count) {
        ret.fst = s;
    } else {
        ret.fst = sv_from_ptr(s.data, idx);
        ret.snd = sv_from_ptr(s.data + idx + 1, s.count - idx - 1);
    }
    return ret;
}

sv sv_substring(sv s, size_t begin, size_t count) {
    if (begin > s.count) {
        sv ret = {0};
        return ret;
    }
    return sv_take_n(sv_drop_n(s, begin), count);
}

bool sv_starts_with(sv self, sv other) {
    if (self.count < other.count) {
        return false;
    }
    for (size_t i = 0; i < other.count; i++) {
        if (self.data[i] != other.data[i]) {
            return false;
        }
    }
    return true;
}

bool sv_starts_with_cstr(sv self, const char *other) {
    return sv_starts_with(self, sv_from_cstr(other));
}

int sv_find(sv self, sv other) {
    size_t idx = 0;
    while (other.count > 0 && self.count >= other.count && !sv_starts_with(self, other)) {
        self = sv_drop_n(self, 1);
        idx++;
    }
    if (self.count >= other.count) {
        return idx;
    }
    return -1;
}

sv_pair sv_split_by_sv(sv self, sv other) {
    sv_pair ret = {0};
    if (other.count == 0) {
        ret.snd = self;
        return ret;
    }
    int idx = sv_find(self, other);
    if (idx < 0) {
        ret.fst = self;
        return ret;
    }
    ret.fst = sv_take_n(self, idx + 1);
    ret.snd = sv_substring(self, idx + other.count, self.count);
    return ret;
}

sv_pair sv_split_by_cstr(sv self, const char *other) {
    return sv_split_by_sv(self, sv_from_cstr(other));
}

#endif // SV_IMPLEMENTATION

#endif // SV_H
