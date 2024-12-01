#include <stdio.h>
#include <sys/errno.h>
#include <stdbool.h>

#define SV_IMPLEMENTATION
#include "sv.h"

#define DA_IMPLEMENTATION
#include "da.h"

#define ALGO_IMPLEMENTATION
#include "algo.h"

#define return_defer(res) do {\
    result = res;\
    goto defer;\
} while(0);\

da_type(string, char);
typedef size_t loc;
da_type(locations, loc);

void heapsort_locations(locations *loc) {
#define fn_compare(x, y) x > y
    heapsort(loc->data, loc->count, fn_compare);
#undef fn_compare
}

size_t bisect_left_locations(loc l, locations loc) {
#define fn_key(i) loc.data[i]
    size_t low = 0;
    size_t high = loc.count;
    bisect_left(l, &low, &high, fn_key);
    return low;
#undef fn_key
}

size_t count_occurrences_using_bisect_left_locations(loc l, locations loc) {
    size_t idx = bisect_left_locations(l, loc);
    size_t cnt = 0;
    while (idx < loc.count && loc.data[idx] == l) {
        idx++;
        cnt++;
    }
    return cnt;
}

int read_entire_file(char *file, string *s) {
    int result = 0;
    FILE *fp = fopen(file, "r");
    if (fp == NULL) {
        printf("ERROR: could not open file: %s for reading: %s\n", file, strerror(errno));
        return_defer(1);
    }
    if (fseek(fp, 0, SEEK_END) < 0) {
        printf("ERROR: could not open file: %s for reading: %s\n", file, strerror(errno));
        return_defer(1);
    }
    size_t count = ftell(fp);
    if (fseek(fp, 0, SEEK_SET) < 0) {
        printf("ERROR: could not open file: %s for reading: %s\n", file, strerror(errno));
        return_defer(1);
    }
    da_ensure_capacity(s, count);
    fread(s->data, 1, count, fp);
    if (ferror(fp)) {
        printf("ERROR: could not read from file: %s\n", file);
        return_defer(1);
    }
    s->count = count;

defer:
    if (fp != NULL) {
        fclose(fp);
    }
    return result;
}

bool parse_locations_from_string(string s, locations *loc1, locations *loc2) {
    sv sv = sv_from_ptr(s.data, s.count);
    while (sv.count > 0) {
        sv = sv_trim_left(sv);
        sv_pair pair = sv_split_by_char(sv, ' ');
        int l1 = (int) strtol(pair.fst.data, NULL, 10);
        if (errno != 0) {
            printf("ERROR: could not parse location id from the input: %s\n", strerror(errno));
            return false;
        }
        if (l1 < 0) {
            printf("ERROR: location ids can't be negative: %d\n", l1);
            return false;
        }
        pair.snd = sv_trim_left(pair.snd);
        int l2 = (int) strtol(pair.snd.data, NULL, 10);
        if (errno != 0) {
            printf("ERROR: could not parse location id from the input: %s\n", strerror(errno));
            return false;
        }
        if (l2 < 0) {
            printf("ERROR: location ids can't be negative: %d\n", l1);
            return false;
        }
        sv = sv_split_by_char(pair.snd, '\n').snd;
        da_push(loc1, ((loc) l1));
        da_push(loc2, ((loc) l2));
    }
    if (loc1->count != loc2->count) {
        printf("ERROR: Counts for loc1 and loc2 must be same (%zu vs. %zu). Something went wrong with parsing.\n", loc1->count, loc2->count);
        return false;
    }
    return true;
}

size_t diff_sum(locations loc1, locations loc2) {
    size_t ans = 0;
    for (size_t i = 0; i < loc1.count; i++) {
        ans += (size_t) abs(((int) loc1.data[i]) - ((int) loc2.data[i]));
    }
    return ans;
}

size_t solve_part2(locations loc1, locations loc2) {
    size_t ans = 0;
    for (size_t i = 0; i < loc1.count; i++) {
        ans += loc1.data[i] * count_occurrences_using_bisect_left_locations(loc1.data[i], loc2);
    }
    return ans;
}

int main(int argc, char **argv) {
    int result = 0;
    string file_content = {0}; da_init(&file_content);
    locations loc1 = {0}; da_init(&loc1);
    locations loc2 = {0}; da_init(&loc2);
    char *program = *argv++; argc--;
    if (argc == 0) {
        printf("ERROR: no parameters provided!\n");
        printf("Usage: %s file1 [file2 file3 ...]\n", program);
        return_defer(1);
    }
    while (argc > 0) {
        char *file = *argv++; argc--;
        if (read_entire_file(file, &file_content) != 0) {
            return_defer(1);
        }
        if (!parse_locations_from_string(file_content, &loc1, &loc2)) {
            return_defer(1);
        }
        printf("File: %s\n", file);
        heapsort_locations(&loc1);
        heapsort_locations(&loc2);
        printf("Part 1: %zu\n", diff_sum(loc1, loc2));
        printf("Part 2: %zu\n", solve_part2(loc1, loc2));
        file_content.count = 0;
        loc1.count = 0;
        loc2.count = 0;
    }

defer:
    if (file_content.data) {
        free(file_content.data);
    }
    if (loc1.data) {
        free(loc1.data);
    }
    if (loc2.data) {
        free(loc2.data);
    }
    return result;
}
