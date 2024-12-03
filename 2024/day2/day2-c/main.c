#include <stdio.h>
#include <sys/errno.h>
#include <stdbool.h>
#include <assert.h>

#define SV_IMPLEMENTATION
#include "sv.h"

#define DA_IMPLEMENTATION
#include "da.h"

#define return_defer(res) do {\
    result = res;\
    goto defer;\
} while(0);\

da_type(string, char);
da_type(levels, int);
da_type(levels_arr, levels);

int levels_pop_at(levels *l, size_t idx) {
    int ret;
    da_pop_at(l, idx, &ret);
    return ret;
}

void levels_insert_at(levels *l, size_t idx, int value) {
    da_insert_at(l, idx, value);
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

bool parse_levels(string input, levels_arr *lev_arr) {
    sv s = sv_from_ptr(input.data, input.count);
    while (s.count > 0) {
        sv_pair pair = sv_split_by_char(s, '\n');
        sv line = pair.fst;
        s = pair.snd;
        da_grow(lev_arr);
        levels *ls = &lev_arr->data[lev_arr->count - 1];
        if (ls->data == NULL) da_init(ls);
        ls->count = 0;
        while (line.count > 0) {
            line = sv_trim_left(line);
            sv_pair pair = sv_split_by_char(line, ' ');
            int l = (int) strtol(pair.fst.data, NULL, 10);
            if (errno != 0) {
                printf("ERROR: could not parse level from the input: %s\n", strerror(errno));
                return false;
            }
            da_push(ls, l);
            line = pair.snd;
        }
    }
    return true;
}

bool all_increasing(levels ls) {
    for (size_t i = 1; i < ls.count; i++) {
        if (ls.data[i] < ls.data[i - 1]) {
            return false;
        }
    }
    return true;
}

bool all_decreasing(levels ls) {
    for (size_t i = 1; i < ls.count; i++) {
        if (ls.data[i] > ls.data[i - 1]) {
            return false;
        }
    }
    return true;
}

bool is_differ_by_in_range(levels ls, size_t low, size_t high) {
    assert(low <= high);
    if (ls.count == 0) {
        return true;
    }
    for (size_t i = 0; i < ls.count - 1; i++) {
        size_t diff = (size_t) abs(ls.data[i] - ls.data[i+1]);
        if ((diff < low) || (diff > high)) {
            return false;
        }
    }
    return true;
}

bool is_safe(levels l) {
    return (all_increasing(l) || all_decreasing(l)) && is_differ_by_in_range(l, 1, 3);
}

size_t solve_part1(levels_arr lev_arr) {
    size_t ans = 0;
    for (size_t i = 0; i < lev_arr.count; i++) {
        levels l = lev_arr.data[i];
        if (is_safe(l)) {
            ans++;
        }
    }
    return ans;
}

size_t solve_part2(levels_arr lev_arr) {
    size_t ans = 0;
    for (size_t i = 0; i < lev_arr.count; i++) {
        levels *l = &lev_arr.data[i];
        if (is_safe(*l)) {
            ans++;
        } else {
            for (size_t j = 0; j < l->count; j++) {
                int temp = levels_pop_at(l, j);
                if (is_safe(*l)) {
                    ans++;
                    levels_insert_at(l, j, temp);
                    break;
                }
                levels_insert_at(l, j, temp);
            }
        }
    }
    return ans;
}

int main(int argc, char **argv) {
    int result = 0;
    string file_content = {0}; da_init(&file_content);
    levels_arr lev_arr = {0}; da_init(&lev_arr);
    char *program = *argv++; argc--;
    if (argc == 0) {
        printf("ERROR: no parameters provided!\n");
        printf("Usage: %s file1 [file2 file3 ...]\n", program);
        return_defer(1);
    }
    while (argc > 0) {
        char *file = *argv++; argc--;
        printf("Solving file: %s\n", file);
        if (read_entire_file(file, &file_content) != 0) {
            return_defer(1);
        }
        if (!parse_levels(file_content, &lev_arr)) {
            return_defer(1);
        }
        printf("Part 1: %zu\n", solve_part1(lev_arr));
        printf("Part 2: %zu\n", solve_part2(lev_arr));
        file_content.count = 0;
        lev_arr.count = 0;
    }

defer:
    if (file_content.data) {
        free(file_content.data);
    }
    if (lev_arr.data) {
        for (size_t i = 0; i < lev_arr.count; i++) {
            if (lev_arr.data[i].data) {
                free(lev_arr.data[i].data);
            }
        }
        free(lev_arr.data);
    }
    return result;
}
