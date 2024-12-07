#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include <sys/errno.h>

#define SV_IMPLEMENTATION
#include "sv.h"

#define DA_IMPLEMENTATION
#include "da.h"

#define return_defer(res) do {\
    result = res;\
    goto defer;\
} while(0);\

da_type(string, char);
da_type(sv_arr, sv);

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

void append_to_sv_arr(string input, sv_arr *s_arr) {
    sv s = sv_from_ptr(input.data, input.count);
    while (s.count > 0) {
        sv_pair pair = sv_split_by_char(s, '\n');
        da_push(s_arr, pair.fst);
        s = pair.snd;
    }
}

char sv_arr_char_at(sv_arr s_arr, char def, int row, int col) {
    if (row < 0 || row >= s_arr.count || col < 0) {
        return def;
    }
    sv s = s_arr.data[row];
    if (col >= s.count) {
        return def;
    }
    return s.data[col];
}

typedef enum {
    UP,
    DOWN,
    LEFT,
    RIGHT,
    DIAG_UP_LEFT,
    DIAG_UP_RIGHT,
    DIAG_DOWN_LEFT,
    DIAG_DOWN_RIGHT,
    COUNT_DIRECTIONS
} direction;

static_assert(COUNT_DIRECTIONS == 8, "Check `enum direction`");
bool sv_arr_match_in_direction(sv_arr s_arr, int row, int col, direction dir, const char *cstr) {
    size_t cstr_len = strlen(cstr);
    for (size_t i = 0; i < cstr_len; i++) {
        if (sv_arr_char_at(s_arr, '\0', row, col) != cstr[i]) {
            return false;
        }
        switch(dir) {
        case UP:
            row--;
            break;
        case DOWN:
            row++;
            break;
        case LEFT:
            col--;
            break;
        case RIGHT:
            col++;
            break;
        case DIAG_UP_LEFT:
            row--;
            col--;
            break;
        case DIAG_UP_RIGHT:
            row--;
            col++;
            break;
        case DIAG_DOWN_LEFT:
            row++;
            col--;
            break;
        case DIAG_DOWN_RIGHT:
            row++;
            col++;
            break;
        default:
            assert(false && "Unreachable!");
        }
    }
    return true;
}

size_t solve_part_1(sv_arr s_arr) {
    size_t ans = 0;
    for (int row = 0; row < s_arr.count; row++) {
        for (int col = 0; col < s_arr.data[row].count; col++) {
            for (size_t dir = 0; dir < COUNT_DIRECTIONS; dir++) {
                if (sv_arr_match_in_direction(s_arr, row, col, dir, "XMAS")) {
                    ans++;
                }
            }
        }
    }
    return ans;
}

size_t solve_part_2(sv_arr s_arr) {
    size_t ans = 0;
    for (int row = 0; row < s_arr.count; row++) {
        for (int col = 0; col < s_arr.data[row].count; col++) {
            // M.M
            // .A.
            // S.S
            if (sv_arr_match_in_direction(s_arr, row, col, DIAG_UP_LEFT   , "AM") &&
                sv_arr_match_in_direction(s_arr, row, col, DIAG_UP_RIGHT  , "AM") &&
                sv_arr_match_in_direction(s_arr, row, col, DIAG_DOWN_LEFT , "AS") &&
                sv_arr_match_in_direction(s_arr, row, col, DIAG_DOWN_RIGHT, "AS")) {
                ans++;
                continue;
            }
            // M.S
            // .A.
            // M.S
            if (sv_arr_match_in_direction(s_arr, row, col, DIAG_UP_LEFT   , "AM") &&
                sv_arr_match_in_direction(s_arr, row, col, DIAG_UP_RIGHT  , "AS") &&
                sv_arr_match_in_direction(s_arr, row, col, DIAG_DOWN_LEFT , "AM") &&
                sv_arr_match_in_direction(s_arr, row, col, DIAG_DOWN_RIGHT, "AS")) {
                ans++;
                continue;
            }
            // S.M
            // .A.
            // S.M
            if (sv_arr_match_in_direction(s_arr, row, col, DIAG_UP_LEFT   , "AS") &&
                sv_arr_match_in_direction(s_arr, row, col, DIAG_UP_RIGHT  , "AM") &&
                sv_arr_match_in_direction(s_arr, row, col, DIAG_DOWN_LEFT , "AS") &&
                sv_arr_match_in_direction(s_arr, row, col, DIAG_DOWN_RIGHT, "AM")) {
                ans++;
                continue;
            }
            // S.S
            // .A.
            // M.M
            if (sv_arr_match_in_direction(s_arr, row, col, DIAG_UP_LEFT   , "AS") &&
                sv_arr_match_in_direction(s_arr, row, col, DIAG_UP_RIGHT  , "AS") &&
                sv_arr_match_in_direction(s_arr, row, col, DIAG_DOWN_LEFT , "AM") &&
                sv_arr_match_in_direction(s_arr, row, col, DIAG_DOWN_RIGHT, "AM")) {
                ans++;
                continue;
            }
        }
    }
    return ans;
}

int main(int argc, char **argv) {
    int result = 0;
    string file_content = {0}; da_init(&file_content);
    sv_arr s_arr = {0}; da_init(&s_arr);
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
        printf("Solving file: %s\n", file);
        append_to_sv_arr(file_content, &s_arr);
        printf("Part 1: %zu\n", solve_part_1(s_arr));
        printf("Part 2: %zu\n", solve_part_2(s_arr));
        file_content.count = 0;
        s_arr.count = 0;
    }

defer:
    if (file_content.data) {
        free(file_content.data);
    }
    if (s_arr.data) {
        free(s_arr.data);
    }
    return result;
}
