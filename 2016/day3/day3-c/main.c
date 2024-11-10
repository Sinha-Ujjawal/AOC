#include <stdio.h>
#include <sys/errno.h>
#include <string.h>

#define DA_IMPLEMENTATION
#include "da.h"

#define SV_IMPLEMENTATION
#include "sv.h"

#define return_defer(res) do { \
    result = res; \
    goto defer; \
} while(0); \

da_type(string, char);

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

int is_valid_triangle(int a, int b, int c) {
    if ((a + b) <= c) return 0;
    if ((a + c) <= b) return 0;
    if ((c + b) <= a) return 0;
    return 1;
}

int parse_row(sv s, int row[3]) {
    sv_pair pair;
    for (size_t i = 0; i < 3; i++) {
        pair = sv_split_by_char(s, ' '); s = sv_trim_left(pair.snd);
        if (pair.fst.count == 0) {
            return -1;
        }
        errno = 0;
        row[i] = (int) strtol(pair.fst.data, NULL, 10);
        if (errno != 0) {
            printf("ERROR: could not parse %zu column from the input: %s\n", i+1, strerror(errno));
            return -1;
        }
    }
    return 0;
}

void solve_part_1(char *file, string triangles) {
    printf("Solving part 1 for file: %s\n", file);
    sv s = sv_from_ptr(triangles.data, triangles.count), line;
    int row[3];
    size_t valid_triangles = 0;
    size_t line_no = 0;
    sv_pair pair;
    while (s.count > 0) {
        pair = sv_split_by_char(s, '\n'); s = pair.snd; line_no++;
        line = sv_trim_left(pair.fst);
        if (parse_row(line, row) != 0) {
            printf("WARN: could not parse triangle from file: %s:%zu\n", file, line_no);
            continue;
        }
        if (is_valid_triangle(row[0], row[1], row[2])) {
            valid_triangles++;
        }
    }
    printf("Total no. of valid triangles: %zu\n", valid_triangles);
}

void solve_part_2(char *file, string triangles) {
    printf("Solving part 2 for file: %s\n", file);
    sv s = sv_from_ptr(triangles.data, triangles.count), line;
    int row1[3], row2[3], row3[3];
    size_t valid_triangles = 0;
    size_t line_no = 0;
    sv_pair pair;
    while (s.count > 0) {
        pair = sv_split_by_char(s, '\n'); s = pair.snd; line_no++;
        line = sv_trim_left(pair.fst);
        if (parse_row(line, row1) != 0) {
            printf("WARN: could not parse triangle from file: %s:%zu\n", file, line_no);
            continue;
        }
        pair = sv_split_by_char(s, '\n'); s = pair.snd; line_no++;
        line = sv_trim_left(pair.fst);
        if (parse_row(line, row2) != 0) {
            printf("WARN: could not parse triangle from file: %s:%zu\n", file, line_no);
            continue;
        }
        pair = sv_split_by_char(s, '\n'); s = pair.snd; line_no++;
        line = sv_trim_left(pair.fst);
        if (parse_row(line, row3) != 0) {
            printf("WARN: could not parse triangle from file: %s:%zu\n", file, line_no);
            continue;
        }
        if (is_valid_triangle(row1[0], row2[0], row3[0])) valid_triangles++;
        if (is_valid_triangle(row1[1], row2[1], row3[1])) valid_triangles++;
        if (is_valid_triangle(row1[2], row2[2], row3[2])) valid_triangles++;
    }
    printf("Total no. of valid triangles: %zu\n", valid_triangles);
}

int main(int argc, char **argv) {
    int result = 0;
    string file_content = {0}; da_init(&file_content);
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
        solve_part_1(file, file_content);
        solve_part_2(file, file_content);
        file_content.count = 0;
    }

defer:
    free(file_content.data);
    return result;
}
