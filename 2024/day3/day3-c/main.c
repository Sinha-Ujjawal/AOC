#include <stdio.h>
#include <sys/errno.h>

#define SV_IMPLEMENTATION
#include "sv.h"

#define DA_IMPLEMENTATION
#include "da.h"

#define return_defer(res) do {\
    result = res;\
    goto defer;\
} while(0);\

typedef enum {
    DO,
    DONT,
    MULT,
    COUNT_INSTRUCTION_TYPE
} instruction_type;

typedef struct {
    instruction_type ins_type;
    int l;
    int r;
} instruction;

da_type(string, char);
da_type(instructions, instruction);

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

void parse_instructions_from_input(string input, instructions *ins_arr) {
    sv s = sv_from_ptr(input.data, input.count);
    while (s.count > 0) {
        if (sv_starts_with_cstr(s, "mul(")) {
            s = sv_drop_n(s, strlen("mul("));
            if (s.count == 0) {
                break;
            }
            char *endptr = NULL;
            int l = (int) strtol(s.data, &endptr, 10);
            if (endptr == NULL || s.data == endptr) {
                continue;
            }
            s.count -= endptr - s.data;
            s.data = endptr;
            if (s.count == 0) {
                break;
            }
            if (s.data[0] != ',') {
                continue;
            }
            s.count -= 1;
            s.data++;
            int r = (int) strtol(s.data, &endptr, 10);
            if (endptr == NULL || s.data == endptr) {
                continue;
            }
            s.count -= endptr - s.data;
            s.data = endptr;
            if (s.count == 0) {
                break;
            }
            if (s.data[0] != ')') {
                continue;
            }
            s.count -= 1;
            s.data++;
            da_grow(ins_arr);
            ins_arr->data[ins_arr->count - 1].ins_type = MULT;
            ins_arr->data[ins_arr->count - 1].l = l;
            ins_arr->data[ins_arr->count - 1].r = r;
            continue;
        }
        if (sv_starts_with_cstr(s, "do()")) {
            s = sv_drop_n(s, strlen("do()"));
            da_grow(ins_arr);
            ins_arr->data[ins_arr->count - 1].ins_type = DO;
            continue;
        }
        if (sv_starts_with_cstr(s, "don't()")) {
            s = sv_drop_n(s, strlen("don't()"));
            da_grow(ins_arr);
            ins_arr->data[ins_arr->count - 1].ins_type = DONT;
            continue;
        }
        s = sv_drop_n(s, 1);
    }
}

size_t solve_part_1(instructions ins_arr) {
    size_t ans = 0;
    for (size_t i = 0; i < ins_arr.count; i++) {
        instruction ins = ins_arr.data[i];
        if (ins.ins_type == MULT) {
            ans += ins.l * ins.r;
        }
    }
    return ans;
}

size_t solve_part_2(instructions ins_arr) {
    size_t ans = 0;
    bool ignore = false;
    for (size_t i = 0; i < ins_arr.count; i++) {
        instruction ins = ins_arr.data[i];
        if (ins.ins_type == MULT && !ignore) {
            ans += ins.l * ins.r;
        } else if (ins.ins_type == DO) {
            ignore = false;
        } else if (ins.ins_type == DONT) {
            ignore = true;
        }
    }
    return ans;
}

int main(int argc, char **argv) {
    int result = 0;
    string file_content = {0}; da_init(&file_content);
    instructions ins_arr = {0}; da_init(&ins_arr);
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
        parse_instructions_from_input(file_content, &ins_arr);
        printf("Part 1: %zu\n", solve_part_1(ins_arr));
        printf("Part 2: %zu\n", solve_part_2(ins_arr));
        file_content.count = 0;
        ins_arr.count = 0;
    }

defer:
    if (file_content.data) {
        free(file_content.data);
    }
    if (ins_arr.data) {
        free(ins_arr.data);
    }
    return result;
}
