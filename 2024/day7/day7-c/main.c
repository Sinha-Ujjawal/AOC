#include <stdio.h>
#include <sys/errno.h>

#define SV_IMPLEMENTATION
#include "sv.h"

#define ARENA_IMPLEMENTATION
#include "arena.h"

void *context_arena_realloc(void *old_ptr, size_t old_bytes, size_t new_bytes);

#define DA_IMPLEMENTATION
#define DA_REALLOC(old_ptr, old_bytes, new_bytes) context_arena_realloc((old_ptr), (old_bytes), (new_bytes))
#include "da.h"

#define return_defer(res) do {\
    result = res;\
    goto defer;\
} while(0);\

arena context_arena = {0};
void *context_arena_realloc(void *old_ptr, size_t old_bytes, size_t new_bytes) {
    void *new_ptr = arena_realloc(&context_arena, old_ptr, old_bytes, new_bytes);
    // printf("Reallocating %p (%zu byte(s)) -> %p (%zu byte(s))\n", old_ptr, old_bytes, new_ptr, new_bytes);
    return new_ptr;
}

da_type(string, char);
da_type(long_arr, long);

typedef enum {
    add,
    mul,
    concat
} operation;

typedef struct {
    long lhs;
    long_arr rhs;
} equation;

da_type(equations, equation);

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

bool equation_from_sv(sv s, equation *eqn) {
    bool result = true;
    sv_pair pair = sv_split_by_cstr(s, ": ");
    long lhs;
    if (!sv_parse_long(pair.fst, &lhs, NULL)) {
        return_defer(false);
    }
    eqn->lhs = lhs;
    da_init(&eqn->rhs);
    s = pair.snd;
    while (s.count > 0) {
        pair = sv_split_by_char(s, ' ');
        long rhs_component;
        if (!sv_parse_long(pair.fst, &rhs_component, NULL)) {
            return_defer(false);
        }
        da_push(&eqn->rhs, rhs_component);
        s = pair.snd;
    }
defer:
    if (!result) {
        printf("ERROR: could not parse equation from: `"SV_FMT"`\n", SV_DATA(s));
    }
    return result;
}

bool equations_from_string(string input, equations *eqns) {
    sv s = sv_from_ptr(input.data, input.count);
    while (s.count > 0) {
        sv_pair pair = sv_split_by_char(s, '\n');
        da_grow(eqns);
        if (!equation_from_sv(pair.fst, &eqns->data[eqns->count - 1])) {
            return false;
        }
        s = pair.snd;
    }
    return true;
}

long concatenate_longs(long num1, long num2) {
    long multiplier = 1;
    while (multiplier <= num2) {
        multiplier *= 10; // Compute 10^digits of num2
    }
    return num1 * multiplier + num2; // Shift num1 and add num2
}

bool can_be_satisfied_recur(long target, long acc, size_t i, long_arr components, const operation *allowed_operations, size_t allowed_operations_count) {
    if (i >= components.count) {
        return target == acc;
    }
    bool result = false;
    for (size_t j = 0; j < allowed_operations_count; j++) {
        long new_acc;
        switch (allowed_operations[j]) {
        case add   : new_acc = acc + components.data[i]; break;
        case mul   : new_acc = acc * components.data[i]; break;
        case concat: new_acc = concatenate_longs(acc, components.data[i]); break;
        default    : new_acc = acc;
        }
        result = result || can_be_satisfied_recur(target, new_acc, i+1, components, allowed_operations, allowed_operations_count);
        if (result) break;
    }
    return result;
}

bool can_be_satisfied(equation eqn, const operation *allowed_operations, size_t allowed_operations_count) {
    if (eqn.rhs.count == 0) {
        return eqn.lhs == 0;
    }
    return can_be_satisfied_recur(eqn.lhs, eqn.rhs.data[0], 1, eqn.rhs, allowed_operations, allowed_operations_count);
}

long solve_part_1(equations eqns) {
    const static operation allowed_operations[2] = {add, mul};
    long result = 0;
    for (size_t i = 0; i < eqns.count; i++) {
        if (can_be_satisfied(eqns.data[i], allowed_operations, 2)) {
            result += eqns.data[i].lhs;
        }
    }
    return result;
}

long solve_part_2(equations eqns) {
    const static operation allowed_operations[3] = {add, mul, concat};
    long result = 0;
    for (size_t i = 0; i < eqns.count; i++) {
        if (can_be_satisfied(eqns.data[i], allowed_operations, 3)) {
            result += eqns.data[i].lhs;
        }
    }
    return result;
}

int main(int argc, char **argv) {
    int result = 0;
    string file_content = {0}; da_init(&file_content);
    equations eqns = {0}; da_init(&eqns);
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
        if (!equations_from_string(file_content, &eqns)) {
            return_defer(1);
        }
        // for (size_t i = 0; i < eqns.count; i++) {
        //     equation eqn = eqns.data[i];
        //     printf("%ld:", eqn.lhs);
        //     for (size_t j = 0; j < eqn.rhs.count; j++) {
        //         printf(" %ld", eqn.rhs.data[j]);
        //     }
        //     printf("\n");
        // }
        printf("Part 1: %ld\n", solve_part_1(eqns));
        printf("Part 2: %ld\n", solve_part_2(eqns));
        file_content.count = 0;
        eqns.count = 0;
    }

defer:
    arena_free(&context_arena);
    return result;
}
