#include <stdio.h>
#include <sys/errno.h>

#define SV_IMPLEMENTATION
#include "sv.h"

#define ARENA_IMPLEMENTATION
#include "arena.h"

const arena default_arena = {0};
arena context_arena = default_arena;

void *context_arena_alloc(size_t bytes) {
    void *ptr = arena_alloc(&context_arena, bytes);
    printf("Allocating %zu bytes, returning pointer: %p\n", bytes, ptr);
    return ptr;
}

void *context_arena_realloc(void *old_ptr, size_t old_bytes, size_t new_bytes) {
    void *ptr = arena_realloc(&context_arena, old_ptr, old_bytes, new_bytes);
    printf("Reallocating old pointer: %p of %zu bytes, to new pointer: %p of %zu bytes\n", old_ptr, old_bytes, ptr, new_bytes);
    return ptr;
}

#define DA_IMPLEMENTATION
#define DA_REALLOC context_arena_realloc
#include "da.h"

#define HT_IMPLEMENTATION
#define HT_MALLOC context_arena_alloc
#define HT_FREE
#include "ht.h"

#define BITARRAY_IMPLEMENTATION
#define BITARRAY_REALLOC context_arena_realloc
#include "bitarray.h"

#define return_defer(res) do {\
    result = res;\
    goto defer;\
} while(0);\

typedef struct {
    size_t x;
    size_t y;
} position;

da_type(positions, position);
ht_type(tower_dict, char, positions, hash_char, is_eql);

typedef struct {
    tower_dict towers;
    size_t width;
    size_t height;
} grid;

typedef enum {
    spawn_once,
    spawn_multiple
} spawn_mode;

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

void parse_row_from_sv(sv s, size_t row, grid *g) {
    for (size_t col = 0; col < s.count; col++) {
        char symbol = s.data[col];
        if (symbol != '.') {
            bool is_new = false;
            ht_slot_tower_dict *slot = ht_insert_key_tower_dict(&g->towers, symbol, &is_new);
            if (is_new) {
                slot->value.count = 0;
            }
            da_ensure_capacity(&slot->value, 1);
            da_grow(&slot->value);
            slot->value.data[slot->value.count - 1].x = row;
            slot->value.data[slot->value.count - 1].y = col;
        }
    }
}

bool parse_grid_from_input(string input, grid *g) {
    if (input.count == 0) {
        printf("ERROR: no characters in input string, cannot parse grid!\n");
        return false;
    }
    sv s = sv_from_ptr(input.data, input.count);
    int width = -1;
    int height = 0;
    while (s.count > 0) {
        sv_pair pair = sv_split_by_char(s, '\n');
        if (width != -1 && pair.fst.count != width) {
            printf("ERROR: width for each rows in the grid should be same. Found %zu, which is not same as previous %d\n", pair.fst.count, width);
            return false;
        }
        parse_row_from_sv(pair.fst, height, g);
        width = pair.fst.count;
        height++;
        s = pair.snd;
    }
    assert(width >= 0);
    g->width = (size_t) width;
    g->height = (size_t) height;
    return true;
}

void mark_anti_towers(grid g, bitarray *ba, spawn_mode mode) {
    for (size_t si = 0; si < g.towers.capacity; si++) {
        ht_slot_tower_dict slot = g.towers.data[si];
        if (slot.is_occupied) {
            positions posns = slot.value;
            for (size_t i = 0; i < posns.count; i++) {
                for (size_t j = i + 1; j < posns.count; j++) {
                    size_t x1 = posns.data[i].x;
                    size_t y1 = posns.data[i].y;
                    size_t x2 = posns.data[j].x;
                    size_t y2 = posns.data[j].y;
                    if (x1 == x2 && y1 == y2) continue;
                    int dx = x2 - x1;
                    int dy = y2 - y1;
                    int at1_x = x1;
                    int at1_y = y1;
                    int at2_x = x2;
                    int at2_y = y2;
                    switch(mode) {
                    case spawn_once:
                        at1_x -= dx;
                        at1_y -= dy;
                        if (at1_x >= 0 && at1_x < g.width && at1_y >= 0 && at1_y < g.height) {
                            bitarray_set(ba, at1_x*g.width + at1_y);
                        }
                        at2_x += dx;
                        at2_y += dy;
                        if (at2_x >= 0 && at2_x < g.width && at2_y >= 0 && at2_y < g.height) {
                            bitarray_set(ba, at2_x*g.width + at2_y);
                        }
                        break;
                    case spawn_multiple:
                        while (at1_x >= 0 && at1_x < g.width && at1_y >= 0 && at1_y < g.height) {
                            bitarray_set(ba, at1_x*g.width + at1_y);
                            at1_x -= dx;
                            at1_y -= dy;
                        }
                        while (at2_x >= 0 && at2_x < g.width && at2_y >= 0 && at2_y < g.height) {
                            bitarray_set(ba, at2_x*g.width + at2_y);
                            at2_x += dx;
                            at2_y += dy;
                        }
                        break;
                    default:
                        printf("ERROR: Unknown spawn_mode: %d\n", mode);
                    }
                }
            }
        }
    }
}

int main(int argc, char **argv) {
    int result = 0;
    char *program = *argv++; argc--;
    if (argc == 0) {
        printf("ERROR: no parameters provided!\n");
        printf("Usage: %s file1 [file2 file3 ...]\n", program);
        return_defer(1);
    }
    while (argc > 0) {
        char *file = *argv++; argc--;
        string file_content = {0}; da_init(&file_content);
        if (read_entire_file(file, &file_content) != 0) {
            return_defer(1);
        }
        grid g = {0};
        ht_init_tower_dict(&g.towers);
        printf("Solving file: %s\n", file);
        if (!parse_grid_from_input(file_content, &g)) {
            return_defer(1);
        }
        bitarray ba = {0};
        bitarray_reset(&ba, g.width*g.height);
        mark_anti_towers(g, &ba, spawn_once);
        printf("Part 1: %zu\n", ba.count);
        mark_anti_towers(g, &ba, spawn_multiple);
        printf("Part 2: %zu\n", ba.count);
        arena_reset(&context_arena);
    }

defer:
    arena_free(&context_arena);
    return result;
}
