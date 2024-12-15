#include <stdio.h>
#include <sys/errno.h>

#define SV_IMPLEMENTATION
#include "sv.h"

#define DA_IMPLEMENTATION
#include "da.h"

#define BITARRAY_IMPLEMENTATION
#include "bitarray.h"

#define return_defer(res) do {\
    result = res;\
    goto defer;\
} while(0);\

da_type(string, char);
da_type(sv_arr, sv);

typedef struct {
    sv_arr rows;
    size_t width;
    size_t height;
} grid;

typedef enum {
    NORTH,
    EAST,
    WEST,
    SOUTH,
    COUNT_DIRECTION,
} direction;

typedef struct {
    int row;
    int col;
    direction dir;
} position;

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

bool string_to_grid(string str, grid *g) {
    sv s = sv_from_ptr(str.data, str.count);
    size_t width = 0, height = 0;
    while (s.count > 0) {
        sv_pair pair = sv_split_by_char(s, '\n');
        if (height > 0 && width != pair.fst.count) {
            printf("All rows should be of same width!\n");
            return false;
        }
        width = pair.fst.count;
        da_push(&g->rows, pair.fst);
        s = pair.snd;
        height++;
    }
    g->width = width;
    g->height = height;
    return true;
}

char grid_char_at(grid g, char def, int row, int col) {
    if (row < 0 || row >= g.rows.count || col < 0) {
        return def;
    }
    if (col >= g.rows.data[row].count) {
        return def;
    }
    return g.rows.data[row].data[col];
}

bool find_gaurd_initial_position(grid g, position *posn) {
    for (int row = 0; row < g.rows.count; row++) {
        for (int col = 0; col < g.rows.data[row].count; col++) {
            if (g.rows.data[row].data[col] == '^') {
                posn->row = row;
                posn->col = col;
                posn->dir = NORTH;
                return true;
            }
        }
    }
    return false;
}

bool in_bounds(grid g, position posn) {
    int row = posn.row;
    int col = posn.col;
    if (row < 0 || row >= g.rows.count || col < 0) {
        return false;
    }
    if (col > g.rows.data[row].count) {
        return false;
    }
    return true;
}

direction turn_right(direction dir) {
    switch(dir) {
    case NORTH: return WEST;
    case EAST:  return NORTH;
    case WEST:  return SOUTH;
    case SOUTH: return EAST;
    default:    return dir;
    }
    return dir;
}

void next_posn(grid g, position *posn) {
    int next_row, next_col;
    switch (posn->dir) {
    case NORTH:
        next_row = posn->row - 1;
        next_col = posn->col;
        break;
    case EAST:
        next_row = posn->row;
        next_col = posn->col - 1;
        break;
    case WEST:
        next_row = posn->row;
        next_col = posn->col + 1;
        break;
    case SOUTH:
        next_row = posn->row + 1;
        next_col = posn->col;
        break;
    default:
        printf("Unreachable!\n");
        return;
    }
    if (grid_char_at(g, '\0', next_row, next_col) == '#') {
        posn->dir = turn_right(posn->dir);
    } else {
        posn->row = next_row;
        posn->col = next_col;
    }
}

typedef struct {
    bitarray north_bm;
    bitarray south_bm;
    bitarray east_bm;
    bitarray west_bm;
    size_t width;
    size_t height;
    size_t set_count;
} bitarray_grid;

void bitarray_grid_reset(bitarray_grid *bg, grid g) {
    bg->width     = g.width;
    bg->height    = g.height;
    bg->set_count = 0;
    bitarray_reset(&bg->north_bm, g.width * g.height);
    bitarray_reset(&bg->south_bm, g.width * g.height);
    bitarray_reset(&bg->east_bm , g.width * g.height);
    bitarray_reset(&bg->west_bm , g.width * g.height);
}

void bitarray_grid_set(bitarray_grid *bg, position posn) {
    size_t index = posn.row*bg->width + posn.col;
    if (!bitarray_isset(bg->north_bm, index)   &&
        !bitarray_isset(bg->south_bm, index)   &&
        !bitarray_isset(bg->east_bm , index)   &&
        !bitarray_isset(bg->west_bm , index)   &&
        posn.row >= 0 && posn.row < bg->height &&
        posn.col >= 0 && posn.col < bg->width  ) {
        bg->set_count++;
    }
    switch(posn.dir) {
    case NORTH:
        bitarray_set(&bg->north_bm, index);
        break;
    case EAST:
        bitarray_set(&bg->east_bm, index);
        break;
    case WEST:
        bitarray_set(&bg->west_bm, index);
        break;
    case SOUTH:
        bitarray_set(&bg->south_bm, index);
        break;
    default:
        break;
    }
}

bool bitarray_grid_isset(bitarray_grid bg, position posn) {
    size_t index = posn.row*bg.width + posn.col;
    switch(posn.dir) {
    case NORTH: return bitarray_isset(bg.north_bm, index);
    case EAST:  return bitarray_isset(bg.east_bm , index);
    case WEST:  return bitarray_isset(bg.west_bm , index);
    case SOUTH: return bitarray_isset(bg.south_bm, index);
    default:    return false;
    }
    return false;
}

void bitarray_grid_print(bitarray_grid bg) {
   printf("bitarray_grid<\n");
   printf("  north: "); bitarray_print(bg.north_bm); 
   printf("  south: "); bitarray_print(bg.south_bm); 
   printf("  east: ") ; bitarray_print(bg.east_bm); 
   printf("  west: ") ; bitarray_print(bg.west_bm); 
   printf(">\n");
}

void print_position(position posn) {
    switch(posn.dir) {
    case NORTH: printf("position<row: %d, col: %d, dir: NORTH>\n", posn.row, posn.col); break;
    case SOUTH: printf("position<row: %d, col: %d, dir: SOUTH>\n", posn.row, posn.col); break;
    case EAST : printf("position<row: %d, col: %d, dir: EAST>\n" , posn.row, posn.col); break;
    case WEST : printf("position<row: %d, col: %d, dir: WEST>\n" , posn.row, posn.col); break;
    default   : printf("position<row: %d, col: %d>\n"            , posn.row, posn.col); break;
    }
}

size_t solve_part_1(bitarray_grid *bg, grid g) {
    position posn = {0};
    if (!find_gaurd_initial_position(g, &posn)) {
        printf("Warning: Could not find initial position of the gaurd (^), returning with 0\n");
        return 0;
    }
    while (in_bounds(g, posn) && !bitarray_grid_isset(*bg, posn)) {
        // print_position(posn);
        bitarray_grid_set(bg, posn);
        // bitarray_grid_print(*bg);
        next_posn(g, &posn);
    }
    return bg->set_count;
}

bool has_loop(bitarray_grid *bg, grid g, position posn) {
    while (in_bounds(g, posn) && !bitarray_grid_isset(*bg, posn)) {
        // print_position(posn);
        bitarray_grid_set(bg, posn);
        // bitarray_grid_print(*bg);
        next_posn(g, &posn);
    }
    bool result = bitarray_grid_isset(*bg, posn);
    return result;
}

size_t solve_part_2(bitarray_grid *bg, grid *g) {
    position posn = {0};
    if (!find_gaurd_initial_position(*g, &posn)) {
        printf("Warning: Could not find initial position of the gaurd (^), returning with 0\n");
        return 0;
    }
    size_t result = 0;
    for (size_t row = 0; row < g->height; row++) {
        for (size_t col = 0; col < g->width; col++) {
            if (g->rows.data[row].data[col] == '.') {
                g->rows.data[row].data[col] = '#';
                bitarray_grid_reset(bg, *g);
                if (has_loop(bg, *g, posn)) {
                    result++;
                }
                g->rows.data[row].data[col] = '.';
            }
        }
    }
    return result;
}

int main(int argc, char **argv) {
    int result = 0;
    string file_content = {0}; da_init(&file_content);
    grid g = {0}; da_init(&g.rows);
    bitarray_grid bg = {0};
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
        if (!string_to_grid(file_content, &g)) {
            return_defer(1);
        }
        bitarray_grid_reset(&bg, g);
        printf("Part 1: %zu\n", solve_part_1(&bg, g));
        printf("Part 2: %zu\n", solve_part_2(&bg, &g));
        file_content.count = 0;
        g.rows.count = 0;
        g.width = 0;
        g.height = 0;
    }

defer:
    if (file_content.data) {
        free(file_content.data);
    }
    if (g.rows.data) {
        free(g.rows.data);
    }
    if (bg.north_bm.data) {
        free(bg.north_bm.data);
    }
    if (bg.south_bm.data) {
        free(bg.south_bm.data);
    }
    if (bg.east_bm.data) {
        free(bg.east_bm.data);
    }
    if (bg.west_bm.data) {
        free(bg.west_bm.data);
    }
    return result;
}
