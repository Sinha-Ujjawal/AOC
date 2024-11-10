#include <stdio.h>
#include <string.h>
#include <sys/errno.h>
#include <math.h>

#define DA_IMPLEMENTATION
#include "da.h"

#define SV_IMPLEMENTATION
#include "sv.h"

#define return_defer(res) do { \
    result = res; \
    goto defer; \
} while(0); \

typedef enum { left, right } rel_direction;

typedef struct {
    rel_direction direction;
    size_t        blocks;
    size_t        line_no;
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

int parse_instruction(sv s, instructions *instructions, size_t line_id) {
    if (s.count < 2) {
        printf("ERROR: Could not parse "SV_FMT": length should be atleast 2\n", SV_DATA(s));
        return 1;
    }
    switch(s.data[0]) {
    case 'L': {
        da_grow(instructions);
        da_last(instructions).direction = left;
        da_last(instructions).blocks    = atoi(sv_drop_n(s, 1).data);
        da_last(instructions).line_no   = line_id + 1;
        break;
    }
    case 'R': {
        da_grow(instructions);
        da_last(instructions).direction = right;
        da_last(instructions).blocks    = atoi(sv_drop_n(s, 1).data);
        da_last(instructions).line_no   = line_id + 1;
        break;
    }
    default:
        printf("ERROR: Could not parse "SV_FMT": unknown first character\n", SV_DATA(s));
        return 1;
    }
    return 0;
}

int parse_instructions_from_input(sv s, instructions *instructions, size_t line_id) {
    sv_pair pair;
    while (s.count != 0) {
        pair = sv_split_by_char(s, ',');
        if (parse_instruction(pair.fst, instructions, line_id) != 0) {
            return 1;
        }
        s = sv_trim_left(pair.snd);
    }
    return 0;
}

int parse_instructions_from_inputs(char *file, string string, instructions *instructions) {
    sv s = sv_from_ptr(string.data, string.count);
    sv_pair pair;
    size_t line_id = 0;
    while (s.count != 0) {
        pair = sv_split_by_char(s, '\n');
        if (parse_instructions_from_input(pair.fst, instructions, line_id++) != 0) {
            printf("ERROR: could not parse file %s:%zu\n", file, line_id);
            return 1;
        }
        s = pair.snd;
    }
    return 0;
}

typedef enum { north, south, east, west } fixed_direction;

typedef struct {
    fixed_direction facing;
    int             x;
    int             y;
} position;

da_type(positions, position);

void update_facing(position *pos, rel_direction direction) {
    if      (direction == right && pos->facing == north) pos->facing = east;
    else if (direction == right && pos->facing == east ) pos->facing = south;
    else if (direction == right && pos->facing == south) pos->facing = west;
    else if (direction == right && pos->facing == west ) pos->facing = north;
    else if (direction == left  && pos->facing == north) pos->facing = west;
    else if (direction == left  && pos->facing == west ) pos->facing = south;
    else if (direction == left  && pos->facing == south) pos->facing = east;
    else if (direction == left  && pos->facing == east ) pos->facing = north;
    else printf("Unreachable!\n");
}

void walk(position *pos, size_t blocks) {
    switch(pos->facing) {
        case north:
            pos->y -= (int) blocks;
            break;
        case south:
            pos->y += (int) blocks;
            break;
        case east:
            pos->x += (int) blocks;
            break;
        case west:
            pos->x -= (int) blocks;
            break;
        default:
            printf("Unreachable\n");
    }
}

void update_position(position *pos, instruction ins) {
    update_facing(pos, ins.direction);
    walk(pos, ins.blocks);
}

void solve_part_1(char *file, instructions instructions) {
    printf("Solving part 1 for file: %s\n", file);
    position origin = {.facing=north, .x=0, .y=0};
    size_t prev_line_no = 1;
    position current = origin;
    for (size_t i = 0; i < instructions.count; i++) {
        instruction ins = instructions.data[i];
        if (ins.line_no > prev_line_no) {
            printf("Solution %s:%zu: final position: (%d, %d), %d blocks away from origin\n", file, prev_line_no, current.x, current.y, abs(current.x) + abs(current.y));
            prev_line_no = ins.line_no;
            current = origin;
        }
        update_position(&current, ins);
    }
    printf("Solution %s:%zu: final position: (%d, %d), %d blocks away from origin\n", file, prev_line_no, current.x, current.y, abs(current.x) + abs(current.y));
}

int intersecting_point(position p1, position p2, position p3, position p4, position *pos) {
    // Calculate the denominator
    int denominator = (p1.x - p2.x) * (p3.y - p4.y) - (p1.y - p2.y) * (p3.x - p4.x);

    // If the denominator is 0, the lines are parallel or coincident
    if (denominator == 0) {
        return -1;
    }

    int numerator_x = (p1.x * p2.y - p1.y * p2.x) * (p3.x - p4.x) - (p1.x - p2.x) * (p3.x * p4.y - p3.y * p4.x);
    int numerator_y = (p1.x * p2.y - p1.y * p2.x) * (p3.y - p4.y) - (p1.y - p2.y) * (p3.x * p4.y - p3.y * p4.x);

    // if denominator does not devide numerators evenly
    if ((numerator_x % denominator) != 0 || (numerator_y % denominator) != 0) {
        return -1;
    }

    // Calculate the intersection point
    int intersect_x = numerator_x / denominator;
    int intersect_y = numerator_y / denominator;

    // Check if the intersection point is within the bounds of both line segments
    if ((intersect_x >= (int) fmin((double) p1.x, (double) p2.x) && intersect_x <= (int) fmax((double) p1.x, (double) p2.x) &&
         intersect_y >= (int) fmin((double) p1.y, (double) p2.y) && intersect_y <= (int) fmax((double) p1.y, (double) p2.y)) &&
        (intersect_x >= (int) fmin((double) p3.x, (double) p4.x) && intersect_x <= (int) fmax((double) p3.x, (double) p4.x) &&
         intersect_y >= (int) fmin((double) p3.y, (double) p4.y) && intersect_y <= (int) fmax((double) p3.y, (double) p4.y))) {
        // Store the intersection point in pos
        pos->x = intersect_x;
        pos->y = intersect_y;
        return 0; // Intersection found
    }

    return -1;
}

int find_intersect(positions posns, position *pos) {
    if (posns.count < 3) {
        return -1;
    }
    position last_to_last_pos = posns.data[posns.count - 2];
    position last_pos         = posns.data[posns.count - 1];
    for (int i = 0; i < posns.count - 3; i++) {
        if (intersecting_point(posns.data[i], posns.data[i + 1], last_to_last_pos, last_pos, pos) == 0) {
            return 0;
        }
    }
    return -1;
}

void solve_part_2(char *file, instructions instructions) {
    printf("Solving part 2 for file: %s\n", file);
    position origin = {.facing=north, .x=0, .y=0};
    size_t prev_line_no = 1;
    position current = origin;
    positions posns = {0}; da_init(&posns);
    position intersect = {0};
    da_push(&posns, current);
    for (size_t i = 0; i < instructions.count; i++) {
        instruction ins = instructions.data[i];
        if (ins.line_no > prev_line_no) {
            printf("Solution %s:%zu: could not find any position that was visited twice\n", file, prev_line_no);
            posns.count = 0;
            prev_line_no = ins.line_no;
            current = origin;
            da_push(&posns, current);
        }
        update_position(&current, ins);
        da_push(&posns, current);
        if (find_intersect(posns, &intersect) == 0) {
            printf("Solution %s:%zu: position: (%d, %d) is the first position that has been visited twice, it is %d bocks away from origin\n", file, prev_line_no, intersect.x, intersect.y, abs(intersect.x) + abs(intersect.y));
            while (i < instructions.count && instructions.data[i].line_no == prev_line_no) {
                i++;
            }
            if (i == instructions.count) goto defer;
            posns.count = 0;
            ins = instructions.data[i];
            prev_line_no = ins.line_no;
            current = origin;
            da_push(&posns, current);
            update_position(&current, ins);
            da_push(&posns, current);
        }
    }
    printf("Solution %s:%zu: could not find any position that was visited twice\n", file, prev_line_no);

defer:
    free(posns.data);
}

int main(int argc, char **argv) {
    int result = 0;
    char *program = *argv++; argc--;
    string file_content = {0}; da_init(&file_content);
    instructions instructions = {0}; da_init(&instructions);
    if (argc == 0) {
        printf("ERROR: no arguments provided!\n");
        printf("Usage: %s file1 [file2 file3 ...]\n", program);
        return_defer(1);
    }
    while (argc > 0) {
        char *file = *argv++; argc--;
        printf("Reading content for file: %s\n", file);
        if (read_entire_file(file, &file_content) != 0) {
            return_defer(1);
        }
        printf("Parsing instructions...\n");
        if (parse_instructions_from_inputs(file, file_content, &instructions) != 0) {
            return_defer(1);
        }
        solve_part_1(file, instructions);
        solve_part_2(file, instructions);
        file_content.count = 0;
        instructions.count = 0;
    }

defer:
    free(file_content.data);
    free(instructions.data);
    return result;
}
