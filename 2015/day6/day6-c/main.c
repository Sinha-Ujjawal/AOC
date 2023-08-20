#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define DA_IMPLEMENTATION
#include "da.h"

#define SV_IMPLEMENTATION
#include "sv.h"

#define eprintf(ErrorMsg, File, LineNo) fprintf(stderr, "Error(%s:%d): %s\n", File, LineNo, ErrorMsg)
#define report_error(S) eprintf(S, __FILE__, __LINE__)

#define READ_FILE_BUFFER_LEN 255

#define GRID_WIDTH 1000
#define GRID_SIZE GRID_WIDTH * GRID_WIDTH
unsigned int GRID[GRID_SIZE];

typedef char *string;
typedef unsigned int usize;

typedef usize instruction_type;
enum instruction_type
{
    turn_on = 1,
    turn_off,
    toggle
};


typedef struct coord
{
    usize x;
    usize y;
} coord;

typedef struct box
{
    coord top_left;
    coord bottom_right;
} box;

typedef struct instruction
{
    instruction_type instruction_type;
    box box;
} instruction;

typedef struct parse_error
{
    usize line_no;
} parse_error;

da_typedef(int, da_int);
da_typedef(string, da_string);
da_typedef(instruction, da_instruction);

int read_lines(string filepath, da_string *lines)
{
    FILE *fp = NULL;
    if (!(fp = fopen(filepath, "r")))
    {
        return -1;
    }
    char buffer[READ_FILE_BUFFER_LEN];
    string line = NULL;
    while (fgets(buffer, READ_FILE_BUFFER_LEN, fp))
    {
        line = malloc(sizeof(char) * READ_FILE_BUFFER_LEN);
        strcpy(line, buffer);
        line[strcspn(line, "\r\n")] = 0;
        da_push((*lines), line);
    };
    return 0;
}

bool parse_instruction_type(String_View *instruction_type_as_string_view, instruction *instruction)
{
    if (sv_starts_with(*instruction_type_as_string_view, SV("turn on ")))
    {
        sv_chop_by_sv(instruction_type_as_string_view, SV("turn on "));
        instruction->instruction_type = turn_on;
        return true;
    }
    else if (sv_starts_with(*instruction_type_as_string_view, SV("turn off ")))
    {
        sv_chop_by_sv(instruction_type_as_string_view, SV("turn off "));
        instruction->instruction_type = turn_off;
        return true;
    }
    else if (sv_starts_with(*instruction_type_as_string_view, SV("toggle ")))
    {
        sv_chop_by_sv(instruction_type_as_string_view, SV("toggle "));
        instruction->instruction_type = toggle;
        return true;
    }
    return false;
}

bool parse_coord(String_View *coord_as_string_view, coord *ret)
{
    String_View x_str = sv_chop_by_delim(coord_as_string_view, ',');
    String_View y_str = *coord_as_string_view;
    usize x = strtoumax(x_str.data, NULL, 10);
    if (x == UINTMAX_MAX && errno == ERANGE)
    {
        return false;
    }
    usize y = strtoumax(y_str.data, NULL, 10);
    if (y == UINTMAX_MAX && errno == ERANGE)
    {
        return false;
    }
    ret->x = x;
    ret->y = y;
    return true;
}

bool parse_box(String_View *box_as_string_view, instruction *instruction)
{
    String_View top_left_string_view = sv_chop_by_sv(box_as_string_view, SV(" through "));
    String_View bottom_right_string_view = *box_as_string_view;
    box *box = &instruction->box;
    if (!parse_coord(&top_left_string_view, &box->top_left))
    {
        return false;
    }
    if (!parse_coord(&bottom_right_string_view, &box->bottom_right))
    {
        return false;
    }
    return true;
}

bool parse_instruction(String_View *line_view, instruction *instruction)
{
    if (!parse_instruction_type(line_view, instruction))
    {
        return false;
    }
    if (!parse_box(line_view, instruction))
    {
        return false;
    }
    return true;
}

bool parse_instructions(da_string lines, da_instruction *instructions, parse_error **perr)
{
    for (usize i = 0; i < lines.length; i++)
    {
        usize line_no = i + 1;
        String_View line_view = sv_from_cstr(lines.data[i]);
        instruction instruction;
        if (parse_instruction(&line_view, &instruction))
        {
            da_push((*instructions), instruction);
        }
        else
        {
            *perr = malloc(sizeof(parse_error));
            (*perr)->line_no = line_no;
            return false;
        }
    }
    return true;
}

usize solve_part_1(da_instruction *instructions) {
    for (usize i = 0; i < GRID_SIZE; i++) {
        GRID[i] = 0;
    }
    for (usize i = 0; i < instructions->length; i++) {
        instruction instruction = instructions->data[i];
        instruction_type instruction_type = instruction.instruction_type;
        box box = instruction.box;
        usize lowx = box.top_left.x;
        usize lowy = box.top_left.y;
        usize highx = box.bottom_right.x;
        usize highy = box.bottom_right.y;
        switch (instruction_type)
        {
        case turn_on:
            for (usize x = lowx; x <= highx; x++) {
                for (usize y = lowy; y <= highy; y++) {
                    usize idx = x * GRID_WIDTH + y;
                    GRID[idx] = 1;
                }
            }
            break;

        case turn_off:
            for (usize x = lowx; x <= highx; x++) {
                for (usize y = lowy; y <= highy; y++) {
                    usize idx = x * GRID_WIDTH + y;
                    GRID[idx] = 0;
                }
            }
            break;

        case toggle:
            for (usize x = lowx; x <= highx; x++) {
                for (usize y = lowy; y <= highy; y++) {
                    usize idx = x * GRID_WIDTH + y;
                    GRID[idx] ^= 1;
                }
            }
            break;
        
        default:
            break;
        }
    }
    usize ret = 0;
    for (usize i = 0; i < GRID_SIZE; i++) {
        ret += GRID[i];
    }
    return ret;
}

usize solve_part_2(da_instruction *instructions) {
    for (usize i = 0; i < GRID_SIZE; i++) {
        GRID[i] = 0;
    }
    for (usize i = 0; i < instructions->length; i++) {
        instruction instruction = instructions->data[i];
        instruction_type instruction_type = instruction.instruction_type;
        box box = instruction.box;
        usize lowx = box.top_left.x;
        usize lowy = box.top_left.y;
        usize highx = box.bottom_right.x;
        usize highy = box.bottom_right.y;
        switch (instruction_type)
        {
        case turn_on:
            for (usize x = lowx; x <= highx; x++) {
                for (usize y = lowy; y <= highy; y++) {
                    usize idx = x * GRID_WIDTH + y;
                    GRID[idx]++;
                }
            }
            break;

        case turn_off:
            for (usize x = lowx; x <= highx; x++) {
                for (usize y = lowy; y <= highy; y++) {
                    usize idx = x * GRID_WIDTH + y;
                    if (GRID[idx])
                        GRID[idx]--;
                }
            }
            break;

        case toggle:
            for (usize x = lowx; x <= highx; x++) {
                for (usize y = lowy; y <= highy; y++) {
                    usize idx = x * GRID_WIDTH + y;
                    GRID[idx] += 2;
                }
            }
            break;
        
        default:
            break;
        }
    }
    usize ret = 0;
    for (usize i = 0; i < GRID_SIZE; i++) {
        ret += GRID[i];
    }
    return ret;
}

int main(int argc, string argv[])
{
    int result = 0;
    string filepath = NULL;
    da_string lines;
    da_instruction instructions;
    parse_error *parse_error = NULL;

    if (argc < 2)
    {
        report_error("No args provided!");
        printf("Usage:\n");
        printf("  ./main.exe {FILEPATH}\n");
        exit(1);
        result = 1;
        goto defer;
    }
    filepath = argv[1];

    da_init(lines);

    if (read_lines(filepath, &lines))
    {
        report_error("Error occured while reading file!");
        perror(filepath);
        exit(1);
        result = 1;
        goto defer;
    }

    da_init_2(instructions, lines.length);

    if (!parse_instructions(lines, &instructions, &parse_error))
    {
        report_error("Error occured while parsing file as instructions!");
        if (parse_error != NULL)
        {
            eprintf("Could not parse!", filepath, parse_error->line_no);
        }
        result = 1;
        goto defer;
    }

    da_free(lines, );

    printf("Part 1: %d\n", solve_part_1(&instructions));
    printf("Part 1: %d\n", solve_part_2(&instructions));

defer:
    da_free(lines, );
    da_free(instructions, &);
    return result;
}
