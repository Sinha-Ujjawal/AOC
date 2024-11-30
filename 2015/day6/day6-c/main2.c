#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SV_IMPLEMENTATION
#include "sv.h"

#define eprintf(ErrorMsg, File, LineNo) fprintf(stderr, "Error(%s:%d): %s\n", File, LineNo, ErrorMsg)
#define report_error(S) eprintf(S, __FILE__, __LINE__)

#define READ_FILE_BUFFER_LEN 255

#define GRID_WIDTH 1000
#define GRID_SIZE GRID_WIDTH * GRID_WIDTH

unsigned int GRID_PART_1[GRID_SIZE] = {0};
unsigned int GRID_PART_2[GRID_SIZE] = {0};

typedef char *string;
typedef unsigned int usize;

typedef enum
{
    turn_on = 1,
    turn_off,
    toggle
} instruction_type;

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
    char *endptr = NULL;
    usize x = strtoull(x_str.data, &endptr, 10);
    if (endptr == x_str.data)
    {
        return false;
    }
    endptr = NULL;
    usize y = strtoull(y_str.data, &endptr, 10);
    if (endptr == y_str.data)
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

int main(int argc, string argv[])
{
    int result = 0;
    FILE *fp = NULL;

    if (argc < 2)
    {
        report_error("No args provided!");
        printf("Usage:\n");
        printf("  ./main2.exe {FILEPATH}\n");
        exit(1);
        result = 1;
        goto defer;
    }

    string filepath = argv[1];
    if (!(fp = fopen(filepath, "r")))
    {
        report_error("Error occured while reading file!");
        perror(filepath);
        exit(1);
        result = 1;
        goto defer;
    }

    char buffer[READ_FILE_BUFFER_LEN];
    usize line_no = 1;
    usize ans_part_1 = 0;
    usize ans_part_2 = 0;
    while (fgets(buffer, READ_FILE_BUFFER_LEN, fp))
    {
        buffer[strcspn(buffer, "\r\n")] = 0;
        String_View line_view = sv_from_cstr(buffer);
        instruction instruction;
        if (parse_instruction(&line_view, &instruction))
        {
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

                        // Part 1
                        if (!GRID_PART_1[idx])
                        {
                            ans_part_1++;
                            GRID_PART_1[idx] = 1;
                        }

                        // Part 2
                        ans_part_2++;
                        GRID_PART_2[idx]++;
                    }
                }
                break;

            case turn_off:
                for (usize x = lowx; x <= highx; x++) {
                    for (usize y = lowy; y <= highy; y++) {
                        usize idx = x * GRID_WIDTH + y;

                        // Part 1
                        if (GRID_PART_1[idx])
                        {
                            ans_part_1--;
                            GRID_PART_1[idx] = 0;
                        }

                        // Part 2
                        if (GRID_PART_2[idx])
                        {
                            ans_part_2--;
                            GRID_PART_2[idx]--;
                        }
                    }
                }
                break;

            case toggle:
                for (usize x = lowx; x <= highx; x++) {
                    for (usize y = lowy; y <= highy; y++) {
                        usize idx = x * GRID_WIDTH + y;

                        // Part 1
                        GRID_PART_1[idx] ^= 1;
                        if (GRID_PART_1[idx])
                        {
                            ans_part_1++;
                        }
                        else {
                            ans_part_1--;
                        }

                        // Part 2
                        ans_part_2 += 2;
                        GRID_PART_2[idx] += 2;
                    }
                }
                break;

            default:
                break;
            }
        }
        else
        {
            report_error("Error occured while parsing file as instructions!");
            eprintf("Could not parse!", filepath, line_no);
            result = 1;
            goto defer;
        }
        line_no++;
    };

    printf("Part 1: %d\n", ans_part_1);
    printf("Part 1: %d\n", ans_part_2);

defer:
    if (fp != NULL) {
        fclose(fp);
    }
    return result;
}
