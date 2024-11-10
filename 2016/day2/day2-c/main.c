#include <stdio.h>
#include <sys/errno.h>
#include <string.h>

#define DA_IMPLEMENTATION
#include "da.h"

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

void decode_bathroom_code(int current_row, int current_col, string instructions, const char *keypad, size_t keypad_width, size_t keypad_height) {
    printf("Bathroom code: ");
    for (size_t i = 0; i < instructions.count; i++) {
        char c = instructions.data[i];
        if (c == '\n') {
            printf("%c", keypad[(current_row * keypad_width) + current_col]);
        }
        else if (c == 'U' && current_row > 0 && keypad[((current_row - 1) * keypad_width) + current_col] != ' ') {
            current_row--;
        }
        else if (c == 'D' && current_row < keypad_height - 1 && keypad[((current_row + 1) * keypad_width) + current_col] != ' ') {
            current_row++;
        }
        else if (c == 'L' && current_col > 0 && keypad[(current_row * keypad_width) + current_col - 1] != ' ') {
            current_col--;
        }
        else if (c == 'R' && current_col < keypad_width - 1 && keypad[(current_row * keypad_width) + current_col + 1] != ' ') {
            current_col++;
        }
    }
    printf("\n");
}

void solve_part_1(string instructions) {
    static const char keypad[3*3] = "1" "2" "3"
                                    "4" "5" "6"
                                    "7" "8" "9";
    decode_bathroom_code(1, 1, instructions, keypad, 3, 3);
}

void solve_part_2(string instructions) {
    static const char keypad[5*5] = " " " " "1" " " " "
                                    " " "2" "3" "4" " "
                                    "5" "6" "7" "8" "9"
                                    " " "A" "B" "C" " "
                                    " " " " "D" " " " ";
    decode_bathroom_code(2, 0, instructions, keypad, 5, 5);
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
        printf("Solving part 1 for file: %s\n", file);
        solve_part_1(file_content);
        printf("Solving part 2 for file: %s\n", file);
        solve_part_2(file_content);
        file_content.count = 0;
    }

defer:
    free(file_content.data);
    return result;
}
