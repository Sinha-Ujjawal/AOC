#include <stdio.h>
#include <sys/errno.h>
#include <stdbool.h>
#include <ctype.h>

#define SV_IMPLEMENTATION
#include "sv.h"

#define DA_IMPLEMENTATION
#include "da.h"

#define return_defer(res) do {\
    result = (res);\
    goto defer;\
} while(0);\

typedef struct {
    sv     encrypted_name;
    size_t sector_id;
    sv     checksum;
} room;

da_type(string, char);
da_type(rooms,  room);

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

bool parse_room(sv line, rooms *rooms) {
    size_t count_encrypted_name = 0;
    for (size_t i = 0; i < line.count; i++) {
        if (isalpha(line.data[i]) || line.data[i] == '-') continue;
        count_encrypted_name = i;
        break;
    }
    if (count_encrypted_name == 0) {
        printf("ERROR: coult not parse encrypted_name from the input\n");
        return false;
    }
    count_encrypted_name -= 1;
    if (line.data[count_encrypted_name] != '-') {
        printf("ERROR: expected '-', got '%c'\n", line.data[count_encrypted_name]);
        return false;
    }
    sv encrypted_name = sv_take_n(line, count_encrypted_name);
    line = sv_drop_n(line, count_encrypted_name + 1);
    errno = 0;
    char *endptr;
    int sector_id = (int) strtol(line.data, &endptr, 10);
    if (errno != 0) {
        printf("ERROR: could not parse sector_id from the input: %s\n", strerror(errno));
        return false;
    }
    if (sector_id < 0) {
        printf("ERROR: sector_id: %d count not be negative\n", sector_id);
        return false;
    }
    line = sv_drop_n(line, endptr - line.data);
    if (line.count != 7) {
        printf("ERROR: expected to end with [*****] with exactly 5 characters\n");
        return false;
    }
    if (line.count < 3) {
        printf("ERROR: no checksum found!\n");
        return false;
    }
    if (line.data[0] != '[') {
        printf("ERROR: expected '[', found '%c'\n", line.data[0]);
        return false;
    }
    for (size_t i = 1; i < line.count - 1; i++) {
        if (!isalpha(line.data[i])) {
            printf("ERROR: expected checksum to be alphabets\n");
            return false;
        }
    }
    if (line.data[line.count - 1] != ']') {
        printf("ERROR: expected ']', found '%c'\n", line.data[line.count - 1]);
        return false;
    }
    sv checksum = sv_substring(line, 1, line.count - 2);
    da_grow(rooms);
    da_last(rooms).encrypted_name = encrypted_name;
    da_last(rooms).sector_id      = sector_id;
    da_last(rooms).checksum       = checksum;
    return true;
}

bool parse_rooms(char *file, string file_content, rooms *rooms) {
    sv s = sv_from_ptr(file_content.data, file_content.count);
    size_t line_no = 1;
    while (s.count > 0) {
        sv_pair pair = sv_split_by_char(s, '\n');
        sv line = sv_trim_left(pair.fst);
        if (!parse_room(line, rooms)) {
            printf("ERROR: could not parse room from file: %s:%zu\n", file, line_no);
            return false;
        }
        s = pair.snd;
        line_no++;
    }
    return true;
}

#define heapify(arr, n, fn_compare, root) {\
    typeof((arr)) items = (arr);\
    size_t count = (n);\
    size_t i = (root);\
    while(1) {\
        size_t r     = i;\
        size_t left  = r << 1;\
        size_t right = left + 1;\
        if (left < count  && (fn_compare(items[left] , items[r]))) {\
            r = left;\
        }\
        if (right < count && (fn_compare(items[right], items[r]))) {\
            r = right;\
        }\
        if (i == r) {\
            break;\
        }\
        typeof(*items) temp = items[r];\
        items[r]            = items[i];\
        items[i]            = temp;\
        i = r;\
    }\
}\

#define build_heap(arr, n, fn_compare) {\
    size_t count = (n);\
    if (count > 0) {\
        int i = (count >> 1) - 1;\
        while (i >= 0) {\
            size_t root = i;\
            heapify(arr, n, fn_compare, root);\
            i--;\
        }\
    }\
}\

typedef struct {
    char   c;
    size_t count;
} char_count;

void max_heapify_char_counts(char_count arr[], size_t n) {
#define fn_compare(x, y) (((x).count > (y).count) || (((x).count == (y).count) && ((x).c <= (y).c)))
    heapify(arr, n, fn_compare, 0);
#undef fn_compare
}

void build_max_heap_char_counts(char_count arr[], size_t n) {
#define fn_compare(x, y) (((x).count > (y).count) || (((x).count == (y).count) && ((x).c <= (y).c)))
    build_heap(arr, n, fn_compare);
#undef fn_compare
}

bool is_valid_room_part_1(room r) {
    static char_count char_counts[26];
    memset(char_counts, 0, sizeof(char_count) * 26);
    for (size_t i = 0; i < r.encrypted_name.count; i++) {
        char c = r.encrypted_name.data[i];
        if (isalpha(c) && c >= 'a' && c <= 'z') {
            char_counts[c - 'a'].c = c;
            char_counts[c - 'a'].count++;
        }
    }
    build_max_heap_char_counts(char_counts, 26);
    for (size_t i = 0; i < 5; i++) {
        if (char_counts[0].c != r.checksum.data[i]) {
            return false;
        }
        char_counts[0] = char_counts[26 - i - 1];
        max_heapify_char_counts(char_counts, 26 - i - 1);
    }
    return true;
}

void solve_part_1(rooms rooms) {
    size_t ans = 0;
    for (size_t i = 0; i < rooms.count; i++) {
        room r = rooms.data[i];
        if (is_valid_room_part_1(r)) {
            ans += r.sector_id;
        }
    }
    printf("Sum of sector_ids for valid rooms: %zu\n", ans);
}

void solve_part_2(rooms rooms) {
#define decipher(c, n) isalpha(c) ? 'a' + ((c - 'a' + n) % 26) : ' '
    for (size_t i = 0; i < rooms.count; i++) {
        room r = rooms.data[i];
        sv en = r.encrypted_name;
        printf("Sector: %zu, Decode ("SV_FMT"): ", r.sector_id, SV_DATA(en));
        for (size_t j = 0; j < en.count; j++) {
            printf("%c", (char) (decipher(en.data[j], r.sector_id)));
        }
        printf("\n");
    }
#undef decipher
}

int main(int argc, char **argv) {
    int result = 0;
    string file_content = {0}; da_init(&file_content);
    rooms rooms = {0}; da_init(&rooms);
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
        if (parse_rooms(file, file_content, &rooms)) {
            printf("Solving part 1 for file: %s\n", file);
            solve_part_1(rooms);
            printf("Solving part 2 for file: %s\n", file);
            solve_part_2(rooms);
        } else {
            printf("ERROR: could not solve file: %s!\n", file);
        }
        file_content.count = 0;
        rooms.count = 0;
    }

defer:
    if (file_content.data) {
        free(file_content.data);
    }
    if (rooms.data) {
        free(rooms.data);
    }
    return result;
}

