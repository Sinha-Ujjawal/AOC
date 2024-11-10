#include <stdio.h>
#include <sys/errno.h>
#include <stdlib.h>

#define SV_IMPLEMENTATION
#include "sv.h"

#define DA_IMPLEMENTATION
#include "da.h"

#define MD5_IMPLEMENTATION
#include "md5.h"

#define return_defer(res) do {\
    result = res;\
    goto defer;\
} while(0);\

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

uint32_t compute_password_part_1(sv s) {
    static char salt_digits[100];
    static MD5Context ctx;
    uint8_t i = 0;
    uint32_t pass = 0;
    size_t salt = 0;
    while (i < 8 && salt < (size_t)(-1)) {
        size_t salt_digits_count = sprintf(salt_digits, "%zu", salt);
        md5Init(&ctx);
        md5Update(&ctx, (uint8_t *)s.data, s.count);
        md5Update(&ctx, (uint8_t *)salt_digits, salt_digits_count);
        md5Finalize(&ctx);
         if (ctx.digest[0] == 0 && ctx.digest[1] == 0 && (ctx.digest[2] >> 4) == 0) {
            printf("md5_salt("SV_FMT""SV_FMT"):\t", (int) s.count, s.data, (int) salt_digits_count, salt_digits);
            for (size_t j = 0; j < 16; j++) {
                if (j > 0) printf(" ");
                printf("%x", ctx.digest[j]);
            }
            printf("\n");
            pass <<= 4;
            pass |= ctx.digest[2] & 15;
            i++;
        }
        salt++;
    }
    return pass;
}

void solve_part_1(string file_content) {
    sv s = sv_from_ptr(file_content.data, file_content.count);
    while (s.count > 0) {
        sv_pair pair = sv_split_by_char(s, '\n');
        sv line = pair.fst;
        uint32_t password = compute_password_part_1(line);
        printf("password("SV_FMT"):\t%x\n", SV_DATA(line), password);
        s = pair.snd;
    }
}

uint32_t compute_password_part_2(sv s) {
    static char salt_digits[100];
    static MD5Context ctx;
    uint8_t i = 0;
    uint32_t pass = 0;
    uint64_t mask = 0;
    size_t salt = 0;
    while (i < 8 && salt < (size_t)(-1)) {
        size_t salt_digits_count = sprintf(salt_digits, "%zu", salt);
        md5Init(&ctx);
        md5Update(&ctx, (uint8_t *)s.data, s.count);
        md5Update(&ctx, (uint8_t *)salt_digits, salt_digits_count);
        md5Finalize(&ctx);
         if (ctx.digest[0] == 0 && ctx.digest[1] == 0 && (ctx.digest[2] >> 4) == 0) {
            char pos = ctx.digest[2] & 15;
            char value = ctx.digest[3] >> 4;
            if (pos < 8 && ((mask >> pos) & 1) == 0) {
                printf("md5_salt("SV_FMT""SV_FMT"):\t", (int) s.count, s.data, (int) salt_digits_count, salt_digits);
                for (size_t j = 0; j < 16; j++) {
                    if (j > 0) printf(" ");
                    printf("%x", ctx.digest[j]);
                }
                printf("\n");
                pass |= value << ((7 - pos) << 2);
                mask |= 1 << pos;
                i++;
            }
        }
        salt++;
    }
    return pass;
}

void solve_part_2(string file_content) {
    sv s = sv_from_ptr(file_content.data, file_content.count);
    while (s.count > 0) {
        sv_pair pair = sv_split_by_char(s, '\n');
        sv line = pair.fst;
        uint32_t password = compute_password_part_2(line);
        printf("password("SV_FMT"):\t%x\n", SV_DATA(line), password);
        s = pair.snd;
    }
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
    if (file_content.data) {
        free(file_content.data);
    }
    return result;
}
