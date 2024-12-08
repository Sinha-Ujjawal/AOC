#include <stdio.h>
#include <sys/errno.h>
#include <stdbool.h>

#define SV_IMPLEMENTATION
#include "sv.h"

#define DA_IMPLEMENTATION
#include "da.h"

#define return_defer(res) do {\
    result = res;\
    goto defer;\
} while(0);\

typedef struct {
    size_t page_before;
    size_t page_after;
} page_ordering_rule;

da_type(page_ordering_rules, page_ordering_rule);
da_type(size_t_arr, size_t);
da_type(string, char);

typedef struct {
    size_t_arr pages;
    size_t_arr page_counts;
} updates;

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

bool parse_ordering_rule(sv line, page_ordering_rules *rules) {
    sv s = line;
    char *endptr = NULL;
    int page_before = (int) strtol(s.data, &endptr, 10);
    if (errno != 0 || endptr == NULL) {
        printf("ERROR: could not parse page from the input: %s\n", strerror(errno));
        goto error;
    }
    if (page_before < 0) {
        printf("ERROR: page number cannot be negative. Found %d\n", page_before);
        goto error;
    }
    s.count -= endptr - s.data;
    s.data = endptr;
    if (s.count == 0) {
        goto error;
    }
    if (s.data[0] != '|') {
        printf("ERROR: expected '|', got '%c'\n", s.data[0]);
        goto error;
    }
    s.count -= 1;
    s.data++;
    if (s.count == 0) {
        goto error;
    }
    int page_after = (int) strtol(s.data, &endptr, 10);
    if (errno != 0 || endptr == NULL) {
        printf("ERROR: could not parse page from the input: %s\n", strerror(errno));
        goto error;
    }
    if (page_after < 0) {
        printf("ERROR: page number cannot be negative. Found %d\n", page_after);
        goto error;
    }
    s.count -= endptr - s.data;
    s.data = endptr;
    if (s.count != 0) {
        goto error;
    }
    da_grow(rules);
    rules->data[rules->count - 1].page_before = (size_t) page_before;
    rules->data[rules->count - 1].page_after  = (size_t) page_after;
    return true;

error:
    printf("ERROR: count not parse the page ordering from: "SV_FMT"\n", SV_DATA(line));
    return false;
}

bool parse_ordering_rules(sv s, page_ordering_rules *rules) {
    while (s.count > 0) {
        sv_pair pair = sv_split_by_char(s, '\n');
        if (!parse_ordering_rule(pair.fst, rules)) {
            return false;
        }
        s = pair.snd;
    }
    return true;
}

bool parse_update(sv line, size_t_arr *pages) {
    sv s = line;
    while (s.count > 0) {
        sv_pair pair = sv_split_by_char(s, ',');
        char *endptr = NULL;
        int page = (int) strtol(pair.fst.data, &endptr, 10);
        if (errno != 0 || endptr == NULL) {
            printf("ERROR: could not parse page from the input: %s\n", strerror(errno));
            goto error;
        }
        if (page < 0) {
            printf("ERROR: page number cannot be negative. Found %d\n", page);
            goto error;
        }
        pair.fst.count -= endptr - pair.fst.data;
        pair.fst.data = endptr;
        if (pair.fst.count != 0) {
            goto error;
        }
        da_push(pages, page);
        s = pair.snd;
    }
    return true;

error:
    printf("ERROR: count not parse the page updates from: "SV_FMT"\n", SV_DATA(line));
    return false;
}

bool parse_updates(sv s, updates *updts) {
    while (s.count > 0) {
        sv_pair pair = sv_split_by_char(s, '\n');
        size_t count_before = updts->pages.count;
        if (!parse_update(pair.fst, &updts->pages)) {
            return false;
        }
        size_t count_after = updts->pages.count;
        if (count_after > count_before) {
            da_push(&updts->page_counts, count_after - count_before);
        }
        s = pair.snd;
    }
    return true;
}

bool parse_input(string input, page_ordering_rules *rules, updates *updts) {
    sv s = sv_from_ptr(input.data, input.count);
    sv_pair pair = sv_split_by_cstr(s, "\n\n");
    if (!parse_ordering_rules(pair.fst, rules)) {
        return false;
    }
    if (!parse_updates(pair.snd, updts)) {
        return false;
    }
    return true;
}

void print_page_ordering_rules(page_ordering_rules rules) {
    for (size_t i = 0; i < rules.count; i++) {
        page_ordering_rule rule = rules.data[i];
        printf("%zu|%zu\n", rule.page_before, rule.page_after);
    }
}

void print_updates(updates updts) {
    size_t curr = 0;
    for (size_t i = 0; i < updts.page_counts.count; i++) {
        size_t count = updts.page_counts.data[i];
        while (count > 0) {
            printf("%zu", updts.pages.data[curr]);
            count--;
            curr++;
            if (count > 0) {
                printf(",");
            } else {
                printf("\n");
            }
        }
    }
}

bool is_rule_sat_for_pages(page_ordering_rule rule, size_t *page_arr, size_t page_count) {
    bool found_after_page = false;
    for (size_t i = 0; i < page_count; i++) {
        if (page_arr[i] == rule.page_before && found_after_page) {
            return false;
        }
        if (page_arr[i] == rule.page_after && !found_after_page) {
            found_after_page = true;
        }
    }
    return true;
}

bool are_rules_sat_for_pages(page_ordering_rules rules, size_t *page_arr, size_t page_count) {
    for (size_t i = 0; i < rules.count; i++) {
        if (!is_rule_sat_for_pages(rules.data[i], page_arr, page_count)) {
            return false;
        }
    }
    return true;
}

bool fix_pages_using_rule(page_ordering_rule rule, size_t *page_arr, size_t page_count) {
    if (page_count == 0) {
        return false;
    }
    bool is_fixed = false;
    size_t i = 0;
    size_t j = page_count - 1;
    while (i < j) {
        while (i < page_count && page_arr[i] != rule.page_after) {
            i++;
        }
        while (j > i && page_arr[j] != rule.page_before) {
            j--;
        }
        if (i < j) {
            is_fixed = true;
            size_t temp = page_arr[i];
            page_arr[i] = page_arr[j];
            page_arr[j] = temp;
        }
    }
    return is_fixed;
}

bool fix_pages_using_rules(page_ordering_rules rules, size_t *page_arr, size_t page_count) {
    bool is_fixed = false;
    while (!are_rules_sat_for_pages(rules, page_arr, page_count)) {
        for (size_t i = 0; i < rules.count; i++) {
            if (fix_pages_using_rule(rules.data[i], page_arr, page_count)) {
                is_fixed = true;
            }
        }
    }
    return is_fixed;
}

size_t solve_part_1(page_ordering_rules rules, updates updts) {
    size_t ans = 0;
    size_t *page_arr = updts.pages.data;
    for (size_t i = 0; i < updts.pages.count; i++) {
        size_t page_count = updts.page_counts.data[i];
        if (are_rules_sat_for_pages(rules, page_arr, page_count)) {
            ans += page_arr[page_count >> 1];
        }
        page_arr += page_count;
    }
    return ans;
}

size_t solve_part_2(page_ordering_rules rules, updates updts) {
    size_t ans = 0;
    size_t_arr arr = {0}; da_init(&arr);
    size_t *page_arr = updts.pages.data;
    for (size_t i = 0; i < updts.pages.count; i++) {
        size_t page_count = updts.page_counts.data[i];
        for (size_t j = 0; j < page_count; j++) {
            da_push(&arr, page_arr[j]);
        }
        if (fix_pages_using_rules(rules, arr.data, arr.count)) {
            ans += arr.data[page_count >> 1];
        }
        page_arr += page_count;
        arr.count = 0;
    }

    if (arr.data) {
        free(arr.data);
    }
    return ans;
}

int main(int argc, char **argv) {
    int result = 0;
    string file_content = {0}; da_init(&file_content);
    char *program = *argv++; argc--;
    page_ordering_rules rules = {0}; da_init(&rules);
    updates updts = {0}; da_init(&updts.pages); da_init(&updts.page_counts);
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
        if (!parse_input(file_content, &rules, &updts)) {
            return_defer(1);
        }
        // print_page_ordering_rules(rules);
        // printf("\n");
        // print_updates(updts);
        printf("Part 1: %zu\n", solve_part_1(rules, updts));
        printf("Part 2: %zu\n", solve_part_2(rules, updts));
        file_content.count = 0;
        rules.count = 0;
        updts.pages.count = 0;
        updts.page_counts.count = 0;
    }

defer:
    if (file_content.data) {
        free(file_content.data);
    }
    if (rules.data) {
        free(rules.data);
    }
    if (updts.pages.data) {
        free(updts.pages.data);
    }
    if (updts.page_counts.data) {
        free(updts.page_counts.data);
    }
    return result;
}
