#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SV_IMPLEMENTATION
#include "sv.h"

#define DA_IMPLEMENTATION
#include "da.h"

#define eprintf(ErrorMsg, File, LineNo) fprintf(stderr, "Error(%s:%d): %s\n", File, LineNo, ErrorMsg)
#define report_error(S) eprintf(S, __FILE__, __LINE__)

typedef char* string;
typedef unsigned int usize;

#define BUFFER_LEN 512

typedef enum
{
    Override = 1,
    Copy,
    And,
    Or,
    LShift,
    RShift,
    Not
} gate;

typedef struct wire
{
    uint16_t value;
    String_View name;
} wire;

typedef struct instruction
{
    gate gate;
    wire in1;
    wire in2;
    wire out;
    uint16_t cached_out;
    bool is_cached;
} instruction;

da_typedef(instruction, instructions)
da_typedef(string, strings)

bool parse_inputs(String_View *sv, instruction *ins)
{
    if (sv_starts_with(*sv, SV("NOT ")))
    {
        ins->gate = Not;
        sv_chop_by_sv(sv, SV("NOT "));
        sv_copy(*sv, ins->in1.name);
        return true;
    }

    String_View left = sv_chop_by_delim(sv, ' ');
    sv_copy(left, ins->in1.name);
    char *endptr = NULL;

    if (sv->count == 0)
    {
        uint16_t value = strtoull(left.data, &endptr, 10);
        if (endptr == left.data)
        {
            ins->gate = Copy;
        }
        else
        {
            ins->gate = Override;
            ins->in1.value = value;
        }
        return true;
    }
    else if (sv_starts_with(*sv, SV("AND ")))
    {
        ins->gate = And;
        sv_chop_by_sv(sv, SV("AND "));
        sv_copy(*sv, ins->in2.name);
        return true;
    }
    else if (sv_starts_with(*sv, SV("OR ")))
    {
        ins->gate = Or;
        sv_chop_by_sv(sv, SV("OR "));
        sv_copy(*sv, ins->in2.name);
        return true;
    }
    else if (sv_starts_with(*sv, SV("LSHIFT ")))
    {
        ins->gate = LShift;
        sv_chop_by_sv(sv, SV("LSHIFT "));
        uint16_t by = strtoull(sv->data, &endptr, 10);
        if (endptr == sv->data)
        {
            return false;
        }
        ins->in2.value = by;
        return true;
    }
    else if (sv_starts_with(*sv, SV("RSHIFT ")))
    {
        ins->gate = RShift;
        sv_chop_by_sv(sv, SV("RSHIFT "));
        uint16_t by = strtoull(sv->data, &endptr, 10);
        if (endptr == sv->data)
        {
            return false;
        }
        ins->in2.value = by;
        return true;
    }

    return false;
}

bool parse_instruction(String_View *sv, instruction *ins)
{
    if ((sv == NULL) || (ins == NULL))
    {
        return false;
    }
    String_View left = sv_chop_by_sv(sv, SV(" -> "));
    String_View right = *sv;
    if (!parse_inputs(&left, ins))
    {
        return false;
    }
    sv_copy(right, ins->out.name);
    ins->is_cached = false;
    return true;
}

void print_instruction(instruction *ins_ptr)
{
    instruction ins = *ins_ptr;
    switch (ins.gate)
    {
    case Override:
        printf("Override: %u -> "SV_Fmt"\n", ins.in1.value, SV_Arg(ins.out.name));
        break;

    case Copy:
        printf("Copy: "SV_Fmt" -> "SV_Fmt"\n", SV_Arg(ins.in1.name), SV_Arg(ins.out.name));
        break;

    case And:
        printf("And: "SV_Fmt" AND "SV_Fmt" -> "SV_Fmt"\n", SV_Arg(ins.in1.name), SV_Arg(ins.in2.name), SV_Arg(ins.out.name));
        break;

    case Or:
        printf("Or: "SV_Fmt" OR "SV_Fmt" -> "SV_Fmt"\n", SV_Arg(ins.in1.name), SV_Arg(ins.in2.name), SV_Arg(ins.out.name));
        break;

    case LShift:
        printf("LShift: "SV_Fmt" LSHIFT %u -> "SV_Fmt"\n", SV_Arg(ins.in1.name), ins.in2.value, SV_Arg(ins.out.name));
        break;

    case RShift:
        printf("RShift: "SV_Fmt" RSHIFT %u -> "SV_Fmt"\n", SV_Arg(ins.in1.name), ins.in2.value, SV_Arg(ins.out.name));
        break;

    case Not:
        printf("Not: NOT "SV_Fmt" -> "SV_Fmt"\n", SV_Arg(ins.in1.name), SV_Arg(ins.out.name));
        break;
    
    default:
        break;
    }
}

uint16_t solve(instructions *instructions, String_View wire_name)
{
    instruction *ins_for_wire = NULL;
    for (usize i = 0; i < instructions->length; i++)
    {
        instruction *ins = &instructions->data[i];
        if (sv_eq(ins->out.name, wire_name))
        {
            ins_for_wire = ins;
            break;
        }
    }
    if (ins_for_wire == NULL)
    {
        return strtoull(wire_name.data, NULL, 10);
    }
    if (ins_for_wire->is_cached)
    {
        return ins_for_wire->cached_out;
    }
    uint16_t in1_value, in2_value;
    switch (ins_for_wire->gate)
    {
    case Override:
        ins_for_wire->cached_out = ins_for_wire->in1.value;
        break;

    case Copy:
        ins_for_wire->cached_out = solve(instructions, ins_for_wire->in1.name);
        break;

    case And:
        in1_value = solve(instructions, ins_for_wire->in1.name);
        in2_value = solve(instructions, ins_for_wire->in2.name);
        ins_for_wire->cached_out = in1_value & in2_value;
        break;

    case Or:
        in1_value = solve(instructions, ins_for_wire->in1.name);
        in2_value = solve(instructions, ins_for_wire->in2.name);
        ins_for_wire->cached_out = in1_value | in2_value;
        break;

    case LShift:
        in1_value = solve(instructions, ins_for_wire->in1.name);
        in2_value = ins_for_wire->in2.value;
        ins_for_wire->cached_out = in1_value << in2_value;
        break;

    case RShift:
        in1_value = solve(instructions, ins_for_wire->in1.name);
        in2_value = ins_for_wire->in2.value;
        ins_for_wire->cached_out = in1_value >> in2_value;
        break;

    case Not:
        ins_for_wire->cached_out = ~solve(instructions, ins_for_wire->in1.name);
        break;
    
    default:
        ins_for_wire->cached_out = 0;
        break;
    }

    ins_for_wire->is_cached = true;
    return ins_for_wire->cached_out;
}

int main(int argc, char **argv)
{
    int result = 0;
    FILE *fp = NULL;
    instructions instructions;
    da_init(instructions);
    strings strings;
    da_init(strings);

    if (argc <= 3)
    {
        report_error("Not enough command line arguments passed");
        printf("Usage:\n");
        printf("  ./main.exe {FILEPATH} {WIRE_PART_1} {WIRE_PART_2}\n");
        result = 1;
        goto defer;
    }

    string filepath = argv[1];
    if ((fp = fopen(filepath, "r")) == NULL)
    {
        report_error("Error occured while reading file!");
        perror(filepath);
        result = 1;
        exit(1);
        goto defer;
    }

    char buffer[BUFFER_LEN];
    char *copy = NULL;
    usize line_no = 1;
    while (fgets(buffer, BUFFER_LEN, fp))
    {
        buffer[strcspn(buffer, "\r\n")] = 0;
        copy = malloc(BUFFER_LEN * sizeof(char));
        strcpy(copy, buffer);
        da_push(strings, copy);
        String_View sv = sv_from_cstr(copy);
        instruction ins;
        if (parse_instruction(&sv, &ins))
        {
            da_push(instructions, ins);
        }
        else
        {
            report_error("Error occured while parsing file as instructions!");
            eprintf("Could not parse!", filepath, line_no);
            result = 1;
            goto defer;
        }
        line_no++;
    }

    String_View wire_a = sv_from_cstr(argv[2]);
    uint16_t part_1_ans = solve(&instructions, wire_a);
    printf("Part 1: %u\n", part_1_ans);
    
    String_View wire_b = sv_from_cstr(argv[3]);
    instruction override_ins = {
        .gate = Override,
        .in1.value = part_1_ans,
        .out.name = wire_b
    };
    da_push(instructions, override_ins);
    #pragma GCC diagnostic ignored "-Wtype-limits"
    da_swap(instructions, 0, instructions.length - 1);
    for (usize i = 0; i < instructions.length; i++)
    {
        instructions.data[i].is_cached = false;
    }
    uint16_t part_2_ans = solve(&instructions, wire_a);
    printf("Part 2: %u\n", part_2_ans);

defer:
    if (fp != NULL)
    {
        fclose(fp);
    }
    da_free(instructions, &);
    da_free(strings, );
    return result;
}
