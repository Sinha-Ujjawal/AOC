using Printf

function print_usage()
    println("Error: No command line arguments supplied!")
    println("Usage: julia.exe main.jl {FILEPATH}")
end

struct Coord
    x::Int32
    y::Int32
end

struct Box
    top_left::Coord
    bottom_right::Coord
end

@enum InstructionType TurnOn=1 TurnOff=2 Toggle=3

struct Instruction
    instruction_type::InstructionType
    box::Box
end

function parse_instruction(line::String) :: Union{Instruction, Nothing}
    matches = match(r"(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)", line)
    if isa(matches, Nothing)
        Nothing
    else
        captures = matches.captures
        instruction_type =
            if captures[1] == "turn on"
                TurnOn
            elseif captures[1] == "turn off"
                TurnOff
            else
                Toggle
            end
        box = Box(
            Coord(parse(Int32, captures[2]), parse(Int32, captures[3])),
            Coord(parse(Int32, captures[4]), parse(Int32, captures[5]))
        )
        Instruction(instruction_type, box)
    end
end

function solve_part_1(instructions::Array{Instruction}) :: Int32
    grid = zeros(Int32, 1000, 1000)
    @inline for instruction in instructions
        instruction_type = instruction.instruction_type
        box = instruction.box
        xl = box.top_left.x + 1
        xu = box.bottom_right.x + 1
        yl = box.top_left.y + 1
        yu = box.bottom_right.y + 1
        if instruction_type == TurnOn
            grid[xl:xu, yl:yu] .= 1
        elseif instruction_type == TurnOff
            grid[xl:xu, yl:yu] .= 0
        else
            grid[xl:xu, yl:yu] .⊻= 1
        end
    end
    sum(sum(grid, dims=1))
end

function solve_part_2(instructions::Array{Instruction}) :: Int32
    grid = zeros(Int32, 1000, 1000)
    @inline for instruction in instructions
        instruction_type = instruction.instruction_type
        box = instruction.box
        xl = box.top_left.x + 1
        xu = box.bottom_right.x + 1
        yl = box.top_left.y + 1
        yu = box.bottom_right.y + 1
        if instruction_type == TurnOn
            grid[xl:xu, yl:yu] .+= 1
        elseif instruction_type == TurnOff
            grid[xl:xu, yl:yu] = max.(grid[xl:xu, yl:yu] .- 1, 0)
        else
            grid[xl:xu, yl:yu] .+= 2
        end
    end
    sum(sum(grid, dims=1))
end

function main()
    if length(ARGS) > 0
        filepath = ARGS[1]
        instructions = Instruction[]
        for (line_no, line) in enumerate(eachline(filepath))
            instruction = parse_instruction(line)
            if isa(instruction, Nothing)
                @printf "Error: Could not parse %s:%d\n" filepath line_no
                exit(1)
            else
                instructions = push!(instructions, instruction)
            end
        end
        @printf "Part 1: %d\n" solve_part_1(instructions)
        @printf "Part 2: %d\n" solve_part_2(instructions)
    else
        print_usage()
    end
end

main()
