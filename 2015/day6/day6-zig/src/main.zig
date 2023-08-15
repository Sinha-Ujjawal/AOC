const std = @import("std");
const io = std.io;
const mem = std.mem;
const fs = std.fs;
const os = std.os;
const log = std.log;
const fmt = std.fmt;

const R_MAXSIZE: usize = 512;
const GRID_WIDTH: usize = 1000;
const GRID_SIZE: usize = GRID_WIDTH * GRID_WIDTH;
const TurnOnString = "turn on ";
const TurnOffString = "turn off ";
const ToggleString = "toggle ";

fn read_line_from_reader(comptime Context: type, comptime ReadError: type, comptime readFn: fn (context: Context, buffer: []u8) ReadError!usize, reader: io.Reader(Context, ReadError, readFn), allocator: mem.Allocator, maxsize: ?usize) !?[]const u8 {
    var array_list = std.ArrayList(u8).init(allocator);
    defer array_list.deinit();
    reader.streamUntilDelimiter(array_list.writer(), '\n', maxsize) catch |err| switch (err) {
        error.EndOfStream => if (array_list.items.len == 0) {
            return null;
        },
        else => |e| return e,
    };
    var ret = try array_list.toOwnedSlice();
    if (ret.len > 1 and std.mem.endsWith(u8, ret, "\r")) {
        ret = ret[0 .. ret.len - 1];
    }
    return ret;
}

fn read_lines_from_reader(comptime Context: type, comptime ReadError: type, comptime readFn: fn (context: Context, buffer: []u8) ReadError!usize, reader: io.Reader(Context, ReadError, readFn), allocator: mem.Allocator, maxsize: ?usize) !std.ArrayList([]const u8) {
    var lines = std.ArrayList([]const u8).init(allocator);
    while (true) {
        if (try read_line_from_reader(Context, ReadError, readFn, reader, allocator, maxsize)) |line| {
            try lines.append(line);
        } else {
            break;
        }
    }
    return lines;
}

fn read_line_from_console(allocator: mem.Allocator, maxsize: ?usize) !?[]const u8 {
    const stdin = io.getStdIn().reader();
    return read_line_from_reader(fs.File, os.ReadError, fs.File.read, stdin, allocator, maxsize);
}

fn read_file_path(allocator: mem.Allocator) ![]const u8 {
    var args = try std.process.ArgIterator.initWithAllocator(allocator);
    defer args.deinit();
    _ = args.skip(); //to skip the zig call
    if (args.next()) |filepath| {
        var array_list = std.ArrayList(u8).init(allocator);
        try array_list.appendSlice(filepath);
        return try array_list.toOwnedSlice();
    }
    const stdout = io.getStdOut().writer();
    try stdout.writeAll("Enter file path: ");
    return (try read_line_from_console(allocator, R_MAXSIZE)) orelse return "";
}

fn read_input(allocator: mem.Allocator) !std.ArrayList([]const u8) {
    const filepath = try read_file_path(allocator);
    var fp = try fs.cwd().openFile(filepath, fs.File.OpenFlags{ .mode = .read_only });
    defer fp.close();
    var reader = fp.reader();
    return read_lines_from_reader(fs.File, os.ReadError, fs.File.read, reader, allocator, R_MAXSIZE);
}

const InstructionType = enum { TurnOn, TurnOff, Toggle };

const Coord = struct { x: usize, y: usize };

const Box = struct { top_left: Coord, bottom_right: Coord };

const Instruction = struct { instruction_type: InstructionType, box: Box };

const ParseError = error{CannotParseLine};

fn parse_instructions(lines: std.ArrayList([]const u8), allocator: mem.Allocator) !std.ArrayList(Instruction) {
    var instructions = std.ArrayList(Instruction).init(allocator);
    for (lines.items) |line| {
        if (try parse_instruction(line)) |instruction| {
            try instructions.append(instruction);
        } else {
            return ParseError.CannotParseLine;
        }
    }
    return instructions;
}

fn parse_instruction(line: []const u8) !?Instruction {
    const parse_instruction_type_result = parse_instruction_type(line) orelse return null;
    const instruction_type = parse_instruction_type_result.instruction_type;
    const box = (try parse_box(parse_instruction_type_result.rest)) orelse return null;
    return Instruction{ .instruction_type = instruction_type, .box = box };
}

const ParseInstructionTypeResult = struct { instruction_type: InstructionType, rest: []const u8 };

fn parse_instruction_type(line: []const u8) ?ParseInstructionTypeResult {
    if (mem.startsWith(u8, line, TurnOnString)) {
        return ParseInstructionTypeResult{ .instruction_type = InstructionType.TurnOn, .rest = line[TurnOnString.len..line.len] };
    } else if (mem.startsWith(u8, line, TurnOffString)) {
        return ParseInstructionTypeResult{ .instruction_type = InstructionType.TurnOff, .rest = line[TurnOffString.len..line.len] };
    } else if (mem.startsWith(u8, line, ToggleString)) {
        return ParseInstructionTypeResult{ .instruction_type = InstructionType.Toggle, .rest = line[ToggleString.len..line.len] };
    } else {
        return null;
    }
}

fn parse_box(line: []const u8) !?Box {
    var split_iter = mem.splitSequence(u8, line, " through ");
    const top_left_coord_str = split_iter.first();
    const bottom_right_coord_str = split_iter.next() orelse return null;
    const top_left = (try parse_coord(top_left_coord_str)) orelse return null;
    const bottom_right = (try parse_coord(bottom_right_coord_str)) orelse return null;
    return Box{ .top_left = top_left, .bottom_right = bottom_right };
}

fn parse_coord(line: []const u8) !?Coord {
    var split_iter = mem.splitSequence(u8, line, ",");
    const x_str = split_iter.first();
    const y_str = split_iter.next() orelse return null;
    const x = try fmt.parseInt(usize, x_str, 10);
    const y = try fmt.parseInt(usize, y_str, 10);
    return Coord{ .x = x, .y = y };
}

const brightness_updater = *const fn (usize) usize;

fn solve(instructions: std.ArrayList(Instruction), turn_on_brightness_updater: brightness_updater, turn_off_brightness_updater: brightness_updater, toggle_brightness_updater: brightness_updater) []usize {
    var grid: [GRID_SIZE]usize = undefined;
    for (0..GRID_SIZE) |idx| {
        grid[idx] = 0;
    }
    for (instructions.items) |instruction| {
        var instruction_type = instruction.instruction_type;
        var box = instruction.box;
        var top_left = box.top_left;
        var bottom_right = box.bottom_right;
        var bupdater = switch (instruction_type) {
            InstructionType.TurnOn => turn_on_brightness_updater,
            InstructionType.TurnOff => turn_off_brightness_updater,
            InstructionType.Toggle => toggle_brightness_updater,
        };
        for (top_left.x..bottom_right.x + 1) |x| {
            for (top_left.y..bottom_right.y + 1) |y| {
                var idx = x * GRID_WIDTH + y;
                grid[idx] = bupdater(grid[idx]);
            }
        }
    }
    return &grid;
}

fn part_1_turn_on_brightness_updater(_: usize) usize {
    return 1;
}

fn part_1_turn_off_brightness_updater(_: usize) usize {
    return 0;
}

fn part_1_toggle_brightness_updater(b: usize) usize {
    if (b > 0) {
        return 0;
    }
    return 1;
}

fn solve_part_1(instructions: std.ArrayList(Instruction)) usize {
    const grid = solve(
        instructions,
        part_1_turn_on_brightness_updater,
        part_1_turn_off_brightness_updater,
        part_1_toggle_brightness_updater,
    );
    var ret: usize = 0;
    for (grid) |x| {
        if (x > 0) {
            ret += 1;
        }
    }
    return ret;
}

fn part_2_turn_on_brightness_updater(b: usize) usize {
    return b + 1;
}

fn part_2_turn_off_brightness_updater(b: usize) usize {
    if (b == 0) {
        return b;
    }
    return b - 1;
}

fn part_2_toggle_brightness_updater(b: usize) usize {
    return b + 2;
}

fn solve_part_2(instructions: std.ArrayList(Instruction)) usize {
    const grid = solve(
        instructions,
        part_2_turn_on_brightness_updater,
        part_2_turn_off_brightness_updater,
        part_2_toggle_brightness_updater,
    );
    var ret: usize = 0;
    for (grid) |x| {
        ret += x;
    }
    return ret;
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const lines = try read_input(allocator);
    const instructions = try parse_instructions(lines, allocator);
    lines.deinit();
    defer instructions.deinit();
    log.info("Part 1: {}", .{solve_part_1(instructions)});
    log.info("Part 2: {}", .{solve_part_2(instructions)});
}
