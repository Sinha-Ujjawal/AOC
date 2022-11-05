from typing import Tuple, Optional, List, Callable, TypeVar, Iterable
from enum import IntEnum, auto
from dataclasses import dataclass
import re
import sys
import numpy as np

T = TypeVar("T")

GRID_SIZE: int = 1000


def or_lazy(
    lazy_thunk: Callable[[], Optional[T]],
    *lazy_thunks: Callable[[], Optional[T]],
) -> Optional[T]:
    for thunk in (lazy_thunk, *lazy_thunks):
        ret = thunk()
        if ret is not None:
            return ret
    return None


class InstructionType(IntEnum):
    TurnOn = auto()
    TurnOff = auto()
    Toggle = auto()


Coordinate = Tuple[int, int]


@dataclass
class Box:
    top_left: Coordinate
    bottom_right: Coordinate


Instruction = Tuple[InstructionType, Box]


def turn_parse_instruction_type(
    instruction_type_str: str,
    instruction_type: InstructionType,
    line: str,
) -> Optional[Instruction]:
    matches = re.findall(
        f"{instruction_type_str} (\d+),(\d+) through (\d+),(\d+)", line
    )
    if len(matches) < 1:
        return None
    matches = matches[0]
    if len(matches) != 4:
        return None
    xl, yl, xu, yu = map(int, matches)
    box = Box((xl, yl), (xu, yu))
    return instruction_type, box


def try_parse_turn_on(line: str) -> Optional[Instruction]:
    return turn_parse_instruction_type("turn on", InstructionType.TurnOn, line)


def try_parse_turn_off(line: str) -> Optional[Instruction]:
    return turn_parse_instruction_type("turn off", InstructionType.TurnOff, line)


def try_parse_toggle(line: str) -> Optional[Instruction]:
    return turn_parse_instruction_type("toggle", InstructionType.Toggle, line)


def parse_line(line: str) -> Optional[Instruction]:
    return or_lazy(
        lambda: try_parse_turn_on(line),
        lambda: try_parse_turn_off(line),
        lambda: try_parse_toggle(line),
    )


def parse_lines(lines: List[str]) -> Optional[List[Instruction]]:
    ret = []
    for line in lines:
        instruction_maybe = parse_line(line)
        if instruction_maybe is None:
            return None
        ret.append(instruction_maybe)
    return ret


def parse_from_file(file_name: str) -> Optional[List[Instruction]]:
    with open(file_name, "r") as fp:
        return parse_lines(fp.readlines())


def solve(
    turn_on_update_fn: Callable[[np.array], np.array],
    turn_off_update_fn: Callable[[np.array], np.array],
    toggle_update_fn: Callable[[np.array], np.array],
    instructions: Iterable[Instruction],
) -> np.array:
    grid = np.zeros(shape=(GRID_SIZE, GRID_SIZE), dtype="int64")

    def update_grid(box: Box, update_fn: Callable[[int], int]) -> None:
        xl, yl = box.top_left
        xu, yu = box.bottom_right
        grid[xl : xu + 1, yl : yu + 1] = np.maximum(
            0, update_fn(grid[xl : xu + 1, yl : yu + 1])
        )

    for (instruction_type, box) in instructions:
        if instruction_type == InstructionType.TurnOn:
            update_grid(box, turn_on_update_fn)
        elif instruction_type == InstructionType.TurnOff:
            update_grid(box, turn_off_update_fn)
        elif instruction_type == InstructionType.Toggle:
            update_grid(box, toggle_update_fn)
        else:
            assert False, f"Unknown InstructionType: {instruction_type}"
    return grid.reshape((GRID_SIZE * GRID_SIZE,))


def solve_part1(instructions: Iterable[Instruction]) -> int:
    turn_on_update_fn = lambda _: 1
    turn_off_update_fn = lambda _: 0
    toggle_update_fn = lambda b: b ^ 1
    final_grid = solve(
        turn_on_update_fn,
        turn_off_update_fn,
        toggle_update_fn,
        instructions,
    )
    return np.sum(final_grid > 0)


def solve_part2(instructions: Iterable[Instruction]) -> int:
    turn_on_update_fn = lambda b: b + 1
    turn_off_update_fn = lambda b: b - 1
    toggle_update_fn = lambda b: b + 2
    final_grid = solve(
        turn_on_update_fn,
        turn_off_update_fn,
        toggle_update_fn,
        instructions,
    )
    return np.sum(final_grid[final_grid > 0])


if __name__ == "__main__":
    if len(sys.argv) > 1:
        file_name = sys.argv[1]
    else:
        file_name = input("Enter file name: ")

    instructions = parse_from_file(file_name)

    if instructions is None:
        raise Exception(f"Unable to parse the file: {file_name}")

    part1_ans = solve_part1(instructions)
    part2_ans = solve_part2(instructions)

    print(f"Part 1: {part1_ans}")
    print(f"Part 2: {part2_ans}")
