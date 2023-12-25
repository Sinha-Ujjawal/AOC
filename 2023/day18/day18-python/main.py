from typing import List, Iterable, Tuple, TypeVar
from dataclasses import dataclass, replace
from enum import IntEnum
import argparse
from pathlib import Path
from itertools import tee

T = TypeVar("T")


def pairwise(items: Iterable[T]) -> Iterable[Tuple[T, T]]:
    xs, ys = tee(items)
    next(ys, None)
    return zip(xs, ys)


class Direction(IntEnum):
    Left: int = 0
    Right: int = 1
    Up: int = 2
    Down: int = 3

    @property
    def offsets(self) -> Tuple[int, int]:
        if self == Direction.Left:
            return (0, -1)
        elif self == Direction.Right:
            return (0, 1)
        elif self == Direction.Up:
            return (-1, 0)
        else:
            return (1, 0)

    @staticmethod
    def from_str(s: str) -> "Direction":
        if s == "L":
            return Direction.Left
        elif s == "R":
            return Direction.Right
        elif s == "U":
            return Direction.Up
        elif s == "D":
            return Direction.Down
        else:
            raise ValueError(f"Unrecognized string: {s=}!")


@dataclass(frozen=True)
class Dig:
    __slots__ = ["direction", "meters", "color_of_edge"]
    direction: Direction
    meters: int


@dataclass(frozen=True, order=True)
class Position:
    __slots__ = ["row", "col"]
    row: int
    col: int

    def proceed(self, direction: Direction, times: int = 1) -> "Position":
        off_row, off_col = direction.offsets
        return replace(
            self, row=self.row + times * off_row, col=self.col + times * off_col
        )

    def plot(self, direction: Direction) -> Iterable["Position"]:
        posn = self
        off_row, off_col = direction.offsets
        while True:
            posn = replace(posn, row=posn.row + off_row, col=posn.col + off_col)
            yield posn


def find_area(polygon: List[Position]) -> float:
    a = p = 0.0
    for prev, next in pairwise(polygon):
        a += next.row * prev.col - next.col * prev.row
        p += abs((next.row - prev.row) + (next.col - prev.col) * 1j)
    return ((abs(a) + abs(p)) / 2) + 1


def solve(dig_plan: Iterable[Dig]) -> int:
    current_posn = start = Position(0, 0)
    polygon = []
    for dig in dig_plan:
        polygon.append(current_posn)
        current_posn = current_posn.proceed(dig.direction, times=dig.meters)
    polygon.append(current_posn)
    return int(abs(find_area(polygon)))


def solve_part_1(lines: List[str]) -> int:
    def parse_dig(line: str) -> Dig:
        direction, meters, _ = line.split()
        return Dig(Direction.from_str(direction), int(meters))

    return solve(parse_dig(line) for line in lines)


def solve_part_2(lines: List[str]) -> int:
    digit_to_direction = {
        "0": Direction.Right,
        "1": Direction.Down,
        "2": Direction.Left,
        "3": Direction.Up,
    }

    def parse_dig(line: str) -> Dig:
        _, _, hex_color = line.split()
        hex_color = hex_color[1:-1]
        direction = digit_to_direction[hex_color[-1]]
        meters = int(hex_color[1:-1], 16)
        return Dig(direction, meters)

    return solve(parse_dig(line) for line in lines)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--filepath", help="File path of the input", required=True)
    args = parser.parse_args()
    lines = Path(args.filepath).read_text().split("\n")
    print(f"Part 1: {solve_part_1(lines)}")
    print(f"Part 2: {solve_part_2(lines)}")


if __name__ == "__main__":
    main()
