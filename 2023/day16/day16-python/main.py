from typing import Callable, TypeVar, Iterable, List, Tuple, Set
from dataclasses import dataclass
from enum import IntEnum
import argparse
from pathlib import Path

T = TypeVar("T")


def bfs(neighbor_fn: Callable[[T], Iterable[T]], start: T) -> Iterable[List[T]]:
    seen_before = {start}
    frontier = [start]
    while frontier:
        yield frontier
        new_frontier = []
        for u in frontier:
            for v in neighbor_fn(u):
                if v not in seen_before:
                    seen_before.add(v)
                    new_frontier.append(v)
        frontier = new_frontier


class Direction(IntEnum):
    Left: int = 0
    Right: int = 1
    Up: int = 2
    Down: int = 3


@dataclass(frozen=True)
class Position:
    row: int
    col: int

    def next_position(self, direction: Direction) -> "Position":
        if direction == Direction.Left:
            return Position(row=self.row, col=self.col - 1)
        elif direction == Direction.Right:
            return Position(row=self.row, col=self.col + 1)
        elif direction == Direction.Up:
            return Position(row=self.row - 1, col=self.col)
        elif direction == Direction.Down:
            return Position(row=self.row + 1, col=self.col)
        else:
            raise ValueError(f"Unexpected {direction=}!")


Node = Tuple[Position, Direction]


@dataclass
class Reflectors:
    grid: List[str]

    @property
    def nrows(self) -> int:
        return len(self.grid)

    @property
    def ncols(self) -> int:
        return len(self.grid[0]) if self.grid else 0

    def neighbor_fn(self, node: Node) -> Iterable[Node]:
        posn, dir = node
        next_posn = posn.next_position(dir)
        row, col = next_posn.row, next_posn.col
        reflectors = self.grid

        if (
            (not reflectors)
            or (row < 0)
            or (row >= len(reflectors))
            or (col < 0)
            or (col >= len(reflectors[0]))
        ):
            return

        char = reflectors[row][col]

        if char == ".":
            yield next_posn, dir

        elif char == "|":
            if dir in (Direction.Left, Direction.Right):
                yield next_posn, Direction.Up
                yield next_posn, Direction.Down
            else:
                yield next_posn, dir

        elif char == "-":
            if dir in (Direction.Down, Direction.Up):
                yield next_posn, Direction.Left
                yield next_posn, Direction.Right
            else:
                yield next_posn, dir

        elif char == "/":
            if dir == Direction.Left:
                yield next_posn, Direction.Down
            elif dir == Direction.Right:
                yield next_posn, Direction.Up
            elif dir == Direction.Up:
                yield next_posn, Direction.Right
            elif dir == Direction.Down:
                yield next_posn, Direction.Left

        elif char == "\\":
            if dir == Direction.Left:
                yield next_posn, Direction.Up
            elif dir == Direction.Right:
                yield next_posn, Direction.Down
            elif dir == Direction.Up:
                yield next_posn, Direction.Left
            elif dir == Direction.Down:
                yield next_posn, Direction.Right

    def energized_cells(self, starting_posn: Position, dir: Direction) -> Set[Position]:
        energized_cells = set()
        for frontier in bfs(self.neighbor_fn, (starting_posn, dir)):
            for posn, _ in frontier:
                energized_cells.add(posn)
        return energized_cells


def solve_part_1(reflectors: Reflectors) -> int:
    return len(reflectors.energized_cells(Position(0, -1), Direction.Right)) - 1


def solve_part_2(reflectors: Reflectors) -> int:
    nrows, ncols = reflectors.nrows, reflectors.ncols
    ret = 0

    for row in range(nrows):
        ret = max(
            ret, len(reflectors.energized_cells(Position(row, -1), Direction.Right)) - 1
        )
        ret = max(
            ret,
            len(reflectors.energized_cells(Position(row, ncols), Direction.Left)) - 1,
        )

    for col in range(ncols):
        ret = max(
            ret, len(reflectors.energized_cells(Position(-1, col), Direction.Down)) - 1
        )
        ret = max(
            ret, len(reflectors.energized_cells(Position(nrows, col), Direction.Up)) - 1
        )

    return ret


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--filepath", help="Path of input file", required=True)
    args = parser.parse_args()
    reflectors = Reflectors(Path(args.filepath).read_text().split())
    print(f"Part 1: {solve_part_1(reflectors)}")
    print(f"Part 2: {solve_part_2(reflectors)}")


if __name__ == "__main__":
    main()
