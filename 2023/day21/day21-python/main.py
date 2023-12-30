from typing import Callable, TypeVar, Iterable, Set, Optional, List
from pathlib import Path
import argparse
from itertools import islice
from dataclasses import dataclass, field
import numpy as np
from numpy.polynomial.polynomial import polyfit

N = TypeVar("N")


def walk(neighbor_fn: Callable[[N], Iterable[N]], start: N) -> Iterable[Set[N]]:
    frontier = {start}
    while frontier:
        yield frontier
        frontier = {v for u in frontier for v in neighbor_fn(u)}


@dataclass(frozen=True)
class Position:
    __slots__ = ["row", "col"]
    row: int
    col: int

    def nsew(self) -> List["Position"]:
        return [
            Position(self.row + 1, self.col),
            Position(self.row - 1, self.col),
            Position(self.row, self.col + 1),
            Position(self.row, self.col - 1),
        ]


@dataclass
class Grid:
    grid: List[str]
    nrow: int = field(init=False)
    ncol: int = field(init=False)

    def __post_init__(self):
        self.nrow = len(self.grid)
        self.ncol = len(self.grid[0]) if self.grid else 0

    def find_start(self) -> Optional[Position]:
        for row_idx, row in enumerate(self.grid):
            for col_idx, cell in enumerate(row):
                if cell == "S":
                    return Position(row_idx, col_idx)
        return None

    def within_grid(self, pos: Position) -> bool:
        return (0 <= pos.row < self.nrow) and (0 <= pos.col < self.ncol)

    def is_blocked(self, pos: Position) -> bool:
        return self.grid[pos.row][pos.col] == "#"

    def wrap(self, pos: Position) -> Position:
        return Position(pos.row % self.nrow, pos.col % self.ncol)

    def render_grid_with(self, nodes: Set[Position]) -> str:
        lines = []
        for row_idx, row in enumerate(self.grid):
            line = []
            for col_idx, cell in enumerate(row):
                if (row_idx, col_idx) in nodes:
                    line.append("O")
                else:
                    line.append(cell)
            lines.append("".join(line))
        return "\n".join(lines)


def solve_part_1(grid: Grid, nth: int) -> int:
    assert nth >= 1
    start_node = grid.find_start()
    if start_node is None:
        print(f"Could not find start node 'S' in the grid.\n{grid=}")
        return -1

    def neighbor_fn(pos: Position) -> Iterable[Position]:
        return filter(
            lambda neighbor: (
                grid.within_grid(neighbor) and not grid.is_blocked(neighbor)
            ),
            pos.nsew(),
        )

    frontiers = iter(walk(neighbor_fn, start_node))
    next(frontiers, None)
    return len(next(islice(frontiers, nth - 1, nth)))


def solve_part_2(grid: Grid) -> int:
    start_node = grid.find_start()
    if start_node is None:
        print(f"Could not find start node 'S' in the grid.\n{grid=}")
        return -1

    def neighbor_fn(pos: Position) -> Iterable[Position]:
        return filter(
            lambda neighbor: (not grid.is_blocked(grid.wrap(neighbor))),
            pos.nsew(),
        )

    # I really don't know how the below logic works
    # I know what's going on here, but I don't know why it works :)
    # YT video for some explaination - https://youtu.be/xHIQ2zHVSjM?si=wqIBA5M1a0q8nASH
    frontier_lengths = map(len, walk(neighbor_fn, start_node))
    target = (26501365 - 65) // 131
    X = [0, 1, 2]
    y = []
    next(frontier_lengths)
    y.append(next(islice(frontier_lengths, 64, 65)))  # after 65 steps
    y.append(next(islice(frontier_lengths, 130, 131)))  # after 131 steps
    y.append(next(islice(frontier_lengths, 130, 131)))  # after another 131 steps

    poly = np.rint(polyfit(X, y, 2)).astype(int).tolist()

    return sum(poly[i] * target**i for i in range(3))


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--filepath", help="File path of the input", required=True)
    args = parser.parse_args()
    grid = Grid(Path(args.filepath).read_text().split())
    print(f"Part 1: {solve_part_1(grid, 64)}")
    print(f"Part 2: {solve_part_2(grid)}")


if __name__ == "__main__":
    main()
