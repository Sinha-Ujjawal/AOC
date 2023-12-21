from typing import List, TypeVar, Iterable
from dataclasses import dataclass
import argparse
from pathlib import Path

T = TypeVar("T")


@dataclass
class Grid:
    grid_mat: List[List[str]]
    nrows: int
    ncols: int

    @staticmethod
    def from_repr(s: str) -> "Grid":
        return Grid(s.split("\n"))

    def __repr__(self):
        return "\n".join(map(lambda row: "".join(row), self.grid_mat))

    def __init__(self, grid: Iterable[str]):
        self.grid_mat = list(map(lambda row: list(row), grid))
        self.nrows = len(self.grid_mat)
        self.ncols = 0 if self.nrows == 0 else len(self.grid_mat[0])

    def total_load_north(self) -> int:
        return sum(
            map(
                lambda pair: pair[1].count("O") * (len(self.grid_mat) - pair[0]),
                enumerate(self.grid_mat),
            )
        )

    def tilt_north(self) -> None:
        hts = [0] * self.ncols
        for i in range(self.nrows):
            for j in range(self.ncols):
                char = self.grid_mat[i][j]
                if char == "O":
                    self.grid_mat[i][j], self.grid_mat[hts[j]][j] = ".", "O"
                    hts[j] += 1
                elif char == "#":
                    hts[j] = i + 1

    def tilt_south(self) -> None:
        hts = [self.nrows - 1] * self.ncols
        for i in range(self.nrows - 1, -1, -1):
            for j in range(self.ncols - 1, -1, -1):
                char = self.grid_mat[i][j]
                if char == "O":
                    self.grid_mat[i][j], self.grid_mat[hts[j]][j] = ".", "O"
                    hts[j] -= 1
                elif char == "#":
                    hts[j] = i - 1

    def tilt_west(self) -> None:
        hts = [0] * self.nrows
        for j in range(self.ncols):
            for i in range(self.nrows):
                char = self.grid_mat[i][j]
                if char == "O":
                    self.grid_mat[i][j], self.grid_mat[i][hts[i]] = ".", "O"
                    hts[i] += 1
                elif char == "#":
                    hts[i] = j + 1

    def tilt_east(self) -> None:
        hts = [self.ncols - 1] * self.nrows
        for j in range(self.ncols - 1, -1, -1):
            for i in range(self.nrows - 1, -1, -1):
                char = self.grid_mat[i][j]
                if char == "O":
                    self.grid_mat[i][j], self.grid_mat[i][hts[i]] = ".", "O"
                    hts[i] -= 1
                elif char == "#":
                    hts[i] = j - 1

    def cycle(self) -> None:
        self.tilt_north()
        self.tilt_west()
        self.tilt_south()
        self.tilt_east()


def solve_part_1(input: List[str]) -> int:
    grid = Grid(input)
    grid.tilt_north()
    return grid.total_load_north()


def solve_part_2(input: List[str]) -> int:
    grid = Grid(input)
    num_cycles = 1_000_000_000
    cycle = 0
    grid_repr = repr(grid)
    seen_before = {grid_repr: 0}
    grid_reprs = [grid_repr]
    while cycle < num_cycles:
        grid.cycle()
        cycle += 1
        grid_repr = repr(grid)
        grid_reprs.append(grid_repr)
        if grid_repr in seen_before:
            cycle_start = seen_before[grid_repr]
            cycle_end = cycle - 1
            cycle_width = cycle_end - cycle_start + 1
            if cycle_start > 0:
                remaining = num_cycles - cycle_start
            else:
                remaining = num_cycles
            grid_idx = cycle_start + (remaining % cycle_width)
            grid = Grid.from_repr(grid_reprs[grid_idx])
            break
        else:
            seen_before[grid_repr] = cycle

    return grid.total_load_north()


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--filepath", help="file path of the input", required=True)
    args = parser.parse_args()
    input = Path(args.filepath).read_text().split()
    print(f"Part 1: {solve_part_1(input)}")
    print(f"Part 2: {solve_part_2(input)}")


if __name__ == "__main__":
    main()
