from typing import List
import argparse
from pathlib import Path


def tilt_north(grid: List[str]) -> List[str]:
    grid_mat = list(map(lambda row: list(row), grid))
    hts = [0] * len(grid[0])
    for i in range(len(grid_mat)):
        for j in range(len(grid_mat[i])):
            char = grid_mat[i][j]
            if char == "O":
                grid_mat[i][j], grid_mat[hts[j]][j] = ".", "O"
                hts[j] += 1
            elif char == "#":
                hts[j] = i + 1
    return list(map(lambda row: "".join(row), grid_mat))


def solve_part_1(grid: List[str]) -> int:
    return sum(
        map(
            lambda pair: pair[1].count("O") * (len(grid) - pair[0]),
            enumerate(tilt_north(grid)),
        )
    )


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--filepath", help="file path of the input", required=True)
    args = parser.parse_args()
    grid = Path(args.filepath).read_text().split()
    print(f"Part 1: {solve_part_1(grid)}")


if __name__ == "__main__":
    main()
