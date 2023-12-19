from typing import Iterator, List, TypeVar
from dataclasses import dataclass
import argparse
from pathlib import Path
from enum import Enum

T = TypeVar("T")


class ReflectionType(Enum):
    Vertical = 0
    Horizontal = 1


@dataclass
class Reflection:
    type: ReflectionType
    low: int
    high: int

    @property
    def width(self) -> int:
        return self.high - self.low + 1

    @property
    def idx(self) -> int:
        return self.low + (self.width >> 1) - 1


def is_palindrome(items: List[T], low: int, high: int) -> bool:
    if low > high:
        return False

    if low < 0 or low > len(items):
        return False

    if high < 0 or high > len(items):
        return False

    while low < high:
        if items[low] != items[high]:
            return False
        low += 1
        high -= 1
    return True


def is_even_palindrome(items: List[T], low: int, high: int) -> bool:
    width = high - low + 1
    return ((width & 1) == 0) and is_palindrome(items, low, high)


def transpose(image: List[str]) -> List[str]:
    return list(map(lambda xs: "".join(xs), zip(*image)))


def find_horizontal_reflections(image: List[str]) -> Iterator[Reflection]:
    for idx in range(len(image) - 1):
        if is_even_palindrome(image, idx, len(image) - 1):
            yield Reflection(
                type=ReflectionType.Horizontal, low=idx, high=len(image) - 1
            )
    for idx in range(1, len(image)):
        if is_even_palindrome(image, 0, idx):
            yield Reflection(type=ReflectionType.Horizontal, low=0, high=idx)


def find_vertical_reflections(image: List[str]) -> Iterator[Reflection]:
    for refl in find_horizontal_reflections(transpose(image)):
        refl.type = ReflectionType.Vertical
        yield refl


def summary(image: List[str]) -> int:
    horiz = next(find_horizontal_reflections(image), None)
    vert = next(find_vertical_reflections(image), None)
    num_rows = 0 if horiz is None else horiz.idx + 1
    num_cols = 0 if vert is None else vert.idx + 1
    return num_cols + num_rows * 100


def solve_part_1(rocks_and_ashes: List[List[str]]):
    return sum(map(summary, rocks_and_ashes))


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--filepath", required=True, help="Filepath of input file")

    args = parser.parse_args()

    filepath = args.filepath
    rocks_and_ashes = [
        rock_and_ashes.split("\n")
        for rock_and_ashes in Path(filepath).read_text().split("\n\n")
    ]
    print(f"Part 1: {solve_part_1(rocks_and_ashes)}")


if __name__ == "__main__":
    main()
