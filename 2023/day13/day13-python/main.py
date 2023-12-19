from typing import Iterator, List, TypeVar, Callable, Optional
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


def is_palindrome(
    *,
    items: List[T],
    low: int,
    high: int,
    diff: Callable[[T, T], int],
    fix_count: int = 0,
) -> bool:
    if low > high:
        return False

    if low < 0 or low > len(items):
        return False

    if high < 0 or high > len(items):
        return False

    while low < high:
        fix_count -= diff(items[low], items[high])
        if fix_count < 0:
            return False
        low += 1
        high -= 1
    return fix_count == 0


def is_even_palindrome(
    *,
    items: List[T],
    low: int,
    high: int,
    diff: Callable[[T, T], int],
    fix_count: int = 0,
) -> bool:
    width = high - low + 1
    return ((width & 1) == 0) and is_palindrome(
        items=items,
        low=low,
        high=high,
        diff=diff,
        fix_count=fix_count,
    )


def transpose(image: List[str]) -> List[str]:
    return list(map(lambda xs: "".join(xs), zip(*image)))


def str_diff(xs: str, ys: str) -> int:
    if len(xs) != len(ys):
        return abs(len(xs) - len(ys))
    return sum(1 for x, y in zip(xs, ys) if x != y)


def find_horizontal_reflections(
    image: List[str], fix_count: int = 0
) -> Iterator[Reflection]:
    for idx in range(1, len(image)):
        if is_even_palindrome(
            items=image,
            low=0,
            high=idx,
            diff=str_diff,
            fix_count=fix_count,
        ):
            yield Reflection(type=ReflectionType.Horizontal, low=0, high=idx)

    for idx in range(len(image) - 1):
        if is_even_palindrome(
            items=image,
            low=idx,
            high=len(image) - 1,
            diff=str_diff,
            fix_count=fix_count,
        ):
            yield Reflection(
                type=ReflectionType.Horizontal, low=idx, high=len(image) - 1
            )


def find_vertical_reflections(
    image: List[str], fix_count: int = 0
) -> Iterator[Reflection]:
    for refl in find_horizontal_reflections(transpose(image), fix_count=fix_count):
        refl.type = ReflectionType.Vertical
        yield refl


def find_mirror(image: List[str], fix_count: int = 0) -> Optional[Reflection]:
    horiz = next(find_horizontal_reflections(image, fix_count=fix_count), None)
    if horiz is not None:
        return horiz
    vert = next(find_vertical_reflections(image, fix_count=fix_count), None)
    if vert is not None:
        return vert
    return None


def summary(image: List[str], fix_count: int = 0) -> int:
    mirror = find_mirror(image, fix_count=fix_count)
    if mirror is not None:
        if mirror.type == ReflectionType.Horizontal:
            return (mirror.idx + 1) * 100
        elif mirror.type == ReflectionType.Vertical:
            return mirror.idx + 1
        else:
            return 0
    return 0


def solve_part_1(rocks_and_ashes: List[List[str]]):
    return sum(map(summary, rocks_and_ashes))


def solve_part_2(rocks_and_ashes: List[List[str]]):
    return sum(map(lambda image: summary(image, fix_count=1), rocks_and_ashes))


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
    print(f"Part 2: {solve_part_2(rocks_and_ashes)}")


if __name__ == "__main__":
    main()
