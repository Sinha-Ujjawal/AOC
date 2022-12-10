from typing import List, Tuple
from dataclasses import dataclass
import re


@dataclass(frozen=True)
class Interval:
    __slots__ = ("start", "end")
    start: int
    end: int


def parse_interval_pair(s: str) -> Tuple[Interval, Interval]:
    [matches] = re.findall("^(\d+)-(\d+),(\d+)-(\d+)$", s)
    if not matches:
        raise ValueError(f'Cannot parse string "{s}"')
    return (
        Interval(start=int(matches[0]), end=int(matches[1])),
        Interval(start=int(matches[2]), end=int(matches[3])),
    )


def parse_interval_pairs(s: str) -> List[Tuple[Interval, Interval]]:
    return list(map(parse_interval_pair, s.strip().split("\n")))


def solve_part_1(interval_pairs: List[Tuple[Interval, Interval]]) -> int:
    def interval_completely_contains(interval_pair: Tuple[Interval, Interval]) -> bool:
        interval_left, interval_right = interval_pair
        ll, lh, rl, rh = (
            interval_left.start,
            interval_left.end,
            interval_right.start,
            interval_right.end,
        )
        return (ll <= lh and rl <= rh) and (
            (rl >= ll and rh <= lh) or (ll >= rl and lh <= rh)
        )

    return sum(1 for _ in filter(interval_completely_contains, interval_pairs))


def solve_part_2(interval_pairs: List[Tuple[Interval, Interval]]) -> int:
    def interval_overlaps(interval_pair: Tuple[Interval, Interval]) -> bool:
        interval_left, interval_right = interval_pair
        ll, lh, rl, rh = (
            interval_left.start,
            interval_left.end,
            interval_right.start,
            interval_right.end,
        )
        return (ll <= lh and rl <= rh) and (
            (ll <= rh and rl <= lh) or (rl <= lh and ll <= rh)
        )

    return sum(1 for _ in filter(interval_overlaps, interval_pairs))


if __name__ == "__main__":
    file_path = input("Enter file path: ")
    contents = open(file_path, "r").read()
    interval_pairs = parse_interval_pairs(contents)
    print(f"Part 1: {solve_part_1(interval_pairs)}")
    print(f"Part 2: {solve_part_2(interval_pairs)}")
