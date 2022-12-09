from typing import List, Any, Iterable, Set, Tuple
import itertools as it
from functools import reduce
import string


def partition(n, items: List[Any]) -> Iterable[Iterable[Any]]:
    for i in range(0, len(items), n):
        yield it.islice(items, i, i + n)


def priority_char(char: str) -> int:
    if char not in string.ascii_letters:
        return 0
    if char in string.ascii_lowercase:
        return ord(char) - ord("a") + 1
    else:
        return ord(char) - ord("A") + 27


def priority_badge(badge: Set[str]) -> int:
    return sum(map(priority_char, badge))


def badge_rusk_sacks(rusk_sacks: Iterable[str]) -> Set[str]:
    return reduce(lambda accum, s: accum.intersection(s), map(set, rusk_sacks))


def priority_rusk_sacks(rusk_sacks: Iterable[str]) -> int:
    return priority_badge(badge_rusk_sacks(rusk_sacks))


def solve(n: int, rusk_sacks: List[str]) -> int:
    return sum(map(priority_rusk_sacks, partition(n, rusk_sacks)))


def solve_part_1(s: str) -> int:
    def split_half(s: str) -> Tuple[str, str]:
        n = len(s)
        return s[: n >> 1], s[n >> 1 :]

    return solve(2, [x for line in s.strip().split("\n") for x in split_half(line)])


def solve_part_2(s: str) -> int:
    return solve(3, s.strip().split("\n"))


if __name__ == "__main__":
    file_path = input("Enter file path: ")
    contents = open(file_path, "r").read()
    print(f"Part 1: {solve_part_1(contents)}")
    print(f"Part 1: {solve_part_2(contents)}")
