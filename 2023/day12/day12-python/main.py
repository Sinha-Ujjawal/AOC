from typing import List, Tuple
from pathlib import Path
from functools import lru_cache
from itertools import repeat, islice


def num_possibilities(springs: str, desc: List[int]) -> int:
    springs = memoryview(bytes(springs, encoding="utf-8"))

    def satisfy(spring_i: int, desc_i: int) -> bool:
        d = desc[desc_i]
        cnt_damaged = 0
        for chr_ord in springs[spring_i:spring_i+d]:
            if chr(chr_ord) in ("?", "#"):
                cnt_damaged += 1
            else:
                break
        next_index = spring_i+d
        return (
            (cnt_damaged == d)
            and (
                (next_index >= len(springs))
                or (chr(springs[next_index]) != "#")
            )
        )

    @lru_cache(maxsize=None)
    def _dp(spring_i: int, desc_i: int) -> int:
        if desc_i >= len(desc):
            for chr_ord in springs[spring_i:]:
                if chr(chr_ord) == "#":
                    return 0
            return 1
        if spring_i >= len(springs):
            return 0
        current_spring = chr(springs[spring_i])
        if not satisfy(spring_i, desc_i):
            if current_spring != "#":
                return _dp(spring_i+1, desc_i)
            else:
                return 0
        else:
            d = desc[desc_i]
            proceed = _dp(spring_i+d+1, desc_i+1)
            if current_spring != "#":
                return _dp(spring_i+1, desc_i) + proceed
            else:
                return proceed

    return _dp(0, 0)


def solve_part_1(lines: List[str]) -> int:
    return sum(
        num_possibilities(springs, list(map(int, desc.split(","))))
        for l in lines
        for springs, desc in [l.split(" ")]
    )


def solve_part_2(lines: List[str]) -> int:
    def replicated(springs_desc: List[str], n: int) -> Tuple[str, str]:
        springs, desc = springs_desc
        return (
            "?".join(islice(repeat(springs), n)),
            ",".join(islice(repeat(desc), n)),
        )

    return sum(
        num_possibilities(springs, list(map(int, desc.split(","))))
        for l in lines
        for springs, desc in [replicated(l.split(" "), 5)]
    )


def main(file_path: str) -> None:
    file_contents = Path(file_path).read_text().splitlines()
    print(f"Part 1: {solve_part_1(file_contents)}")
    print(f"Part 2: {solve_part_2(file_contents)}")


if __name__ == "__main__":
    file_path = input("Enter file path: ")
    main(file_path)
