from typing import Tuple, Dict, Callable
from itertools import islice
from pathlib import Path

def mk_calib_fn(patterns: Dict[str, int]) -> Callable[[str], int]:
    def _(line: str) -> int:
        first_digit = None
        last_digit = None
        while line != "":
            for pattern, val in patterns.items():
                if line.startswith(pattern):
                    if first_digit is None:
                        first_digit = val
                    else:
                        last_digit = val
                    break
            line = line[1:]
        if first_digit is None:
            return 0
        if last_digit is None:
            last_digit = first_digit
        return first_digit*10 + last_digit
    return _

def solve_for_file(file_path: Path) -> Tuple[int, int]:
    patterns_for_part1 = {
        "1": 1,
        "2": 2,
        "3": 3,
        "4": 4,
        "5": 5,
        "6": 6,
        "7": 7,
        "8": 8,
        "9": 9,
    }
    calib_fn_part1 = mk_calib_fn(patterns_for_part1)
    patterns_for_part2 = {
        **patterns_for_part1,
        "one"  : 1,
        "two"  : 2,
        "three": 3,
        "four" : 4,
        "five" : 5,
        "six"  : 6,
        "seven": 7,
        "eight": 8,
        "nine" : 9,
    }
    calib_fn_part2 = mk_calib_fn(patterns_for_part2)
    lines = file_path.read_text().split("\n")
    part1_ans = sum(map(calib_fn_part1, lines))
    part2_ans = sum(map(calib_fn_part2, lines))
    return part1_ans, part2_ans

def main() -> None:
    file_path = input("Enter file path: ")
    part1_ans, part2_ans = solve_for_file(Path(file_path))
    print(f"Part 1: {part1_ans}")
    print(f"Part 1: {part2_ans}")

if __name__ == "__main__":
    main()
