from typing import List
from dataclasses import dataclass
import argparse
from pathlib import Path


def hash(s: str) -> int:
    h = 0
    for char in s:
        h = ((h + ord(char)) * 17) % 256
    return h


def solve_part_1(steps: List[str]) -> int:
    return sum(map(hash, steps))


def solve_part_2(steps: List[str]) -> int:
    @dataclass
    class KV:
        key: str
        value: int

    htable: List[List[KV]] = [list() for _ in range(256)]
    for step in steps:
        if step.endswith("-"):
            key = step[:-1]
            h = hash(key)
            htable[h] = list(filter(lambda kv: kv.key != key, htable[h]))
        else:
            key, value_s = step.split("=")
            value = int(value_s)
            h = hash(key)
            found = False
            for entry in htable[h]:
                if entry.key == key:
                    entry.value = value
                    found = True
                    break
            if not found:
                htable[h].append(KV(key, value))

    ret = 0
    for box_idx, slots in enumerate(htable, 1):
        for slot_idx, entry in enumerate(slots, 1):
            focal_len = entry.value
            ret += box_idx * slot_idx * focal_len

    return ret


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--filepath", help="Path of input file", required=True)
    args = parser.parse_args()
    steps = Path(args.filepath).read_text().split(",")
    print(f"Part 1: {solve_part_1(steps)}")
    print(f"Part 2: {solve_part_2(steps)}")


if __name__ == "__main__":
    main()
