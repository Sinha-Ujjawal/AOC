from typing import List
import itertools as it


Calorie = int
Calories = List[Calorie]
ElvesCalories = List[Calories]


def pipe(x, *funcs):
    for func in funcs:
        x = func(x)
    return x


def parse_eleves_calorories_from_file(file: str) -> ElvesCalories:
    ret = []
    with open(file) as fp:
        for group in fp.read().strip().split("\n\n"):
            ret.append([int(line) for line in group.split("\n")])
    return ret


def solve_part1(elves_calories: ElvesCalories) -> int:
    return pipe(
        elves_calories,
        lambda elves_calories: map(sum, elves_calories),
        max,
    )


def solve_part2(elves_calories: ElvesCalories) -> int:
    return pipe(
        elves_calories,
        lambda elves_calories: map(sum, elves_calories),
        lambda cals: sorted(cals, reverse=True),
        lambda cals: it.islice(cals, 3),
        sum,
    )


if __name__ == "__main__":
    filename = input("Enter file name: ")
    elves_calories = parse_eleves_calorories_from_file(filename)
    part1_ans = solve_part1(elves_calories)
    print(f"Part 1: {part1_ans}")
    part2_ans = solve_part2(elves_calories)
    print(f"Part 2: {part2_ans}")
