from typing import Union, Dict, List, Tuple, Optional, Iterable
import argparse
from pathlib import Path
from dataclasses import dataclass, replace
from enum import IntEnum
from operator import __lt__, __gt__


class Status(IntEnum):
    Accepted: int = 0
    Rejected: int = 1


class PartType(IntEnum):
    ExtremelyCoolLooking: int = 0
    Musical: int = 1
    Aerodynamic: int = 2
    Shiny: int = 3


@dataclass(frozen=True)
class Part:
    x: int
    m: int
    a: int
    s: int

    @property
    def xmas(self) -> int:
        return self.x + self.m + self.a + self.s


@dataclass(frozen=True)
class PartRange:
    x: Tuple[int, int]
    m: Tuple[int, int]
    a: Tuple[int, int]
    s: Tuple[int, int]

    @property
    def num_possibilities(self) -> int:
        x1, x2 = self.x
        m1, m2 = self.m
        a1, a2 = self.a
        s1, s2 = self.s
        return (x2 - x1 + 1) * (m2 - m1 + 1) * (a2 - a1 + 1) * (s2 - s1 + 1)


class Comparator(IntEnum):
    LessThan: int = 0
    GreaterThan: int = 1

    def satisfy(self, x: int, y: int) -> bool:
        if self == Comparator.LessThan:
            return x < y
        else:
            return x > y

    def split(
        self, pair: Tuple[int, int], v: int
    ) -> Tuple[Optional[Tuple[int, int]], Optional[Tuple[int, int]]]:
        x1, x2 = pair
        if self == Comparator.LessThan:
            if x1 < v <= x2:
                return (x1, v - 1), (v, x2)
            elif v > x2:
                return pair, None
            else:
                return None, pair
        else:
            if x1 <= v < x2:
                return (v + 1, x2), (x1, v)
            elif v < x1:
                return pair, None
            else:
                return None, pair


@dataclass(frozen=True)
class Condition:
    part_type: PartType
    comparator: Comparator
    value: int

    def satisfy(self, part: Part) -> bool:
        if self.part_type == PartType.ExtremelyCoolLooking:
            return self.comparator.satisfy(part.x, self.value)
        elif self.part_type == PartType.Musical:
            return self.comparator.satisfy(part.m, self.value)
        elif self.part_type == PartType.Aerodynamic:
            return self.comparator.satisfy(part.a, self.value)
        else:
            return self.comparator.satisfy(part.s, self.value)

    def split(
        self, part_range: PartRange
    ) -> Tuple[Optional[PartRange], Optional[PartRange]]:
        key = None
        if self.part_type == PartType.ExtremelyCoolLooking:
            key = "x"
        elif self.part_type == PartType.Musical:
            key = "m"
        elif self.part_type == PartType.Aerodynamic:
            key = "a"
        else:
            key = "s"

        val = getattr(part_range, key)

        satisfy, not_satisfy = self.comparator.split(val, self.value)
        satisfy_part_range = (
            None if satisfy is None else replace(part_range, **{key: satisfy})
        )
        not_satisfy_part_range = (
            None if not_satisfy is None else replace(part_range, **{key: not_satisfy})
        )
        return satisfy_part_range, not_satisfy_part_range


@dataclass(frozen=True)
class Rule:
    destination: Union[str, Status]
    condition: Optional[Condition] = None

    def satisfy(self, part: Part) -> bool:
        return self.condition is None or self.condition.satisfy(part)

    def split(
        self, part_range: PartRange
    ) -> Tuple[Optional[PartRange], Optional[PartRange]]:
        if self.condition is None:
            return part_range, None
        else:
            return self.condition.split(part_range)


Workflows = Dict[str, List[Rule]]
Parts = List[Part]


def parse_workflows(workflows: str) -> Workflows:
    part_types = {
        "x": PartType.ExtremelyCoolLooking,
        "m": PartType.Musical,
        "a": PartType.Aerodynamic,
        "s": PartType.Shiny,
    }

    cmprs = {
        "<": Comparator.LessThan,
        ">": Comparator.GreaterThan,
    }

    def parse_condition(cond: str) -> Condition:
        part_type_str, cmpr, value_str = cond[0], cond[1], cond[2:]
        part_type = part_types[part_type_str]
        comparator = cmprs[cmpr]
        return Condition(part_type, comparator, int(value_str))

    def parse_dest(text: str) -> Union[str, Status]:
        if text == "A":
            return Status.Accepted
        elif text == "R":
            return Status.Rejected
        else:
            return text

    def parse_rule(rule: str) -> Rule:
        parts = rule.split(":")
        if len(parts) == 1:
            return Rule(parse_dest(rule))
        elif len(parts) == 2:
            cond = parse_condition(parts[0])
            dest = parse_dest(parts[1])
            return Rule(dest, cond)
        else:
            raise ValueError(f"Cannot parse into a rule: {rule=}!")

    def parse_workflow(line: str) -> Tuple[str, List[Rule]]:
        first_open_curly_bracket = line.find("{")
        workflow_id = line[:first_open_curly_bracket]
        rules = list(
            map(parse_rule, line[first_open_curly_bracket + 1 : -1].split(","))
        )
        return workflow_id, rules

    ret: Workflows = {}
    for line in workflows.split():
        workflow_id, rules = parse_workflow(line)
        ret[workflow_id] = rules
    return ret


def parse_parts(parts: str) -> Parts:
    def parse_part(part_str: str) -> Part:
        part_str = part_str[1:-1]
        return eval(f"Part({part_str})")

    return list(map(parse_part, parts.split("\n")))


def parse_system(text: str) -> Tuple[Workflows, Parts]:
    workflows_str, parts_str = text.split("\n\n")
    workflows = parse_workflows(workflows_str)
    parts = parse_parts(parts_str)
    return workflows, parts


def process_parts(workflows: Workflows, parts: Parts) -> Iterable[Tuple[Part, Status]]:
    def process_part(part: Part) -> Status:
        status: Union[str, Status] = "in"
        while not isinstance(status, Status):
            workflow_id: str = status
            rules = workflows.get(workflow_id, [])
            prev_workflow_id = workflow_id
            for rule in rules:
                if rule.satisfy(part):
                    status = rule.destination
                    if isinstance(status, str):
                        workflow_id = status
                    else:
                        return status
                    break

            if workflow_id == prev_workflow_id:
                raise ValueError(
                    f"No rule satisfied for {workflow_id=}, {rules=}, {part=}!"
                )
        return status

    for part in parts:
        yield part, process_part(part)


def solve_part_1(workflows: Workflows, parts: Parts) -> int:
    ret = 0
    for part, status in process_parts(workflows, parts):
        if status == Status.Accepted:
            ret += part.xmas
    return ret


def accepted_ranges(workflows: Workflows, part_range: PartRange) -> Iterable[PartRange]:
    def recur(status: Union[str, Status], part_range: PartRange) -> Iterable[PartRange]:
        if isinstance(status, Status):
            if status == Status.Accepted:
                yield part_range
                return
            else:
                return
        workflow_id: str = status
        rules = workflows.get(workflow_id, [])
        for rule in rules:
            satisfy, not_satisfy = rule.split(part_range)
            if satisfy is not None:
                yield from recur(rule.destination, satisfy)
            if not_satisfy is None:
                break
            part_range = not_satisfy

    yield from recur("in", part_range)


def solve_part_2(workflows: Workflows) -> int:
    ranges = accepted_ranges(
        workflows,
        PartRange(
            x=(1, 4000),
            m=(1, 4000),
            a=(1, 4000),
            s=(1, 4000),
        ),
    )
    return sum(rng.num_possibilities for rng in ranges)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--filepath", help="File path of the input", required=True)
    args = parser.parse_args()
    workflows, parts = parse_system(Path(args.filepath).read_text())
    print(f"Part 1: {solve_part_1(workflows, parts)}")
    print(f"Part 2: {solve_part_2(workflows)}")


if __name__ == "__main__":
    main()
