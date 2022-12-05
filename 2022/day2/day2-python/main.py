from typing import List
from dataclasses import dataclass
from enum import IntEnum, auto


class Outcome(IntEnum):
    Won = auto()
    Lost = auto()
    Tie = auto()


class Hand(IntEnum):
    Rock = auto()
    Paper = auto()
    Scissor = auto()

    def decide(self, outcome: Outcome) -> "Hand":
        if self == Hand.Rock and outcome == Outcome.Lost:
            return Hand.Scissor
        elif self == Hand.Paper and outcome == Outcome.Lost:
            return Hand.Rock
        elif self == Hand.Scissor and outcome == Outcome.Lost:
            return Hand.Paper
        elif self == Hand.Rock and outcome == Outcome.Won:
            return Hand.Paper
        elif self == Hand.Paper and outcome == Outcome.Won:
            return Hand.Scissor
        elif self == Hand.Scissor and outcome == Outcome.Won:
            return Hand.Rock
        else:  # outcome == Outcome.Tie
            return self


@dataclass(frozen=True)
class Play:
    opponent: Hand
    you: Hand

    @property
    def outcome(self) -> Outcome:
        p = (self.you, self.opponent)
        if (
            (p == (Hand.Rock, Hand.Scissor))
            or (p == (Hand.Scissor, Hand.Paper))
            or (p == (Hand.Paper, Hand.Rock))
        ):
            return Outcome.Won
        elif self.you == self.opponent:
            return Outcome.Tie
        else:
            return Outcome.Lost


def score_plays(plays: List[Play]) -> int:
    def score(play: Play) -> int:
        return score_hand(play.you) + score_outcome(play.outcome)

    def score_hand(hand: Hand) -> int:
        if hand == Hand.Rock:
            return 1
        elif hand == Hand.Paper:
            return 2
        elif hand == Hand.Scissor:
            return 3
        else:
            raise ValueError(f"Unreachable!, given: {hand}")

    def score_outcome(outcome: Outcome) -> int:
        if outcome == Outcome.Won:
            return 6
        elif outcome == Outcome.Tie:
            return 3
        elif outcome == Outcome.Lost:
            return 0
        else:
            raise ValueError(f"Unreachable!, given: {outcome}")

    return sum(map(score, plays))


def solve_part1(s: str) -> int:
    def parse_line(line: str) -> Play:
        if line == "A X":
            return Play(opponent=Hand.Rock, you=Hand.Rock)
        elif line == "A Y":
            return Play(opponent=Hand.Rock, you=Hand.Paper)
        elif line == "A Z":
            return Play(opponent=Hand.Rock, you=Hand.Scissor)
        elif line == "B X":
            return Play(opponent=Hand.Paper, you=Hand.Rock)
        elif line == "B Y":
            return Play(opponent=Hand.Paper, you=Hand.Paper)
        elif line == "B Z":
            return Play(opponent=Hand.Paper, you=Hand.Scissor)
        elif line == "C X":
            return Play(opponent=Hand.Scissor, you=Hand.Rock)
        elif line == "C Y":
            return Play(opponent=Hand.Scissor, you=Hand.Paper)
        elif line == "C Z":
            return Play(opponent=Hand.Scissor, you=Hand.Scissor)
        else:
            raise ValueError(f"Unable to parse the line: {line}")

    def parse_plays(s: str) -> List[Play]:
        return list(map(parse_line, s.strip().split("\n")))

    return score_plays(parse_plays(s))


def solve_part2(s: str) -> int:
    def parse_line(line: str) -> Play:
        if line == "A X":
            return Play(opponent=Hand.Rock, you=Hand.Rock.decide(Outcome.Lost))
        elif line == "A Y":
            return Play(opponent=Hand.Rock, you=Hand.Rock.decide(Outcome.Tie))
        elif line == "A Z":
            return Play(opponent=Hand.Rock, you=Hand.Rock.decide(Outcome.Won))
        elif line == "B X":
            return Play(opponent=Hand.Paper, you=Hand.Paper.decide(Outcome.Lost))
        elif line == "B Y":
            return Play(opponent=Hand.Paper, you=Hand.Paper.decide(Outcome.Tie))
        elif line == "B Z":
            return Play(opponent=Hand.Paper, you=Hand.Paper.decide(Outcome.Won))
        elif line == "C X":
            return Play(opponent=Hand.Scissor, you=Hand.Scissor.decide(Outcome.Lost))
        elif line == "C Y":
            return Play(opponent=Hand.Scissor, you=Hand.Scissor.decide(Outcome.Tie))
        elif line == "C Z":
            return Play(opponent=Hand.Scissor, you=Hand.Scissor.decide(Outcome.Won))
        else:
            raise ValueError(f"Unable to parse the line: {line}")

    def parse_plays(s: str) -> List[Play]:
        return list(map(parse_line, s.strip().split("\n")))

    return score_plays(parse_plays(s))


def main():
    file_name = input("Enter file path: ")
    contents = open(file_name, "r").read()
    part1_ans = solve_part1(contents)
    print(f"Part 1: {part1_ans}")
    part2_ans = solve_part2(contents)
    print(f"Part 2: {part2_ans}")


if __name__ == "__main__":
    main()
