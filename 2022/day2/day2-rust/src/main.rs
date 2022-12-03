use std::io::{BufRead, Write};

fn prompt(msg: &'static str) -> std::io::Result<String> {
    let mut ret = String::new();
    print!("{}", msg);
    std::io::stdout().flush()?;
    std::io::stdin().lock().read_line(&mut ret)?;
    Ok(ret.trim().to_string())
}

#[derive(Debug)]
enum MyError {
    IOError(std::io::Error),
    ParseError(String),
}

#[derive(PartialEq, Copy, Clone)]
enum Hand {
    Rock,
    Paper,
    Scissor,
}

#[derive(PartialEq, Copy, Clone)]
enum Outcome {
    Won,
    Lost,
    Tie,
}

struct Play {
    opponent: Hand,
    you: Hand,
}

trait Score {
    fn score(&self) -> usize;
}

impl From<&Play> for Outcome {
    fn from(play: &Play) -> Self {
        match (&play.you, &play.opponent) {
            (Hand::Rock, Hand::Scissor) => Outcome::Won,
            (Hand::Paper, Hand::Rock) => Outcome::Won,
            (Hand::Scissor, Hand::Paper) => Outcome::Won,
            (you, opponent) if you == opponent => Outcome::Tie,
            _ => Outcome::Lost,
        }
    }
}

impl Score for Outcome {
    fn score(&self) -> usize {
        match self {
            Outcome::Won => 6,
            Outcome::Tie => 3,
            Outcome::Lost => 0,
        }
    }
}

impl Hand {
    fn decide(&self, outcome: &Outcome) -> Self {
        match (self, outcome) {
            (Hand::Rock, Outcome::Lost) => Hand::Scissor,
            (Hand::Paper, Outcome::Lost) => Hand::Rock,
            (Hand::Scissor, Outcome::Lost) => Hand::Paper,
            (Hand::Rock, Outcome::Won) => Hand::Paper,
            (Hand::Paper, Outcome::Won) => Hand::Scissor,
            (Hand::Scissor, Outcome::Won) => Hand::Rock,
            (x, Outcome::Tie) => *x,
        }
    }
}

impl Score for Hand {
    fn score(&self) -> usize {
        match self {
            Hand::Rock => 1,
            Hand::Paper => 2,
            Hand::Scissor => 3,
        }
    }
}

impl Score for Play {
    fn score(&self) -> usize {
        self.you.score() + Outcome::from(self).score()
    }
}

impl Score for Vec<Play> {
    fn score(&self) -> usize {
        self.into_iter().map(|p| p.score()).sum()
    }
}

fn solve_part1(s: &str) -> Result<usize, MyError> {
    use Hand::*;
    let plays = s
        .trim()
        .split("\n")
        .into_iter()
        .map(|s| match s {
            "A X" => Ok(Play {
                opponent: Rock,
                you: Rock,
            }),
            "A Y" => Ok(Play {
                opponent: Rock,
                you: Paper,
            }),
            "A Z" => Ok(Play {
                opponent: Rock,
                you: Scissor,
            }),

            "B X" => Ok(Play {
                opponent: Paper,
                you: Rock,
            }),
            "B Y" => Ok(Play {
                opponent: Paper,
                you: Paper,
            }),
            "B Z" => Ok(Play {
                opponent: Paper,
                you: Scissor,
            }),

            "C X" => Ok(Play {
                opponent: Scissor,
                you: Rock,
            }),
            "C Y" => Ok(Play {
                opponent: Scissor,
                you: Paper,
            }),
            "C Z" => Ok(Play {
                opponent: Scissor,
                you: Scissor,
            }),

            _ => Err(MyError::ParseError(s.to_string())),
        })
        .collect::<Result<Vec<Play>, MyError>>()?;
    Ok(plays.score())
}

fn solve_part2(s: &str) -> Result<usize, MyError> {
    use Hand::*;
    use Outcome::*;
    let plays = s
        .trim()
        .split("\n")
        .into_iter()
        .map(|s| match s {
            "A X" => Ok(Play {
                opponent: Rock,
                you: Rock.decide(&Lost),
            }),
            "A Y" => Ok(Play {
                opponent: Rock,
                you: Rock.decide(&Tie),
            }),
            "A Z" => Ok(Play {
                opponent: Rock,
                you: Rock.decide(&Won),
            }),

            "B X" => Ok(Play {
                opponent: Paper,
                you: Paper.decide(&Lost),
            }),
            "B Y" => Ok(Play {
                opponent: Paper,
                you: Paper.decide(&Tie),
            }),
            "B Z" => Ok(Play {
                opponent: Paper,
                you: Paper.decide(&Won),
            }),

            "C X" => Ok(Play {
                opponent: Scissor,
                you: Scissor.decide(&Lost),
            }),
            "C Y" => Ok(Play {
                opponent: Scissor,
                you: Scissor.decide(&Tie),
            }),
            "C Z" => Ok(Play {
                opponent: Scissor,
                you: Scissor.decide(&Won),
            }),

            _ => Err(MyError::ParseError(s.to_string())),
        })
        .collect::<Result<Vec<Play>, MyError>>()?;
    Ok(plays.score())
}

fn main() -> Result<(), MyError> {
    let filepath = prompt("Enter file path: ").map_err(MyError::IOError)?;
    let contents = std::fs::read_to_string(filepath).map_err(MyError::IOError)?;
    let part1_ans = solve_part1(&contents)?;
    println!("Part 1: {}", part1_ans);
    let part2_ans = solve_part2(&contents)?;
    println!("Part 2: {}", part2_ans);
    Ok(())
}
