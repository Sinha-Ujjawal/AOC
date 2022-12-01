use std::io::{BufRead, Write};

fn prompt(msg: &'static str) -> std::io::Result<String> {
    print!("{}", msg);
    let mut ret = String::new();
    std::io::stdout().flush()?;
    std::io::stdin().lock().read_line(&mut ret)?;
    Ok(ret.trim().to_string())
}

#[derive(Debug)]
enum MyError {
    IOError(std::io::Error),
    ParseError(String),
}

type Calorie = usize;
type Calories = Vec<Calorie>;
type ElvesCalories = Vec<Calories>;

fn read_elves_calories_from_file(filename: &str) -> Result<ElvesCalories, MyError> {
    std::fs::read_to_string(filename)
        .map_err(MyError::IOError)?
        .trim()
        .split("\n\n")
        .map(|group| {
            group
                .split("\n")
                .map(|s| s.parse().map_err(|_| MyError::ParseError(s.to_string())))
                .collect()
        })
        .collect()
}

fn solve_part1(elves_calories: &ElvesCalories) -> Calorie {
    elves_calories
        .into_iter()
        .map(|cals| cals.into_iter().sum())
        .max()
        .unwrap_or(0)
}

fn solve_part2(elves_calories: &ElvesCalories) -> Calorie {
    let mut calories = elves_calories
        .into_iter()
        .map(|cals| cals.into_iter().sum())
        .collect::<Vec<Calorie>>();
    calories.sort();
    calories.reverse();
    calories.into_iter().take(3).sum()
}

fn main() -> Result<(), MyError> {
    let filename = prompt("Enter file name: ").map_err(MyError::IOError)?;
    let elves_calories = read_elves_calories_from_file(&filename)?;
    let part1_ans = solve_part1(&elves_calories);
    println!("Part 1: {}", part1_ans);
    let part2_ans = solve_part2(&elves_calories);
    println!("Part 1: {}", part2_ans);
    Ok(())
}
