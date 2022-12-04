use itertools::Itertools;
use std::collections::HashSet;
use std::io::{BufRead, Write};

fn prompt(msg: &'static str) -> std::io::Result<String> {
    let mut ret = String::new();
    print!("{}", msg);
    std::io::stdout().flush()?;
    std::io::stdin().lock().read_line(&mut ret)?;
    Ok(ret.trim().to_string())
}

trait Priority {
    fn priority(&self) -> usize;
}

impl Priority for char {
    fn priority(&self) -> usize {
        if !self.is_alphabetic() {
            return 0;
        }
        if self.is_lowercase() {
            return (u32::from(*self) - u32::from('a') + 1) as usize;
        }
        return (u32::from(*self) - u32::from('A') + 27) as usize;
    }
}

impl Priority for HashSet<char> {
    fn priority(&self) -> usize {
        self.into_iter().map(|c| c.priority()).sum()
    }
}

fn solve<'a, I>(rust_sacks: I, group_size: usize) -> usize
where
    I: Iterator<Item = &'a str>,
{
    rust_sacks
        .chunks(group_size)
        .into_iter()
        .map(|group| {
            group
                .map(|s| HashSet::from_iter(s.chars()))
                .reduce(|accum, item| accum.intersection(&item).map(|c| *c).collect())
                .map(|hset| hset.priority())
                .unwrap_or(0)
        })
        .sum()
}

fn solve_part1(string: &str) -> usize {
    let rust_sacks = string.lines().into_iter().flat_map(|line| {
        let n = line.len();
        [&line[0..n >> 1], &line[n >> 1..]]
    });
    solve(rust_sacks, 2)
}

fn solve_part2(string: &str) -> usize {
    let rust_sacks = string.lines().into_iter();
    solve(rust_sacks, 3)
}

fn main() -> std::io::Result<()> {
    let file_path = prompt("Enter file path: ")?;
    let contents = std::fs::read_to_string(file_path)?;
    let part1_ans = solve_part1(&contents);
    println!("Part 1: {}", part1_ans);
    let part2_ans = solve_part2(&contents);
    println!("Part 2: {}", part2_ans);
    Ok(())
}
