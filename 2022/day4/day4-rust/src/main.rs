use std::io::{BufRead, Write};

fn prompt(msg: &'static str) -> std::io::Result<String> {
    let mut ret = String::new();
    print!("{}", msg);
    std::io::stdout().flush()?;
    std::io::stdin().lock().read_line(&mut ret)?;
    Ok(ret.trim_end().to_string())
}

#[derive(Debug)]
#[allow(dead_code)]
struct ParseErrorInfo {
    line: String,
    line_no: usize,
}

#[derive(Debug)]
enum MyError {
    IOError(std::io::Error),
    ParseError(ParseErrorInfo),
}

#[derive(Debug)]
struct Interval {
    start: usize,
    end: usize,
}

impl Interval {
    fn parse(s: &str) -> Option<Interval> {
        let (start_str, end_str) = s.split_once("-")?;
        let start = start_str.parse().ok()?;
        let end = end_str.parse().ok()?;
        Some(Interval { start, end })
    }
}

fn parse_intervals(s: &str) -> Result<Vec<(Interval, Interval)>, MyError> {
    s.lines()
        .into_iter()
        .enumerate()
        .map(|(line_no, line)| {
            let err = || {
                MyError::ParseError(ParseErrorInfo {
                    line_no,
                    line: line.to_string(),
                })
            };
            if let Some((left_interval_str, right_interval_str)) = line.split_once(",") {
                let left_interval = Interval::parse(left_interval_str).ok_or(err())?;
                let right_interval = Interval::parse(right_interval_str).ok_or(err())?;
                Ok((left_interval, right_interval))
            } else {
                Err(err())
            }
        })
        .collect()
}

fn solve_part_1(intervals: &Vec<(Interval, Interval)>) -> usize {
    intervals
        .into_iter()
        .filter(
            |(Interval { start: ll, end: lh }, Interval { start: rl, end: rh })| {
                ll <= lh && rl <= rh && ((rl >= ll && rh <= lh) || (ll >= rl && lh <= rh))
            },
        )
        .count()
}

fn solve_part_2(intervals: &Vec<(Interval, Interval)>) -> usize {
    intervals
        .into_iter()
        .filter(
            |(Interval { start: ll, end: lh }, Interval { start: rl, end: rh })| {
                ll <= lh && rl <= rh && ((ll <= rh && rl <= lh) || (rl <= lh && ll <= rh))
            },
        )
        .count()
}

fn main() -> Result<(), MyError> {
    let file_path = prompt("Enter file path: ").map_err(MyError::IOError)?;
    let contents = std::fs::read_to_string(&file_path).map_err(MyError::IOError)?;
    let intervals = parse_intervals(&contents)?;
    println!("Part 1: {}", solve_part_1(&intervals));
    println!("Part 2: {}", solve_part_2(&intervals));
    Ok(())
}
