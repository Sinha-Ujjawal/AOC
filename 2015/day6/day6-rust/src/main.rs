use std::io::Write;

fn prompt(msg: &'static str) -> String {
    print!("{}", msg);
    std::io::stdout().flush().expect("Flushing STDOUT failed!");
    let mut out = String::new();
    std::io::stdin()
        .read_line(&mut out)
        .expect("Failed to read a line from STDIN");
    out.trim().to_string()
}

#[derive(Debug, Clone)]
struct Coord {
    row: usize,
    col: usize,
}

impl Coord {
    pub fn new(row: usize, col: usize) -> Self {
        Self { row, col }
    }

    pub fn parse(s: &str) -> Option<Self> {
        let (x_str, y_str) = s.split_once(",")?;
        let x = x_str.parse().ok()?;
        let y = y_str.parse().ok()?;
        Some(Self::new(x, y))
    }
}

#[derive(Debug, Clone)]
struct Rect(Coord, Coord);

impl Rect {
    pub fn parse(s: &str) -> Option<Self> {
        let (start_coord_str, end_coord_str) = s.split_once(" through ")?;
        let start_coord = Coord::parse(start_coord_str)?;
        let end_coord = Coord::parse(end_coord_str)?;
        Some(Rect(start_coord, end_coord))
    }
}

struct LightGrid {
    grid: Vec<u64>,
}

type UpdateFn = fn(u64) -> u64;

impl LightGrid {
    pub fn new() -> Self {
        Self {
            grid: Vec::from_iter(std::iter::repeat(0).take(1000 * 1000)),
        }
    }

    pub fn update(&mut self, update_fn: UpdateFn, rect: &Rect) {
        (rect.0.row..(rect.1.row + 1)).for_each(|row| {
            (rect.0.col..(rect.1.col + 1)).for_each(|col| {
                let idx = row * 1000 + col;
                self.grid[idx] = std::cmp::max(0, update_fn(self.grid[idx]))
            })
        })
    }
}

#[derive(Debug, Clone)]
enum Instruction {
    TurnOn(Rect),
    TurnOff(Rect),
    Toggle(Rect),
}

impl Instruction {
    pub fn from_line(line: &str) -> Option<Self> {
        use Instruction::*;
        if let Some(rest) = line.strip_prefix("turn on ") {
            Rect::parse(rest).map(TurnOn)
        } else if let Some(rest) = line.strip_prefix("turn off ") {
            Rect::parse(rest).map(TurnOff)
        } else if let Some(rest) = line.strip_prefix("toggle ") {
            Rect::parse(rest).map(Toggle)
        } else {
            None
        }
    }

    pub fn from_lines(lines: core::str::Lines) -> Option<Vec<Self>> {
        lines.into_iter().map(|s| Self::from_line(&s)).collect()
    }
}

fn solve(
    instructions: &Vec<Instruction>,
    turn_on_update_fn: UpdateFn,
    turn_off_update_fn: UpdateFn,
    toggle_update_fn: UpdateFn,
) -> LightGrid {
    let mut grid = LightGrid::new();
    instructions.into_iter().for_each(|instruction| {
        use Instruction::*;
        match instruction {
            TurnOn(rect) => grid.update(turn_on_update_fn, rect),
            TurnOff(rect) => grid.update(turn_off_update_fn, rect),
            Toggle(rect) => grid.update(toggle_update_fn, rect),
        }
    });
    grid
}

fn solve_part_1(instructions: &Vec<Instruction>) -> usize {
    let turn_on_update_fn = |_| 1;
    let turn_off_update_fn = |_| 0;
    let toggle_update_fn = |b| b ^ 1;
    solve(
        instructions,
        turn_on_update_fn,
        turn_off_update_fn,
        toggle_update_fn,
    )
    .grid
    .into_iter()
    .filter(|x| *x > 0)
    .count()
}

fn solve_part_2(instructions: &Vec<Instruction>) -> u64 {
    let turn_on_update_fn = |b| b + 1;
    let turn_off_update_fn = |b| if b > 0 { b - 1 } else { 0 };
    let toggle_update_fn = |b| b + 2;
    solve(
        instructions,
        turn_on_update_fn,
        turn_off_update_fn,
        toggle_update_fn,
    )
    .grid
    .into_iter()
    .filter(|x| *x > 0)
    .sum()
}

fn main() {
    let file_name = prompt("Enter file name: ");
    let file_contents =
        std::fs::read_to_string(&file_name).expect(&format!("Unable to read file: {}", file_name));
    let instructions = Instruction::from_lines(file_contents.lines())
        .expect(&format!("Failed parsing the file: {}", file_name));
    let part1_ans = solve_part_1(&instructions);
    let part2_ans = solve_part_2(&instructions);
    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
}
