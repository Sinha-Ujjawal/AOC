use std::io::{BufRead, Write};

fn prompt(msg: &'static str) -> std::io::Result<String> {
    let mut ret = String::new();
    print!("{}", msg);
    std::io::stdout().flush()?;
    std::io::stdin().lock().read_line(&mut ret)?;
    Ok(ret.trim_end().to_string())
}

#[derive(Debug)]
enum MyError {
    IOError(std::io::Error),
}

fn main() -> Result<(), MyError> {
    let file_path = prompt("Enter file path: ").map_err(MyError::IOError)?;
    let contents = std::fs::read_to_string(&file_path).map_err(MyError::IOError)?;
    println!("{}", contents);
    Ok(())
}
