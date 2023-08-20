library(stringr)

print_help <- function() {
  print("Error: No args provided!")
  print("Usage: Rscript.exe main.R {FILEPATH}")
}

parse_instructions <- function(lines) {
  ret <- str_match_all(lines, "(turn on|turn off|toggle) (\\d+),(\\d+) through (\\d+),(\\d+)") # nolint: line_length_linter.
  ret <- as.data.frame(ret)
  colnames(ret) <- c(
    "line",
    "instruction_type",
    "top_left_x", "top_left_y",
    "bottom_right_x", "bottom_right_y"
  )
  ret$top_left_x     <- strtoi(ret$top_left_x)
  ret$top_left_y     <- strtoi(ret$top_left_y)
  ret$bottom_right_x <- strtoi(ret$bottom_right_x)
  ret$bottom_right_y <- strtoi(ret$bottom_right_y)
  ret
}

solve_part_1 <- function(instructions) {
  grid <- array(rep(0, 1e6), dim = c(1000, 1000))
  for (i in seq_len(nrow(instructions))) {
    row              <- instructions[i,]
    instruction_type <- row$instruction_type
    xl               <- row$top_left_x     + 1
    xu               <- row$bottom_right_x + 1
    yl               <- row$top_left_y     + 1
    yu               <- row$bottom_right_y + 1
    if (instruction_type == "turn on") {
      grid[xl:xu, yl:yu] <- 1
    } else if (instruction_type == "turn off") {
      grid[xl:xu, yl:yu] <- 0
    } else if (instruction_type == "toggle") {
      grid[xl:xu, yl:yu] <- bitwXor(grid[xl:xu, yl:yu], 1)
    }
  }
  sum(grid)
}

solve_part_2 <- function(instructions) {
  grid <- array(rep(0, 1e6), dim = c(1000, 1000))
  for (i in seq_len(nrow(instructions))) {
    row              <- instructions[i,]
    instruction_type <- row$instruction_type
    xl               <- row$top_left_x     + 1
    xu               <- row$bottom_right_x + 1
    yl               <- row$top_left_y     + 1
    yu               <- row$bottom_right_y + 1
    if (instruction_type == "turn on") {
      grid[xl:xu, yl:yu] <- grid[xl:xu, yl:yu] + 1
    } else if (instruction_type == "turn off") {
      grid[xl:xu, yl:yu] <- pmax(grid[xl:xu, yl:yu] - 1, 0)
    } else if (instruction_type == "toggle") {
      grid[xl:xu, yl:yu] <- grid[xl:xu, yl:yu] + 2
    }
  }
  sum(grid)
}

main <- function() {
  args <- commandArgs()
  if (length(args) > 4) {
    file_path <- args[[length(args)]]
    df_lines <- read.delim(file_path, header = FALSE, sep = "\n")
    colnames(df_lines) <- c("Lines")
    instructions <- parse_instructions(df_lines["Lines"])
    print(paste0("Part 1: ", solve_part_1(instructions)))
    print(paste0("Part 2: ", solve_part_2(instructions)))
  } else {
    print_help()
  }
}

main()
