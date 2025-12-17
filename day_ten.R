# Factory Puzzle Solver

# Read input from file
input_lines <- readLines("day_ten_input.txt")

# Remove empty lines
input_lines <- input_lines[nchar(trimws(input_lines)) > 0]

total_presses <- 0
n_machines <- length(input_lines)

cat(sprintf("Processing %d machines...\n", n_machines))
pb <- txtProgressBar(min = 0, max = n_machines, style = 3)

for (i in seq_along(input_lines)) {
  line <- input_lines[i]
  
  # Extract target pattern from [brackets]
  target_match <- regmatches(line, regexpr("\\[[.#]+\\]", line))
  target_str <- gsub("\\[|\\]", "", target_match)
  target <- as.integer(strsplit(target_str, "")[[1]] == "#")
  n_lights <- length(target)
  
  # Extract button schematics from (parentheses)
  button_matches <- regmatches(line, gregexpr("\\([0-9,]+\\)", line))[[1]]
  n_buttons <- length(button_matches)
  
  # Convert each button to a binary vector
  button_vectors <- list()
  for (j in seq_along(button_matches)) {
    nums_str <- gsub("\\(|\\)", "", button_matches[j])
    indices <- as.integer(strsplit(nums_str, ",")[[1]])
    vec <- rep(0L, n_lights)
    vec[indices + 1] <- 1L
    button_vectors[[j]] <- vec
  }
  
  # Try all subsets, starting from smallest
  min_presses <- Inf
  
  for (num_pressed in 0:n_buttons) {
    if (num_pressed >= min_presses) break
    
    if (num_pressed == 0) {
      combos <- list(integer(0))
    } else {
      combos <- combn(1:n_buttons, num_pressed, simplify = FALSE)
    }
    
    for (pressed in combos) {
      if (length(pressed) == 0) {
        effect <- rep(0L, n_lights)
      } else {
        effect <- rep(0L, n_lights)
        for (p in pressed) {
          effect <- (effect + button_vectors[[p]]) %% 2
        }
      }
      
      if (all(effect == target)) {
        min_presses <- num_pressed
        break
      }
    }
    
    if (!is.infinite(min_presses) && min_presses == num_pressed) break
  }
  
  total_presses <- total_presses + min_presses
  setTxtProgressBar(pb, i)
}

close(pb)
cat(sprintf("\nTotal: %d\n", total_presses))


# Factory Puzzle - Part 2
# Integer Linear Programming approach

library(lpSolve)

# Read input from file
input_lines <- readLines("day_ten_input.txt")

# Remove empty lines
input_lines <- input_lines[nchar(trimws(input_lines)) > 0]

total_presses <- 0
n_machines <- length(input_lines)

cat(sprintf("Processing %d machines...\n", n_machines))
pb <- txtProgressBar(min = 0, max = n_machines, style = 3)

for (i in seq_along(input_lines)) {
  line <- input_lines[i]
  
  # Extract button schematics from (parentheses)
  button_matches <- regmatches(line, gregexpr("\\([0-9,]+\\)", line))[[1]]
  n_buttons <- length(button_matches)
  
  # Extract joltage requirements from {curly braces}
  joltage_match <- regmatches(line, regexpr("\\{[0-9,]+\\}", line))
  joltage_str <- gsub("\\{|\\}", "", joltage_match)
  targets <- as.integer(strsplit(joltage_str, ",")[[1]])
  n_counters <- length(targets)
  
  # Build constraint matrix A (n_counters x n_buttons)
  # A[i,j] = 1 if button j affects counter i
  A <- matrix(0L, nrow = n_counters, ncol = n_buttons)
  
  for (j in seq_along(button_matches)) {
    nums_str <- gsub("\\(|\\)", "", button_matches[j])
    indices <- as.integer(strsplit(nums_str, ",")[[1]])
    A[indices + 1, j] <- 1L
  }
  
  # Solve ILP:
  # minimize: sum(x)
  # subject to: A * x = targets
  # x >= 0, integer
  
  result <- lp(direction = "min",
               objective.in = rep(1, n_buttons),
               const.mat = A,
               const.dir = rep("=", n_counters),
               const.rhs = targets,
               all.int = TRUE)
  
  if (result$status == 0) {
    total_presses <- total_presses + result$objval
  } else {
    cat(sprintf("\nMachine %d: NO SOLUTION FOUND!\n", i))
  }
  
  setTxtProgressBar(pb, i)
}

close(pb)
cat(sprintf("\nTotal: %d\n", total_presses))