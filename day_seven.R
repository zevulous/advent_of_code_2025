# ============================================================================
# ADVENT OF CODE - DAY 7
# Problem: Tachyon beam simulation through a grid
# Part 1: Count how many times the beam splits (hits '^' splitters)
# Part 2: Count total distinct timelines created by all splits
# ============================================================================

options(scipen = 999) # Avoid scientific notation in output

# ============================================================================
# PART ONE: Count beam splits
# Input file format:
#   - Character grid where each cell is one character
#   - 'S' marks the starting position(s) in the first row
#   - '.' allows the beam to pass through unchanged
#   - '^' splits the beam diagonally left AND right (beam doesn't continue straight)
# ============================================================================

# --- Read and parse the input file into a character grid ---
lines <- readLines('day_seven_test_input.txt')
n_lines <- length(lines)

# Split each line into individual characters and create a dataframe
df <- as.data.frame(do.call(rbind, strsplit(lines, '')))
colnames(df) <- paste0('C', 1:ncol(df))

# --- Initialize beam tracking ---
# Boolean vector: TRUE if a tachyon beam is present in that column
tachyon_check <- rep(FALSE, ncol(df))
tachyon_check[which(df[1,] == 'S')] <- TRUE  # Start where 'S' appears in row 1

split_counter <- 0  # Track total number of splits

# --- Simulate beam movement row by row ---
for (i in 2:nrow(df)){
  
  row_of_interest <- df[i,]
  
  for (j in 1:ncol(df)){
    
    if (tachyon_check[j] & df[i, j] == '.'){
      # Beam passes through empty space - continues in same column
      tachyon_check[j] <- TRUE
    } else if (tachyon_check[j] & df[i, j] == '^'){
      # Beam hits splitter - diverges left and right, original path ends
      tachyon_check[j - 1] <- TRUE
      tachyon_check[j + 1] <- TRUE
      tachyon_check[j] <- FALSE
      split_counter <- split_counter + 1
    } else {
      # No beam here or beam is blocked - nothing to do
      next
    }
  }
}

split_counter  # Answer for Part One: total number of splits

# ============================================================================
# PART TWO: Count total distinct timelines
#
# Challenge: Each split creates new timelines that can themselves split
# Solution: Instead of tracking presence (TRUE/FALSE), track COUNT of timelines
#           at each column position
#
# Key insight: When a timeline hits a splitter, it creates two new timelines
#              (one left, one right). Multiple timelines in the same column
#              can accumulate and each will split independently.
#
# Example: If 3 timelines hit a '^', we get 3 timelines going left AND
#          3 timelines going right (6 total, but grouped by position)
# ============================================================================

# --- Initialize timeline counting ---
# Integer vector: count of timelines present in each column
timeline_count <- rep(0, ncol(df))
timeline_count[which(df[1,] == 'S')] <- 1  # Start with 1 timeline at 'S'

# --- Simulate timeline propagation row by row ---
for (i in 2:nrow(df)){
  
  # Fresh count for this row (timelines don't persist if blocked)
  new_count <- rep(0, ncol(df))
  
  for (j in 1:ncol(df)){
    if (timeline_count[j] > 0){  # Only process columns with active timelines
      if (df[i, j] == '.'){
        # Empty space - all timelines continue straight down
        new_count[j] <- new_count[j] + timeline_count[j]
      } else if (df[i, j] == '^'){
        # Splitter - all timelines split left and right
        # Boundary checks prevent index out of bounds errors
        if (j > 1) new_count[j - 1] <- new_count[j - 1] + timeline_count[j]
        if (j < ncol(df)) new_count[j + 1] <- new_count[j + 1] + timeline_count[j]
      }
      # Note: if cell is anything else, timelines are blocked (not added to new_count)
    }
  }
  
  # Update timeline positions for next row
  timeline_count <- new_count
}

sum(timeline_count)  # Answer for Part Two: total surviving timelines