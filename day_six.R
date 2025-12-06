# ============================================================================
# ADVENT OF CODE - DAY 6
# Problem: Cephalopod math operations
# Part 1: Transpose input matrix, then sum or multiply each row based on operator
# Part 2: Account for column alignment (left vs right) when reading digits
#         vertically in reverse order
# ============================================================================

library(tidyverse)
options(scipen = 999) # Avoid scientific notation in output

# ============================================================================
# PART ONE: Basic cephalopod math (transposed operations)
# Input file format:
#   - Space-separated values in rows
#   - Last column contains operator ('+' or '*')
# The trick: transpose the matrix, then apply row operations
# ============================================================================

# --- Read and parse input ---
lines <- readLines('day_six_input.txt')
lines <- trimws(lines) # Remove leading/trailing whitespace
lines <- strsplit(lines, " +") # Split by one or more spaces

# --- Build and transpose the data frame ---
df <- as.data.frame(do.call(rbind, lines))
df <- t(df) # Cephalopod math = transposed matrix
df <- as.data.frame(df)

# Convert all columns except the last (operator) to numeric
df[, 1:(ncol(df) - 1)] <- lapply(df[, 1:(ncol(df) - 1)], as.numeric)

# --- Process each row: sum or multiply based on operator ---
running_total <- 0

for (i in 1:nrow(df)) {
  values <- df[i, 1:(ncol(df) - 1)] # All values except operator

  if (df[i, ncol(df)] == '+') {
    row_total <- sum(values)
  } else {
    row_total <- prod(values)
  }

  running_total <- running_total + row_total
}

running_total # Answer for Part One


# ============================================================================
# PART TWO: Advanced cephalopod math with positional alignment
#
# Challenge: Column alignment affects how digits are read vertically
# - Right-aligned columns: digits align from the right
# - Left-aligned columns: digits align from the left
#
# Process:
# 1. Detect token positions (start/end) to determine alignment
# 2. For each column, reverse digits and pad based on alignment
# 3. Read digits vertically to form new numbers
# 4. Apply operator (sum or product) to those numbers
#
# Example verification:
#   Column V1: 356 * 24 * 1 = 8544
#   Column V2: 8 + 248 + 369 = 625
# ============================================================================

lines <- readLines('day_six_input.txt')
# NOTE: Don't trim whitespace yet - we need position info for alignment detection

# --- Helper function: Find start/end positions of each token in a line ---
find_token_positions <- function(line) {
  m <- gregexpr("\\S+", line)[[1]] # Match all non-whitespace sequences

  # Handle empty lines

  if (m[1] == -1)
    return(data.frame(
      start = integer(),
      end = integer(),
      value = character(),
      stringsAsFactors = FALSE
    ))

  starts <- as.integer(m)
  lengths <- attr(m, "match.length")
  values <- substring(line, starts, starts + lengths - 1)

  data.frame(
    start = starts,
    end = starts + lengths - 1,
    value = values,
    stringsAsFactors = FALSE
  )
}

# --- Extract token positions for all lines ---
tokens_list <- lapply(lines, find_token_positions)

# --- Build dataframe of values (ignoring positions for now) ---
df <- as.data.frame(do.call(rbind, lapply(tokens_list, function(x) x$value)))

# --- Determine alignment for each column ---
# Compare start positions across rows (excluding the operator row)
# If all starts are identical → left-aligned
# Otherwise → right-aligned
n_cols <- ncol(df)
num_lines <- length(lines) - 1 # Exclude operator row

alignment <- character(n_cols)

for (col in 1:n_cols) {
  starts <- sapply(1:num_lines, function(row) tokens_list[[row]]$start[col])
  ends <- sapply(1:num_lines, function(row) tokens_list[[row]]$end[col])

  if (length(unique(starts)) == 1) {
    alignment[col] <- "left"
  } else {
    alignment[col] <- "right"
  }
}

# Append alignment info as a new row in the dataframe
df <- rbind(df, alignment)

# --- Process each column with alignment-aware digit extraction ---
running_total <- 0

for (col in 1:ncol(df)) {
  v <- df[[col]]
  v <- strsplit(v, "") # Split each value into individual characters

  # Keep alignment indicator as a single string (not split)
  v[[length(v)]] <- paste(v[[length(v)]], collapse = "")

  # Find the maximum digit length for padding
  max_length <- max(sapply(v, length))

  # --- Reverse digits and pad based on alignment ---
  # Process all rows except operator row and alignment row
  for (i in 1:(length(v) - 2)) {
    v[[i]] <- rev(v[[i]]) # Reverse digit order (right-to-left reading)

    # Pad with 'x' placeholders to align digits properly
    while (length(v[[i]]) < max_length) {
      if (v[[length(v)]] == "right") {
        v[[i]] <- c(v[[i]], "x") # Right-aligned: pad at end
      } else {
        v[[i]] <- c("x", v[[i]]) # Left-aligned: pad at start
      }
    }
  }

  # --- Read digits vertically to form new numbers ---
  values <- c()

  for (i in 1:max_length) {
    # Extract the i-th digit from each value (excluding operator and alignment rows)
    digits <- sapply(v[1:(length(v) - 2)], function(x) x[i])

    # Remove placeholder 'x' values and combine remaining digits
    digit_value <- as.numeric(paste(digits[digits != "x"], collapse = ""))
    values <- c(values, digit_value)
  }

  # --- Apply operator (from the second-to-last row) ---
  if (v[[length(v) - 1]][1] == '+') {
    row_total <- sum(values)
  } else {
    row_total <- prod(values)
  }

  running_total <- running_total + row_total

  ## Debug output: column index, extracted values, and column total
  #cat(paste(col, values, row_total, '\n'))
}

running_total # Answer for Part Two
