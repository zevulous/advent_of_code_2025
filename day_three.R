# ============================================================================
# ADVENT OF CODE - DAY 3
# Problem: Extract the largest possible number from a string of digits
# by selecting digits in order (maintaining relative position)
# ============================================================================

# PART ONE: Build the largest 2-digit number from each line
# Rule: Pick the largest digit for the tens place, then pick the largest
# remaining digit (to the right of it) for the ones place
# ============================================================================

# Read input - each line is a string of digits
input_file <- "day_three_input.txt"
lines <- readLines(input_file)
lines <- trimws(lines)  # Remove any leading/trailing whitespace

# Helper function: Split a string into chunks of a given size
# For this problem, we use chunk_size=1 to get individual characters
split_chunks <- function(x, chunk_size = 2) {
  starts <- seq(1, nchar(x), by = chunk_size)
  ends <- pmin(starts + chunk_size - 1, nchar(x))
  substring(x, starts, ends)
}

# Initialize vector to store the "joltage" value for each line
joltages <- rep(NA, length(lines))

for (i in 1:length(lines)) {
  test <- lines[i]
  
  # Split the string into individual digit characters
  split_test <- split_chunks(test, chunk_size = 1)
  
  # TENS PLACE: Find the maximum digit, but NOT from the last position
  
  # (we need to leave at least one digit for the ones place)
  # Example: "3142" -> search positions 1-3 ("3","1","4") -> max is 4
  tens_place <- max(as.numeric(split_test[1:(nchar(test) - 1)]))
  
  # Find WHERE that maximum digit occurs (leftmost occurrence)
  max_location <- which.max(as.numeric(split_test[1:(nchar(test) - 1)]))
  
  # ONES PLACE: Find the maximum digit AFTER the tens place position
  # Example: If max was at position 3, search from position 4 onwards
  ones_place <- max(as.numeric(split_test[(max_location + 1):length(split_test)]))
  
  # Combine into a two-digit number
  # Example: tens=4, ones=2 -> 42
  joltages[i] <- as.numeric(paste0(tens_place, ones_place))
}

# Answer is the sum of all joltages
sum(joltages)


# ============================================================================
# PART TWO: Build the largest 12-digit number from each line
# This generalizes Part One using a greedy algorithm
# 
# Key insight: To build the largest k-digit subsequence:
#   - For digit 1: pick the largest digit that leaves enough room for k-1 more
#   - For digit 2: pick the largest digit after that, leaving room for k-2 more
#   - Continue until all k digits are selected
#
# The "valid range" shrinks as we go - we can't pick a digit so late in the
# string that we can't fit the remaining digits after it
# ============================================================================

for (i in 1:length(lines)) {
  test <- lines[i]
  
  # Split into individual digit characters
  split_test <- split_chunks(test, chunk_size = 1)
  n <- length(split_test)  # Total number of digits in this line
  
  # Will hold our 12 selected digits
  result_digits <- rep(NA, 12)
  
  # Track the position of the last digit we selected
  # (next digit must come AFTER this position)
  current_pos <- 0
  
  # Greedily select each of the 12 digits
  for (d in 1:12) {
    # VALID RANGE CALCULATION:
    # - Start: must be after the previously selected digit
    valid_start <- current_pos + 1
    
    # - End: must leave enough digits for the remaining positions
    #   If we're selecting digit d (out of 12), we still need (12 - d) more
    #   So we can go up to position: n - (12 - d) = n - 12 + d
    #   Example: n=20, d=1 -> can pick from positions 1-9 (leaving 11 for digits 2-12)
    #   Example: n=20, d=12 -> can pick from positions 12-20 (last digit, any remaining works)
    valid_end <- n - 12 + d
    
    # Define the searchable range
    valid_range <- valid_start:valid_end
    
    # Find the position of the maximum digit within the valid range
    # which.max returns position relative to the subset, so we adjust
    max_in_range <- which.max(as.numeric(split_test[valid_range]))
    max_location <- valid_start + max_in_range - 1  # Convert to absolute position
    
    # Record this digit and update our position marker
    result_digits[d] <- split_test[max_location]
    current_pos <- max_location  # Next digit must come after this
  }
  
  # Combine all 12 digits into a single number
  joltages[i] <- as.numeric(paste0(result_digits, collapse = ""))
}

# Answer is the sum of all joltages
options(scipen = 999) # Switches away from scientific notation
sum(joltages)
