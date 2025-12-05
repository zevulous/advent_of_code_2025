# ============================================================================
# ADVENT OF CODE - DAY 1
# Problem: Track a value that changes based on R(ight)/L(eft) instructions
# R adds to the value, L subtracts from it
# ============================================================================

# PART ONE: Count how many times the running total is divisible by 100
# We only check AFTER each instruction is applied
# ============================================================================

# Starting value is 50
# R means add, L means subtract
# Count how many times the value is 0 mod 100

# Read input from file (each line is like "R25" or "L10")
input_file <- "day_one_input.txt"
lines <- readLines(input_file)

# Initialize starting position and counter
value <- 50   # Starting value as specified by the puzzle
count <- 0    # Tracks how many times we land on a multiple of 100

# Process each instruction line
for (line in lines) {
  # Skip empty lines to avoid parsing errors
  if (nchar(line) == 0) next
  
  # Parse the instruction:
  # - First character is direction (R or L)
  # - Remaining characters are the numeric amount
  direction <- substr(line, 1, 1)
  number <- as.numeric(substr(line, 2, nchar(line)))
  
  # Apply the operation based on direction
  if (direction == "R") {
    value <- value + number      # R = Right = Add
  } else if (direction == "L") {
    value <- value - number      # L = Left = Subtract
  }
  
  # Check if current value is divisible by 100
  if (value %% 100 == 0) {
    count <- count + 1
    cat("Value", value, "is divisible by 100\n")  # Debug output
  }
}

# Output results
cat("\nNumber of times value was 0 mod 100:", count, "\n")
cat("Final value:", value, "\n")


# ============================================================================
# PART TWO: Circular track interpretation
# Now we treat the value as a position on a circular dial/track (0-99)
# Instead of just checking final positions, we count how many times
# we PASS THROUGH position 0 during each rotation
# 
# Think of it like a combination lock dial - if you're at position 50
# and rotate right by 150 units, you'll pass through 0 one or more times
# ============================================================================

# Function to count how many times we cross position 0 during a single rotation
# Parameters:
#   start_pos: Current position on the dial (0-99)
#   direction: "R" for clockwise, "L" for counter-clockwise
#   distance: How many units to rotate
count_zeros_during_rotation <- function(start_pos, direction, distance) {
  # Edge case: no movement means no zeros crossed
  if (distance == 0) return(0)
  
  zeros_count <- 0
  
  if (direction == "R") {
    # Rotating right (clockwise): position increases
    # We hit 0 when (start_pos + k) mod 100 = 0, for k in 1..distance
    # This happens when k = (100 - start_pos), (200 - start_pos), etc.
    
    if (start_pos == 0) {
      # Special case: Starting at 0
      # We DON'T count the starting position, only positions we move TO
      # We hit 0 again after every 100 units traveled
      zeros_count <- distance %/% 100
    } else {
      # General case: First zero crossing is at k = (100 - start_pos)
      # Example: start at 50, first zero at k=50, then k=150, k=250...
      first_zero <- 100 - start_pos
      
      if (first_zero <= distance) {
        # We reach at least one zero
        # Count: 1 for the first, plus one more for each additional 100 units
        zeros_count <- 1 + (distance - first_zero) %/% 100
      }
      # If first_zero > distance, we don't reach 0 at all
    }
    
  } else {
    # Rotating left (counter-clockwise): position decreases
    # We hit 0 when (start_pos - k) mod 100 = 0, for k in 1..distance
    # This happens when k = start_pos, (start_pos + 100), etc.
    
    if (start_pos == 0) {
      # Special case: Starting at 0
      # Moving left, we hit 0 again after every 100 units
      zeros_count <- distance %/% 100
    } else {
      # General case: First zero crossing is at k = start_pos
      # Example: start at 50, first zero at k=50 (we've gone left to 0)
      if (start_pos <= distance) {
        # We reach at least one zero
        zeros_count <- 1 + (distance - start_pos) %/% 100
      }
      # If start_pos > distance, we don't reach 0
    }
  }
  
  return(zeros_count)
}


# Main solver function for Part Two
solve_part_two <- function(input_file) {
  # Read all instructions from the input file
  lines <- readLines(input_file)
  
  # Initialize: start at position 50 on the circular dial
  current_pos <- 50
  total_zeros <- 0  # Accumulator for all zero crossings
  
  # Process each rotation instruction
  for (line in lines) {
    # Parse direction and distance from instruction string
    direction <- substr(line, 1, 1)
    distance <- as.numeric(substr(line, 2, nchar(line)))
    
    # Count how many times we pass through 0 during THIS rotation
    zeros_during <- count_zeros_during_rotation(
      current_pos,
      direction,
      distance
    )
    total_zeros <- total_zeros + zeros_during
    
    # Update position on the circular dial (mod 100)
    if (direction == "R") {
      # Moving right: simple modulo
      current_pos <- (current_pos + distance) %% 100
    } else {
      # Moving left: need to handle negative modulo properly in R
      # The double-modulo trick ensures we get a positive result
      # Example: (50 - 60) %% 100 = -10, but we want 90
      current_pos <- ((current_pos - distance) %% 100 + 100) %% 100
    }
    
    # Debug output (uncomment to trace execution)
    # cat(sprintf("After %s: position = %d, zeros during = %d\n",
    #             line, current_pos, zeros_during))
  }
  
  return(total_zeros)
}


# ============================================================================
# TESTING AND EXECUTION
# ============================================================================

# Sample input for testing/validation
sample_input <- c(
  "L68",   # From 50, go left 68 -> cross 0 once -> end at 82
  
  "L30",   # From 82, go left 30 -> cross 0? -> end at 52
  "R48",   # From 52, go right 48 -> cross 0? -> end at 0
  "L5",    # From 0, go left 5 -> no crossing -> end at 95
  "R60",   # From 95, go right 60 -> cross 0? -> end at 55
  "L55",   # From 55, go left 55 -> cross 0 once -> end at 0
  "L1",    # From 0, go left 1 -> no crossing -> end at 99
  "L99",   # From 99, go left 99 -> no crossing -> end at 0
  "R14",   # From 0, go right 14 -> no crossing -> end at 14
  "L82"    # From 14, go left 82 -> no crossing -> end at 32
)

# Create a temporary file to test the solver
temp_file <- tempfile()
writeLines(sample_input, temp_file)

# Run test with sample data
result <- solve_part_two(temp_file)
cat("Example result:", result, "\n")  # Expected output: 6

# Clean up temporary test file
unlink(temp_file)

# Run with actual puzzle input
result <- solve_part_two("day_one_input.txt")
cat("Part Two answer:", result, "\n")