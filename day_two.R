# ============================================================================
# ADVENT OF CODE - DAY 2
# Problem: Find "invalid IDs" within given numeric ranges
# An invalid ID is a number that consists of repeating digit patterns
# ============================================================================

# PART ONE: Find numbers where the first half of digits equals the second half
# Example: 1212 is invalid because "12" == "12", 123123 because "123" == "123"
# ============================================================================

library(tidyverse)

# Input: comma-separated ranges in format "start-end"
input <- "874324-1096487,6106748-6273465,1751-4283,294380-348021,5217788-5252660,828815656-828846474,66486-157652,477-1035,20185-55252,17-47,375278481-375470130,141-453,33680490-33821359,88845663-88931344,621298-752726,21764551-21780350,58537958-58673847,9983248-10042949,4457-9048,9292891448-9292952618,4382577-4494092,199525-259728,9934981035-9935011120,6738255458-6738272752,8275916-8338174,1-15,68-128,7366340343-7366538971,82803431-82838224,72410788-72501583"

# Parse input: remove newlines, split by comma, and trim whitespace
ranges_str <- strsplit(gsub("\n", "", input), ",")[[1]]
ranges_str <- trimws(ranges_str)

# Accumulator for sum of all invalid IDs found
invalid_id_sum <- 0

# Iterate through each range
for (i in 1:length(ranges_str)) {
  test <- ranges_str[i]
  
  # Extract start and end values from the range string (e.g., "100-200")
  start <- as.numeric(strsplit(test, "-")[[1]][1])
  end <- as.numeric(strsplit(test, "-")[[1]][2])
  
  # Check every number in the range
  for (value in start:end) {
    length <- nchar(value)  # Count digits in the number
    
    # Skip odd-length numbers (can't split evenly into two halves)
    if (length %% 2 == 1) {
      next
    } else {
      half_length <- length / 2
      
      # Split the number string into first and second halves
      first_half <- as.character(value) %>% substring(., 1, half_length)
      second_half <- as.character(value) %>%
        substring(., half_length + 1, length)
      
      # If halves match, this is an "invalid ID" - add to sum
      if (first_half == second_half) {
        invalid_id_sum <- invalid_id_sum + value
      } else {
        next
      }
    }
  }
}

invalid_id_sum  # Output the result for Part One


# PART TWO: Generalized pattern matching
# Find numbers that can be split into ANY number of identical chunks
# Example: 121212 is invalid (three "12"s), 111 is invalid (three "1"s)
# This extends Part One by checking all possible chunk sizes, not just halves
# ============================================================================

library(numbers)  # For the divisors() function

part_two_id_sum <- 0

# Helper function: split a string into chunks of a given size
# Example: split_chunks("121212", 2) returns c("12", "12", "12")
split_chunks <- function(x, chunk_size = 2) {
  starts <- seq(1, nchar(x), by = chunk_size)  # Starting positions of each chunk
  ends <- pmin(starts + chunk_size - 1, nchar(x))  # Ending positions
  substring(x, starts, ends)  # Extract all chunks
}

# Iterate through each range (reusing parsed ranges from Part One)
# This is phenomenally slow, but I don't feel like optimizing it
for (i in 1:length(ranges_str)) {
  test <- ranges_str[i]
  
  # Extract start and end values
  start <- as.numeric(strsplit(test, "-")[[1]][1])
  end <- as.numeric(strsplit(test, "-")[[1]][2])
  
  # Check every number in the range
  for (value in start:end) {
    length <- nchar(value)
    
    # Get all divisors of the length EXCEPT the length itself
    # These are valid chunk sizes (we need at least 2 chunks to compare)
    # Example: length=6 -> divisors are 1,2,3 (excluding 6)
    options <- setdiff(divisors(length), length)
    
    # Try each possible chunk size
    for (chunk_size in options) {
      # Split number into chunks of this size
      test_list <- split_chunks(as.character(value), chunk_size)
      
      # If ALL chunks are identical, this is an invalid ID
      if (all(test_list == test_list[1])) {
        part_two_id_sum <- part_two_id_sum + value
        break  # Stop checking other chunk sizes (avoid double-counting)
      }
    }
  }
}

part_two_id_sum  # Output the result for Part Two