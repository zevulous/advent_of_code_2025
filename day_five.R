# ============================================================================
# ADVENT OF CODE - DAY 5
# Problem: Range validation and counting
# Part 1: Check which ingredient values fall within any defined range
# Part 2: Count total unique values covered by all ranges (merged)
# ============================================================================

library(tidyverse)
options(scipen = 999) # Avoid scientific notation in output

# ============================================================================
# PART ONE: Validate ingredients against freshness ranges
# Input file format:
#   - First section: ranges like "10-20" (one per line)
#   - Blank line separator
#   - Second section: ingredient values (one number per line)
# An ingredient is "good" if it falls within ANY of the defined ranges
# ============================================================================

# Read the input file
lines <- readLines('day_five_input.txt')
n_lines <- length(lines)

# Find the blank line that separates ranges from ingredients
breakpoint <- which(lines == "")

# --- Parse the freshness ranges ---
fresh_lists <- lines[1:(breakpoint - 1)]

# Split each "start-end" string and convert to a dataframe
ranges <- do.call(rbind, strsplit(fresh_lists, "-"))
ranges <- data.frame(
  start = as.numeric(ranges[, 1]),
  end = as.numeric(ranges[, 2])
)

# --- Parse the ingredient values ---
ingredients <- lines[(breakpoint + 1):n_lines]
ingredients <- as.numeric(ingredients)

# --- Check each ingredient against all ranges ---
good <- rep(NA, length(ingredients))

for (i in 1:length(ingredients)) {
  test <- ingredients[i]
  # TRUE if ingredient falls within ANY range (start <= test <= end)
  good[i] <- any(test >= ranges$start & test <= ranges$end)
}

sum(good) # Answer for Part One: count of valid ingredients


# ============================================================================
# PART TWO: Count unique "fresh" values across all ranges
#
# Challenge: Brute-force expansion of ranges causes memory issues
# Solution: Merge overlapping ranges into groups, then sum each group's span
#
# Key insight: After sorting by start value, ranges overlap if:
#   start of range X <= max end value seen so far
#
# Example of merging:
#   [1-5], [3-8], [10-15] → Group 1: [1-8], Group 2: [10-15]
#   Total unique values = (8-1+1) + (15-10+1) = 8 + 6 = 14
# ============================================================================

# Sort ranges by start value (critical for overlap detection)
ranges <- ranges %>% arrange(start)

# --- Assign each range to a group based on overlap ---
group_number_tracker <- rep(NA, nrow(ranges))
group_number <- 1
group_number_tracker[1] <- group_number

# Track the maximum end value seen in the current group
# Note: Can't just use the previous range's end - need the MAX end seen
# (e.g., [1-100], [5-10] - the second range is contained within the first)
running_max_end <- ranges$end[1]

for (i in 2:nrow(ranges)) {
  if (ranges$start[i] <= running_max_end) {
    # This range overlaps with or is adjacent to current group
    group_number_tracker[i] <- group_number
    running_max_end <- max(running_max_end, ranges$end[i])
  } else {
    # Gap detected - start a new group
    group_number <- group_number + 1
    group_number_tracker[i] <- group_number
    running_max_end <- ranges$end[i]
  }
}

# Add group assignments to the dataframe
ranges$group_number <- group_number_tracker
n_groups <- max(ranges$group_number)

# --- Calculate total unique values across all groups ---
count_of_values <- 0

for (i in 1:n_groups) {
  # Get all ranges belonging to this group
  ranges_in_that_group <- ranges %>% filter(group_number == i)

  # Group span: from earliest start to latest end
  start_value <- ranges_in_that_group$start[1] # Already sorted, so first is min

  end_value <- max(ranges_in_that_group$end)

  # Count unique values in this group's span (inclusive)
  length_of_group_range <- length(seq(start_value, end_value))
  count_of_values <- count_of_values + length_of_group_range
}

count_of_values # Answer for Part Two: total unique fresh values
