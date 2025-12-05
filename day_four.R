# ============================================================================
# ADVENT OF CODE - DAY 4
# Problem: Grid analysis of '@' symbols (called "rolls")
# Analyze adjacency relationships - a roll is "accessible" if it has
# fewer than 4 neighboring rolls (out of 8 possible neighbors)
# ============================================================================

# PART ONE: Count how many rolls are currently "accessible"
# A roll is accessible if it has fewer than 4 adjacent rolls
# (Think of it like: rolls packed tightly together are inaccessible,
# but those on edges or corners can be reached)
# ============================================================================

# Read the grid from file - each line is a row of characters
lines <- readLines('day_four_input.txt')

# Get grid dimensions
nrows <- length(lines)
ncols <- nchar(lines[1])

# Create an empty dataframe to hold our boolean grid
df <- data.frame(matrix(nrow = nrows, ncol = ncols))

# Parse the input: convert '@' characters to TRUE, everything else to FALSE
for (i in 1:nrows) {
  characters <- strsplit(lines[i], '')[[1]]  # Split line into individual chars
  for (j in 1:ncols) {
    df[i, j] <- characters[j] == '@'  # TRUE if this cell contains a roll
  }
}

# PAD THE GRID WITH NA BORDERS
# This clever trick avoids boundary checking when looking at neighbors
# Before: a 5x5 grid
# After:  a 7x7 grid with NA on all edges
# This way, edge cells can safely check "neighbors" that are just NA
df <- as.data.frame(as.matrix(
  data.frame(
    NA,                    # Add NA column on the left
    rbind(NA, df, NA),     # Add NA rows on top and bottom
    NA                     # Add NA column on the right
  )
))
# Note: Original data is now at positions [2:(nrows+1), 2:(ncols+1)]

# Count rolls that are "accessible" (have fewer than 4 neighbors)
accessible_rolls <- 0

# Iterate through the original grid area (offset by 1 due to padding)
for (i in 2:(nrows + 1)) {
  for (j in 2:(ncols + 1)) {
    
    # Only process cells that contain a roll
    if (df[i, j] == TRUE) {
      
      # Initialize all 8 neighbor flags to FALSE
      top_left <- FALSE
      top <- FALSE
      top_right <- FALSE
      left <- FALSE
      right <- FALSE
      bottom_left <- FALSE
      bottom <- FALSE
      bottom_right <- FALSE
      
      # Check all 8 adjacent cells (Moore neighborhood)
      # The padding ensures these indices are always valid
      #
      #   [i-1,j-1] [i-1,j] [i-1,j+1]
      #   [i,  j-1] [CELL ] [i,  j+1]
      #   [i+1,j-1] [i+1,j] [i+1,j+1]
      #
      top_left <- df[i - 1, j - 1]
      top <- df[i - 1, j]
      top_right <- df[i - 1, j + 1]
      left <- df[i, j - 1]
      right <- df[i, j + 1]
      bottom_left <- df[i + 1, j - 1]
      bottom <- df[i + 1, j]
      bottom_right <- df[i + 1, j + 1]
      
      # Collect all neighbor values
      adjacent_spots <- c(
        top_left,
        top,
        top_right,
        left,
        right,
        bottom_left,
        bottom,
        bottom_right
      )
      
      # Count neighboring rolls (TRUE values)
      # na.rm=TRUE ignores the NA padding cells
      rolls_near <- sum(adjacent_spots, na.rm = TRUE)
      
      # A roll is "accessible" if it has fewer than 4 neighbors
      # (i.e., it's somewhat exposed, not buried in the middle)
      if (rolls_near < 4) {
        accessible_rolls <- accessible_rolls + 1
      }
      
    } else {
      next  # Skip empty cells
    }
  }
}

accessible_rolls  # Answer for Part One


# ============================================================================
# PART TWO: Cascading removal simulation
# Remove all accessible rolls, then repeat until no more can be removed
# 
# This is like an erosion simulation:
#   - Round 1: Remove all rolls with < 4 neighbors
#   - Round 2: Some previously "buried" rolls now have fewer neighbors, remove them
#   - Continue until the remaining rolls all have 4+ neighbors (stable core)
#
# Count the TOTAL number of rolls removed across all rounds
# ============================================================================

total_rolls <- 0    # Accumulator for all rolls removed
round_rolls <- 1    # Rolls removed this round (initialize to enter loop)

# Continue until a round removes zero rolls (stable state reached)
while (round_rolls > 0) {
  
  round_rolls <- 0  # Reset counter for this round
  
  # Scan the entire grid
  for (i in 2:(nrows + 1)) {
    for (j in 2:(ncols + 1)) {
      
      # Only process cells that still contain a roll
      if (df[i, j] == TRUE) {
        
        # Initialize neighbor flags
        top_left <- FALSE
        top <- FALSE
        top_right <- FALSE
        left <- FALSE
        right <- FALSE
        bottom_left <- FALSE
        bottom <- FALSE
        bottom_right <- FALSE
        
        # Check all 8 neighbors
        top_left <- df[i - 1, j - 1]
        top <- df[i - 1, j]
        top_right <- df[i - 1, j + 1]
        left <- df[i, j - 1]
        right <- df[i, j + 1]
        bottom_left <- df[i + 1, j - 1]
        bottom <- df[i + 1, j]
        bottom_right <- df[i + 1, j + 1]
        
        adjacent_spots <- c(
          top_left,
          top,
          top_right,
          left,
          right,
          bottom_left,
          bottom,
          bottom_right
        )
        
        # Count neighboring rolls
        rolls_near <- sum(adjacent_spots, na.rm = TRUE)
        
        # If accessible (< 4 neighbors), REMOVE this roll
        if (rolls_near < 4) {
          round_rolls <- round_rolls + 1   # Track removals this round
          total_rolls <- total_rolls + 1   # Track total removals
          df[i, j] <- FALSE                # Remove the roll from the grid
        }
        
      } else {
        next  # Skip empty cells
      }
    }
  }
  # End of round - if round_rolls > 0, loop continues
  # Removed rolls may expose new accessible rolls for next round
}

total_rolls  # Answer for Part Two: total rolls removed before reaching stability