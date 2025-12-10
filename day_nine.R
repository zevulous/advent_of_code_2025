# ============================================================================
# ADVENT OF CODE - DAY 9
# Problem: Finding rectangles from coordinate pairs
# Part 1: Find the largest rectangle formed by any two points as corners
# Part 2: Find the largest rectangle that fits entirely inside the polygon
# ============================================================================

# ============================================================================
# PART ONE: Find maximum rectangle area from point pairs
# Input file format:
#   - Each line contains comma-separated x,y coordinates
#   - Example: "5,10" represents a point at (5, 10)
# Rectangle: Treat any two points as opposite corners of an axis-aligned rectangle
# ============================================================================

# --- Read and parse 2D coordinates ---
lines <- readLines('day_nine_input.txt')
positions <- as.data.frame(do.call(rbind, strsplit(lines, ',')))
names(positions) <- c('x', 'y')
positions$x <- as.numeric(positions$x)
positions$y <- as.numeric(positions$y)

# --- Generate all unique pairs of points ---
# combn returns a 2-row matrix: row 1 = first point index, row 2 = second point index
pairs_idx <- combn(1:nrow(positions), 2)

# --- Calculate rectangle area for each pair ---
# Area formula: (width + 1) * (height + 1) for inclusive grid counting
pairs_df <- data.frame(
  x1 = positions$x[pairs_idx[1,]],
  y1 = positions$y[pairs_idx[1,]],
  x2 = positions$x[pairs_idx[2,]],
  y2 = positions$y[pairs_idx[2,]]
) %>%
  mutate(area = ((abs(x1 - x2)+1) * (abs(y1 - y2) + 1))) %>%
  arrange(-area)  # Sort descending by area (largest first)

# Answer for Part One: largest rectangle dimensions and area
pairs_df %>% head(1)

# ============================================================================
# PART TWO: Find largest rectangle contained entirely within the polygon
#
# Challenge: Brute-force grid expansion causes memory issues for large rectangles
# Solution: Iterate through pairs (largest first) and validate containment
#
# Validation approach:
#   1. Ray casting: Check if rectangle center is inside the polygon
#   2. Edge intersection: Ensure no polygon edge cuts through rectangle interior
#
# Key insight: Points form polygon vertices in order, so we check edges
#              formed by consecutive vertices (with wraparound)
# ============================================================================

n_vertices <- nrow(positions)

for (i in 1:nrow(pairs_df)) {
  row <- pairs_df[i, ]
  
  # --- Define rectangle bounds ---
  x_min <- min(row$x1, row$x2)
  x_max <- max(row$x1, row$x2)
  y_min <- min(row$y1, row$y2)
  y_max <- max(row$y1, row$y2)
  
  # --- Ray casting algorithm to check if center is inside polygon ---
  # Cast a horizontal ray from center to infinity; count edge crossings
  # Odd crossings = inside, even crossings = outside
  cx <- (x_min + x_max) / 2
  cy <- (y_min + y_max) / 2
  
  inside <- FALSE
  j <- n_vertices  # Start with last vertex (for wraparound edge)
  
  for (k in 1:n_vertices) {
    # Current edge: from vertex j to vertex k
    xi <- positions$x[k]
    yi <- positions$y[k]
    xj <- positions$x[j]
    yj <- positions$y[j]
    
    # Check if horizontal ray from (cx, cy) crosses this edge
    # Condition 1: Edge spans the y-coordinate of center
    # Condition 2: Intersection point is to the right of center
    if (((yi > cy) != (yj > cy)) &&
        (cx < (xj - xi) * (cy - yi) / (yj - yi) + xi)) {
      inside <- !inside  # Toggle inside/outside state
    }
    j <- k  # Move to next edge
  }
  
  # Skip if center is outside polygon
  if (!inside) next
  
  # --- Check if any polygon edge cuts through rectangle interior ---
  # An edge "cuts through" if it passes through the interior (not just touching)
  # Assumes polygon edges are axis-aligned (horizontal or vertical only)
  edge_cuts <- FALSE
  
  for (k in 1:n_vertices) {
    # Edge from vertex k to vertex j (next vertex with wraparound)
    j <- (k %% n_vertices) + 1
    ex1 <- positions$x[k]; ey1 <- positions$y[k]
    ex2 <- positions$x[j]; ey2 <- positions$y[j]
    
    if (ey1 == ey2) {
      # Horizontal edge: check if it crosses interior vertically
      # Must be strictly between y bounds AND overlap x range
      if (ey1 > y_min && ey1 < y_max &&
          min(ex1, ex2) < x_max && max(ex1, ex2) > x_min) {
        edge_cuts <- TRUE
        break
      }
    } else {
      # Vertical edge: check if it crosses interior horizontally
      # Must be strictly between x bounds AND overlap y range
      if (ex1 > x_min && ex1 < x_max &&
          min(ey1, ey2) < y_max && max(ey1, ey2) > y_min) {
        edge_cuts <- TRUE
        break
      }
    }
  }
  
  # If no edges cut through, this rectangle is fully contained
  if (!edge_cuts) {
    # Answer for Part Two: area of largest interior rectangle
    print(row$area)
    break
  }
}