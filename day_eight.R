# ============================================================================
# ADVENT OF CODE - DAY 8
# Problem: 3D point clustering based on proximity
# Part 1: Build circuits by connecting closest pairs; find 3 largest clusters
# Part 2: Cluster into exactly 2 groups; find closest unconnected pair
# ============================================================================

library(tidyverse)
library(progress)
options(scipen = 999)  # Avoid scientific notation in output

# ============================================================================
# SETUP: Parse 3D coordinates and compute all pairwise distances
# Input file format:
#   - Each line contains comma-separated x,y,z coordinates
#   - Example: "10,25,30" represents a point at (10, 25, 30)
# ============================================================================

# --- Read and parse 3D coordinates ---
lines <- readLines('day_eight_input.txt')

# test: n_circuits <- 10
n_circuits <- 1000  # Number of merge operations for Part 1

# Split each line by comma and convert to dataframe
positions <- as.data.frame(do.call(rbind, strsplit(lines, ',')))
names(positions) <- c('x', 'y', 'z')
positions$label <- 1:nrow(positions)  # Assign unique ID to each point
positions$x <- as.numeric(positions$x)
positions$y <- as.numeric(positions$y)
positions$z <- as.numeric(positions$z)

# --- Generate all unique pairs of points ---
# expand.grid creates all combinations; filter to keep only list1 < list2
combos <- expand.grid(list1 = 1:length(lines), list2 = 1:length(lines))
combos <- combos[combos$list1 < combos$list2, ]

# --- Calculate 3D Euclidean distance for each pair ---
combos$distance <- sqrt(
  (positions$x[combos$list1] - positions$x[combos$list2])^2 +
    (positions$y[combos$list1] - positions$y[combos$list2])^2 +
    (positions$z[combos$list1] - positions$z[combos$list2])^2
)

# ============================================================================
# PART ONE: Build circuits using greedy closest-pair merging
#
# Algorithm: Union-Find style clustering
#   1. Start with each point in its own circuit (cluster)
#   2. Repeatedly take the shortest edge and merge those two circuits
#   3. Skip if both points already belong to the same circuit
#   4. Stop after n_circuits merge operations
#
# Key insight: This is similar to Kruskal's MST algorithm but we stop early
# ============================================================================

# --- Initialize: each point starts in its own circuit ---
circuits <- as.list(1:nrow(positions))
circuits_used <- 0

# Progress bar for circuit building
pb2 <- progress_bar$new(
  format = "Building circuits [:bar] :percent | :current/:total | ETA: :eta",
  total = n_circuits,
  clear = FALSE
)

while (circuits_used < n_circuits){
  circuits_used <- circuits_used + 1
  
  # Sort to get shortest remaining edge first
  combos <- combos %>% arrange(distance)
  
  # Get the two points connected by shortest edge
  first_value <- combos$list1[1]
  second_value <- combos$list2[1]
  
  # Find which circuits contain these points
  first_circuit <- which(sapply(circuits, function(x) first_value %in% x))
  second_circuit <- which(sapply(circuits, function(x) second_value %in% x))
  
  # Remove this edge from consideration (already processed)
  combos <- combos[2:nrow(combos),]
  
  # Skip if already in same circuit (would create a cycle)
  if (first_circuit == second_circuit){
    pb2$tick()
    next
  }
  
  # Merge: combine second circuit into first, then delete second
  circuits[[first_circuit]] <- c(circuits[[first_circuit]], circuits[[second_circuit]])
  circuits[[second_circuit]] <- NULL
  pb2$tick()
}

# --- Find the 3 largest circuits ---
lengths(circuits) %>% 
  sort(decreasing = TRUE) %>% 
  head(3) 

# Answer for Part One: product of the 3 largest circuit sizes
lengths(circuits) %>% 
  sort(decreasing = TRUE) %>% 
  head(3) %>%
  prod()

# ============================================================================
# PART TWO: Cluster into exactly 2 groups, find bridge points
#
# Challenge: Find the two points that would connect the final two clusters
# Solution: 
#   1. Merge until only 2 circuits remain (need n-2 merges for n points)
#   2. Find the shortest edge connecting points in different circuits
#
# Key insight: This finds the "weakest link" between two natural clusters
# ============================================================================

# --- Regenerate combos (consumed in Part 1) ---
# Note: Could save a backup before Part 1 with: combos_backup <- combos
combos <- expand.grid(list1 = 1:length(lines), list2 = 1:length(lines))
combos <- combos[combos$list1 < combos$list2, ]
combos$distance <- sqrt(
  (positions$x[combos$list1] - positions$x[combos$list2])^2 +
    (positions$y[combos$list1] - positions$y[combos$list2])^2 +
    (positions$z[combos$list1] - positions$z[combos$list2])^2
)

# Pre-sort by distance (more efficient than sorting in loop)
combos <- combos %>% arrange(distance)

# --- Reset circuits: each point starts alone ---
circuits <- as.list(1:nrow(positions))

# We need exactly (n - 2) merges to go from n circuits to 2
pb3 <- progress_bar$new(
  format = "Building circuits [:bar] :percent | :current/:total | ETA: :eta",
  total = nrow(positions) - 2,
  clear = FALSE
)

# --- Merge until only 2 circuits remain ---
edge_idx <- 1  # Track position in sorted edge list (don't remove edges)

while (length(circuits) > 2) {
  
  # Get next shortest edge
  first_value <- combos$list1[edge_idx]
  second_value <- combos$list2[edge_idx]
  edge_idx <- edge_idx + 1
  
  # Find which circuits contain these points
  first_circuit <- which(sapply(circuits, function(x) first_value %in% x))
  second_circuit <- which(sapply(circuits, function(x) second_value %in% x))
  
  # Skip if already in same circuit
  if (first_circuit == second_circuit) {
    next
  }
  
  # Merge circuits
  circuits[[first_circuit]] <- c(circuits[[first_circuit]], circuits[[second_circuit]])
  circuits[[second_circuit]] <- NULL
  pb3$tick()
}

# --- Find the closest pair of points in different circuits ---
# This is the "bridge" that would connect the two clusters
closest_non_connected <- combos %>%
  rowwise() %>%
  mutate(
    # Determine which circuit each point belongs to
    circuit1 = which(sapply(circuits, function(x) list1 %in% x)),
    circuit2 = which(sapply(circuits, function(x) list2 %in% x))
  ) %>%
  ungroup() %>%
  as.data.frame() %>%
  filter(circuit1 != circuit2) %>%  # Keep only inter-cluster edges
  slice_min(distance)               # Get the shortest one

# Answer for Part Two: product of x-coordinates of the bridge points
positions %>% 
  filter(label %in% c(closest_non_connected$list1, closest_non_connected$list2)) %>%
  pull(x) %>%
  prod()