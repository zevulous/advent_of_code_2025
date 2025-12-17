# day eleven
library(igraph)
options(scipen = 999)

# Read and parse the file
lines <- readLines("day_eleven_input.txt")

# Create edge list from the parsed data
edges <- do.call(rbind, lapply(lines, function(line) {
  parts <- strsplit(line, ":\\s*")[[1]]
  parent <- trimws(parts[1])
  children <- strsplit(trimws(parts[2]), "\\s+")[[1]]
  data.frame(from = parent, to = children)
}))

# Create directed graph
g <- graph_from_data_frame(edges, directed = TRUE)
reachable <- subcomponent(g, "you", mode = "out")
g_from_you <- induced_subgraph(g, reachable)

paths <- all_simple_paths(g_from_you, from = "you", to = "out", mode = "out")

# Count them
length(paths)

# part two

# Read and parse the file
lines <- readLines("day_eleven_input.txt")

# Create edge list from the parsed data
edges <- do.call(rbind, lapply(lines, function(line) {
  parts <- strsplit(line, ":\\s*")[[1]]
  parent <- trimws(parts[1])
  children <- strsplit(trimws(parts[2]), "\\s+")[[1]]
  data.frame(from = parent, to = children)
}))

# Create directed graph
g <- graph_from_data_frame(edges, directed = TRUE)
reachable <- subcomponent(g, "svr", mode = "out")
g_from_svr <- induced_subgraph(g, reachable)

count_paths <- function(g, from, to) {
  if (from == to) return(1)
  
  path_count <- setNames(rep(0, vcount(g)), V(g)$name)
  path_count[from] <- 1
  
  for (node in names(topo_sort(g, mode = "out"))) {
    for (succ in names(neighbors(g, node, mode = "out"))) {
      path_count[succ] <- path_count[succ] + path_count[node]
    }
  }
  
  return(path_count[to])
}

# Count paths through both "fft" AND "dac"
# Must consider both orderings: fft before dac, or dac before fft

n_fft_first <- count_paths(g_from_svr, "svr", "fft") *
  count_paths(g_from_svr, "fft", "dac") *
  count_paths(g_from_svr, "dac", "out")

n_dac_first <- count_paths(g_from_svr, "svr", "dac") *
  count_paths(g_from_svr, "dac", "fft") *
  count_paths(g_from_svr, "fft", "out")

total <- n_fft_first + n_dac_first
total
