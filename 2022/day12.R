######################################################################################################
# Advent of Code 2022                                                                                #
# https://adventofcode.com/2022                                                                      #
# Author: Garrett Schirmer                                                                           #
######################################################################################################

library(tidyverse)
library(igraph)

######################################################################################################
# Day 12, Part 1                                                                                     #
######################################################################################################

input <- read_lines("input_day12.txt")

# I'm going to take a graph-search approach to this, since we're looking for shortest paths

n_row <- length(input)

input <- input %>%
    strsplit(split = "") %>%
    unlist() %>%
    matrix(nrow = n_row, byrow = TRUE)

# Define the nodes (named by their row and column in the input grid)
nodes <- tidyr::crossing(R = 1:nrow(input),
                  C = 1:ncol(input)) %>%
    unite("node", R:C) %>%
    pull(node)

# Define a function to return a list of reachable coordinates from current spot
# Taking into account we can only reach spots that are at most one higher than current letter
fGetReachableSpots <- function(grid, start){

    start_row <- start[1]
    start_col <- start[2]

    start_letter <- grid[start_row, start_col]
    start_letter <- gsub("S", "a", gsub("E", "z", start_letter))
    start_letter_value <- match(start_letter, letters)

    adjacent <- list(c(start_row - 1, start_col),
                     c(start_row + 1, start_col),
                     c(start_row, start_col - 1),
                     c(start_row, start_col + 1))

    reachable <- list()
    for (a in adjacent){
        if (all(a >= c(1, 1) & a <= c(nrow(grid), ncol(grid)))){
            end_letter <- grid[a[1], a[2]]
            end_letter <- gsub("S", "a", gsub("E", "z", end_letter))

            if (match(end_letter, letters) - start_letter_value <= 1){
                reachable[[length(reachable) + 1]] <- a
            }
        }
    }

    return(reachable)
}

# Create an adjacency matrix by checking which nodes are reachable from each node
adjacency <- matrix(0, nrow = length(nodes), ncol = length(nodes), dimnames = list(nodes, nodes))
for (i in 1:nrow(adjacency)){
    start <- nodes[i] %>%
        strsplit(split = "_") %>%
        unlist() %>%
        as.numeric()

    reachable <- fGetReachableSpots(input, start)
    reachable_nodes <- lapply(reachable, FUN = paste, collapse = "_")

    adjacency[i, which(nodes %in% unlist(reachable_nodes))] <- 1
}

# Use the igraph package to turn the adjacency matrix into a graph
g <- graph.adjacency(adjacency)

start_node <- paste(which(input == "S", arr.ind = TRUE), collapse = "_")
end_node <- paste(which(input == "E", arr.ind = TRUE), collapse = "_")

# Pull the length of the shortest path from the start node to the end node
solution_1 <- shortest_paths(g, from=start_node, to=end_node, algorithm="dijkstra")$vpath[[1]] %>%
    length() - 1

solution_1

######################################################################################################
# Day 12, Part 2                                                                                     #
######################################################################################################

# We can apply the same approach by looping through all possible starting points
starting_nodes <- paste(which(input == "a", arr.ind = TRUE)[,1],
                        which(input == "a", arr.ind = TRUE)[,2],
                        sep = "_")

fewest_steps <- solution_1
for (i in starting_nodes){
    print(paste(match(i, starting_nodes), fewest_steps))
    steps <- shortest_paths(g, from=i, to=end_node, algorithm="dijkstra")$vpath[[1]] %>%
        length() - 1

    if (steps > 0){
        fewest_steps <- min(steps, fewest_steps)
    }
}

solution_2 <- fewest_steps
