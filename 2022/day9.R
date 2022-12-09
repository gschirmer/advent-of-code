######################################################################################################
# Advent of Code 2022                                                                                #
# https://adventofcode.com/2022                                                                      #
# Author: Garrett Schirmer                                                                           #
######################################################################################################

library(tidyverse)

######################################################################################################
# Day 9, Part 1                                                                                      #
######################################################################################################

input <- read_lines("input_day9.txt")

# I'm going to loop through the directions, keeping track of the head/tail positions at each step
head_pos <- c(0, 0)
tail_pos <- c(0, 0)
visited_list <- list(tail_pos)
movements <- list(R = c(1, 0), U = c(0, 1), L = c(-1, 0), D = c(0, -1))

for (i in input){
    direction <- unlist(strsplit(i, split = " "))[1]
    spaces <- unlist(strsplit(i, split = " "))[2] %>%
        as.numeric()

    # Move the head, one space at a time
    for (j in 1:spaces){
        head_pos <- head_pos + movements[[direction]]

        # Move the tail, if needed
        # The tail only moves when the absolute difference in positions is > 1 in either dimension
        diff <- head_pos - tail_pos
        if (any(abs(diff) > 1)){

            # The tail moves at most 1 spot in each dimension (in the direction of the difference)
            tail_move_direction <- sign(diff)
            tail_pos <- tail_pos + (pmin(1, abs(diff)) * tail_move_direction)

            visited_list[[length(visited_list) + 1]] <- tail_pos
        }
    }
}

solution_1 <- length(unique(visited_list))

solution_1

######################################################################################################
# Day 9, Part 2                                                                                      #
######################################################################################################

# Same general framework, just using a list of positions rather than just head/tail
positions_list <- replicate(10, list(c(0, 0)))
visited_list <- list(positions_list[[10]])

for (i in input){
    direction <- unlist(strsplit(i, split = " "))[1]
    spaces <- unlist(strsplit(i, split = " "))[2] %>%
        as.numeric()

    # Move the head, one space at a time
    for (j in 1:spaces){
        positions_list[[1]] <- positions_list[[1]] + movements[[direction]]

        # Move the rest of the positions, if needed
        # Same logic as part 1, just iterating through each of the knots
        for (p in 2:length(positions_list)){
            diff <- positions_list[[p - 1]] - positions_list[[p]]
            if (any(abs(diff) > 1)){
                positions_list[[p]] <- positions_list[[p]] + (pmin(1, abs(diff)) * sign(diff))

                if (p == length(positions_list)){
                    visited_list[[length(visited_list) + 1]] <- positions_list[[p]]
                }
            }
        }
    }
}

solution_2 <- length(unique(visited_list))

solution_2
