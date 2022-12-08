######################################################################################################
# Advent of Code 2022                                                                                #
# https://adventofcode.com/2022                                                                      #
# Author: Garrett Schirmer                                                                           #
######################################################################################################

library(tidyverse)

######################################################################################################
# Day 8, Part 1                                                                                      #
######################################################################################################

input <- read_lines("input_day8.txt")

# I'm going to split each character into a separate column
n_col <- nchar(input[1])

input <- input %>%
    as_tibble() %>%
    separate(value, sep=1:n_col, into = paste0("X", 1:n_col)) %>%
    mutate(across(everything(), as.numeric)) %>%
    as.matrix()

# Define a function to check whether or not each tree in a vector is "visible"
# Where "visible" means "is the largest number in the vector so far" (using cummax())
fIsVisible <- function(x){
    output <- x > cummax(lag(x, 1, default = -1))

    return(output)
}

visibility_grid <- matrix(NA, nrow=nrow(input), ncol=ncol(input))

# Check row-wise visibility first
for (i in 1:nrow(visibility_grid)){
    x <- input[i,]
    visibility_grid[i,] <- fIsVisible(x) | rev(fIsVisible(rev(x)))
}

# Then check column-wise visibility
for (i in 1:ncol(visibility_grid)){
    x <- input[,i]
    visibility_grid[,i] <- visibility_grid[,i] | fIsVisible(x) | rev(fIsVisible(rev(x)))
}

solution_1 <- sum(visibility_grid)

solution_1

######################################################################################################
# Day 8, Part 2                                                                                      #
######################################################################################################

# Define a function that returns the number of visible trees for the first element in a vector
fVisibleTrees <- function(x){
    big_trees <- which(x >= x[1])

    if (length(big_trees) < 2){
        return(length(x) - 1)
    }
    return(big_trees[2] - 1)
}

scenic_scores <- matrix(0, nrow=nrow(input), ncol=ncol(input))

# Loop through the elements of the matrix, calculating scenic scores along the way
n_row <- nrow(input)
n_col <- ncol(input)
for (i in 1:n_row){
    for (j in 1:n_col){
        up_score <- fVisibleTrees(rev(input[1:i, j]))
        down_score <- fVisibleTrees(input[i:n_row, j])
        left_score <- fVisibleTrees(rev(input[i, 1:j]))
        right_score <- fVisibleTrees(input[i, j:n_col])

        scenic_scores[i, j] <- prod(up_score, down_score, left_score, right_score)
    }
}

solution_2 <- max(scenic_scores)

solution_2
