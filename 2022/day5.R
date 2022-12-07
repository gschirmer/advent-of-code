######################################################################################################
# Advent of Code 2022                                                                                #
# https://adventofcode.com/2022                                                                      #
# Author: Garrett Schirmer                                                                           #
######################################################################################################

library(tidyverse)
library(stringr)

######################################################################################################
# Day 5, Part 1                                                                                      #
######################################################################################################

# I'm going to split the input into two parts, the stacks and the directions
# The stacks are encoded vertically on the first 8 lines, so I'll just read those lines
input_stacks <- read_lines("input_day5.txt", n_max=8) %>%
    # First, split into individual characters (which will make it easier to extract the containers)
    strsplit(split="") %>%
    unlist() %>%
    matrix(byrow = TRUE, nrow = 8) %>%
    as_tibble() %>%
    # Only keep the positions associated with the container names (every 4th character)
    select(2 + (0:8 * 4)) %>%
    as.list() %>%
    # Remove blank characters from list vectors
    lapply(FUN = function(x){x[x != " "]})

# At this point, input_stacks is a list of vectors corresponding to the containers on each stack
# Within each vector the containers are ordered from top (position 1) to bottom (last position)

# Next, pull the directions from the input file
input_directions <- read_lines("input_day5.txt")[-(1:10)] %>%
    as_tibble() %>%
    mutate(n = word(value, 2, 2)) %>%
    mutate(from = word(value, 4, 4)) %>%
    mutate(to = word(value, 6, 6)) %>%
    mutate(across(n:to, as.numeric))

# Ok, I think it's time for a nice old fashioned for-loop because my brain is tired
current_stacks <- input_stacks
for (i in 1:nrow(input_directions)){
    n <- input_directions$n[i]
    from_stack <- input_directions$from[i]
    to_stack <- input_directions$to[i]

    for (j in 1:n){
        container <- current_stacks[[from_stack]][1]
        current_stacks[[to_stack]] <- c(container, current_stacks[[to_stack]])
        current_stacks[[from_stack]] <- current_stacks[[from_stack]][-1]
    }
}

solution_1 <- current_stacks %>%
    lapply(FUN = function(x){x[1]}) %>%
    unlist() %>%
    paste(collapse="")

solution_1

######################################################################################################
# Day 5, Part 2                                                                                      #
######################################################################################################

# Luckily, this is easier than part 1... I just need to update the moving logic in the for-loop
current_stacks <- input_stacks
for (i in 1:nrow(input_directions)){
    n <- input_directions$n[i]
    from_stack <- input_directions$from[i]
    to_stack <- input_directions$to[i]

    containers <- current_stacks[[from_stack]][1:n]
    current_stacks[[to_stack]] <- c(containers, current_stacks[[to_stack]])
    current_stacks[[from_stack]] <- current_stacks[[from_stack]][-(1:n)]
}

solution_2 <- current_stacks %>%
    lapply(FUN = function(x){x[1]}) %>%
    unlist() %>%
    paste(collapse="")

solution_2
