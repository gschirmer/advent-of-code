######################################################################################################
# Advent of Code 2022                                                                                #
# https://adventofcode.com/2022                                                                      #
# Author: Garrett Schirmer                                                                           #
######################################################################################################

library(tidyverse)

######################################################################################################
# Day 6, Part 1                                                                                      #
######################################################################################################

# Pull the input in as a vector of individual characters
input <- read_lines("input_day6.txt") %>%
    strsplit(split="") %>%
    unlist()

# Given that this is a pretty small list of characters, I'm just going to loop through
i <- 4
while (length(unique(input[(i - 3):i])) < 4){
    i <- i + 1
}

solution_1 <- i

######################################################################################################
# Day 6, Part 2                                                                                      #
######################################################################################################

# Cool, this is equally easy
i <- 14
while (length(unique(input[(i - 13):i])) < 14){
    i <- i + 1
}

solution_2 <- i
