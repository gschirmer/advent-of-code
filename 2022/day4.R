######################################################################################################
# Advent of Code 2022                                                                                #
# https://adventofcode.com/2022                                                                      #
# Author: Garrett Schirmer                                                                           #
######################################################################################################

library(tidyverse)
library(stringr)

######################################################################################################
# Day 4, Part 1                                                                                      #
######################################################################################################

input <- read.csv("input_day4.txt", header = FALSE, col.names = c("a1", "a2"))

# Compare min/max of each assignment to check for containment
solution_1 <- input %>%
    separate(a1, sep="-", into=c("min1", "max1"), convert=TRUE) %>%
    separate(a2, sep="-", into=c("min2", "max2"), convert=TRUE) %>%
    mutate(contained = ifelse((min1 <= min2 & max1 >= max2) | (min1 >= min2 & max1 <= max2), 1, 0)) %>%
    select(contained) %>%
    sum()

solution_1

######################################################################################################
# Day 4, Part 2                                                                                      #
######################################################################################################

# Same structure, just slightly different logic to define containment
solution_2 <- input %>%
    separate(a1, sep="-", into=c("min1", "max1"), convert=TRUE) %>%
    separate(a2, sep="-", into=c("min2", "max2"), convert=TRUE) %>%
    mutate(contained = ifelse(max1 < min2 | min1 > max2, 0, 1)) %>%
    select(contained) %>%
    sum()

solution_2
