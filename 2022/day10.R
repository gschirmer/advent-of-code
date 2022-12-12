######################################################################################################
# Advent of Code 2022                                                                                #
# https://adventofcode.com/2022                                                                      #
# Author: Garrett Schirmer                                                                           #
######################################################################################################

library(tidyverse)

######################################################################################################
# Day 10, Part 1                                                                                     #
######################################################################################################

# Going to do something slightly strange and split the input by newlines AND spaces
# That way the addx lines get split into two rows, so I can use the rows to count the cycle number
input <- read_lines("input_day10.txt") %>%
    strsplit(split = " ") %>%
    unlist() %>%
    as_tibble() %>%
    mutate(cycle_num = row_number()) %>%
    mutate(increment = ifelse(
        value %in% c("noop", "addx"), 0, as.numeric(value)
    )) %>%
    # I'm using cumsum lagged by 1 to get the value of X during the cycle rather than after the cycle
    mutate(X = cumsum(lag(increment, 1, default = 0)) + 1) %>%
    mutate(signal_strength = cycle_num * X)

solution_1 <- input %>%
    filter(cycle_num %in% c(20, 60, 100, 140, 180, 220)) %>%
    select(signal_strength) %>%
    sum()

solution_1

######################################################################################################
# Day 10, Part 2                                                                                     #
######################################################################################################

WIDTH <- 40
HEIGHT <- 6

# This seems fun! I should be able to use the same input framework as part 1
# I just need to indicate whether each cycle produces a lit (#) or dark (.) pixel

solution_2 <- input %>%
    mutate(pixel_position = (cycle_num - 1) %% WIDTH) %>%
    mutate(pixel_generated = ifelse(pixel_position >= X - 1 & pixel_position <= X + 1, "#", ".")) %>%
    pull(pixel_generated) %>%
    matrix(ncol = HEIGHT) %>%
    t()

view(solution_2)
