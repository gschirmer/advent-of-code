######################################################################################################
# Advent of Code 2022                                                                                #
# https://adventofcode.com/2022                                                                      #
# Author: Garrett Schirmer                                                                           #
######################################################################################################

library(tidyverse)

######################################################################################################
# Day 2, Part 1                                                                                      #
######################################################################################################

input <- read.table("input_day2.txt",
                    col.names=c("first_code", "second_code"))

# Define reference table of 9 possible round score outcomes
scores_table_base <- list(first_code = c("A", "B", "C"),
                          second_code = c("X", "Y", "Z")) %>%
    expand.grid(stringsAsFactors=FALSE)

scores_table <- scores_table_base %>%
    mutate(opp_choice = match(first_code, c("A", "B", "C"))) %>%
    mutate(my_choice = match(second_code, c("X", "Y", "Z"))) %>%
    # Some fun scoring logic here... made easier by using base 3
    # Draws always have (my_choice - opp_choice) %% 3 == 0 (obviously)
    # Wins always have (my_choice - opp_choice) %% 3 == 1
    # Loses always have (my_choice - opp_choice) %% 3 == 2
    mutate(my_score = my_choice + c(0, 3, 6)[match((my_choice - opp_choice) %% 3, c(2, 0, 1))])

# Join the input codes with the scores reference table, then sum my_score
solution_1 <- input %>%
    left_join(scores_table,
              by=c("first_code", "second_code")) %>%
    select(my_score) %>%
    sum()

solution_1

######################################################################################################
# Day 2, Part 2                                                                                      #
######################################################################################################

# Ok, so I just need to revise the my_choice logic in scores_table to reflect the new code meanings
scores_table_revised <- scores_table %>%
    mutate(my_choice = ifelse(second_code == "Y", opp_choice,
                             ifelse(second_code == "X", ((opp_choice - 2) %% 3) + 1,
                                    (opp_choice) %% 3 + 1))) %>%
    mutate(my_score = my_choice + c(0, 3, 6)[match((my_choice - opp_choice) %% 3, c(2, 0, 1))])

solution_2 <- input %>%
    left_join(scores_table_revised,
              by=c("first_code", "second_code")) %>%
    select(my_score) %>%
    sum()

solution_2
