######################################################################################################
# Advent of Code 2022                                                                                #
# https://adventofcode.com/2022                                                                      #
# Author: Garrett Schirmer                                                                           #
######################################################################################################

library(tidyverse)

######################################################################################################
# Day 1, Part 1                                                                                      #
######################################################################################################

# Note: I saved the input as a txt file
input <- read.delim2("input_day1.txt", sep="\n",
                     header=FALSE,
                     blank.lines.skip=FALSE,
                     col.names="calories")

working_data <- input %>%
    ## Assign an elf_num column which counts (one plus) the cumulative number of blank (NA) calories
    mutate(elf_num = 1 + cumsum(is.na(calories))) %>%
    # Then remove the blank (NA) calorie rows (I don't need to do this, but I want to)
    filter(!is.na(calories))

# Next, group total calories by elf
elf_totals <- working_data %>%
    group_by(elf_num) %>%
    summarize(total_calories = sum(calories))

# Lastly, return the maximum total calories
solution_1 <- max(elf_totals$total_calories)

solution_1

######################################################################################################
# Day 1, Part 2                                                                                      #
######################################################################################################

# This is pretty straight-forward... just pull the top 3 total_calories and then sum them
solution_2 <- elf_totals %>%
    slice_max(total_calories, n=3) %>%
    select(total_calories) %>%
    sum()

solution_2
