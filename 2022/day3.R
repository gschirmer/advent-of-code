######################################################################################################
# Advent of Code 2022                                                                                #
# https://adventofcode.com/2022                                                                      #
# Author: Garrett Schirmer                                                                           #
######################################################################################################

library(tidyverse)

######################################################################################################
# Day 3, Part 1                                                                                      #
######################################################################################################

input <- read.table("input_day3.txt", col.names = "code")

# Define a function to return the character that appears in both halves of a string
# Taking the intersection of the unlisted characters for each half
fGetRepeatChar <- Vectorize(function(x){

    # Split each string into individual characters, then find the intersection
    repeat_char <- intersect(
        unlist(strsplit(
                substr(x, 1, nchar(x) / 2),
                split="")),
        unlist(strsplit(
                substr(x, (nchar(x) / 2) + 1, nchar(x)),
                split=""))
    )
    return(unique(repeat_char))
})

# Figure out the repeat character in each string and then calculate the priority value
working_data <- input %>%
    mutate(repeat_char = fGetRepeatChar(code)) %>%
    # Using R's built-in letters vector for this part
    # letters only contains lower-case, so I'll need to add 26 for the upper-case letters
    mutate(priority = ifelse(repeat_char == toupper(repeat_char), 26, 0) +
               match(tolower(repeat_char), letters))

solution_1 <- working_data %>%
    select(priority) %>%
    sum()

solution_1

######################################################################################################
# Day 3, Part 2                                                                                      #
######################################################################################################

# First, define a function to return all characters shared by each string in a vector (x)
fGetSharedChars <- function(x){
    # Split each string into individual characters, then find the intersection
    shared_chars <- x %>%
        strsplit(split="") %>%
        Reduce(f=intersect) %>%
        unique() %>%
        # Return the shared characters as a single string
        paste0(collapse="")

    return(shared_chars)
}

# Split elves into groups, then find shared characters
working_data_2 <- input %>%
    mutate(group_num = (row_number() - 1) %/% 3) %>%
    group_by(group_num) %>%
    summarize(shared_char = fGetSharedChars(code)) %>%
    mutate(priority = ifelse(shared_char == toupper(shared_char), 26, 0) +
               match(tolower(shared_char), letters))

solution_2 <- working_data_2 %>%
    select(priority) %>%
    sum()

solution_2
