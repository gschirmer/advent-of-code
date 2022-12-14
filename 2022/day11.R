######################################################################################################
# Advent of Code 2022                                                                                #
# https://adventofcode.com/2022                                                                      #
# Author: Garrett Schirmer                                                                           #
######################################################################################################

library(tidyverse)

######################################################################################################
# Day 11, Part 1                                                                                     #
######################################################################################################

input <- read_lines("input_day11.txt") %>%
    trimws()

# I'm going to try to structure this data as a nested list
# The outer list will have one entry for each monkey
# Each monkey will have it's own list of attributed (which themselves could also contain lists, etc.)
# There's probably some cool way to turn the input into json, but I'm not going down that rabbit hole

monkey_list <- list()
current_monkey <- NA
for (i in input[input != ""]){

    # If this row is listing a new monkey...
    if (substr(i, 1, 1) == "M"){
        current_monkey <- substr(i, 8, 8)
        monkey_list[[current_monkey]] <- list()
    }

    # If this row is listing the starting items...
    if (substr(i, 1, 1) == "S"){
        items <- substr(i, 17, nchar(i)) %>%
            strsplit(split = ", ") %>%
            unlist() %>%
            as.numeric()

        monkey_list[[current_monkey]][["Items"]] <- items
    }

    # If this row is defining the operation...
    if (substr(i, 1, 1) == "O"){
        operation <- substr(i, 12, nchar(i))
        monkey_list[[current_monkey]][["Operation"]] <- operation
    }

    # If this row is defining the test (divisor)...
    if (substr(i, 1, 1) == "T"){
        test <- i %>%
            strsplit(split = " ") %>%
            unlist()
        monkey_list[[current_monkey]][["Divisor"]] <- as.numeric(test[4])
    }

    # If this row is defining the "if true" monkey...
    if (substr(i, 1, 4) == "If t"){
        monkey_list[[current_monkey]][["IfTrue"]] <- substr(i, nchar(i), nchar(i))
    }

    # If this row is defining the "if false" monkey...
    if (substr(i, 1, 4) == "If f"){
        monkey_list[[current_monkey]][["IfFalse"]] <- substr(i, nchar(i), nchar(i))
    }
}

# Ok, that felt gross, but I think I have the list structured properly
# Next step is to loop through the list and perform all the proper operations
# While also keeping track of each monkey's activity level

N_ROUNDS <- 20
monkey_activities <- rep(0, length(monkey_list))
starting_monkey_list <- monkey_list

for (n in 1:N_ROUNDS){
    for (m in names(monkey_list)){
        items <- monkey_list[[m]]$Items
        operation <- monkey_list[[m]]$Operation
        divisor <- monkey_list[[m]]$Divisor
        to_true <- monkey_list[[m]]$IfTrue
        to_false <- monkey_list[[m]]$IfFalse
        for (i in items){
            # First, increase monkey activity by 1
            monkey_activities[as.numeric(m) + 1] <- 1 + monkey_activities[as.numeric(m) + 1]

            # Then perform the operation using the handy-dandy eval(parse()) combo
            old <- i
            eval(parse(text = operation)) # This always creates a variable called "new"

            new_worry <- floor(new / 3)

            # Test if new worry is divisible by divisor
            if (new_worry %% divisor == 0){
                monkey_list[[to_true]]$Items <- c(monkey_list[[to_true]]$Items, new_worry)
            } else {
                monkey_list[[to_false]]$Items <- c(monkey_list[[to_false]]$Items, new_worry)
            }

            # Remove item from current monkey's list
            monkey_list[[m]]$Items <- monkey_list[[m]]$Items[-1]

        }
    }
}

solution_1 <- monkey_activities %>%
    sort(decreasing = TRUE) %>%
    head(2) %>%
    prod()

solution_1

######################################################################################################
# Day 11, Part 1                                                                                     #
######################################################################################################

# These numbers are going to blow up real quick. Luckily, all the divisors we're checking are primes.
# Which means that if the worry level ever exceeds the product of those primes, we can subtract that.

N_ROUNDS <- 10000
monkey_activities <- rep(0, length(monkey_list))
monkey_list <- starting_monkey_list
product_of_primes <- 2 * 3 * 5 * 7 * 11 * 13 * 17 * 19

for (n in 1:N_ROUNDS){
    for (m in names(monkey_list)){
        items <- monkey_list[[m]]$Items
        operation <- monkey_list[[m]]$Operation
        divisor <- monkey_list[[m]]$Divisor
        to_true <- monkey_list[[m]]$IfTrue
        to_false <- monkey_list[[m]]$IfFalse
        for (i in items){
            # First, increase monkey activity by 1
            monkey_activities[as.numeric(m) + 1] <- 1 + monkey_activities[as.numeric(m) + 1]

            old <- i
            eval(parse(text = operation))

            # Here's where we take the new worry mod the max possible worry (product_of_primes)
            new_worry <- new %% product_of_primes

            # Test if new worry is divisible by divisor
            if (new_worry %% divisor == 0){
                monkey_list[[to_true]]$Items <- c(monkey_list[[to_true]]$Items, new_worry)
            } else {
                monkey_list[[to_false]]$Items <- c(monkey_list[[to_false]]$Items, new_worry)
            }

            # Remove item from current monkey's list
            monkey_list[[m]]$Items <- monkey_list[[m]]$Items[-1]

        }
    }
}

solution_2 <- monkey_activities %>%
    sort(decreasing = TRUE) %>%
    head(2) %>%
    prod()

solution_2
