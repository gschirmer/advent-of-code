######################################################################################################
# Advent of Code 2022                                                                                #
# https://adventofcode.com/2022                                                                      #
# Author: Garrett Schirmer                                                                           #
######################################################################################################

library(tidyverse)

######################################################################################################
# Day 7, Part 1                                                                                      #
######################################################################################################

input <- read_lines("input_day7.txt")

# I'm just going to loop through the input lines, keeping track of directory sizes as I go
# I'll need to track each unique directory path and the total size of files it contains

# Initialize a table to keep track of directory stats
directory_stats <- data.frame(name = "/",
                              size = 0) %>%
    as_tibble()

# Initialize a vector to track the current directory path
current_directory <- c()

# Loop through each input line
for (l in input){

    # First, check if we're changing directories using substr()
    if (substr(l, 1, 4) == "$ cd"){

        # We're either going back one directory...
        if (substr(l, 6, 7) == ".."){
            current_directory <- head(current_directory, -1)
        }
        # ... or going into a directory that's in the current directory
        else {
            current_directory <- c(current_directory, substr(l, 6, nchar(l)))
        }

        # Add the current directory path to directory_stats, if it's new
        path <- paste(current_directory, collapse = "/")

        if (!path %in% directory_stats$name){
            directory_stats <- directory_stats %>%
                add_row(name = path,
                        size = 0)
        }
    }

    # Other than changing directories, we only need to check for individual files
    # Files are the only lines that don't start with "c" or "$"
    if (!substr(l, 1, 1) %in% c("d", "$")){
        this_size <- l %>%
            strsplit(split=" ") %>%
            unlist() %>%
            head(1) %>%
            as.numeric()

        # Add the file size to every parent directory
        for (j in 1:length(current_directory)){
            this_path <- paste(current_directory[1:j], collapse = "/")

            directory_stats <- directory_stats %>%
                mutate(size = size + ifelse(name == this_path,
                                            this_size, 0))
        }
    }
}

# Now we just need to get the sum of all sizes that are under 100,000
solution_1 <- directory_stats %>%
    filter(size <= 100000) %>%
    select(size) %>%
    sum()

solution_1

######################################################################################################
# Day 7, Part 2                                                                                      #
######################################################################################################

# Here we just need to filter for directories that would free up enough space (>= used space - 4e7)
solution_2 <- directory_stats %>%
    filter(size >= max(size) - 40000000) %>%
    select(size) %>%
    min()

solution_2
