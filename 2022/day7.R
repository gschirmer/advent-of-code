######################################################################################################
# Advent of Code 2022                                                                                #
# https://adventofcode.com/2022                                                                      #
# Author: Garrett Schirmer                                                                           #
######################################################################################################

library(tidyverse)
library(data.tree)

######################################################################################################
# Day 7, Part 1                                                                                      #
######################################################################################################

# Pull the input in as a vector of individual characters
input <- read_lines("input_day7.txt")

# I'm just going to solve this with a loop first and then maybe come up with something more elegant
# Storing directory stats as we go

# Initialize a table to keep track of directory stats
directory_stats <- data.frame(name = "/",
                              depth = 0,
                              parent = NA,
                              direct_file_names = "",
                              direct_file_sizes = 0,
                              directories_within = "") %>%
    as_tibble()

current_directory <- c() # A vector of directory names (filepath)

for (l in input){
    # Check if we're changing directories
    if (substr(l, 1, 4) == "$ cd"){

        # Either going back one directory or jumping into named directory
        if (substr(l, 6, 7) == ".."){
            current_directory <- current_directory[-length(current_directory)]
        } else {
            current_directory <- c(current_directory, substr(l, 6, nchar(l)))
        }
    }

    # Check if we're listing directory contents
    if (substr(l, 1, 1) != "$"){
        split_line <- unlist(strsplit(l, split=" "))
        path <- paste(current_directory, collapse=" ")

        if (split_line[1] == "dir"){
            # If this is a new directory, add it to the directory_stats table
            if (!paste(path, split_line[2], sep=" ") %in% directory_stats$name){
                directory_stats <- directory_stats %>%
                    add_row(name = paste(path, split_line[2], sep=" "),
                            depth = length(current_directory),
                            parent = path,
                            direct_file_names = "",
                            direct_file_sizes = 0,
                            directories_within = "")
            }

            # Add the directory name to the directories_within col for the current directory
            directory_stats <- directory_stats %>%
                mutate(directories_within = ifelse(name == path,
                                                  paste(directories_within, paste(path, split_line[2], sep=" "), sep=","),
                                                  directories_within))
        }

        # Otherwise this is a file
        else {
            # Add the file stats to the current directory's row in directory_stats
            directory_stats <- directory_stats %>%
                mutate(direct_file_names = ifelse(name == path,
                                                  paste(direct_file_names, split_line[2], sep=","),
                                                  direct_file_names)) %>%
                mutate(direct_file_sizes = ifelse(name == path,
                                                  direct_file_sizes + as.numeric(split_line[1]),
                                                  direct_file_sizes))

        }
    }
}

# Now we need to figure out the total size of all files contained within each directory
# We'll start at the max depth, then add the direct file sizes to the parent directories
directory_stats$total_file_sizes <- directory_stats$direct_file_sizes
directory_stats <- directory_stats[with(directory_stats, order(-depth)),]

for (i in 1:(nrow(directory_stats) - 1)){
    this_size <- directory_stats$total_file_sizes[i]
    this_parent <- directory_stats$parent[i]

    directory_stats <- directory_stats %>%
        mutate(total_file_sizes = ifelse(name == this_parent,
                                         total_file_sizes + this_size,
                                         total_file_sizes))
}


solution_1 <- directory_stats %>%
    filter(total_file_sizes <= 100000) %>%
    select(total_file_sizes) %>%
    sum()

solution_1

######################################################################################################
# Day 7, Part 2                                                                                      #
######################################################################################################

solution_2 <- directory_stats %>%
    filter(total_file_sizes >= max(total_file_sizes) - 40000000) %>%
    select(total_file_sizes) %>%
    min()

solution_1
