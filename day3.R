# Advent Of Code 2021 - Day 3 ----
# Day 3: Binary Diagnostic

library(magrittr)
library(dplyr)

# Part 1 ----
input <- readLines("input/day3.txt")

digits <- lapply(1:nchar(input[1]), function(x) {
    out <- substr(input, x, x)
    out <- as.integer(out)
    return (out)
})

decipher <- function(x, gamma = TRUE) {
        num_of_rows <- length(x)
        num_of_ones <- sum(x)
        
        if (gamma) {
            if (num_of_ones > (num_of_rows/2)) {
                return (1)
            } else {
                return (0)
            }    
        } else {
            if (num_of_ones < (num_of_rows/2)) {
                return (1)
            } else {
                return (0)
            }
        }
}

gamma_output <- lapply(digits, decipher) %>% 
    unlist() %>% 
    paste(., collapse="")

epsilon_output <- lapply(digits, decipher, gamma = FALSE) %>% 
    unlist() %>% 
    paste(., collapse = "")

gamma <- strtoi(gamma_output, base = 2)
epsilon <- strtoi(epsilon_output, base = 2)

gamma * epsilon

# Part 2 ----
input_df <- sapply(input, strsplit, split = "", fixed = TRUE) %>% 
    do.call("rbind", args = .) %>% 
    as.data.frame() %>% 
    as_tibble()

gamma_o <- strsplit(gamma_output, split = "", fixed = TRUE)[[1]]
























