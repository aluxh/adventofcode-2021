# Advent Of Code 2021 - Day 1 ----
# Day 1: Sonar Sweep 

# Part 1 ----
# Input
input <- readLines("input/day1.txt")
input <- as.integer(input)

compare <- function(x) {
    out <- vector()
    
    for (i in seq_along(1:length(x))) {
        out <- c(out, x[i+1] > x[i])
    }
    
    return (out)
}

sum(compare(input), na.rm = T)


# Part 2 ----
slide_window <- function(x, n = 2) {
    out <- vector()
    
    for(i in seq_along(1:length(x))) {
        out <- c(out, sum(input[i:(i+n)]))
    }
    return (out)
}

sum(compare(slide_window(input)), na.rm = T)

