# Advent Of Code 2021 - Day 2 ----
# Day 2: Dive!

library(magrittr)

# Part 1 ----
input <- readLines("input/day2.txt") 

input <- input %>% 
    strsplit(., " ", fixed = TRUE) %>% 
    do.call("rbind", args = .) %>% 
    as.data.frame()

input$V2 <- as.integer(input$V2)

head(input)

# 1580000

navigate <- function(df) {
    hori <- 0L
    depth <- 0L
    for (i in seq_along(1:nrow(df))) {
        if (df[i, 1] == "up") {
            depth = depth - input[i, 2]
        } else if (df[i, 1] == "down") {
            depth = depth + input[i, 2]
        } else {
            hori = hori + input[i, 2]
        }
    }
    cat(paste("horizontal:", hori, "\ndepth:", depth, "\n"))
    return (hori * depth)
}

navigate(input)

# Part 2
navigate_w_aim <- function(df) {
    hori <- 0L
    depth <- 0L
    aim <- 0L
    for (i in seq_along(1:nrow(df))) {
        if (df[i, 1] == "up") {
            aim = aim - df[i, 2]
        } else if (df[i, 1] == "down") {
            aim = aim + df[i, 2]
        } else {
            hori = hori + df[i, 2]
            depth = depth + (aim * df[i, 2])
        }
    }
    cat(paste("horizontal:", hori, "\ndepth:", depth, "\naim:", aim, "\n"))
    return (hori * depth)
}

navigate_w_aim(input)






