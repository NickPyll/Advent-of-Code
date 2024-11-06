# Day 1 Sonar Sweep----
## Part 1 ----
# How many times does depth increase?

library(tidyverse)

# depths <- read.table('2021 Challenge/data/input_day1_test.txt') |> pull()
depths <- read.table('2021 Challenge/data/input_day1.txt') |> pull()

increase <- 0
depth <- head(depths, 1) 

for (i in depths){
  if (i > depth){
    increase <- increase + 1
  }
  depth <- i
}

## Part 2 ----
# Sum the triple depths and count increases

increase <- 0
depth <- sum(depths[1:3]) 

for (i in 1:(length(depths) - 2)){
  depth_sum <- depths[i] + (depths[i + 1]) + (depths[i + 2])
  if (depth_sum > depth){
    increase <- increase + 1
  }
  depth <- depth_sum
}
