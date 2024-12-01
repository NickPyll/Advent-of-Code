# Day 1 Historian Hysteria----

library(tidyverse)

input <- read.table("2024 Challenge/data/input_day1.txt")

input <-
  tribble(
    ~V1, ~V2,
    3, 4,
    4, 3,
    2, 5,
    1, 3,
    3, 9,
    3, 3
  )

## Part 1 ----

left <- input |>
  select(V1) |>
  arrange(V1) |>
  pull()
right <- input |>
  select(V2) |>
  arrange(V2) |>
  pull()

sum(abs(left - right))

## Part 2 ----

similarity_score <- 0

for (left_num in left) {
  similarity_multiplier <- sum(unlist(right) == left_num)
  similarity_score <- similarity_score + left_num * similarity_multiplier
}
