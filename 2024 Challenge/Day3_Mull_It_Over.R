# --- Day 3: Mull It Over ---

library(tidyverse)

# input <- readLines("2024 Challenge/data/input_day3_test.txt")
input <- readLines("2024 Challenge/data/input_day3.txt")

pattern <- "mul\\(\\d+,\\d+\\)"

answer <- 0

for (i in 1:length(input)) {
  matches <-
    str_match_all(input, pattern)[[i]] |>
    data.frame() |>
    rename(pair = 1) |>
    mutate(
      pair = str_remove(pair, "mul"),
      pair = gsub("[()]", "", pair)
    ) |>
    separate(pair, c("int1", "int2"), sep = ",") |>
    mutate(
      int1 = as.numeric(int1),
      int2 = as.numeric(int2)
    ) |>
    mutate(prod = int1 * int2)

  answer <- answer + sum(matches$prod)
}
