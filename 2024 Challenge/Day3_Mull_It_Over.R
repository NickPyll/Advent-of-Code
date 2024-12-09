# --- Day 3: Mull It Over ---

library(tidyverse)

# input <- readLines("2024 Challenge/data/input_day3_test.txt")
input <- paste(readLines("2024 Challenge/data/input_day3.txt"), collapse = "")

pattern <- "mul\\(\\d+,\\d+\\)"
pattern_do <- "mul\\(\\d+,\\d+\\)|do\\(.*?\\)|don't\\(.*?\\)"


# Part 1
answer <- 0

matches <-
  str_match_all(input, pattern) |>
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

# Part 2
answer <- 0

matches <-
  str_match_all(input, pattern_do) |>
  data.frame() |>
  rename(pair = 1) |>
  mutate(
    pair = str_remove(pair, "mul"),
    pair = str_remove(pair, "'"),
    pair = gsub("[()]", "", pair)
  )

do <- ifelse(matches[[1]][[1]] == "dont", 0, 1)

matches <-
  matches |>
  mutate(do_mult = if_else(pair == "dont", 0, if_else(pair == "do", 1, NA))) |>
  fill(do_mult) |>
  filter(!pair %in% c("do", "dont")) |>
  separate(pair, c("int1", "int2"), sep = ",") |>
  mutate(
    do_mult = if_else(is.na(do_mult), do, do_mult),
    prod = do_mult * as.numeric(int1) * as.numeric(int2)
  )

answer <- answer + sum(matches$prod)
