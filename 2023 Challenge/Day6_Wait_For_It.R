# Day 6 Wait For It----
## Part 1 ----
# 
# 

library(tidyverse)

input <- read.csv('data/input_day6.txt',
# input <- read.csv('data/input_day6_test.txt',
                  sep = ":",
                  header = FALSE,
                  col.names = c('var', 'V1')) |> 
  mutate(value = strsplit(V1, " ")) |> 
  unnest(value) |> 
  mutate(value = as.numeric(value)) |> 
  filter(!is.na(value)) |> 
  select(var, value) |> 
  group_by(var) |> 
  mutate(raceID = row_number()) |>
  ungroup() |> 
  pivot_wider(names_from = var, values_from = value) |> 
  rename(time_available = Time, distance_record = Distance)

# at what time t does d exceed D?

input <-
  input |> 
  group_by(raceID) %>% 
  slice(rep(1:n(), first(time_available))) |> 
  group_by(raceID) %>% 
  mutate(hold_time = row_number()) |> 
  ungroup() |> 
  mutate(velocity = hold_time,
         time_available = time_available - hold_time,
         distance_traveled = time_available * velocity) |> 
  filter(distance_traveled> distance_record) |> 
  group_by(raceID) |> 
  summarize(ways_to_win = n()) |> 
  summarize(answer = prod(ways_to_win))

## Part 2

input <- read.csv('data/input_day6.txt',
# input <- read.csv('data/input_day6_test.txt',
                  sep = ":",
                  header = FALSE,
                  col.names = c('var', 'V1')) |> 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" "))) |> 
  mutate(value = as.numeric(V1)) |> 
  select(var, value) |> 
  group_by(var) |> 
  mutate(raceID = row_number()) |>
  ungroup() |> 
  pivot_wider(names_from = var, values_from = value) |> 
  rename(time_available = Time, distance_record = Distance)

input <-
  input |> 
  group_by(raceID) %>% 
  slice(rep(1:n(), first(time_available))) |> 
  group_by(raceID) %>% 
  mutate(hold_time = row_number()) |> 
  ungroup() |> 
  mutate(velocity = hold_time,
         time_available = time_available - hold_time,
         distance_traveled = time_available * velocity) |> 
  filter(distance_traveled> distance_record) |> 
  group_by(raceID) |> 
  summarize(ways_to_win = n()) |> 
  summarize(answer = prod(ways_to_win))
