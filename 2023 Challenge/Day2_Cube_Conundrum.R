# Day 2 Cube Conundrum----
## Part 1 ----
# Eliminate any games in which any of the thresholds have been exceeded
# Red > 12
# Green > 13
# Blue > 14
# Add game ID numbers for all remaining games

library(tidyverse)

input <- read.csv('data/input_day2.txt', sep = ";", header = FALSE) 

df <-
  input |> 
  separate(V1, c('GameID', 'V1'), ":") |> 
  pivot_longer(!GameID) |>
  select(-name) |>
  separate('value', paste("Value", 1:3, sep = "_"), sep = ",", extra = "drop") |>
  pivot_longer(!GameID) |>
  select(-name) |>
  filter(!is.na(value), value != '') |>
  separate('value', c('drop', 'count', 'color'), sep = ' ') |>
  select(-drop) |>
  mutate(count = as.numeric(count)) |> 
  mutate(
    red_exclude = if_else(color == 'red' & count > 12, 1, 0),
    green_exclude = if_else(color == 'green' & count > 13, 1, 0),
    blue_exclude = if_else(color == 'blue' & count > 14, 1, 0)
  ) |> 
  mutate(exclude = red_exclude + green_exclude + blue_exclude) |> 
  group_by(GameID) |>
  summarize(exclude = max(exclude)) |>
  ungroup() |> 
  mutate(GameID_num = as.numeric(gsub("[^0-9.-]", "", GameID))) |> 
  filter(exclude == 0) 

df |> 
  summarise(sum = sum(GameID_num)) |> 
  pull()

# Day 2 Cube Conundrum----
## Part 2 ----
# Find minimum number of cubes that each game would require
# Multiply those numbers together
# Sum for entire set

df <-
  input |> 
  separate(V1, c('GameID', 'V1'), ":") |> 
  pivot_longer(!GameID) |>
  select(-name) |>
  separate('value', paste("Value", 1:3, sep = "_"), sep = ",", extra = "drop") |>
  pivot_longer(!GameID) |>
  select(-name) |>
  filter(!is.na(value), value != '') |>
  separate('value', c('drop', 'count', 'color'), sep = ' ') |>
  select(-drop) |>
  mutate(count = as.numeric(count)) |> 
  group_by(GameID, color) |>
  summarize(count = max(count)) |>
  ungroup() |> 
  pivot_wider(names_from = color, values_from = count) |> 
  mutate(power = red*green*blue)

df |> 
  summarise(sum = sum(power)) |> 
  pull()
