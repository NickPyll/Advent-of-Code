# Day 1 Rock Paper Scissors ----
## Part 1 ----
# Count total points

library(tidyverse)

# strategy_guide <- read.table('2022 Challenge/data/input_day2_test.txt', col.names = c('actual', 'suggested'))
strategy_guide <- read.table('2022 Challenge/data/input_day2.txt', col.names = c('actual', 'suggested'))

strategy_guide |> 
  inner_join(
    tribble(
      ~suggested, ~suggested_mod, ~shape_score,
      'X', 'A', 1,
      'Y', 'B', 2,
      'Z', 'C', 3
    ),
    by = 'suggested') |> 
  mutate(
    round_score = if_else(suggested_mod == actual, 3 + shape_score,
                          if_else(paste0(suggested_mod, actual) %in% c('AC', 'BA', 'CB'), 6 + shape_score, shape_score))) |> 
  summarize(answer = sum(round_score))


## Part 2

strategy_guide |> 
  rename(outcome = suggested) |> 
  inner_join(
    tribble(
      ~actual, ~outcome, ~chosen, ~shape_score,
      'A','X','C',3,
      'A','Y','A',1,
      'A','Z','B',2,
      'B','X','A',1,
      'B','Y','B',2,
      'B','Z','C',3,
      'C','X','B',2,
      'C','Y','C',3,
      'C','Z','A',1),
    by = c('actual', 'outcome')) |> 
  mutate(
    round_score = if_else(outcome == 'Y', 3 + shape_score, if_else(outcome == 'Z', 6 + shape_score, shape_score))) |>  
  summarize(answer = sum(round_score))


