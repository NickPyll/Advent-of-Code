# Day 7 Camel Cards----
## Part 1 ----
# 
# 

# library(tidyverse)

input <- read.csv('data/input_day7.txt',
                  sep = " ",
                  header = FALSE,
                  col.names = c('hand', 'bid')) 

# input <- read.csv('data/input_day7.txt',
#                   sep = " ",
#                   header = FALSE,
#                   col.names = c('hand', 'bid')) 

# answer <-
#   input |> 
#   mutate(card = str_split(hand,"(?=.)")) |>  
#   unnest() |> 
#   filter(card != "") |>  
#   group_by(hand, bid, card) |>  
#   summarize(value = n()) |>  
#   ungroup() |> 
#   select(-card) |> 
#   group_by(hand, bid) |> 
#   arrange(desc(value), .by_group = TRUE) |> 
#   ungroup() |> 
#   group_by(hand, bid) |> 
#   summarize(
#     distinct_cards = n(),
#     max_card_count = max(value),
#     min_card_count = min(value)
#   ) |> 
#   ungroup() |> 
#   mutate(
#     type = if_else(max_card_count == 5, 5,
#                    if_else(max_card_count == 4, 4,
#                            if_else(min_card_count == 2 & max_card_count == 3, 3.5,
#                                    if_else(min_card_count == 1 & max_card_count == 3, 3,
#                                            if_else(distinct_cards == 3 & max_card_count == 2, 2,
#                                                    if_else(distinct_cards == 5, 0, 1))))))) |> 
#   mutate(
#     c1 = substr(hand, 1, 1),
#     c2 = substr(hand, 2, 2),
#     c3 = substr(hand, 3, 3),
#     c4 = substr(hand, 4, 4),
#     c5 = substr(hand, 5, 5)
#   ) |> 
#   pivot_longer(cols = starts_with('c'), names_to = 'name', values_to = 'value') |> 
#   mutate(
#     value = str_replace(value, 'T', '10'),
#     value = str_replace(value, 'J', '11'),
#     value = str_replace(value, 'Q', '12'),
#     value = str_replace(value, 'K', '13'),
#     value = str_replace(value, 'A', '14'),
#     value = as.numeric(value)) |> 
#   pivot_wider(names_from = name, values_from = value) |> 
#   arrange(type, c1, c2, c3, c4, c5) |> 
#   mutate(
#     rank = row_number(),
#     hand_value = bid*rank) |> 
#   summarize(answer = sum(hand_value)) |> 
#   pull()
# 

joker_count <-
  input |> 
  mutate(card = str_split(hand,"(?=.)")) |>  
  unnest() |> 
  filter(card == 'J') |>  
  group_by(hand, bid, card) |>  
  summarize(value = n()) |>  
  ungroup() |> 
  rename(joker_count = value) |> 
  select(-card) 

answer <-
  input |> 
  mutate(card = str_split(hand,"(?=.)")) |>  
  unnest() |> 
  filter(card != "", card != 'J') |>  
  group_by(hand, bid, card) |>  
  summarize(value = n()) |>  
  ungroup() |> 
  select(-card) |> 
  group_by(hand, bid) |> 
  arrange(desc(value), .by_group = TRUE) |> 
  ungroup() |> 
  group_by(hand, bid) |> 
  summarize(
    distinct_cards = n(),
    max_card_count = max(value),
    min_card_count = min(value)
  ) |> 
  ungroup() |> 
  full_join(joker_count, by = c('hand', 'bid')) |> 
  mutate_all(~replace_na(., 0)) |> 
  mutate(max_card_count = max_card_count + joker_count) |> 
  mutate(
    type = if_else(max_card_count == 5, 5,
                   if_else(max_card_count == 4, 4,
                           if_else(min_card_count == 2 & max_card_count == 3, 3.5,
                                   if_else(min_card_count == 1 & max_card_count == 3, 3,
                                           if_else(distinct_cards == 3 & max_card_count == 2, 2,
                                                   if_else(distinct_cards == 5, 0, 1))))))) |> 
  mutate(
    c1 = substr(hand, 1, 1),
    c2 = substr(hand, 2, 2),
    c3 = substr(hand, 3, 3),
    c4 = substr(hand, 4, 4),
    c5 = substr(hand, 5, 5)
  ) |> 
  pivot_longer(cols = starts_with('c'), names_to = 'name', values_to = 'value') |> 
  mutate(
    value = str_replace(value, 'T', '10'),
    value = str_replace(value, 'J', '1'),
    value = str_replace(value, 'Q', '12'),
    value = str_replace(value, 'K', '13'),
    value = str_replace(value, 'A', '14'),
    value = as.numeric(value)) |> 
  pivot_wider(names_from = name, values_from = value) |> 
  arrange(type, c1, c2, c3, c4, c5) |> 
  mutate(
    rank = row_number(),
    hand_value = bid*rank) 

answer |> 
  summarize(answer = sum(hand_value)) |> pull()



