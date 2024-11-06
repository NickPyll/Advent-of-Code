# Day 11 Cosmic Expansion----
## Part 1 ----

library(tidyverse)
library(data.table)

file <- 'data/input_day11.txt'

universe <- as.list(unlist(readLines(file)))

universe_df <- as.data.frame(do.call('cbind', universe)) |> 
  mutate(pivot_id = 1) |> 
  pivot_longer(!pivot_id) |> 
  mutate(pivot_id = row_number()) 

extra_rows <-
  universe_df |>
  filter(!grepl(pattern = '#', value)) |> 
  select(pivot_id) |> 
  pull()

expand <- seq(1:999999)/1000000

pivot_id <- as.vector(sapply(extra_rows,function(i) (i + expand)))

pivot_id <- data.frame(pivot_id)

universe_df <-
  universe_df |> 
  bind_rows(pivot_id) |> 
  arrange(pivot_id) |> 
  fill(value) |> 
  mutate(
    name = if_else(is.na(name), paste0('V', pivot_id), name)) 

universe_df <-
  universe_df |> 
  mutate(v = str_split(value, "(?=.)")) |>
  unnest(cols = c(v)) |> 
  filter(v != "") |> 
  group_by(pivot_id, name, value) |>
  mutate(k = paste0("c",row_number()))  |>
  ungroup() |> 
  spread(k,v) |> 
  select(starts_with('c')) 

universe_df <-
  universe_df |> 
  select(str_sort(names(universe_df), numeric = TRUE))
  
universe_matrix <- t(as.matrix(universe_df))

universe_df_trans <- as.data.frame(universe_matrix) |> 
  rownames_to_column('pivot_id') |> 
  mutate(pivot_id = as.numeric(str_remove(pattern = 'c', pivot_id))) |> 
  arrange(pivot_id) |>  
  unite("value", starts_with('V'), remove = TRUE, sep = '')

extra_rows <-
  universe_df_trans |>
  filter(!grepl(pattern = '#', value)) |> 
  select(pivot_id) |> 
  pull()

pivot_id <- as.vector(sapply(extra_rows,function(i) (i + expand)))

pivot_id <- data.frame(pivot_id)

universe_df_trans <-
  universe_df_trans |> 
  bind_rows(pivot_id) |> 
  arrange(pivot_id) |> 
  fill(value) |> 
  mutate(v = str_split(value, "(?=.)")) |>
  unnest(cols = c(v)) |> 
  filter(v != "") |> 
  group_by(pivot_id) |>
  mutate(k = paste0("c",row_number()))  |>
  ungroup() |> 
  spread(k,v) |> 
  select(starts_with('c')) 

universe_df_trans <-
  universe_df_trans |> 
  select(str_sort(names(universe_df_trans), numeric = TRUE)) |> 
  unite("value", starts_with('c'), remove = TRUE, sep = '')

all_char <- str_locate_all(universe_df_trans$value, '#')

char_list <- map(all_char, as.data.table)
char <- rbindlist(char_list, fill = TRUE, idcol = T) |> 
  mutate(x = 1) |> 
  rename(from_row = .id,
         from_col = start) |> 
  select(-end)

char |> 
  inner_join(char |> 
               rename(to_row = from_row,
                      to_col = from_col), 
             by = 'x', relationship = 'many-to-many') |> 
  mutate(distance = abs(from_col - to_col) + abs(from_row - to_row)) |> 
  summarize(sum(distance/2)) |> 
  pull()


