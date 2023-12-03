# Day 1 Gear Ratios----
## Part 1 ----
# Identify all part numbers
# Sum all part numbers

library(tidyverse)
library(purrr)
library(data.table)

# input <-
#   c('467..114..',
#     '...*......',
#     '..35..633.',
#     '......#...',
#     '617*......',
#     '.....+.58.',
#     '..592.....',
#     '......755.',
#     '...$.*....',
#     '.664.598..')

input <- readLines('data/input_day3.txt')

# ok so i need to keep everything above except 114 and 58, because they are not adjacent to a special character

x.loc_num <- str_locate_all(input, '[0-9]+')
x.loc_spec <- str_locate_all(input, '[^0-9.]')

part_number <- as.numeric(unlist(regmatches(input, gregexpr("[[:digit:]]+", input))))

x.dt_list <- map(x.loc_num, as.data.table)
x.dt <- rbindlist(x.dt_list, fill = TRUE, idcol = T)
df_loc_num <- 
  data.frame(part_number, x.dt) |> 
  rename(row =.id) |> 
  mutate(part_number_id = row_number())

x.dt_list <- map(x.loc_spec, as.data.table)
x.dt <- rbindlist(x.dt_list, fill = TRUE, idcol = T)
y.df_loc_spec <- as.data.frame(x.dt) |> 
  rename(row =.id,
         mark = start) |>
  select(-end) |> 
  mutate(id = row_number())
  
rm(list = ls(pattern = "x."))
rm(part_number)

y.spec1 <-
  y.df_loc_spec |> 
  mutate(row = row - 1)

y.spec2 <-
  y.df_loc_spec |> 
  mutate(row = row + 1)

y.spec3 <-
  y.df_loc_spec |> 
  mutate(mark = mark - 1)

y.spec4 <-
  y.df_loc_spec |> 
  mutate(mark = mark + 1)

y.spec5 <-
  y.df_loc_spec |> 
  mutate(row = row - 1,
         mark = mark - 1)

y.spec6 <-
  y.df_loc_spec |> 
  mutate(row = row - 1,
         mark = mark + 1)

y.spec7 <-
  y.df_loc_spec |> 
  mutate(row = row + 1,
         mark = mark - 1)

y.spec8 <-
  y.df_loc_spec |> 
  mutate(row = row + 1,
         mark = mark + 1)


df_loc_spec <- bind_rows(y.spec1, y.spec2, y.spec3, y.spec4, y.spec5, y.spec6, y.spec7, y.spec8, y.df_loc_spec)

rm(list = ls(pattern = "y."))

df_loc_num |> 
  inner_join(df_loc_spec, by = 'row', relationship = "many-to-many") |> 
  mutate(include = if_else(mark <= end & mark >= start, 1, 0)) |> 
  group_by(part_number, row, part_number_id) |>
  summarize(include = max(include)) |>
  ungroup() |>
  filter(include == 1) |>
  summarize(sum = sum(part_number)) |> 
  pull()

## Part 2 ----
# Identify all asterisks which are adjacent to exactly two numbers
# Multiply two numbers
# Sum all products

rm(df_loc_spec)

special_character <- unlist(regmatches(input, gregexpr("[^0-9.]", input)))

x.loc_spec <- str_locate_all(input, '[^0-9.]')

x.dt_list <- map(x.loc_spec, as.data.table)
x.dt <- rbindlist(x.dt_list, fill = TRUE, idcol = T)
y.df_loc_spec <- 
  data.frame(special_character, x.dt) |> 
  rename(row =.id,
         mark = start) |>
  select(-end) |> 
  mutate(special_character_id = row_number())

rm(list = ls(pattern = "x."))
rm(special_character)

y.spec1 <-
  y.df_loc_spec |> 
  mutate(row = row - 1)

y.spec2 <-
  y.df_loc_spec |> 
  mutate(row = row + 1)

y.spec3 <-
  y.df_loc_spec |> 
  mutate(mark = mark - 1)

y.spec4 <-
  y.df_loc_spec |> 
  mutate(mark = mark + 1)

y.spec5 <-
  y.df_loc_spec |> 
  mutate(row = row - 1,
         mark = mark - 1)

y.spec6 <-
  y.df_loc_spec |> 
  mutate(row = row - 1,
         mark = mark + 1)

y.spec7 <-
  y.df_loc_spec |> 
  mutate(row = row + 1,
         mark = mark - 1)

y.spec8 <-
  y.df_loc_spec |> 
  mutate(row = row + 1,
         mark = mark + 1)

df_loc_spec <- bind_rows(y.spec1, y.spec2, y.spec3, y.spec4, y.spec5, y.spec6, y.spec7, y.spec8, y.df_loc_spec) |> 
  filter(special_character == "*")

rm(list = ls(pattern = "y."))

gears <- 
  df_loc_num |> 
  inner_join(df_loc_spec, by = 'row', relationship = "many-to-many") |> 
  mutate(include = if_else(mark <= end & mark >= start, 1, 0)) |>
  group_by(special_character_id, part_number_id, part_number) |>
  summarize(include = max(include)) |>
  ungroup() |>
  filter(include == 1) 

gears |> 
  group_by(special_character_id) |> 
  summarize(is_gear = n()) |> 
  ungroup() |> 
  filter(is_gear == 2) |> 
  inner_join(gears, by = 'special_character_id') |> 
  select(special_character_id, part_number) |> 
  group_by(special_character_id) |> 
  mutate(row = row_number()) |> 
  ungroup() |> 
  pivot_wider(names_from = row, values_from = part_number, names_prefix = 'part') |> 
  mutate(gear_ratio = part1 * part2) |> 
  summarize(sum = sum(gear_ratio)) |> 
  pull()


  
  summarize(sum = sum(part_number)) |> 
  pull()

  
  