# Day 4 Scratchcards----
## Part 1 ----
# Match game numbers with winning number
# Point Total equals 2^(match - 1)
# Add up all card point totals

library(tidyverse)

# input <-
#   c('Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53',
#     'Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19',
#     'Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1',
#     'Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83',
#     'Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36',
#     'Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11'
#   ) |>
#   data.frame() |>
#   setNames('V1') |>
#   separate(V1, c('CardID', 'V1'), ":") |>
#   separate(V1, c('winning_nums', 'playing_nums'), sep = "\\|")

input <- read.csv('data/input_day4.txt',
                  sep = ":",
                  header = FALSE,
                  col.names = c('CardID', 'V1')) |>
  separate(V1, c('winning_nums', 'playing_nums'), sep = "\\|")

winning_df <-
  input |> 
  select(CardID, winning_nums) |> 
  mutate(nums = strsplit(winning_nums, " ")) |> 
  unnest(nums) |> 
  mutate(nums = as.numeric(nums)) |> 
  filter(!is.na(nums))

playing_df <-
  input |> 
  select(CardID, playing_nums) |> 
  mutate(nums = strsplit(playing_nums, " ")) |> 
  unnest(nums) |> 
  mutate(nums = as.numeric(nums)) |> 
  filter(!is.na(nums))

winning_df |> 
  inner_join(playing_df, by = c('CardID', 'nums')) |> 
  group_by(CardID) |> 
  summarize(matches = n()) |> 
  mutate(points = 2^(matches-1)) |> 
  summarize(sum = sum(points)) |> 
  pull()

## Part 2 ----
# Count winning numbers on a card
# Copy the following cards
# How many total cards

matches <-
  winning_df |> 
  select(CardID) |> 
  distinct() |> 
  left_join(winning_df |> 
              inner_join(playing_df, by = c('CardID', 'nums')) |> 
              group_by(CardID) |> 
              summarize(matches = n()),
            by = 'CardID') |> 
  mutate(across(everything(), .fns = ~replace_na(.,0))) |> 
  mutate(CardID = row_number()) |> 
  mutate(cards = 1)

for(i in 1:nrow(matches)){

  copy_count <- matches$matches[[i]]
  
  cards <- matches$cards[[i]]
  
  if(copy_count > 0){
    for(j in 1:cards){
      for(k in 1:copy_count){
        
        matches[i+k, 3] <- matches[i+k, 3] + 1
        # print(paste("i =", i, ". j =", j, ". k =", k))
        # print(copy_count)
        # print(matches)
        print(i)
      }
    }
    

  }
}

sum(matches$cards)
