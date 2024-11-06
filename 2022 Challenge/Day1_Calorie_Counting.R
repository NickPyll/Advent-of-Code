# Day 1 Calorie Counting ----
## Part 1 ----
# Which elf has the most calories?

library(tidyverse)

# calorie_list <- readLines('2022 Challenge/data/input_day1_test.txt')
calorie_list <- readLines('2022 Challenge/data/input_day1.txt')

calorie_list <- append(calorie_list, '')

elf_df <-
  data.frame(
    elf_id = integer(),
    calories = integer()
  )

elf_id <- 1
calories <- 0

for (i in 1:length(calorie_list)){
  
  if(calorie_list[i] != '' ){
    calories <- calories + as.numeric(calorie_list[i])
  } else{
    elf_df <- rbind(elf_df, data.frame(elf_id, calories))
    elf_id <- elf_id + 1
    calories <- 0
  }

}

# pt 1 answer
elf_df |> 
  arrange(calories) |> 
  tail(1) |> 
  select(calories)

# pt 2 answer
elf_df |> 
  arrange(calories) |> 
  tail(3) |>
  summarize(calories = sum(calories))
