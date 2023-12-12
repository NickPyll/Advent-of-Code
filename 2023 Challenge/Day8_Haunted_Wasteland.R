# Day 8 Haunted Wasteland----
## Part 1 ----
# 
# 

library(tidyverse)
library(pracma)

file <- 'data/input_day8.txt'

commands <- read.csv(file, header = FALSE, nrows = 1) |> pull()
commands <- strsplit(str_dup(commands, 100000), "")[[1]]

coords <- read.csv(file, sep = " ", header = FALSE, skip = 1) |> 
  select(-V2) |> 
  rename(start_location = V1,
         L = V3,
         R = V4) |> 
  mutate_all(funs(gsub("[[:punct:]]", "", .))) |> 
  pivot_longer(!start_location, names_to = 'command', values_to = 'end_location')

# Part 1 start
# current <- 'AAA'
# step <- 1

current_list <- 
  coords |> 
  filter(grepl("A$", start_location)) |> 
  distinct(start_location) |> 
  pull(start_location)

# Part 2
for(j in 1:length(current_list)){
  
  current <- current_list[[j]]
  step <- 1
  
  for(i in 1:length(commands)){
    
    # print(j)
    # print(current)
  
    do <- commands[[i]]
  
    # print(do)
    
    x.current <-
      coords |>
      filter(start_location == current,
             command == do) |>
      pull(end_location)
    
    # print(x.current)

    current <- x.current

    if(str_sub(current, -1, -1) == 'Z'){
      print(step)
      break
    }

    step <- step + 1

  }

}

answer <- mLCM(c(21409,11653,19241,12737,14363,15989))

# # Part 1
# for(i in 1:length(commands)){
#   
#   do <- commands[[i]]
#   
#   x.current <-
#     coords |>
#     filter(start_location == current,
#            command == do) |>
#     pull(end_location)
# 
#   current <- x.current
# 
#   if(current == 'ZZZ'){
#     break
#   }
# 
#   step <- step + 1
#   
# }
