# Day 9 Mirage Maintenance----
## Part 1 ----

library(tidyverse)

file <- 'data/input_day9.txt'

value_list <- read.csv(file, header = FALSE) |> pull()

next_value <- list()

# Part 1
for(i in 1:length(value_list)){

  initial_value <- as.numeric(strsplit(value_list, ' ')[[i]])
  zero_check <- 1
  next_value[[i]] <- list()

  for(j in 1:length(initial_value)){

    next_value[[i]][[j]] <- tail(initial_value, n = 1)
    initial_value <- diff(initial_value)
    zero_check <- if_else(max(initial_value) == 0 & min(initial_value) == 0, 0, 1)

    if(zero_check == 0){
      break
    }
  }
}

print(paste('Part 1 answer: ', sum(unlist(next_value))))

# Part 2

for(i in 1:length(value_list)){
  
  initial_value <- as.numeric(strsplit(value_list, ' ')[[i]])
  zero_check <- 1
  next_value[[i]] <- list()
  
  for(j in 1:length(initial_value)){
    
    next_value[[i]][[j]] <- head(initial_value, n = 1)
    initial_value <- diff(initial_value)
    next_value[[i]][[j+1]] <- head(initial_value, n = 1)
    
    zero_check <- if_else(max(initial_value) == 0 & min(initial_value) == 0, 0, 1)
    
    if(zero_check == 0){
      break
    }
  }
}

answer <- list()

for(i in 1:length(next_value)){
  
  value_list <- next_value[[i]]
  subtract <- 0
  
  for(j in 1:length(value_list) - 1){
    
    subtract_from <- value_list[[length(value_list) - j]]
    answer[i] <- subtract_from - subtract

    if(j == length(value_list)){
      break
    }
    
    subtract <- answer[[i]]
  }
}

print(paste('Part 1 answer: ', sum(unlist(answer))))


