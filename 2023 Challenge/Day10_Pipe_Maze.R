# Day 10 Pipe Maze----
## Part 1 ----

library(tidyverse)
library(data.table)

file <- 'data/input_day10.txt'

full_board <- as.list(unlist(read.table(file)))

row_S <- which(grepl("S", full_board) == TRUE)
column_S <- unlist(gregexpr('S', full_board[grepl("S", full_board)]))

up_S <- str_sub(full_board[[row_S - 1]], start = column_S, end = column_S)
down_S <- str_sub(full_board[[row_S + 1]], start = column_S, end = column_S)
right_S <- str_sub(full_board[[row_S]], start = column_S + 1, end = column_S + 1)
left_S <- str_sub(full_board[[row_S]], start = column_S - 1, end = column_S - 1)

S <- 0
step <- 1
start_row <- row_S
start_column <- column_S
next_step <- right_S   
direction <- 'E'

while(next_step != 'S'){
  
  print(paste0('Step ', step, ': you have a ', next_step, '. Start at ', start_row, ', ', start_column, ' and move ', direction))

  if(direction == 'S' & next_step == '|'){
    start_row <- start_row + 1
    direction <- 'S'
  } else if(direction == 'S' & next_step == 'J'){
    start_column <- start_column - 1
    direction <- 'W'
  } else if(direction == 'S' & next_step == 'L'){
    start_column <- start_column + 1
    direction <- 'E'
  } else if(direction == 'N' & next_step == '|'){
    start_row <- start_row - 1
    direction <- 'N'
  } else if(direction == 'N' & next_step == 'F'){
    start_column <- start_column + 1
    direction <- 'E'
  } else if(direction == 'N' & next_step == '7'){
    start_column <- start_column - 1
    direction <- 'W'
  } else if(direction == 'E' & next_step == '-'){
    start_column <- start_column + 1
    direction <- 'E'
  } else if(direction == 'E' & next_step == '7'){
    start_row <- start_row + 1
    direction <- 'S'
  } else if(direction == 'E' & next_step == 'J'){
    start_row <- start_row - 1
    direction <- 'N'
  } else if(direction == 'W' & next_step == '-'){
    start_column <- start_column - 1
    direction <- 'W'
  } else if(direction == 'W' & next_step == 'F'){
    start_row <- start_row + 1
    direction <- 'S'
  } else if(direction == 'W' & next_step == 'L'){
    start_row <- start_row - 1
    direction <- 'N'
  }
  next_step <- str_sub(full_board[[start_row + 1]], start = start_column, end = start_column)
  step <- step + 1
  
}

step / 2
