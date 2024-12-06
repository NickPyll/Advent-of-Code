--- Day 4: Giant Squid ---

library(tidyverse)

called_numbers <- 
  as.integer(strsplit(readLines("2021 Challenge/data/input_day4_test.txt", n = 1), ",")[[1]])

boards <-
  read.table("2021 Challenge/data/input_day4_test.txt", skip = 2)

n_boards <- nrow(boards) / 5
b <- 0

board_list <- list()

for(board in 1:n_boards){
  x.df <- boards |> 
    filter(
      row_number() > b,
      row_number() <= board * 5
    )
  
  b <- b + 5

  board_list[[board]] <- x.df
}

result_list <- list()

for(board in 1:n_boards){
  x.df <- data.frame(matrix(1, nrow = 5, ncol = 5))  
  result_list[[board]] <- x.df
}

for(i in 1:length(called_numbers)){
  call <- called_numbers[[i]]

  for(i in 1:n_boards){
    # locate if and where the called number is on each board
    # mark that same coordinate as a 0 on the corresponding result board
  }

  # check sums -- look for sums of 0s

}


