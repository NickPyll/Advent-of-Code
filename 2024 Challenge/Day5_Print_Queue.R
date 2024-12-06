# --- Day 5: Print Queue ---

library(tidyverse)

# input <- readLines("2024 Challenge/data/input_day5_test.txt")
input <- readLines("2024 Challenge/data/input_day5.txt")

empty_element <- which(input == "")

rules <- input[1:(empty_element - 1)]
update_list <- lapply(input[(empty_element + 1):length(input)], function(x) strsplit(x, ",")[[1]])

generate_revpairs <- function(x) {
  elements <- strsplit(x, ",")[[1]]
  revpairs <- combn(elements, 2, FUN = function(y) paste(rev(y), collapse = "|"))
  return(as.list(revpairs))
}

revpair_list <- lapply(input[(empty_element + 1):length(input)], generate_revpairs)

answer <- 0

for (i in 1:length(update_list)) {
  order_fail <- 0

  for (j in 1:length(revpair_list[[i]])) {
    if (revpair_list[[i]][[j]] %in% rules) {
      order_fail <- 1
      break
    }
  }
  if (order_fail == 0) {
    middle <- as.numeric(update_list[[i]][[ceiling(length(update_list[[i]]) / 2)]])
    answer <- answer + middle
  }
}
