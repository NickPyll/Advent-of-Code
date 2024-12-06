# Day 18 Lavaduct Lagoon----
## Part 1 ----

library(tidyverse)

input <- read.table("2023 Challenge/data/input_day18_test.txt")

direction_list <- input |> pull(V1)
dig_count <- input |> pull(V2)

# dug <- list(list())

# initialize

dug <- data.frame(r = as.numeric(), c = as.numeric())

rval <- 1
cval <- 1

for (i in 1:14) {
  if (direction_list[[i]] == "R") {
    for (j in 1:dig_count[[i]]) {
      cval <- cval + 1
      dug <-
        dug |>
        add_row(r = rval, c = cval)
    }
  } else if (direction_list[[i]] == "D") {
    for (j in 1:dig_count[[i]]) {
      rval <- rval + 1
      dug <-
        dug |>
        add_row(r = rval, c = cval)
    }
  } else if (direction_list[[i]] == "L") {
    for (j in 1:dig_count[[i]]) {
      cval <- cval - 1
      dug <-
        dug |>
        add_row(r = rval, c = cval)
    }
  } else if (direction_list[[i]] == "U") {
    for (j in 1:dig_count[[i]]) {
      rval <- rval - 1
      dug <-
        dug |>
        add_row(r = rval, c = cval)
    }
  }
}

max_r <- max(dug$r)
max_c <- max(dug$c)

dug_matrix <- matrix(0, nrow = max_r, ncol = max_c)

for (i in seq_len(nrow(dug))) {
  dug_matrix[dug$r[i], dug$c[i]] <- 1
}
