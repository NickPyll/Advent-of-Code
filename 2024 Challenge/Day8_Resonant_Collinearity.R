# --- Day 8: Resonant Collinearity ---

library(tidyverse)

# file <- "2024 Challenge/data/input_day8_test.txt"
file <- "2024 Challenge/data/input_day8.txt"

map <- do.call(rbind, strsplit(readLines(file), split = ""))

# loc_A <- which(map == "A", arr.ind = TRUE) |> as.data.frame()
# loc_0 <- which(map == "0", arr.ind = TRUE) |> as.data.frame()

range_r <- nrow(map)
range_c <- ncol(map)

antennae <- map |>
  as.vector() |>
  table() |>
  (\(x) names(x[x > 1 & names(x) != "."]))()

# Part 1

# antinodes <-
#   data.frame(
#     arow1 = as.numeric(),
#     acol1 = as.numeric(),
#     arow2 = as.numeric(),
#     acol2 = as.numeric())

# for(i in antennae){
#   x.df <- which(map == i, arr.ind = TRUE) |> as.data.frame()

#   x.antinodes <-
#     x.df |>
#     rename(row_1 = row, col_1 = col) |>
#     cross_join(
#       x.df |>
#         rename(row_2 = row, col_2 = col)) |>
#     filter(!(row_1 == row_2 & col_1 == col_2)) |>
#     mutate(
#       row_diff = row_2 - row_1,
#       col_diff = col_2 - col_1,
#       arow1 = row_1 - row_diff,
#       acol1 = col_1 - col_diff,
#       arow2 = row_2 + row_diff,
#       acol2 = col_2 + col_diff
#     ) |>
#     select(starts_with('arow'), starts_with('acol'))

#   antinodes <- antinodes |> bind_rows(x.antinodes)
# }

# antinodes |>
#   pivot_longer(
#     cols = everything(),
#     names_to = c(".value", "type"),
#     names_pattern = "a(\\w+)(\\d+)") |>
#   as.data.frame() |>
#   filter(row > 0, col > 0, row <= range_r, col <= range_c) |>
#   distinct(row, col) |>
#   tally()

# Part 2

antinodes <-
  data.frame(
    row = as.numeric(),
    col = as.numeric()
  )

for (i in antennae) {
  # i <- 'A'

  x.df <- which(map == i, arr.ind = TRUE) |> as.data.frame()

  antinodes <- antinodes |> bind_rows(x.df)

  x.pairs <-
    x.df |>
    mutate(key = row_number()) |>
    full_join(
      x.df |>
        mutate(key = row_number()),
      by = character(), suffix = c("1", "2")
    ) |>
    filter(key1 < key2) |>
    select(-key1, -key2) |>
    mutate(
      row_diff = row2 - row1,
      col_diff = col2 - col1
    )

  for (j in 1:nrow(x.pairs)) {
    # j <- 1

    new_row <- x.pairs$row1[j]
    new_col <- x.pairs$col1[j]
    k <- 1

    while (new_row > 0 & new_col > 0 & new_row <= range_r & new_col <= range_c) {
      new_row <- new_row - x.pairs$row_diff[j]
      new_col <- new_col - x.pairs$col_diff[j]

      antinodes <- rbind(antinodes, data.frame(row = new_row, col = new_col))

      k <- k + 1
    }

    new_row <- x.pairs$row2[j]
    new_col <- x.pairs$col2[j]

    while (new_row > 0 & new_col > 0 & new_row <= range_r & new_col <= range_c) {
      new_row <- new_row + x.pairs$row_diff[j]
      new_col <- new_col + x.pairs$col_diff[j]

      antinodes <- rbind(antinodes, data.frame(row = new_row, col = new_col))

      k <- k + 1
    }
  }
}

antinodes |>
  filter(row > 0, col > 0, row <= range_r, col <= range_c) |>
  distinct(row, col) |>
  tally()
