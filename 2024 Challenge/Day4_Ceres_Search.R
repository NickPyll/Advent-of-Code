# --- Day 4: Ceres Search ---

library(tidyverse)

# input <- do.call(rbind, strsplit(readLines("2024 Challenge/data/input_day4_test.txt"), split = ""))
input <- do.call(rbind, strsplit(readLines("2024 Challenge/data/input_day4.txt"), split = ""))

num_rows <- nrow(input)
num_cols <- ncol(input)

xmas_search <- function(word) {
  word <- paste0(word, collapse = "")
  if (word == "XMAS") {
    xmas_add <- 1
  } else {
    xmas_add <- 0
  }
}

xmas_count <- 0

for (r in 1:num_rows) {
  for (c in 1:num_cols) {
    # down
    if (r <= num_rows - 3) {
      xmas_count <- xmas_count + xmas_search(input[r:(r + 3), c])
    }

    # up
    if (r >= 4) {
      xmas_count <- xmas_count + xmas_search(input[r:(r - 3), c])
    }

    # right
    if (c <= num_cols - 3) {
      xmas_count <- xmas_count + xmas_search(input[r, c:(c + 3)])
    }

    # left
    if (c >= 4) {
      xmas_count <- xmas_count + xmas_search(input[r, c:(c - 3)])
    }

    # down right
    if (c <= num_cols - 3 & r <= num_rows - 3) {
      xmas_count <- xmas_count + xmas_search(diag(input[r:(r + 3), c:(c + 3)]))
    }

    # down left
    if (c >= 4 & r <= num_rows - 3) {
      xmas_count <- xmas_count + xmas_search(diag(input[r:(r + 3), c:(c - 3)]))
    }

    # up left
    if (c >= 4 & r >= 4) {
      xmas_count <- xmas_count + xmas_search(diag(input[r:(r - 3), c:(c - 3)]))
    }

    # up right
    if (c <= (num_rows - 3) & r >= 4) {
      xmas_count <- xmas_count + xmas_search(diag(input[r:(r - 3), c:(c + 3)]))
    }
    # print(paste(r, c, xmas_count))
  }
}

sam_results <- data.frame(r_center = as.numeric(), c_center = as.numeric())

for (r in 1:num_rows) {
  for (c in 1:num_cols) {
    # down right
    if (c <= num_cols - 2 & r <= num_rows - 2) {
      word <- paste0(diag(input[r:(r + 2), c:(c + 2)]), collapse = "")
      if (word == "MAS" | word == "SAM") {
        sam_results[nrow(sam_results) + 1, ] <- c(r + 1, c + 1)
      }
    }

    # down left
    if (c >= 3 & r <= num_rows - 2) {
      word <- paste0(diag(input[r:(r + 2), c:(c - 2)]), collapse = "")
      if (word == "MAS" | word == "SAM") {
        sam_results[nrow(sam_results) + 1, ] <- c(r + 1, c - 1)
      }
    }
  }
}

sam_results |>
  group_by(r_center, c_center) |>
  summarize(count = n(), .groups = "drop") |>
  filter(count == 2) |>
  tally()
