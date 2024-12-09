# --- Day 6: Guard Gallivant ---

library(tidyverse)

# map <- do.call(rbind, strsplit(readLines("2024 Challenge/data/input_day6_test.txt"), split = ""))
map <- do.call(rbind, strsplit(readLines("2024 Challenge/data/input_day6.txt"), split = ""))

# Part 1

guard_direction <- "north"
guard_position <- which(map == "^", arr.ind = TRUE)
guard_row <- guard_position[[1]]
guard_col <- guard_position[[2]]

num_rows <- nrow(map)
num_cols <- ncol(map)

path <- data.frame(guard_row, guard_col)
out <- FALSE

while (out == FALSE) {
  if (guard_direction == "north") {
    if (guard_row == 1) {
      out <- TRUE
      break
    } else if (map[guard_row - 1, guard_col] == "#") {
      guard_direction <- "east"
    } else {
      guard_row <- guard_row - 1
      path <- path |> bind_rows(data.frame(guard_row, guard_col))
    }
  }

  if (guard_direction == "east") {
    if (guard_col == num_cols) {
      out <- TRUE
      break
    } else if (map[guard_row, guard_col + 1] == "#") {
      guard_direction <- "south"
    } else {
      guard_col <- guard_col + 1
      path <- path |> bind_rows(data.frame(guard_row, guard_col))
    }
  }

  if (guard_direction == "south") {
    if (guard_row == num_rows) {
      out <- TRUE
      break
    } else if (map[guard_row + 1, guard_col] == "#") {
      guard_direction <- "west"
    } else {
      guard_row <- guard_row + 1
      path <- path |> bind_rows(data.frame(guard_row, guard_col))
    }
  }

  if (guard_direction == "west") {
    if (guard_col == 1) {
      out <- TRUE
      break
    } else if (map[guard_row, guard_col - 1] == "#") {
      guard_direction <- "north"
    } else {
      guard_col <- guard_col - 1
      path <- path |> bind_rows(data.frame(guard_row, guard_col))
    }
  }
}

guard_path <-
  path |>
  distinct(guard_row, guard_col)

# answer_1 <- guard_path |> tally()

# Part 2

obstruction_options <- 0

# iterate through each obstruction (can't be first position -- guard already there)
for (i in 2:nrow(guard_path)) {
  print(i)

  # reset initial guard position for next obstruction
  guard_direction <- "north"
  guard_row <- guard_position[[1]]
  guard_col <- guard_position[[2]]

  # create empty data frame to store new path
  path_mod <- data.frame(guard_row, guard_col, guard_direction)

  # initialize exit parameters
  out <- FALSE
  loop <- FALSE

  # reset and modify map with obstruction
  map_mod <- map
  map_mod[guard_path$guard_row[i], guard_path$guard_col[i]] <- "#"

  while (out + loop == 0) {
    if (guard_direction == "north") {
      if (guard_row == 1) {
        out <- TRUE
      } else if (map_mod[guard_row - 1, guard_col] == "#") {
        guard_direction <- "east"
      } else {
        guard_row <- guard_row - 1
        path_mod <- path_mod |> bind_rows(data.frame(guard_row, guard_col, guard_direction))

        path_count <- nrow(path_mod)
        path_distinct <- path_mod |>
          distinct() |>
          tally() |>
          pull()

        loop <- path_distinct < path_count

        if (loop == TRUE) {
          obstruction_options <- obstruction_options + 1
          break
        }
      }
    }

    if (guard_direction == "east") {
      if (guard_col == num_cols) {
        out <- TRUE
      } else if (map_mod[guard_row, guard_col + 1] == "#") {
        guard_direction <- "south"
      } else {
        guard_col <- guard_col + 1
        path_mod <- path_mod |> bind_rows(data.frame(guard_row, guard_col, guard_direction))

        path_count <- nrow(path_mod)
        path_distinct <- path_mod |>
          distinct() |>
          tally() |>
          pull()

        loop <- path_distinct < path_count

        if (loop == TRUE) {
          obstruction_options <- obstruction_options + 1
          break
        }
      }
    }

    if (guard_direction == "south") {
      if (guard_row == num_rows) {
        out <- TRUE
      } else if (map_mod[guard_row + 1, guard_col] == "#") {
        guard_direction <- "west"
      } else {
        guard_row <- guard_row + 1
        path_mod <- path_mod |> bind_rows(data.frame(guard_row, guard_col, guard_direction))

        path_count <- nrow(path_mod)
        path_distinct <- path_mod |>
          distinct() |>
          tally() |>
          pull()

        loop <- path_distinct < path_count

        if (loop == TRUE) {
          obstruction_options <- obstruction_options + 1
          break
        }
      }
    }

    if (guard_direction == "west") {
      if (guard_col == 1) {
        out <- TRUE
      } else if (map_mod[guard_row, guard_col - 1] == "#") {
        guard_direction <- "north"
      } else {
        guard_col <- guard_col - 1
        path_mod <- path_mod |> bind_rows(data.frame(guard_row, guard_col, guard_direction))

        path_count <- nrow(path_mod)
        path_distinct <- path_mod |>
          distinct() |>
          tally() |>
          pull()

        loop <- path_distinct < path_count

        if (loop == TRUE) {
          obstruction_options <- obstruction_options + 1
          break
        }
      }
    }
  }
}
