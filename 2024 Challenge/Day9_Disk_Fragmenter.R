# --- Day 9: Disk Fragmenter ---

library(tidyverse)

file <- "2024 Challenge/data/input_day9_test.txt"

disk_map <- do.call(rbind, strsplit(readLines(file), split = ""))


# create block map
blocks_map <- c()

for (i in 1:(length(disk_map))) {
  num_blocks <- as.numeric(disk_map[i])

  if (num_blocks > 0) {
    if ((i %% 2) == 0) {
      id <- "."
    } else {
      id <- floor(i / 2)
    }

    blocks <- paste(rep(id, num_blocks), collapse = "")
    blocks_map <- c(blocks_map, blocks)
  }
}
blocks_map_string <- paste(blocks_map, collapse = "")

# reorder block map
reversed_digits <- strsplit(gsub("[^0-9]", "", blocks_map_string), "")[[1]] |> rev()
compacted_files <- ""
digit_index <- 1

for (char in strsplit(blocks_map_string, "")[[1]]) {
  if (nchar(compacted_files) < length(reversed_digits)) {
    if (char == ".") {
      compacted_files <- paste0(compacted_files, reversed_digits[digit_index])
      digit_index <- digit_index + 1
    } else {
      compacted_files <- paste0(compacted_files, char)
    }
  }
}

ids <- as.numeric(strsplit(compacted_files, "")[[1]])
positions <- seq_along(ids) - 1
result <- sum(ids * positions)
