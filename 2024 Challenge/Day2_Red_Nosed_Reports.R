# --- Day 2: Red-Nosed Reports ---

library(tidyverse)

# input <- lapply(as.list(unlist(readLines("2024 Challenge/data/input_day2_test.txt"))), function(x) as.numeric(strsplit(x, " ")[[1]]))
input <- lapply(as.list(unlist(readLines("2024 Challenge/data/input_day2.txt"))), function(x) as.numeric(strsplit(x, " ")[[1]]))

# Part 1

safe_count <- 0

for (row in 1:length(input)) {
  report_diff <- diff((input[[row]]))

  if (max(abs(report_diff)) <= 3) {
    if (sum(report_diff > 0) == length(report_diff) |
      sum(report_diff < 0) == length(report_diff)) {
      safe_count <- safe_count + 1
    }
  }
}

# Part 2

safe_count <- 0

for (row in 1:length(input)) {
  report <- as.list(unlist(input[[row]]))

  report_safe <- 0

  for (element in 1:length(report)) {
    report[[element]] <- NULL

    report_diff <- diff(unlist(report))

    if (max(abs(report_diff)) <= 3) {
      if (sum(report_diff > 0) == length(report_diff) |
        sum(report_diff < 0) == length(report_diff)) {
        report_safe <- report_safe + 1

        status <- "safe"
      } else {
        status <- "unsafe"
      }
    } else {
      status <- "unsafe"
    }

    report <- as.list(unlist(input[[row]]))
  }

  if (report_safe > 0) {
    safe_count <- safe_count + 1
  }
}
